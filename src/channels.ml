(*
 * Copyright (c) 2012 Citrix Inc
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open Lwt

type t = {
  read: Cstruct.t -> Cstruct.t Lwt.t;
  really_read: Cstruct.t -> unit Lwt.t;
  write: Cstruct.t -> Cstruct.t Lwt.t;
  really_write: Cstruct.t -> unit Lwt.t;
  really_write_offset: int64 ref;
  skip: int64 -> unit Lwt.t;
  close: unit -> unit Lwt.t
}

exception Impossible_to_seek

let debug_io = ref false

let complete name offset op fd buffer =
  if !debug_io
  then Printf.fprintf stderr "%s offset=%s length=%d\n%!" name (match offset with Some x -> Int64.to_string x | None -> "None") (Cstruct.len buffer);
  let open Lwt in
  let ofs = buffer.Cstruct.off in
  let len = buffer.Cstruct.len in
  let buf = buffer.Cstruct.buffer in
  let rec loop acc fd buf ofs len =
    op fd buf ofs len >>= fun n ->
    let len' = len - n in
    let acc' = acc + n in
    if len' = 0 || n = 0
    then return acc'
    else loop acc' fd buf (ofs + n) len' in
  loop 0 fd buf ofs len >>= fun n ->
  if n = 0 && len <> 0
  then fail End_of_file
  else return ()


let of_raw_fd fd =
  let really_write_offset = ref 0L in
  let read buf =
    Lwt_bytes.read fd buf.Cstruct.buffer buf.Cstruct.off buf.Cstruct.len >>= fun n ->
    return (Cstruct.sub buf 0 n) in
  let write buf =
    Lwt_bytes.write fd buf.Cstruct.buffer buf.Cstruct.off buf.Cstruct.len >>= fun n ->
    really_write_offset := Int64.(add !really_write_offset (of_int n));
    return (Cstruct.shift buf n) in
  let really_read = complete "read" None Lwt_bytes.read fd in
  let really_write buf =
    complete "write" (Some !really_write_offset) Lwt_bytes.write fd buf >>= fun () ->
    really_write_offset := Int64.(add !really_write_offset (of_int (Cstruct.len buf)));
    return () in
  let skip _ = fail Impossible_to_seek in
  let close () = Lwt_unix.close fd in
  return { read; write; really_read; really_write; really_write_offset; skip; close }

let of_seekable_fd fd =
  of_raw_fd fd >>= fun c ->
  let skip n =
    Lwt_unix.LargeFile.lseek fd n Unix.SEEK_CUR >>= fun offset ->
    c.really_write_offset := offset;
    return () in
  return { c with skip }

let _ = Ssl.init ()

let default_good_ciphers = "!EXPORT:RSA+AES128-SHA256"
let default_legacy_ciphers = "RSA+AES256-SHA:RSA+AES128-SHA:RSA+RC4-SHA:RSA+DES-CBC3-SHA"

let sslctx_cached = ref None

let gen_sslctx legacy ciphers =
  let ctx =
    (* According to the documentation in ssl.mli, here SSLv23 means
     * "accept all possible protocols (SSLv2 if supported by openssl,
     *  SSLv3, TLSv1, TLSv1.1 and TLSv1.2)"
     * whereas TLSv1_2 means "only TLS v1.2 protocol" *)
    Ssl.create_context
      (if legacy then Ssl.SSLv23 else Ssl.TLSv1_2)
      Ssl.Client_context
  in
  if legacy then (
    (* We created the context with all protocols enabled, so we must disable
     * the ones we do not want. In the Ssl module, SSLv23 has a different
     * meaning for the disable_protocols function: in ssl.mli the documentation
     * for this function says, "Note that [SSLv23] disables both SSLv2 and
     * SSLv3 (as opposed to all the protocols)."
     *)
    (* Disable SSL v2 and v3, leaving only TLSv1.0 and TLSv1.1 and TLSv1.2 *)
    (* We don't need 1.1, but if we add it to the list then 1.2 gets disabled
     * too: a bug in the Ssl module v0.5.2 (or the libssl it is using) *)
    Ssl.disable_protocols ctx [Ssl.SSLv23];
  );
  Ssl.set_cipher_list ctx (match ciphers with
    | Some c -> c
    | None -> (if legacy then (default_good_ciphers^":"^default_legacy_ciphers)
      else default_good_ciphers)
  );
  ctx

let sslctx legacy ciphers =
  match !sslctx_cached with
    | Some ctx -> ctx
    | None -> (
      let ctx = gen_sslctx legacy ciphers in
      sslctx_cached := Some ctx;
      ctx
    )

(* The first call to this sets up the TLS configuration, then in
 * any subsequent calls fd is the only parameter that is used. *)
let of_ssl_fd ~legacy ?ciphers fd =
  let really_write_offset = ref 0L in
  Lwt_ssl.ssl_connect fd (sslctx legacy ciphers) >>= fun sock ->
  let read buf =
    Lwt_ssl.read_bytes sock buf.Cstruct.buffer buf.Cstruct.off buf.Cstruct.len >>= fun n ->
    return (Cstruct.sub buf 0 n) in
  let write buf =
    Lwt_ssl.write_bytes sock buf.Cstruct.buffer buf.Cstruct.off buf.Cstruct.len >>= fun n ->
    really_write_offset := Int64.(add !really_write_offset (of_int n));
    return (Cstruct.shift buf n) in
  let really_read = complete "read" None Lwt_ssl.read_bytes sock  in
  let really_write buf =
    complete "write" (Some !really_write_offset) Lwt_ssl.write_bytes sock buf >>= fun () ->
    really_write_offset := Int64.(add !really_write_offset (of_int (Cstruct.len buf)));
    return () in
  let skip _ = fail Impossible_to_seek in
  let close () =
    Lwt_ssl.close sock in
  return { read; write; really_read; really_write; really_write_offset; skip; close }


