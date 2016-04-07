(*
 * Copyright (C) 2006-2013 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

(* Simple CLI which remotes command-lines to xapi, and receives back
   commands to execute locally (print this string, ask this question,
   upload this file) *)

open Lwt

open Cli_protocol
open Cohttp
module Request = Request.Make(Cohttp_unbuffered_io)
module Response = Response.Make(Cohttp_unbuffered_io)

(* Param config priorities:
   explicit cmd option > XE_XXX env variable > ~/.xe rc file > default
*)

let xapiserver = ref "127.0.0.1"
let xapiuname = ref "root"
let xapipword = ref "null"
let xapipasswordfile = ref ""
let xapicompathost = ref "127.0.0.1"
let xapiport = ref None
let get_xapiport ssl =
  match !xapiport with
    None -> if ssl then 443 else 80
  | Some p -> p

let xeusessl = ref true
let allow_ssl_legacy = ref false
let ciphersuites = ref None
let xedebug = ref false
let xedebugonfail = ref false

let debug_channel = ref None
let debug_file = ref None

let heartbeat_version = 0.2
let heartbeat_interval = 300.

let long_connection_retry_timeout = 5.

let error fmt = Printf.fprintf stderr fmt
let debug fmt =
  let printer s = match !debug_channel with
    | Some c -> output_string c s
    | None -> () in
  Printf.kprintf printer fmt

(* usage message *)
exception Usage

let usage () =
    error "Usage: %s <cmd> [-s server] [-p port] ([-u username] [-pw password] or [-pwf <password file>]) <other arguments>\n" Sys.argv.(0);
    error "\nA full list of commands can be obtained by running \n\t%s help -s <server> -p <port>\n" Sys.argv.(0)

let is_localhost ip = ip = "127.0.0.1"

let startswith prefix x =
  let prefix' = String.length prefix in
  let x' = String.length x in
  x' >= prefix' && (String.sub x 0 prefix' = prefix)

let parse_url url =
  let uri = Uri.of_string url in
  match Uri.scheme uri, Uri.host uri with
  | Some "https", Some host ->
    host, Uri.path uri
  | _ ->
    !xapiserver, Uri.path uri

(* Read the password file *)
let read_pwf () =
  try
    let ic = open_in !xapipasswordfile in
    try
      xapiuname := (input_line ic);
      xapipword := (input_line ic)
    with End_of_file ->
      error "Error: password file format: expecting username on the first line, password on the second line\n";
      exit 1
  with _ ->
    error "Error opening password file '%s'\n" !xapipasswordfile;
    exit 1


let parse_port (x: string) =
  try
    let p = int_of_string x in
    if p < 0 || p > 65535 then failwith "illegal";
    p
  with _ ->
    error "Port number must be an integer (0-65535)\n";
    raise Usage

let filter_chars s valid =
  let badchars = ref false in
  let buf = Buffer.create 0 in
  for i = 0 to String.length s - 1 do
    if !badchars then (
      if valid s.[i] then Buffer.add_char buf s.[i]
    ) else (
      if not (valid s.[i]) then (
      Buffer.add_substring buf s 0 i;
      badchars := true
      )
    )
  done;
  if !badchars then Buffer.contents buf else s

(* Extract the arguments we're interested in. Return a list of the argumets we know *)
(* nothing about. These will get passed straight into the server *)
let parse_args =

  (* Set the key to the value. Return whether the key is one we know about *)
  (* compat mode is special as the argument is passed in two places. Once  *)
  (* at the top of the message to the cli server in order to indicate that *)
  (* we need to use 'geneva style' parsing - that is, allow key = value as *)
  (* opposed to key=value. Secondly, the key then gets passed along with   *)
  (* all the others to the operations. So we need to register it's there,  *)
  (* but not strip it                                                      *)

  let reserve_args = ref [] in

  let set_keyword (k,v) =
    try
      (match k with
       | "server" -> xapiserver := v
       | "port" -> xapiport := Some (parse_port v)
       | "username" -> xapiuname := v
       | "password" -> xapipword := v
       | "passwordfile" -> xapipasswordfile := v
       | "nossl"   -> xeusessl := not(bool_of_string v)
       | "allow-ssl-legacy" -> allow_ssl_legacy := (bool_of_string v)
       | "ciphersuites" -> ciphersuites := Some v
       | "debug" -> xedebug := (try bool_of_string v with _ -> false)
       | "debugonfail" -> xedebugonfail := (try bool_of_string v with _ -> false)
       | _ -> raise Not_found);
      true
    with Not_found -> false in

  let parse_opt args =
    match args with
    | "-s" :: server :: xs -> Some ("server", server, xs)
    | "-p" :: port :: xs -> Some("port", port, xs)
    | "-u" :: uname :: xs -> Some("username", uname, xs)
    | "-pw" :: pw :: xs -> Some("password", pw, xs)
    | "-pwf" :: pwf :: xs -> Some("passwordfile", pwf, xs)
    | "--nossl" :: xs -> Some("nossl", "true", xs)
    | "--allow-ssl-legacy" :: xs -> Some("allow-ssl-legacy", "true", xs)
    | "--ciphersuites" :: c :: xs -> Some("ciphersuites", c, xs)
    | "--debug" :: xs -> Some("debug", "true", xs)
    | "--debug-on-fail" :: xs -> Some("debugonfail", "true", xs)
    | "-h" :: h :: xs -> Some("server", h, xs)
    | _ -> None in

  let parse_eql arg =
    try
      let eq = String.index arg '=' in
      let k = String.sub arg 0 eq in
      let v = String.sub arg (eq+1) (String.length arg - (eq+1)) in
      Some (k,v)
    with _ -> None in

  let rec process_args = function
    | [] -> []
    | args ->
        match parse_opt args with
        | Some(k, v, rest) ->
            if set_keyword(k, v) then process_args rest else process_eql args
        | None ->
            process_eql args
  and process_eql = function
    | [] -> []
    | arg :: args ->
        match parse_eql arg with
        | Some(k, v) when set_keyword(k,v) -> process_args args
        | _ -> arg :: process_args args in

  fun args ->
    let rcs = Options.read_rc() in
    let rcs_rest =
      List.map (fun (k,v) -> k^"="^v)
        (List.filter (fun (k, v) -> not (set_keyword (k,v))) rcs) in
    let extras =
      let extra_args = try Sys.getenv "XE_EXTRA_ARGS" with Not_found -> "" in
      let l = ref [] and pos = ref 0 and i = ref 0 in
      while !pos < String.length extra_args do
        if extra_args.[!pos] = ',' then (incr pos; i := !pos)
        else
          if !i >= String.length extra_args
            || extra_args.[!i] = ',' && extra_args.[!i-1] <> '\\' then
              (let seg = String.sub extra_args !pos (!i - !pos) in
               l := filter_chars seg ((<>) '\\') :: !l;
               incr i; pos := !i)
          else incr i
      done;
      List.rev !l  in
    let extras_rest = process_args extras in
    let help = ref false in
    let args' = List.filter (fun s -> s<>"-help" && s <> "--help") args in
    if List.length args' < List.length args then help := true;
    let args_rest = process_args args in
    if !help then raise Usage;
    let () =
      if !xapipasswordfile <> "" then read_pwf ();
      if !xedebug then debug_channel := Some stderr;
      if !xedebugonfail then begin
        let tmpfile, tmpch = Filename.open_temp_file "xe_debug" "tmp" in
        debug_file := Some tmpfile;
        debug_channel := Some tmpch
      end in
    args_rest @ extras_rest @ rcs_rest @ !reserve_args

let socket sockaddr =
  let family = match sockaddr with
  | Lwt_unix.ADDR_INET(_, _) -> Unix.PF_INET
  | Lwt_unix.ADDR_UNIX _ -> Unix.PF_UNIX in
  Lwt_unix.socket family Unix.SOCK_STREAM 0

let open_tcp_ssl host =
  Lwt_unix.gethostbyname host >>= fun host_entry ->
  let sockaddr = Lwt_unix.ADDR_INET(host_entry.Lwt_unix.h_addr_list.(0), get_xapiport true) in
  let sock = socket sockaddr in
  Lwt_unix.connect sock sockaddr >>= fun () ->
  Channels.of_ssl_fd ~legacy:!allow_ssl_legacy ~ciphers:!ciphersuites sock

let open_tcp host =
  if !xeusessl && not(is_localhost host) then (* never use SSL on-host *)
    open_tcp_ssl host
  else begin
    Lwt_unix.gethostbyname host >>= fun host_entry ->
    let sockaddr = Lwt_unix.ADDR_INET(host_entry.Lwt_unix.h_addr_list.(0), get_xapiport false) in
    let sock = socket sockaddr in
    Lwt_unix.connect sock sockaddr >>= fun () ->
    Channels.of_raw_fd sock
  end

let open_channels () =
  if is_localhost !xapiserver then begin
    try_lwt
      let sockaddr = Lwt_unix.ADDR_UNIX (Filename.concat "/var/lib/xcp" "xapi") in
      let sock = socket sockaddr in
      Lwt_unix.connect sock sockaddr >>= fun () ->
      Channels.of_raw_fd sock
    with _ ->
      open_tcp !xapiserver
  end else
    open_tcp !xapiserver

let copy_with_heartbeat in_ch out_ch heartbeat_fun =
  let buffer = Cstruct.create (16 * 1024) in
  let copy_thread =
    let rec loop () =
      lwt input = in_ch.Channels.read buffer in
      if Cstruct.len input = 0
      then return ()
      else
        lwt () = out_ch.Channels.really_write input in
        loop () in
    loop () in
  let request_cancel, _ = Lwt.task () in
  let heartbeat_thread =
    while_lwt true do
      lwt () = Lwt.pick [ Lwt_unix.sleep heartbeat_interval; request_cancel ] in
      heartbeat_fun ()
    done in
  lwt t = copy_thread in
  (* We mustn't interrupt the heartbeat_fun since that might
     leave the channel in a corrupted state *)
  Lwt.cancel request_cancel;
  lwt t = try_lwt heartbeat_thread with Lwt.Canceled -> return () in
  return ()

exception Http_failure
exception Connect_failure
exception Protocol_version_mismatch of string
exception ClientSideError of string
exception Unexpected_msg of message
exception Server_internal_error

let handle_unmarshal_failure ex c = match ex with
  | Unmarshal_failure (e, s) ->
    debug "Read: %s\n" s;
    if String.length s >= 4 && String.uppercase (String.sub s 0 4) = "HTTP"
    then fail Server_internal_error
    else fail e
  | e -> fail e

let main_loop control =
  (* Intially exchange version information *)
  lwt major', minor' =
    try_lwt unmarshal_protocol control with
    | Unmarshal_failure (_, "") -> fail Connect_failure
    | e -> handle_unmarshal_failure e control in
  let msg = Printf.sprintf "Server has protocol version %d.%d. Client has %d.%d" major' minor' major minor in
  debug "%s\n%!" msg;
  lwt () =
    if major' <> major
    then fail (Protocol_version_mismatch msg)
    else return () in
  let with_heartbeat =
    major' * 10 + minor' >= int_of_float (heartbeat_version *. 10.) in
  let heartbeat_fun =
    if with_heartbeat
    then (fun () -> marshal control (Response Wait))
    else (fun () -> return ()) in
  lwt () = marshal_protocol control in

  let exit_code = ref None in
  lwt () = while_lwt !exit_code = None do
    lwt cmd = try_lwt unmarshal control
              with e -> handle_unmarshal_failure e control in
    debug "Read: %s\n%!" (string_of_message cmd); flush stderr;
    match cmd with
    | Command (Print x) -> print_endline x; flush stdout; return ()
    | Command (PrintStderr x) -> Printf.fprintf stderr "%s%!" x; return ()
    | Command (Debug x) -> debug "debug from server: %s\n%!" x; return ()
    | Command (Load x) ->
        begin
          try_lwt
            lwt fd = Lwt_unix.openfile x [ Unix.O_RDONLY ] 0 in
            lwt file = Channels.of_raw_fd fd in
            lwt () = marshal control (Response OK) in
            let length = (Unix.stat x).Unix.st_size in
            lwt () = marshal control (Blob (Chunk (Int32.of_int length))) in
            let buffer = Cstruct.create (16 * 1024 * 1024) in
            let rec loop remaining =
              if remaining <= 0L
              then return ()
              else
                let size = Int64.(to_int (min remaining (of_int (Cstruct.len buffer)))) in
                let fragment = Cstruct.sub buffer 0 size in
                lwt () = file.Channels.really_read fragment in
                lwt () = control.Channels.really_write fragment in
                let remaining = Int64.(sub remaining (of_int size)) in
                loop remaining in
            lwt () = loop (Int64.of_int length) in
            lwt () = marshal control (Blob End) in
            file.Channels.close ()
          with
          | e -> marshal control (Response Failed)
        end
    | Command (HttpConnect(url)) ->
      let server, path = parse_url url in
      let final = ref false in
      let tc_save = ref None in
      let connection c =
        (* Cohttp doesn't support CONNECT verbs *)
        let request = Printf.sprintf "CONNECT %s HTTP/1.0\r\ncontent-length: 0\r\n\r\n" path in
        let buffer = Cstruct.create (String.length request) in
	Cstruct.blit_from_string request 0 buffer 0 (Cstruct.len buffer);
	lwt () = c.Channels.really_write buffer in
        Response.read (Cohttp_unbuffered_io.make_input c) >>= fun r ->
        begin match r with
        | `Invalid x -> fail (Failure ("Unable to parse HTTP response from server: " ^ x))
        | `Eof -> fail (Failure "Connection closed prematurely while parsing HTTP response from server.")
        | `Ok x ->
          let code = Code.code_of_status (Cohttp.Response.status x) in
          match code with
	  | 200 ->
            if !tc_save = None then begin
              (* Remember the current terminal state so we can restore it *)
              let tc = Unix.tcgetattr Unix.stdin in
              (* Switch into a raw mode, passing through stuff like Control + C *)
              let tc' = { tc with
                Unix.c_ignbrk = false; c_brkint = false; c_parmrk = false;
                c_istrip = false; c_inlcr = false; c_igncr = false;
                c_icrnl = false; c_ixon = false; c_opost = false; c_echo = false;
                c_echonl = false; c_icanon = false; c_isig = false;
                (* IEXTEN? *)
                c_csize = 8; c_parenb = false; c_vmin = 0; c_vtime = 0;
              } in
              Unix.tcsetattr Unix.stdin Unix.TCSANOW tc';
              tc_save := Some tc
            end;
            let finished = ref false in
            lwt () = while_lwt not !finished do
              (* thread which copies from fd -> stdout *)
              let t1 =
                let buffer = Cstruct.create 1024 in
                lwt stdout = Channels.of_raw_fd Lwt_unix.stdout in
                while_lwt true do
                  lwt input = c.Channels.read buffer in
                  stdout.Channels.really_write input
                done in
              (* thread which copies from stdin -> fd *)
              let t2 =
                let buffer = Cstruct.create 1024 in
                lwt stdin = Channels.of_raw_fd Lwt_unix.stdin in
                while_lwt true do
                  lwt input = stdin.Channels.read buffer in
                  (* Look for connection termination request *)
                  for i = 0 to Cstruct.len input - 1 do
                    if Cstruct.get_uint8 input i = 0x1d then final := true
                  done;
                  c.Channels.really_write input
                done in
              (* a thread which periodically sends a heartbeat *)
              let hb =
                while_lwt true do
                  lwt () = Lwt_unix.sleep heartbeat_interval in
                  heartbeat_fun ()
                done in
              lwt () = Lwt.pick [ t1; t2; hb ] in
              return ()
           done in
           marshal control (Response OK)
         | 302 ->
           Printf.fprintf stderr "Server replied with HTTP 404: the console is not available\n";
           marshal control (Response Failed)
         | _ ->
           Printf.fprintf stderr "HTTP connection failed\n";
           marshal control (Response Failed)
       end in
     let delay = ref 0.1 in
     let rec keep_connection () =
       try_lwt
         lwt c = open_tcp server in
         delay := 0.1;
         try_lwt connection c
         finally c.Channels.close ()
       with
         | Unix.Unix_error (_, _, _) when !delay <= long_connection_retry_timeout ->
           lwt () = Lwt_unix.sleep !delay in
           delay := !delay *. 2.;
           keep_connection ()
         | e ->
           prerr_endline (Printexc.to_string e);
           marshal control (Response Failed) in
      lwt () = keep_connection () in
      (match !tc_save with
      | Some tc ->
        Unix.tcsetattr Unix.stdin Unix.TCSANOW tc;
        print_endline "\r";
        return ();
      | None -> return ())
    | Command (HttpPut(filename, url)) ->
        let _ =
          try_lwt
            lwt () =
              if not (Sys.file_exists filename)
              then fail (ClientSideError (Printf.sprintf "file '%s' does not exist" filename))
              else return () in
            let rec doit url =
              let server, path = parse_url url in
              lwt fd = Lwt_unix.openfile filename [ Unix.O_RDONLY ] 0o0 in
              lwt stats = Lwt_unix.LargeFile.stat filename in
              let file_size = stats.Lwt_unix.LargeFile.st_size in
              lwt c = open_tcp server in
              let headers = Header.of_list [ "content-length", Int64.to_string file_size ] in
              let request = Cohttp.Request.make ~meth:`PUT ~version:`HTTP_1_0 ~headers (Uri.of_string url) in
              Request.write (fun writer -> return_unit) request c >>= fun () ->
              Response.read (Cohttp_unbuffered_io.make_input c) >>= fun r ->
              begin match r with
              | `Invalid x -> fail (Failure ("Unable to parse HTTP response from server" ^ x))
              | `Eof -> fail (Failure "Connection closed prematurely when parsing HTTP response")
              | `Ok x ->
                let code = Code.code_of_status (Cohttp.Response.status x) in
                match code with
                | 200 ->
                  lwt fd_c = Channels.of_raw_fd fd in
                  lwt () = copy_with_heartbeat fd_c c heartbeat_fun in
                  marshal control (Response OK)
                | 302 ->
                  let headers = Cohttp.Response.headers x in
                  begin match Header.get headers "location" with
                  | Some url ->
                    lwt () = c.Channels.close () in
                    doit url
                  | None ->
                    failwith "No location in 302 redirect"
                  end
                | _ -> failwith "Unhandled response code"
              end in
            lwt () = doit url in
            marshal control (Response OK)
          with
          | ClientSideError msg ->
              lwt () = marshal control (Response Failed) in
              Printf.fprintf stderr "Operation failed. Error: %s\n" msg;
              exit_code := Some 1;
              return ()
          | e ->
              debug "HttpPut failure: %s\n%!" (Printexc.to_string e);
              (* Assume the server will figure out what's wrong and tell us over
                 the normal communication channel *)
              marshal control (Response Failed) in
        return ()
    | Command (HttpGet(filename, url)) ->
        let _ =
          try_lwt
            lwt () =
                if Sys.file_exists filename
                then fail (ClientSideError (Printf.sprintf "file '%s' already exists" filename))
                else return () in
            let rec doit url =
              let server, path = parse_url url in
              debug "Opening connection to server '%s' path '%s'\n%!" server path;
              lwt c = open_tcp server in
              let request = Cohttp.Request.make ~meth:`GET ~version:`HTTP_1_0 (Uri.of_string url) in
              Request.write (fun _ -> return_unit) request c >>= fun () ->
              Response.read (Cohttp_unbuffered_io.make_input c) >>= fun r ->
              begin match r with
              | `Invalid x -> fail (Failure ("Unable to parse HTTP response from server: " ^ x))
              | `Eof -> fail (Failure "Connection closed prematurely when parsing HTTP response")
              | `Ok x ->
                let code = Code.code_of_status (Cohttp.Response.status x) in
                match code with
                | 200 ->
                  lwt fd =
                    if filename = ""
                    then return (Lwt_unix.dup Lwt_unix.stdout)
                    else Lwt_unix.openfile filename [ Unix.O_WRONLY; Unix.O_EXCL; Unix.O_CREAT ] 0o0600 in
                  lwt file = Channels.of_raw_fd fd in
                  try_lwt copy_with_heartbeat c file heartbeat_fun
                  finally file.Channels.close ()
                | 302 ->
                  let headers = Cohttp.Response.headers x in
                  begin match Header.get headers "location" with
                  | Some url ->
                    lwt () = c.Channels.close () in
                    doit url
                  | None ->
                    failwith "No location in 302 redirect"
                  end
                | _ -> failwith "Unhandled response code"
              end
            in
            lwt () = doit url in
            marshal control (Response OK)
          with
          | ClientSideError msg ->
              lwt () = marshal control (Response Failed) in
              Printf.fprintf stderr "Operation failed. Error: %s\n" msg;
              exit_code := Some 1;
              return ()
          | e ->
              debug "HttpGet failure: %s\n%!" (Printexc.to_string e);
              marshal control (Response Failed) in
        return ()
    | Command Prompt ->
        let data = input_line stdin in
        lwt () = marshal control (Blob (Chunk (Int32.of_int (String.length data)))) in
        let buffer = Cstruct.create (String.length data) in
        Cstruct.blit_from_string data 0 buffer 0 (Cstruct.len buffer);
        lwt () = control.Channels.really_write buffer in
        marshal control (Blob End)
    | Command (Error(code, params)) ->
        error "Error code: %s\n" code;
        error "Error parameters: %s\n" (String.concat ", " params);
        return ()
    | Command (Exit c) ->
        exit_code := Some c;
        return ()
    | x ->
        fail (Unexpected_msg x)
  done in
  match !exit_code with Some c -> return c | _ -> assert false

let main () : unit Lwt.t =
  let exit_status = ref 1 in
  (* Sys.argv has at least one element: the name of the running binary *)
  let args = List.tl (Array.to_list Sys.argv) in

  if List.mem "-version" args then begin
    Printf.printf "ThinCLI protocol: %d.%d\n" major minor;
    exit 0
  end;

  lwt () = try_lwt
    let args = parse_args args in

    if List.length args < 1 then fail Usage else begin

      let body = String.concat "\n" (args @ [("username="^ !xapiuname);("password="^ !xapipword)]) in
      open_channels () >>= fun c ->
      let headers = Header.init () in
      let headers = Header.add headers "User-agent" (Printf.sprintf "xe-cli/Unix/%d.%d" major minor) in
      let headers = Header.add headers "content-length" (string_of_int (String.length body)) in
      let request = Cohttp.Request.make ~meth:`POST ~version:`HTTP_1_0 ~headers (Uri.make ~path:"/cli" ()) in
      Request.write (fun wr -> Request.write_body wr body) request c >>= fun () ->
      (* NB: there is no HTTP response normally *)
      lwt status = main_loop c in
      exit_status := status;
      return ()
    end
  with
  | Usage ->
      exit_status := 0;
      usage ();
      return ()
  | Not_a_cli_server ->
      error "Failed to contact a running XenServer management agent.\n";
      error "Try specifying a server name and port.\n";
      usage();
      return ()
  | Protocol_version_mismatch x ->
      error "Protocol version mismatch: %s.\n" x;
      error "Try specifying a server name and port on the command-line.\n";
      usage();
      return ()
  | Not_found ->
      error "Host '%s' not found.\n" !xapiserver;
      return ()
  | Unix.Unix_error(err,fn,arg) ->
      error "Error: %s (calling %s %s)\n" (Unix.error_message err) fn arg;
      return ()
  | Connect_failure ->
      error "Unable to contact server. Please check server and port settings.\n";
      return ()
  | End_of_file ->
      error "Lost connection to the server.\n";
      return ()
  | Unexpected_msg m ->
      error "Unexpected message from server: %s" (string_of_message m);
      return ()
  | Server_internal_error ->
      error "Server internal error.\n";
      return ()
  | e ->
      error "Unhandled exception\n%s\n" (Printexc.to_string e);
      return () in
  begin match !debug_file, !debug_channel with
  | Some f, Some ch -> begin
      close_out ch;
      if !exit_status <> 0 then begin
        output_string stderr "\nDebug info:\n\n";
        output_string stderr (Unixext.string_of_file f)
      end;
      try Unix.unlink f with _ -> ()
    end
  | _ -> ()
  end;
  exit !exit_status

let _ =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> exit 1));
  let () = Lwt_main.run (main ()) in
  ()
