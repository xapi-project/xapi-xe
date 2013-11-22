(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
(** 
 * @group Command-Line Interface (CLI)
 *)

open Lwt

(** Used to ensure that we actually are talking to a thin CLI server *)
let major = 0
let minor = 2

(** A prefix string which should be unique, used to detect that we're talking to
    a totally different kind of server (eg a standard HTTP server) *)
let prefix = "XenSource thin CLI protocol"

(** Command sent by the server to the client.
    If the command is "Save" then the server waits for "OK" from the client
    and then streams a list of data chunks to the client. *)
type command = 
  | Print of string
  | Debug of string                     (* debug message to optionally display *)
  | Load of string                      (* filename *)
  | HttpGet of string * string          (* filename * path *)
  | HttpPut of string * string          (* filename * path *)
  | HttpConnect of string               (* path *)
  | Prompt                              (* request the user enter some text *) 
  | Exit of int                         (* exit with a success or failure code *)
  | Error of string * string list       (* code params *)
  | PrintStderr of string               (* print something to stderr *)

(** In response to a server command, the client sends one of these.
    If the command was "Load" or "Prompt" then the client sends a list
    of data chunks. *)
type response = 
  | OK
  | Wait
  | Failed

(** When streaming binary data, send in chunks with a known length and a
    special End marker at the end. *)
type blob_header = 
  | Chunk of int32
  | End

type message = 
  | Command of command
  | Response of response
  | Blob of blob_header

(*****************************************************************************)
(* Pretty-print functions                                                    *)

let string_of_command = function
  | Print x                  -> "Print " ^ x
  | Debug x                  -> "Debug " ^ x
  | Load x                   -> "Load " ^ x
  | HttpGet (filename, path) -> "HttpGet " ^ path ^ " -> " ^ filename
  | HttpPut (filename, path) -> "HttpPut " ^ path ^ " -> " ^ filename
  | HttpConnect path         -> "HttpConnect " ^ path
  | Prompt                   -> "Prompt"
  | Exit x                   -> "Exit " ^ (string_of_int x)
  | Error (code, params)     -> "Error " ^ code ^ " [ " ^ (String.concat "; " params) ^ "]"
  | PrintStderr x            -> "PrintStderr " ^ x

let string_of_response = function
  | OK      -> "OK"
  | Wait    -> "Wait"
  | Failed  -> "Failed"

let string_of_blob_header = function
  | Chunk x -> "Chunk " ^ (Int32.to_string x)
  | End     -> "End"

let string_of_message = function
  | Command x  -> "Command " ^ (string_of_command x)
  | Response x -> "Response " ^ (string_of_response x)
  | Blob x     -> "Blob " ^ (string_of_blob_header x)

(*****************************************************************************)
(* Marshal/Unmarshal primitives                                              *)

let marshal_int32 x = 
  let (lsr) a b = Int32.shift_right_logical a b
  and (&&) a b = Int32.logand a b in
  let a = (x lsr 0) && 0xffl 
  and b = (x lsr 8) && 0xffl
  and c = (x lsr 16) && 0xffl
  and d = (x lsr 24) && 0xffl in
  let result = String.make 4 '\000' in
  result.[0] <- char_of_int (Int32.to_int a);
  result.[1] <- char_of_int (Int32.to_int b);
  result.[2] <- char_of_int (Int32.to_int c);
  result.[3] <- char_of_int (Int32.to_int d);
  result

let marshal_int x = marshal_int32 (Int32.of_int x)

let marshal_string x = marshal_int (String.length x) ^ x

let marshal_list f x = 
  marshal_int (List.length x) ^ (String.concat "" (List.map f x))

type context = string * int (* offset *)

let unmarshal_int32 (s, offset) = 
  let (lsl) a b = Int32.shift_left a b
  and (||) a b = Int32.logor a b in
  let a = Int32.of_int (int_of_char (s.[offset + 0])) 
  and b = Int32.of_int (int_of_char (s.[offset + 1])) 
  and c = Int32.of_int (int_of_char (s.[offset + 2])) 
  and d = Int32.of_int (int_of_char (s.[offset + 3])) in
  (a lsl 0) || (b lsl 8) || (c lsl 16) || (d lsl 24), (s, offset + 4)

let unmarshal_int pos = 
  let x, pos = unmarshal_int32 pos in
  Int32.to_int x, pos

let unmarshal_string pos = 
  let len, (s, offset) = unmarshal_int pos in
  String.sub s offset len, (s, offset + len)

let unmarshal_list pos f = 
  let len, pos = unmarshal_int pos in
  let rec loop pos acc = function
    | 0 -> List.rev acc, pos
    | n -> 
	let item, pos = f pos in
	loop pos (item :: acc) (n - 1) in
  loop pos [] len
  

(*****************************************************************************)
(* Marshal/Unmarshal higher-level messages                                   *)

(* Highest command id: 17 *)

let marshal_command = function
  | Print x -> marshal_int 0 ^ (marshal_string x)
  | Debug x -> marshal_int 15 ^ (marshal_string x)
  | Load x  -> marshal_int 1 ^ (marshal_string x)
  | HttpGet (a, b) -> marshal_int 12 ^ (marshal_string a) ^ (marshal_string b)
  | HttpPut (a, b) -> marshal_int 13 ^ (marshal_string a) ^ (marshal_string b)
  | HttpConnect a -> marshal_int 17 ^ (marshal_string a)
  | Prompt  -> marshal_int 3
  | Exit x  -> marshal_int 4 ^ (marshal_int x)
  | Error (x, xs) -> marshal_int 14 ^ (marshal_string x) ^ (marshal_list marshal_string xs)
  | PrintStderr x -> marshal_int 16 ^ (marshal_string x)

exception Unknown_tag of string * int
  
let unmarshal_command pos = 
  let tag, pos = unmarshal_int pos in match tag with
    | 0 -> let body, pos = unmarshal_string pos in Print body, pos
    | 15 -> let body, pos = unmarshal_string pos in Debug body, pos
    | 1 -> let body, pos = unmarshal_string pos in Load body, pos
    | 12 ->
	let a, pos = unmarshal_string pos in
	let b, pos = unmarshal_string pos in
	HttpGet(a, b), pos
    | 13 ->
	let a, pos = unmarshal_string pos in
	let b, pos = unmarshal_string pos in
	HttpPut(a, b), pos
    | 3 -> Prompt, pos
    | 4 -> let body, pos = unmarshal_int pos in Exit body, pos
	| 17 ->
	let a, pos = unmarshal_string pos in
	HttpConnect(a), pos
    | 14 ->
	let code, pos = unmarshal_string pos in
	let params, pos = unmarshal_list pos unmarshal_string in
	Error(code, params), pos
    | 16 -> let body, pos = unmarshal_string pos in PrintStderr body, pos
    | n -> raise (Unknown_tag("command", n))

let marshal_response = function
  | OK      -> marshal_int 5
  | Wait    -> marshal_int 18
  | Failed  -> marshal_int 6

let unmarshal_response pos = 
  let tag, pos = unmarshal_int pos in match tag with
    | 5 -> OK, pos
    | 18 -> Wait, pos
    | 6 -> Failed, pos
    | n -> raise (Unknown_tag("response", n))

let marshal_blob_header = function
  | Chunk x -> marshal_int 7 ^ (marshal_int32 x)
  | End     -> marshal_int 8

let unmarshal_blob_header pos = 
  let tag, pos = unmarshal_int pos in match tag with
    | 7 -> let body, pos = unmarshal_int32 pos in Chunk body, pos
    | 8 -> End, pos
    | n -> raise (Unknown_tag("blob_header", n))

let marshal_message = function
  | Command x  -> marshal_int 9 ^ (marshal_command x)
  | Response x -> marshal_int 10 ^ (marshal_response x)
  | Blob x     -> marshal_int 11 ^ (marshal_blob_header x) 

(** Marshal a message to a channel prefixing it with total header length *)
let marshal c x = 
  let payload = marshal_message x in
  let length = marshal_int (String.length payload) in
  let buffer = Cstruct.create (String.length payload + (String.length length)) in
  Cstruct.blit_from_string length 0 buffer 0 (String.length length);
  Cstruct.blit_from_string payload 0 buffer (String.length length) (String.length payload);
  c.Channels.really_write buffer

exception Unmarshal_failure of exn * string

let unmarshal_message pos = 
  let tag, pos = unmarshal_int pos in match tag with
    | 9 -> let body, pos = unmarshal_command pos in Command body, pos
    | 10 -> let body, pos = unmarshal_response pos in Response body, pos
    | 11 -> let body, pos = unmarshal_blob_header pos in Blob body, pos
    | n -> raise (Unknown_tag("blob_header", n))

(** Unmarshal a message from a channel *)
let unmarshal c =
  try_lwt
    let length_buffer = Cstruct.create 4 in
    lwt () = c.Channels.really_read length_buffer in
    let head = Cstruct.to_string length_buffer in
    let length, _ = unmarshal_int (head, 0) in
    let body_buffer = Cstruct.create length in
    lwt () = c.Channels.really_read body_buffer in
    let body = Cstruct.to_string body_buffer in
    return (fst (unmarshal_message (body, 0)) )
  with e ->
    fail (Unmarshal_failure (e, "not sure what should go here"))

let marshal_protocol c =
  let major = marshal_int major in
  let minor = marshal_int minor in
  let buffer = Cstruct.create (String.(length prefix + (length major) + (length minor))) in
  Cstruct.blit_from_string prefix 0 buffer 0 (String.length prefix);
  Cstruct.blit_from_string major 0 buffer (String.length prefix) (String.length major);
  Cstruct.blit_from_string minor 0 buffer (String.(length prefix + (length major))) (String.length minor);
  c.Channels.really_write buffer

exception Protocol_mismatch of string
exception Not_a_cli_server

let unmarshal_protocol c =
  try_lwt
    let buffer = Cstruct.create (String.length prefix + 8) in
    lwt () = c.Channels.really_read buffer in
    let prefix' = Cstruct.(to_string (sub buffer 0 (String.length prefix))) in
    let major_str = Cstruct.(to_string (sub buffer (String.length prefix) 4)) in
    let minor_str = Cstruct.(to_string (sub buffer (String.length prefix + 4) 4)) in
    lwt () = if prefix' <> prefix then fail Not_a_cli_server else return () in
    let major', _ = unmarshal_int (major_str, 0) in
    let minor', _ = unmarshal_int (minor_str, 0) in
    return (major', minor')
  with e -> fail (Unmarshal_failure (e, "what should go here"))


(*****************************************************************************)
(* Marshal/Unmarshal unit test                                               *)

let marshal_unmarshal (a: message) = 
  let x = marshal_message a in
  let b, (s, offset) = unmarshal_message (x, 0) in
  if a <> b 
  then failwith (Printf.sprintf "marshal_unmarshal failure: %s <> %s" 
		   (string_of_message a) (string_of_message b));
  if String.length x <> offset
  then failwith (Printf.sprintf "Failed to consume all data in marshal_unmarshal %s (length=%d offset=%d)"
		   (string_of_message a) (String.length x) offset)

let examples = 
  [ Command (Print "Hello there");
    Command (Debug "this is debug output");
    Command (Load "ova.xml");
    Command (HttpGet("foo.export", "/import"));
    Command (HttpPut("foo.export", "/export"));
    Command Prompt;
    Command (Exit 5);
    Response OK;
    Response Failed;
    Response Wait;
    Blob (Chunk 1024l);
    Blob (Chunk 10240l);
    Blob (Chunk 102400l);
    Blob (Chunk 1024000l);
    Blob (Chunk 10240000l);
    Blob End;
    Command(Error ("somecode", ["a"; "b"; "c"]));
    Command(Error ("another", []));
  ]

let test () = List.iter marshal_unmarshal examples

let _ = test ()
