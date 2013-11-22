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
(** apply the clean_f function after fct function has been called.
 * Even if fct raises an exception, clean_f is applied
 *)

let exnhook = ref None 

let finally fct clean_f =
	let result = try
		fct ();
	with
		exn ->
		  (match !exnhook with None -> () | Some f -> f exn);
		  clean_f (); raise exn in
	clean_f ();
	result

(* non polymorphic ignore function *)
let ignore_int v = let (_: int) = v in ()
let ignore_int64 v = let (_: int64) = v in ()
let ignore_int32 v = let (_: int32) = v in ()
let ignore_string v = let (_: string) = v in ()
let ignore_float v = let (_: float) = v in ()
let ignore_bool v = let (_: bool) = v in ()

