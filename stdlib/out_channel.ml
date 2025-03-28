(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
open! Stdlib
type t = out_channel

type open_flag = Stdlib.open_flag =
  | Open_rdonly
  | Open_wronly
  | Open_append
  | Open_creat
  | Open_trunc
  | Open_excl
  | Open_binary
  | Open_text
  | Open_nonblock

let stdout = Stdlib.stdout
let stderr = Stdlib.stderr
let open_bin = Stdlib.open_out_bin
let open_text = Stdlib.open_out
let open_gen = Stdlib.open_out_gen

let with_open openfun s f =
  let oc = openfun s in
  Fun.protect ~finally:(fun () -> Stdlib.close_out_noerr oc)
    (fun () -> f oc)

let with_open_bin s f =
  with_open Stdlib.open_out_bin s f

let with_open_text s f =
  with_open Stdlib.open_out s f

let with_open_gen flags perm s f =
  with_open (Stdlib.open_out_gen flags perm) s f

external unsafe_output_bigarray :
  t -> _ Bigarray.Array1.t -> int -> int -> unit @@ portable
  = "caml_ml_output_bigarray"

let seek = Stdlib.LargeFile.seek_out
let pos = Stdlib.LargeFile.pos_out
let length = Stdlib.LargeFile.out_channel_length
let close = Stdlib.close_out
let close_noerr = Stdlib.close_out_noerr
let flush = Stdlib.flush
let flush_all = Stdlib.flush_all
let output_char = Stdlib.output_char
let output_byte = Stdlib.output_byte
let output_string = Stdlib.output_string
let output_bytes = Stdlib.output_bytes
let output = Stdlib.output
let output_substring = Stdlib.output_substring
let output_bigarray oc buf ofs len =
  if ofs < 0 || len < 0 || ofs > Bigarray.Array1.dim buf - len
  then invalid_arg "output_bigarray"
  else unsafe_output_bigarray oc buf ofs len

let set_binary_mode = Stdlib.set_binary_mode_out

external is_binary_mode : out_channel -> bool @@ portable = "caml_ml_is_binary_mode"

external set_buffered : t -> bool -> unit @@ portable = "caml_ml_set_buffered"

external is_buffered : t -> bool @@ portable = "caml_ml_is_buffered"

external isatty : t -> bool @@ portable = "caml_sys_isatty"
