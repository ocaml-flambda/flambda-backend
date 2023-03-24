# 1 "compressed_marshal.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1997 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.flambda_o3]

type extern_flags =
No_sharing
| Closures
| Compat_32
| Compression
(* note: this type definition is used in 'runtime/debugger.c' *)


external to_channel: out_channel -> 'a -> extern_flags list -> unit
    = "caml_output_value"
external to_bytes: 'a -> extern_flags list -> bytes
    = "caml_output_value_to_bytes"
external to_string: 'a -> extern_flags list -> string
    = "caml_output_value_to_string"
external to_buffer_unsafe:
      bytes -> int -> int -> 'a -> extern_flags list -> int
    = "caml_output_value_to_buffer"

let to_buffer buff ofs len v flags =
  if ofs < 0 || len < 0 || ofs > Bytes.length buff - len
  then invalid_arg "Marshal.to_buffer: substring out of bounds"
  else to_buffer_unsafe buff ofs len v flags
