# 1 "compressed_marshal.mli"
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

type extern_flags =
No_sharing                          (** Don't preserve sharing *)
| Closures                            (** Send function closures *)
| Compat_32                           (** Ensure 32-bit compatibility *)
| Compression
(** The flags to the [Marshal.to_*] functions below. *)

external to_bytes :
  'a -> extern_flags list -> bytes = "caml_output_value_to_bytes"

val to_channel : out_channel -> 'a -> extern_flags list -> unit

external to_string :
  'a -> extern_flags list -> string = "caml_output_value_to_string"

val to_buffer : bytes -> int -> int -> 'a -> extern_flags list -> int
