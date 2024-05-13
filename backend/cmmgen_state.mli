(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                     Mark Shinwell, Jane Street Europe                  *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Mutable state used by [Cmmgen]. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type ustructured_constant =
  | Const_float32 of float
  | Const_float of float
  | Const_int32 of int32
  | Const_int64 of int64
  | Const_nativeint of nativeint
  | Const_vec128 of { high : int64; low : int64 }
  | Const_block of int * uconstant list
  | Const_float_array of float list
  | Const_string of string

and uconstant =
  | Const_ref of string * ustructured_constant option
  | Const_int of int

(* Comparison functions for constants *)

val compare_structured_constants:
        ustructured_constant -> ustructured_constant -> int
val compare_constants:
        uconstant -> uconstant -> int

type constant =
  | Const_table of Cmm.is_global * Cmm.data_item list

val add_constant : Misc.Stdlib.String.t -> constant -> unit

val add_data_items : Cmm.data_item list -> unit

val get_and_clear_constants : unit -> constant Misc.Stdlib.String.Map.t

val get_and_clear_data_items : unit -> Cmm.data_item list

val add_structured_constant : Cmm.symbol -> ustructured_constant -> unit

val clear_local_structured_constants : unit -> unit

val add_global_structured_constant : string -> ustructured_constant -> unit

val get_structured_constant : string -> (Cmm.is_global * ustructured_constant) option

val structured_constant_of_sym : string -> ustructured_constant option
