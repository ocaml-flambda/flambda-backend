(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2019--2019 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Result accumulator structure used during Flambda to Cmm translation. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t
(** An accumulator for the cmm phrases that result from translating
    pre-allocated static blocks, functions, and gc roots.
    In addition to storing already translated static data, these
    accumulators also allow working on the elaboration of a Cmm.data_item
    list (named the current_data). *)

val empty : t
(** The empty result. *)

val archive_data : t -> t
(** Archive the current data into the list of already translated data. *)

val update_data : t -> (Cmm.data_item list -> Cmm.data_item list) -> t
(** Update the current data of the accumulator. *)

val set_data : t -> Cmm.data_item list -> t
(** Set the current data to the given list.
    @raise Assertion_failure if the current data is not empty. *)

val add_gc_roots : t -> Symbol.t list -> t
(** Add a gc root to the accumulator. *)

val add_function : t -> Cmm.fundecl -> t
(** Add a function translation. *)

(* CR mshinwell: Use a "private" record for the return type of this. *)
val to_cmm
   : t
   -> Cmm.phrase list * (Symbol.t list) * (Cmm.phrase list)
(** Return the translated data present in the accumulator, as a triple:
    [data_item_list * gc_roots * functions]. *)


