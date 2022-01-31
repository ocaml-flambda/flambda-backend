(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2019 OCamlPro SAS                                          *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** A description of the different contexts in which names may occur. *)

type t

type kind = t

val normal : t

(** A name that does not occur in terms (but may be required for the generation
    of debugging information), but does occur in types. *)
val in_types : t

(** A name that neither occurs in names nor types, but is required for the
    generation of debugging information. *)
val phantom : t

val is_normal : t -> bool

val is_phantom : t -> bool

val is_in_types : t -> bool

val min_in_types : t

val min_in_terms : t

val top : t

val can_be_in_terms : t -> bool

val max_in_terms : t -> t -> t

include Container_types.S with type t := t

val compare_total_order : t -> t -> int

val compare_partial_order : t -> t -> int option

val compare : t -> t -> [`Be_explicit_about_total_or_partial_ordering]

val to_int : t -> int

val of_int : int -> t

(* CR mshinwell: some of these may not be needed now *)
val max_to_int : int

type descr = private
  | Normal
  | Phantom
  | In_types

val descr : t -> descr

module Or_absent : sig
  type t = private
    | Absent
    | Present of kind

  val absent : t

  val present : kind -> t

  val is_present : t -> bool

  val is_present_as_normal : t -> bool

  include Container_types.S with type t := t

  val compare_total_order : t -> t -> int

  val compare_partial_order : t -> t -> int option

  val compare : t -> t -> [`Be_explicit_about_total_or_partial_ordering]
end
