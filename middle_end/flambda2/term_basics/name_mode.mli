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

(** The modalities of names, which describe the different contexts in which
    names may occur. *)

type t = private
  | Normal
  | Phantom
  | In_types

type name_mode = t

(** Normal variables bind values that exist at runtime. *)
val normal : t

(** In-types variables, sometimes known as irrelevant variables, are those that
    exist only within the environments and judgements of the Flambda 2 type
    system. There is no runtime value associated with an in-types variable. *)
val in_types : t

(** Phantom variables bind terms that are used for the generation of debugging
    information. There is no runtime value associated with a phantom variable. *)
val phantom : t

val is_normal : t -> bool

val is_phantom : t -> bool

val is_in_types : t -> bool

val can_be_in_terms : t -> bool

(** [join_in_terms] must not be applied to the in-types mode. *)
val join_in_terms : t -> t -> t

include Container_types.S with type t := t

val compare_total_order : t -> t -> int

val compare_partial_order : t -> t -> int option

(** This shadows [compare] from the above [include]. *)
val compare : t -> t -> [`Be_explicit_about_total_or_partial_ordering]

module Or_absent : sig
  type t = private
    | Absent
    | Present of name_mode

  val absent : t

  val present : name_mode -> t

  val is_present : t -> bool

  val is_present_as_normal : t -> bool

  include Container_types.S with type t := t

  val compare_partial_order : t -> t -> int option

  (** This shadows [compare] from the above [include]. *)
  val compare : t -> t -> [`Be_explicit_about_total_or_partial_ordering]

  val join_in_terms : t -> t -> t
end
