(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2018 OCamlPro SAS                                    *)
(*   Copyright 2014--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** Tags on runtime boxed values. *)

include Container_types.S

type tag = t

val create : int -> t option
val create_exn : int -> t

val create_from_targetint : Targetint_31_63.t -> t option
val create_from_targetint_imm : Targetint_31_63.Imm.t -> t option

val to_int : t -> int
val to_target_imm : t -> Targetint_31_63.t
val to_targetint : t -> Targetint_32_64.t
val to_targetint_ocaml : t -> Targetint_31_63.Imm.t

val zero : t

(* CR mshinwell: Remove "_tag" suffixes *)
val string_tag : t
val closure_tag : t
val infix_tag : t
val double_tag : t
val double_array_tag : t
val custom_tag : t
val object_tag : t
val forward_tag : t
val lazy_tag : t

(** A tag to be used when a [Tag.t] must be provided, but it will never be
    used. *)
val arbitrary : t

(** Returns [true] iff the supplied tag is that of a GC-scannable block. *)
val is_structured_block : t -> bool

(** Returns [true] iff the supplied tag is that of a GC-scannable block, but
    the block is not treated like a variant, for example a lazy value. *)
val is_structured_block_but_not_a_variant : t -> bool

(** Returns the set of all tags allowed for regular blocks *)
val all_regular_tags : Set.t

(* CR mshinwell: This name should be changed---all "value"s are scannable.
   "Structured"? *)
module Scannable : sig
  (** Tags that are strictly less than [No_scan_tag], corresponding to
      blocks with fields that can be scanned by the GC. *)
  type t

  (** Raises not only if the supplied integer is less than 0 but also if
      it is greater than or equal to [No_scan_tag]. *)
  val create_exn : int -> t

  val create : int -> t option

  val of_tag : tag -> t option

  val to_int : t -> int
  val to_targetint : t -> Targetint_32_64.t
  val to_tag : t -> tag

  val zero : t
  val object_tag : t

  include Container_types.S with type t := t
end

val to_scannable_set : Set.t -> Scannable.Set.t

module Non_scannable : sig
  (** Tags that are at or above [No_scan_tag], corresponding to blocks whose
      fields cannot be scanned by the GC. *)
  type t

  (** Raises if the supplied integer is less than [No_scan_tag]. *)
  val create_exn : int -> t

  val of_tag : tag -> t option

  val to_int : t -> int
  val to_tag : t -> tag

  include Container_types.S with type t := t
end
