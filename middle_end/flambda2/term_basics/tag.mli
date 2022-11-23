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

(** Tags on OCaml values. *)

include Container_types.S

type tag = t

val create_exn : int -> t

val create_from_targetint : Targetint_31_63.t -> t option

val to_int : t -> int

val to_targetint_31_63 : t -> Targetint_31_63.t

val zero : t

val string_tag : t

val closure_tag : t

val infix_tag : t

val double_tag : t

val double_array_tag : t

val custom_tag : t

val object_tag : t

val forward_tag : t

val lazy_tag : t

(** Returns [true] iff the supplied tag is that of a GC-scannable block. *)
val is_structured_block : t -> bool

(** Returns [true] iff the supplied tag is that of a GC-scannable block, but
    outside the range of data constructors (or arrays, records, etc.), for
    example a lazy value. *)
val is_structured_block_but_not_data_constructor : t -> bool

module Scannable : sig
  (** Tags that are strictly less than [No_scan_tag], corresponding to blocks
      with fields that can be scanned by the GC. *)
  type t

  (** Raises not only if the supplied integer is less than 0 but also if it is
      greater than or equal to [No_scan_tag]. *)
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
