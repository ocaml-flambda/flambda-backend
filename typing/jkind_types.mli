(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Liam Stevenson, Jane Street, New York                 *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** You should use the types defined in [Jkind] (which redefines the
   types in this file) rather than using this file directly, unless you
   are in [Types] or [Primitive]. *)

(* This module defines types used in the module Jkind. This is to avoid
   a mutual dependencies between jkind.ml(i) and types.ml(i) and bewteen
   jkind.ml(i) and primitive.ml(i). Polymorphic versions of types are defined
   here, with type parameters that are meant to be filled by types defined in
   types.ml(i). jkind.ml(i) redefines the types from this file types.ml
   with the type variables instantiated. types.ml also redefines the types
   from this file with the type variables instantiated, but only for internal
   use. primitive.ml(i) uses the type [Jkind.const], and types.ml(i) depends on
   prmitive.ml(i), so [Jkind.const] is defined here and primitive.ml(i) also
   uses this module.

   Dependency chain without Jkind_types:
         _____________________
         |         |         |
         |         |         V
   Primitive <-- Types <-- Jkind

   Dependency chain with Jkind_types:
        ______________________________________
        |                          |         |
        V                          |         |
   Jkind_types <-- Primitive <-- Types <-- Jkind

   All definitions here are commented in jkind.ml or jkind.mli. *)

module Sort : sig
  (* We need to expose these details for use in [Jkind] *)

  (* Comments in [Jkind_intf.ml] *)
  type base =
    | Void
    | Value
    | Float64
    | Float32
    | Word
    | Bits8
    | Bits16
    | Bits32
    | Bits64
    | Vec128

  val to_string_base : base -> string

  val equal_base : base -> base -> bool

  type t =
    | Var of var
    | Base of base
    | Product of t list

  and var

  include
    Jkind_intf.Sort with type t := t and type var := var and type base := base

  val set_change_log : (change -> unit) -> unit

  type equate_result =
    | Unequal
    | Equal_mutated_first
    | Equal_mutated_second
    | Equal_mutated_both
    | Equal_no_mutation

  val equate_tracking_mutation : t -> t -> equate_result

  (** Post-condition (which holds deeply within the sort): If the
      result is a [Var v], then [!v] is [None]. *)
  val get : t -> t

  (** Decompose a sort into a list (of the given length) of fresh sort variables,
      equating the input sort with the product of the output sorts. *)
  val decompose_into_product : t -> int -> t list option
end

module Layout : sig
  (** Note that products have two possible encodings: as [Product ...] or as
      [Sort (Product ...]. This duplication is hard to eliminate because of the
      possibility that a sort variable may be instantiated by a product sort. *)
  type 'sort t =
    | Sort of 'sort
    | Product of 'sort t list
    | Any

  module Const : sig
    type t =
      | Any
      | Base of Sort.base
      | Product of t list
  end
end

module Layout_and_axes : sig
  open Allowance

  (* We need the variance annotation here to allow [any_dummy_jkind] to be
     polymorphic in its allowances. Otherwise the value restriction bites.
     Sigh. *)
  type ('layout, +'d) t =
    { layout : 'layout;
      modes_upper_bounds : Mode.Alloc.Const.t;
      externality_upper_bound : Jkind_axis.Externality.t;
      nullability_upper_bound : Jkind_axis.Nullability.t
    }
    constraint 'd = 'l * 'r

  val map : ('a -> 'b) -> ('a, 'd) t -> ('b, 'd) t

  val map_option : ('a -> 'b option) -> ('a, 'd) t -> ('b, 'd) t option

  val equal :
    ('layout -> 'layout -> bool) ->
    ('layout, allowed * allowed) t ->
    ('layout, allowed * allowed) t ->
    bool

  (* An equality check should work over [lr]s only. But we need this
     to do memoization in serialization. Happily, that's after all
     inference is done, when worrying about l and r does not matter
     any more. *)
  val equal_after_all_inference_is_done :
    ('layout -> 'layout -> bool) -> ('layout, 'd1) t -> ('layout, 'd2) t -> bool

  val try_allow_l : ('layout, 'l * 'r) t -> ('layout, allowed * 'r) t option

  val try_allow_r : ('layout, 'l * 'r) t -> ('layout, 'l * allowed) t option

  val sub :
    ('layout -> 'layout -> Misc.Le_result.t) ->
    ('layout, allowed * 'r) t ->
    ('layout, 'l * allowed) t ->
    Misc.Le_result.t

  val format :
    (Format.formatter -> 'layout -> unit) ->
    Format.formatter ->
    ('layout, 'd) t ->
    unit
end

module Jkind_desc : sig
  type ('type_expr, +'d) t = (Sort.t Layout.t, 'd) Layout_and_axes.t

  type 'type_expr packed = Pack : ('type_expr, 'd) t -> 'type_expr packed
  [@@unboxed]
end

type 'type_expr history =
  | Interact of
      { reason : Jkind_intf.History.interact_reason;
        jkind1 : 'type_expr Jkind_desc.packed;
        history1 : 'type_expr history;
        jkind2 : 'type_expr Jkind_desc.packed;
        history2 : 'type_expr history
      }
  | Creation of Jkind_intf.History.creation_reason

type ('type_expr, +'d) t =
  { jkind : ('type_expr, 'd) Jkind_desc.t;
    annotation : Parsetree.jkind_annotation option;
    history : 'type_expr history;
    has_warned : bool
  }

(** CR layouts v2.8: remove this when printing is improved *)
module Const : sig
  type ('type_expr, +'d) t = (Layout.Const.t, 'd) Layout_and_axes.t
end
