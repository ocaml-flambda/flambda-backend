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

(* This module defines types used in the module Jkind. This is to avoid a mutual
   dependencies between jkind.ml(i) and types.ml(i) and bewteen jkind.ml(i) and
   primitive.ml(i). Polymorphic versions of types are defined here, with type
   parameters that are meant to be filled by types defined in
   types.ml(i). jkind.ml(i) redefines the types from this file types.ml with the
   type variables instantiated. types.ml also redefines the types from this file
   with the type variables instantiated, but only for internal
   use. primitive.ml(i) uses the type [Jkind.Const.t], and types.ml(i) depends
   on primitive.ml(i), so [Jkind.Const.t] is defined here and primitive.ml(i)
   also uses this module.

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
