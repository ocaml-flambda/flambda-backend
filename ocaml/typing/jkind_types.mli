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
   use. primitive.ml(i) uses the type [Jkind.Type.const], and types.ml(i) depends on
   prmitive.ml(i), so [Jkind.Type.const] is defined here and primitive.ml(i) also
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

module History : sig
  type 'desc t =
    | Interact of
        { reason : Jkind_intf.History.interact_reason;
          lhs_jkind : 'desc;
          lhs_history : 'desc t;
          rhs_jkind : 'desc;
          rhs_history : 'desc t
        }
    | Projection of
        { reason : Jkind_intf.History.project_reason;
          jkind : 'desc;
          history : 'desc t
        }
    | Creation of Jkind_intf.History.creation_reason

  val desc_map : ('a -> 'b) -> 'a t -> 'b t
end

module Type : sig
  module Sort : sig
    (* We need to expose these details for use in [Jkind] *)

    (* Comments in [Jkind_intf.ml] *)
    type const =
      | Void
      | Value
      | Float64
      | Float32
      | Word
      | Bits32
      | Bits64

    type t =
      | Var of var
      | Const of const

    and var = t option ref

    include
      Jkind_intf.Sort
        with type t := t
         and type var := var
         and type const := const

    val set_change_log : (change -> unit) -> unit

    type equate_result =
      | Unequal
      | Equal_mutated_first
      | Equal_mutated_second
      | Equal_no_mutation

    val equate_tracking_mutation : t -> t -> equate_result

    val get : t -> t

    val to_string : t -> string
  end

  module Layout : sig
    type 'sort layout =
      | Sort of 'sort
      | Any

    module Const : sig
      type t = Sort.const layout

      module Legacy : sig
        type t =
          | Any
          | Value
          | Void
          | Immediate64
          | Immediate
          | Float64
          | Float32
          | Word
          | Bits32
          | Bits64
      end
    end

    type t = Sort.t layout
  end

  module Externality : sig
    type t =
      | External
      | External64
      | Internal
  end

  module Modes = Mode.Alloc.Const

  module Jkind_desc : sig
    type 'type_expr t =
      { layout : Layout.t;
        modes_upper_bounds : Modes.t;
        externality_upper_bound : Externality.t
      }
  end

  module Const : sig
    type 'type_expr t =
      { layout : Layout.Const.t;
        modes_upper_bounds : Modes.t;
        externality_upper_bound : Externality.t
      }
  end

  type 'type_expr annotation = 'type_expr Const.t * Jane_syntax.Jkind.annotation
end

module Arrow : sig
  type 'a t =
    { args : 'a list;
      result : 'a
    }
end

module Jkind_desc : sig
  type 'type_expr t =
    | Type of 'type_expr Type.Jkind_desc.t
    | Arrow of 'type_expr t Arrow.t
    | Top
end

type 'type_expr history = 'type_expr Jkind_desc.t History.t

type 'type_expr type_jkind =
  { jkind : 'type_expr Type.Jkind_desc.t;
    history : 'type_expr history;
    has_warned : bool
  }

type 'type_expr t =
  { jkind : 'type_expr Jkind_desc.t;
    history : 'type_expr history;
    has_warned : bool
  }

module Const : sig
  type 'type_expr t =
    | Type of 'type_expr Type.Const.t
    | Arrow of 'type_expr t Arrow.t
    | Top
end

type 'type_expr annotation = 'type_expr Const.t * Jane_syntax.Jkind.annotation
