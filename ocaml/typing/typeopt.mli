(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1998 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Auxiliaries for type-based optimizations, e.g. array kinds *)

val is_function_type :
      Env.t -> Types.type_expr -> (Types.type_expr * Types.type_expr) option
val is_base_type : Env.t -> Types.type_expr -> Path.t -> bool

val maybe_pointer_type : Env.t -> Types.type_expr
  -> Lambda.immediate_or_pointer
val maybe_pointer : Typedtree.expression -> Lambda.immediate_or_pointer

(* Supplying [None] for [elt_sort] should be avoided when possible. It
   will result in a call to [Ctype.type_sort] which can be expensive. *)
val array_type_kind :
  elt_sort:(Jkind.Sort.t option)
  -> Env.t -> Location.t -> Types.type_expr -> Lambda.array_kind
val array_kind :
  Typedtree.expression -> Jkind.Sort.t -> Lambda.array_kind
val array_pattern_kind :
  Typedtree.pattern -> Jkind.Sort.t -> Lambda.array_kind
val bigarray_type_kind_and_layout :
      Env.t -> Types.type_expr -> Lambda.bigarray_kind * Lambda.bigarray_layout

(* CR layouts v7: [layout], [function_return_layout], [function2_return_layout],
   and [layout_of_sort] have had location arguments added just to support the
   void check error message.  These arguments can be removed when we're happy to
   take that check out.
*)
val layout :
  Env.t -> Location.t -> Jkind.sort -> Types.type_expr -> Lambda.layout

(* These translate a type system sort to a lambda layout.  The function [layout]
   gives a more precise result---this should only be used when the kind is
   needed for compilation but the precise Lambda.layout isn't needed for
   optimization.  [layout_of_sort] gracefully errors on void, while
   [layout_of_const_sort] loudly fails on void. *)
val layout_of_sort : Location.t -> Jkind.sort -> Lambda.layout
val layout_of_const_sort : Jkind.Sort.const -> Lambda.layout

(* Given a function type and the sort of its return type, compute the layout of
   its return type. *)
val function_return_layout :
  Env.t -> Location.t -> Jkind.sort -> Types.type_expr -> Lambda.layout

(* Given a function type with two arguments and the sort of its return type,
   compute the layout of its return type. *)
val function2_return_layout :
  Env.t -> Location.t -> Jkind.sort -> Types.type_expr -> Lambda.layout

(* Given a function type and the sort of its argument, compute the layout
   of its argument.  Fails loudly if the type isn't a function type. *)
val function_arg_layout :
  Env.t -> Location.t -> Jkind.sort -> Types.type_expr -> Lambda.layout

val value_kind : Env.t -> Location.t -> Types.type_expr -> Lambda.value_kind

val classify_lazy_argument : Typedtree.expression ->
                             [ `Constant_or_function
                             | `Float_that_cannot_be_shortcut
                             | `Identifier of [`Forward_value | `Other]
                             | `Other]

val layout_union :
      Lambda.layout -> Lambda.layout -> Lambda.layout
  (** [layout_union layout1 layout2] is a layout at least as general as
      [layout1] and [layout2] *)
