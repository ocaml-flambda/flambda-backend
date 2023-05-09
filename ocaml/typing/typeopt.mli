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

val array_type_kind : Env.t -> Types.type_expr -> Lambda.array_kind
val array_kind : Typedtree.expression -> Lambda.array_kind
val array_pattern_kind : Typedtree.pattern -> Lambda.array_kind
val bigarray_type_kind_and_layout :
      Env.t -> Types.type_expr -> Lambda.bigarray_kind * Lambda.bigarray_layout

(* CR layouts: `layout` should have a `sort` argument.  *)
(* CR layouts v2: [layout], [function_return_layout] and
   [function2_return_layout] have had location arguments added just to support
   the void check error message.  These arguments can be removed when we're
   happy to take that check out. *)
val layout : Env.t -> Location.t -> Types.type_expr -> Lambda.layout

val function_return_layout :
  Env.t -> Location.t -> Types.type_expr -> Lambda.layout
(* Gives the return layout of a function with two arguments. *)
val function2_return_layout :
  Env.t -> Location.t -> Types.type_expr -> Lambda.layout

val classify_lazy_argument : Typedtree.expression ->
                             [ `Constant_or_function
                             | `Float_that_cannot_be_shortcut
                             | `Identifier of [`Forward_value | `Other]
                             | `Other]

val layout_union :
      Lambda.layout -> Lambda.layout -> Lambda.layout
  (** [layout_union layout1 layout2] is a layout at least as general as
      [layout1] and [layout2] *)

val reset_cache : unit -> unit

