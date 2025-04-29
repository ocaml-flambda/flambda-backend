(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2021 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Flambda.Import

type t =
  { dacc : Downwards_acc.t;
    bindings_to_place : Expr_builder.binding_to_place list;
    was_lifted_set_of_closures : bool
  }

let with_dacc ~dacc t = { t with dacc }

let create dacc bindings_to_place =
  (* If [original_defining_expr] was simplified to a new term then the benefit
     of doing so is counted in [simplify_named]. *)
  { dacc; bindings_to_place; was_lifted_set_of_closures = false }

let create_have_lifted_set_of_closures dacc bound_vars_to_symbols
    ~original_defining_expr =
  (* The benefit of lifting the set of closures is added in [Simplify_named]. *)
  { dacc;
    bindings_to_place =
      List.mapi
        (fun i (var, sym) ->
          Expr_builder.Keep_binding
            { let_bound = Bound_pattern.singleton var;
              simplified_defining_expr =
                Simplified_named.create
                  (Named.create_simple (Simple.symbol sym));
              original_defining_expr =
                (if i = 0 then Some original_defining_expr else None)
            })
        bound_vars_to_symbols;
    was_lifted_set_of_closures = true
  }

let dacc t = t.dacc

let no_bindings t =
  match t.bindings_to_place with [] -> true | _ :: _ -> false

let bindings_to_place t = t.bindings_to_place

let was_lifted_set_of_closures t = t.was_lifted_set_of_closures
