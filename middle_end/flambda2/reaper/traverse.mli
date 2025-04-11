(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Nathanaëlle Courant, Pierre Chambart, OCamlPro               *)
(*                                                                        *)
(*   Copyright 2024 OCamlPro SAS                                          *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type result =
  { holed : Rev_expr.t;
    deps : Global_flow_graph.graph;
    kinds : Flambda_kind.t Name.Map.t;
    fixed_arity_continuations : Continuation.Set.t;
    continuation_info : Traverse_acc.continuation_info Continuation.Map.t;
    code_deps : Traverse_acc.code_dep Code_id.Map.t
  }

val run : Flambda_unit.t -> result
