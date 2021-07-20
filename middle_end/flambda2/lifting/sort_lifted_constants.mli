(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Sort a group of lifted constants into an order suitable for binding by
    one or more [Let_symbol] bindings.  This includes grouping together sets
    of closures with recursion between them (c.f. the
    [Let_symbol_expr.Bound_symbol.Sets_of_closures] constructor). *)

[@@@ocaml.warning "+a-30-40-41-42"]

open! Simplify_import

(* CR mshinwell: Split [Simplify_envs] into separate files then move this
   sorting algorithm into [LCS] *)
val sort : LCS.t -> LCS.t
