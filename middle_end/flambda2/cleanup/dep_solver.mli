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

type elt =
  | Top
  | Fields of Code_id_or_name.Set.t Global_flow_graph.Field.Map.t
  | Bottom

type result = (Code_id_or_name.t, elt) Hashtbl.t

val pp_result : Format.formatter -> result -> unit

val fixpoint : Global_flow_graph.graph -> result
