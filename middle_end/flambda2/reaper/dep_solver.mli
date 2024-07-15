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

type field_elt =
  | Field_top
  | Field_vals of Code_id_or_name.Set.t

type elt =
  | Top
  | Fields of field_elt Global_flow_graph.Field.Map.t
  | Bottom

type result = (Code_id_or_name.t, elt) Hashtbl.t

val pp_elt : Format.formatter -> elt -> unit

val pp_result : Format.formatter -> result -> unit

val fixpoint : Global_flow_graph.graph -> result
