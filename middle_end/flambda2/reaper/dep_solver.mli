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

type result

val pp_result : Format.formatter -> result -> unit

val fixpoint : Global_flow_graph.graph -> result

val has_use : result -> Code_id_or_name.t -> bool

val field_used :
  result -> Code_id_or_name.t -> Global_flow_graph.Field.t -> bool

(** Color of node when producing the graph as a .dot *)
val print_color : result -> Code_id_or_name.t -> string
