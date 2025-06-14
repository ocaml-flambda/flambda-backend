(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*    Pierre Chambart and Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2021--2021 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t

val create :
  dom:Dominator_graph.alias_map ->
  dom_graph:Dominator_graph.t ->
  source_info:Flow_types.Acc.t ->
  control_flow_graph:Control_flow_graph.t ->
  required_names:Name.Set.t ->
  return_continuation:Continuation.t ->
  exn_continuation:Continuation.t ->
  t

val make_result : t -> Flow_types.Mutable_unboxing_result.t * Simple.Set.t

val pp_node : t -> Format.formatter -> Continuation.t -> unit
