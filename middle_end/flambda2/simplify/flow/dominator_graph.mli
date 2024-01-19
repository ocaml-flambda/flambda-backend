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

module G : Strongly_connected_components.S with module Id := Simple

(** An internal type for the data_flow graph *)
type t =
  { required_names : Name.Set.t;
    params_kind : Flambda_kind.With_subkind.t Variable.Map.t;
    graph : G.directed_graph;
    dominator_roots : Simple.Set.t
        (* variables that are dominated only by themselves, usually because a
           constant or a symbol can flow to that variable, and thus that
           variable cannot be dominated by another variable. *)
  }

type alias_map = Simple.t Variable.Map.t

(** Create the data flow graph *)
val create :
  required_names:Name.Set.t ->
  return_continuation:Continuation.t ->
  exn_continuation:Continuation.t ->
  Flow_types.Continuation_info.t Continuation.Map.t ->
  t

val dominator_analysis : t -> alias_map

val aliases_kind : t -> alias_map -> Flambda_kind.t Variable.Map.t

module Dot : sig
  (** Printing function *)
  val print :
    ctx:int ->
    print_name:string ->
    doms:alias_map ->
    Format.formatter ->
    t ->
    unit
end
