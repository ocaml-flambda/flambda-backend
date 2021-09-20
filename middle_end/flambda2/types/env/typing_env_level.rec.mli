(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

val print : Format.formatter -> t -> unit

(* val invariant : t -> unit *)

val empty : unit -> t

val is_empty : t -> bool

(* val defined_vars : t -> Flambda_kind.t Variable.Map.t *)

val defined_names : t -> Name.Set.t

(* val defines_name_but_no_equations : t -> Name.t -> bool *)

val fold_on_defined_vars :
  (Variable.t -> Flambda_kind.t -> 'a -> 'a) -> t -> 'a -> 'a

val equations : t -> Type_grammar.t Name.Map.t

(* val one_equation : Name.t -> Type_grammar.t -> t *)

val add_definition : t -> Variable.t -> Flambda_kind.t -> Binding_time.t -> t

val add_or_replace_equation : t -> Name.t -> Type_grammar.t -> t

val add_symbol_projection : t -> Variable.t -> Symbol_projection.t -> t

val symbol_projections : t -> Symbol_projection.t Variable.Map.t

val concat : t -> t -> t

(* val meet : Meet_env.t -> t -> t -> t *)

val n_way_join :
  env_at_fork:Typing_env.t ->
  (Typing_env.t * Apply_cont_rewrite_id.t * Continuation_use_kind.t * t) list ->
  params:Bound_parameter.t list ->
  extra_lifted_consts_in_use_envs:Symbol.Set.t ->
  extra_allowed_names:Name_occurrences.t ->
  t

(* CR vlaviron: this is only needed because Typing_env_extension creates a
   Name_abstraction over it. These functions should not be called, as levels are
   not exported. *)
include Contains_ids.S with type t := t
