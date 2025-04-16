(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Basile Clément, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2013--2025 OCamlPro SAS                                    *)
(*   Copyright 2014--2025 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type env_id

type 'a join_arg = env_id * 'a

type t

val target_join_env : t -> Typing_env.t

val joined_env : t -> env_id -> Typing_env.t

val n_way_join_simples :
  t -> Flambda_kind.t -> Simple.t join_arg list -> Simple.t Or_bottom.t * t

type n_way_join_type =
  t -> Type_grammar.t join_arg list -> Type_grammar.t Or_unknown.t * t

val n_way_join_env_extension :
  n_way_join_type:n_way_join_type ->
  meet_type:Typing_env.meet_type ->
  t ->
  Typing_env_extension.t join_arg list ->
  (Typing_env_extension.t * t) Or_bottom.t

val cut_and_n_way_join :
  n_way_join_type:n_way_join_type ->
  meet_type:Typing_env.meet_type ->
  cut_after:Scope.t ->
  Typing_env.t ->
  Typing_env.t list ->
  Typing_env.t
