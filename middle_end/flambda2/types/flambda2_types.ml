(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Typing_env = struct
  include Typing_env

  let add_equation t name ty =
    add_equation t name ty ~meet_type:Meet_and_join.meet_type

  let add_equations_on_params t ~params ~param_types =
    add_equations_on_params t ~params ~param_types
      ~meet_type:Meet_and_join.meet_type

  let add_env_extension t extension =
    add_env_extension t extension ~meet_type:Meet_and_join.meet_type

  let add_env_extension_with_extra_variables t extension =
    add_env_extension_with_extra_variables t extension
      ~meet_type:Meet_and_join.meet_type

  module Alias_set = Aliases.Alias_set
end

module Typing_env_extension = struct
  include Typing_env_extension

  let meet = Meet_and_join.meet_env_extension
end

type typing_env = Typing_env.t

type typing_env_extension = Typing_env_extension.t

include Type_grammar
include More_type_creators
include Expand_head
include Meet_and_join
include Provers
include Reify
include Join_levels
module Code_age_relation = Code_age_relation

let join ?bound_name central_env ~left_env ~left_ty ~right_env ~right_ty =
  let join_env = Typing_env.Join_env.create central_env ~left_env ~right_env in
  match join ?bound_name join_env left_ty right_ty with
  | Unknown -> unknown_like left_ty
  | Known ty -> ty

let remove_outermost_alias env ty =
  Expand_head.expand_head env ty |> Expand_head.Expanded_type.to_type
