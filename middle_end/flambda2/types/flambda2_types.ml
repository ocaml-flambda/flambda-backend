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

module Typing_env_extension = Typing_env_extension

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

let remove_outermost_alias env ty =
  Expand_head.expand_head env ty |> Expand_head.Expanded_type.to_type
