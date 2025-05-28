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
    add_equation t name ty ~meet_type:(Meet.meet_type ())

  let add_is_null_relation t name ~scrutinee =
    add_equation t name (Type_grammar.is_null ~scrutinee)

  let add_is_int_relation t name ~scrutinee =
    add_equation t name (Type_grammar.is_int_for_scrutinee ~scrutinee)

  let add_get_tag_relation t name ~scrutinee =
    add_equation t name (Type_grammar.get_tag_for_block ~block:scrutinee)

  let add_equations_on_params t ~params ~param_types =
    add_equations_on_params t ~params ~param_types
      ~meet_type:(Meet.meet_type ())

  let add_env_extension t extension =
    add_env_extension t extension ~meet_type:(Meet.meet_type ())

  let add_env_extension_with_extra_variables t extension =
    add_env_extension_with_extra_variables t extension
      ~meet_type:(Meet.meet_type ())

  module Alias_set = Aliases.Alias_set
end

module Typing_env_extension = struct
  include Typing_env_extension

  let add_is_null_relation t name ~scrutinee =
    add_or_replace_equation t name (Type_grammar.is_null ~scrutinee)

  let add_is_int_relation t name ~scrutinee =
    add_or_replace_equation t name
      (Type_grammar.is_int_for_scrutinee ~scrutinee)

  let add_get_tag_relation t name ~scrutinee =
    add_or_replace_equation t name
      (Type_grammar.get_tag_for_block ~block:scrutinee)

  let add_untag_relation t name ~scrutinee =
    add_or_replace_equation t name
      (Type_grammar.untag_immediate_for_scrutinee ~scrutinee)
end

type typing_env = Typing_env.t

type typing_env_extension = Typing_env_extension.t

include Type_grammar
include More_type_creators
include Expand_head
include Meet
include Provers
include Reify
include Join_levels
module Code_age_relation = Code_age_relation

let remove_outermost_alias env ty =
  Expand_head.expand_head env ty |> Expand_head.Expanded_type.to_type

module Equal_types_for_debug = struct
  let equal_type env t1 t2 =
    Equal_types_for_debug.equal_type ~meet_type:(Meet.meet_type ()) env t1 t2

  let equal_env_extension env ext1 ext2 =
    Equal_types_for_debug.equal_env_extension ~meet_type:(Meet.meet_type ()) env
      ext1 ext2
end
