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

[@@@ocaml.warning "+a-30-40-41-42"]

module Typing_env = struct
  include Typing_env

  let add_equation t name ty =
    add_equation t name ty ~meet_type:Meet_and_join.meet

  let add_equations_on_params t ~params ~param_types =
    add_equations_on_params t ~params ~param_types ~meet_type:Meet_and_join.meet

  let add_env_extension t extension =
    add_env_extension t extension ~meet_type:Meet_and_join.meet

  let add_env_extension_with_extra_variables t extension =
    add_env_extension_with_extra_variables t extension
      ~meet_type:Meet_and_join.meet

  module Alias_set = Aliases.Alias_set
end

module Typing_env_extension = struct
  include Typing_env_extension

  let meet env t1 t2 =
    Meet_and_join.meet_env_extension (Typing_env.Meet_env.create env) t1 t2
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

let meet env t1 t2 : _ Or_bottom.t =
  let meet_env = Typing_env.Meet_env.create env in
  meet meet_env t1 t2

let join ?bound_name central_env ~left_env ~left_ty ~right_env ~right_ty =
  let join_env = Typing_env.Join_env.create central_env ~left_env ~right_env in
  match join ?bound_name join_env left_ty right_ty with
  | Unknown -> unknown_like left_ty
  | Known ty -> ty

let extract_symbol_approx env symbol find_code =
  let rec type_to_approx (ty : t) : _ Value_approximation.t =
    let expanded = Expand_head.expand_head env ty in
    match Expand_head.Expanded_type.descr expanded with
    | Unknown | Bottom -> Value_unknown
    | Ok
        ( Naked_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
        | Naked_nativeint _ | Rec_info _ | Region _ ) ->
      Misc.fatal_error
        "Typing_env.Serializable.to_closure_conversion_approx: Wrong kind"
    | Ok (Value ty) -> begin
      match ty with
      | Array _ | String _ | Boxed_float _ | Boxed_int32 _ | Boxed_int64 _
      | Boxed_nativeint _ | Mutable_block _ ->
        Value_unknown
      | Closures { by_closure_id; alloc_mode = _ } -> (
        match Row_like_for_closures.get_singleton by_closure_id with
        | None -> Value_unknown
        | Some ((closure_id, _contents), closures_entry) -> begin
          match Closures_entry.find_function_type closures_entry closure_id with
          | Bottom | Unknown -> Value_unknown
          | Ok function_type ->
            let code_id = Function_type.code_id function_type in
            let code_or_meta = find_code code_id in
            Closure_approximation (code_id, code_or_meta)
        end)
      | Variant
          { immediates = Unknown; blocks = _; is_unique = _; alloc_mode = _ }
      | Variant
          { immediates = _; blocks = Unknown; is_unique = _; alloc_mode = _ } ->
        Value_unknown
      | Variant
          { immediates = Known imms;
            blocks = Known blocks;
            is_unique = _;
            alloc_mode
          } ->
        if Type_grammar.is_obviously_bottom imms
        then
          match Row_like_for_blocks.get_singleton blocks with
          | None -> Value_unknown
          | Some ((_tag, _size), fields) ->
            let fields =
              List.map type_to_approx (Product.Int_indexed.components fields)
            in
            let alloc_mode =
              match alloc_mode with
              | Known am -> am
              | Unknown -> Alloc_mode.Heap
            in
            Block_approximation (Array.of_list fields, alloc_mode)
        else Value_unknown
    end
  in
  let get_symbol_type sym =
    Typing_env.find env (Name.symbol sym) (Some Flambda_kind.value)
  in
  type_to_approx (get_symbol_type symbol)
