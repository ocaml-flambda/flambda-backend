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

module DA = Downwards_acc
module T = Flambda2_types
module TE = T.Typing_env

exception Simple_not_in_scope

let simplify_simple0 dacc simple ~min_name_mode =
  let typing_env = DA.typing_env dacc in
  match TE.type_simple_in_term_exn typing_env simple ~min_name_mode with
  | exception Not_found -> raise Simple_not_in_scope
  | ty ->
    (* [ty] will always be an alias type; see the implementation of
       [TE.get_canonical_simple_in_term_exn]. *)
    let simple = T.get_alias_exn ty in
    let coercion = Simple.coercion simple in
    if Coercion.is_id coercion
    then ty
    else
      let coercion = Simplify_coercion.simplify_coercion dacc coercion in
      let simple =
        Simple.with_coercion (Simple.without_coercion simple) coercion
      in
      T.alias_type_of (T.kind ty) simple

let simplify_simple dacc simple ~min_name_mode =
  match simplify_simple0 dacc simple ~min_name_mode with
  | res -> res
  | exception Simple_not_in_scope ->
    Misc.fatal_errorf
      "No canonical [Simple] for %a exists at the@ requested name mode (%a) or \
       one greater.@ Downwards accumulator:@ %a"
      Simple.print simple Name_mode.print min_name_mode DA.print dacc

let simplify_simple_if_in_scope dacc simple ~min_name_mode =
  match simplify_simple0 dacc simple ~min_name_mode with
  | res -> Some res
  | exception Simple_not_in_scope -> None

type simplify_simples_result =
  { simples : Simple.t list;
    simple_tys : Flambda2_types.t list
  }

let simplify_simples dacc simples =
  let simple_tys =
    ListLabels.map simples ~f:(fun simple ->
        simplify_simple dacc simple ~min_name_mode:Name_mode.normal)
  in
  { simples = ListLabels.map simple_tys ~f:T.get_alias_exn; simple_tys }
