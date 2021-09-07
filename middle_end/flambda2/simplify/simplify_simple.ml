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

module DA = Downwards_acc
module T = Flambda_type
module TE = T.Typing_env

(* CR lmaurer: Not clear why we need both of these. *)

(* These should return [Simple.t], since most invocations just immediately call
   [T.get_alias_exn] on the result. *)

let simplify_simple dacc simple ~min_name_mode =
  let typing_env = DA.typing_env dacc in
  match TE.type_simple_in_term_exn typing_env simple ~min_name_mode with
  | exception Not_found ->
    Misc.fatal_errorf
      "No canonical [Simple] for %a exists at the@ requested name mode (%a) or \
       one greater.@ Downwards accumulator:@ %a"
      Simple.print simple Name_mode.print min_name_mode DA.print dacc
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

type simplify_simples_result =
  { simples : Simple.t list; simple_tys : Flambda_type.t list }

let simplify_simples dacc simples =
  let simple_tys =
    ListLabels.map simples ~f:(fun simple ->
        simplify_simple dacc simple ~min_name_mode:Name_mode.normal)
  in
  { simples = ListLabels.map simple_tys ~f:T.get_alias_exn; simple_tys }
