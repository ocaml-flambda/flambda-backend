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

[@@@ocaml.warning "+a-30-40-41-42"]

type t

(* val print : Format.formatter -> t -> unit *)

val create :
  Typing_env.t -> left_env:Typing_env.t -> right_env:Typing_env.t -> t

(* val create_for_meet : Meet_env.t -> t *)

val target_join_env : t -> Typing_env.t

val left_join_env : t -> Typing_env.t

val right_join_env : t -> Typing_env.t

(* val flip_join_envs : t -> t *)

val now_joining : t -> Simple.t -> Simple.t -> t

val already_joining : t -> Simple.t -> Simple.t -> bool
