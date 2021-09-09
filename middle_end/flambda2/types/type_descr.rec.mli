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

(** Descriptions of types of a particular kind. *)

module Make
    (Head : Type_head_intf.S
              with type meet_env := Meet_env.t
              with type join_env := Join_env.t
              with type typing_env := Typing_env.t
              with type typing_env_extension := Typing_env_extension.t
              with type type_grammar := Type_grammar.t) :
  Type_descr_intf.S
    with type flambda_type := Type_grammar.t
    with type typing_env := Typing_env.t
    with type typing_env_level := Typing_env_level.t
    with type typing_env_extension := Typing_env_extension.t
    with type meet_env := Meet_env.t
    with type join_env := Join_env.t
    with type head := Head.t
