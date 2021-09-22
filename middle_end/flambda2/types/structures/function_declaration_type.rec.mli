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

module T0 : sig
  type t

  val code_id : t -> Code_id.t

  val rec_info : t -> Type_grammar.t
end

type t = T0.t Or_unknown_or_bottom.t

val create : Code_id.t -> rec_info:Type_grammar.t -> t

include
  Type_structure_intf.S
    with type t := t
    with type flambda_type := Type_grammar.t
    with type typing_env := Typing_env.t
    with type meet_env := Meet_env.t
    with type join_env := Join_env.t
    with type typing_env_extension := Typing_env_extension.t

val apply_coercion : t -> Coercion.t -> t Or_bottom.t
