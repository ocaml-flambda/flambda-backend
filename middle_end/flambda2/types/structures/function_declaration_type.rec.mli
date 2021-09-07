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

module Inlinable : sig
  type t

  val code_id : t -> Code_id.t

  val rec_info : t -> Type_grammar.t

  val must_be_inlined : t -> bool
end

module Non_inlinable : sig
  type t

  val code_id : t -> Code_id.t
end

type t0 = private Inlinable of Inlinable.t | Non_inlinable of Non_inlinable.t

type t = t0 Or_unknown_or_bottom.t

val create :
  code:Flambda.Code.t ->
  rec_info:Type_grammar.t ->
  t * Function_decl_inlining_decision.t

val create_non_inlinable : code_id:Code_id.t -> t

include
  Type_structure_intf.S
    with type t := t
    with type flambda_type := Type_grammar.t
    with type typing_env := Typing_env.t
    with type meet_env := Meet_env.t
    with type join_env := Join_env.t
    with type typing_env_extension := Typing_env_extension.t

val apply_coercion : t -> Coercion.t -> t Or_bottom.t
