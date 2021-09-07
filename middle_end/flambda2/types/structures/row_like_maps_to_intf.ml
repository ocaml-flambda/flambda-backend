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

(** Interface to be satisfied by the right-hand side of a [Row_like] mapping. *)

module type S = sig
  type flambda_type

  type typing_env

  type meet_env

  type join_env

  type typing_env_extension

  type t

  val fields_kind : t -> Flambda_kind.t

  include
    Type_structure_intf.S
      with type t := t
      with type flambda_type := flambda_type
      with type typing_env := typing_env
      with type meet_env := meet_env
      with type join_env := join_env
      with type typing_env_extension := typing_env_extension
end
