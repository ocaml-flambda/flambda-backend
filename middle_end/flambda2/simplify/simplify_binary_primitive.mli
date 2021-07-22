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

(** Simplification of primitives taking two arguments. *)

val simplify_binary_primitive
   : Downwards_acc.t
  -> Flambda_primitive.binary_primitive
  -> arg1:Simple.t
  -> arg1_ty:Flambda_type.t
  -> arg2:Simple.t
  -> arg2_ty:Flambda_type.t
  -> Debuginfo.t
  -> result_var:Var_in_binding_pos.t
  -> Simplified_named.t * Flambda_type.Typing_env_extension.t
       * Simple.t list * Downwards_acc.t
