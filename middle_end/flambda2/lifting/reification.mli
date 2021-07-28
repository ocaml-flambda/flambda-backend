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

(** Construct terms using only information from types. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

val try_to_reify
   : Downwards_acc.t
  -> Simplified_named.t
  -> bound_to:Var_in_binding_pos.t
  -> kind_of_bound_to:Flambda_kind.t
  -> allow_lifting:bool
  -> Simplified_named.t * Downwards_acc.t * Flambda_type.t
