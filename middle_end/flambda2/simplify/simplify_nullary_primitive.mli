(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*      Pierre Chambart & Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

(** Simplification of primitives taking no argument. *)

val simplify_nullary_primitive
   : Downwards_acc.t
  -> Flambda_primitive.nullary_primitive
  -> Debuginfo.t
  -> result_var:Var_in_binding_pos.t
  -> Simplified_named.t * Flambda_type.Typing_env_extension.t
       * Simple.t list * Downwards_acc.t
