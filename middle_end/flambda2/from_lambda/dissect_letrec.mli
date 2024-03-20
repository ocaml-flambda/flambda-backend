(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*      Pierre Chambart, Vincent Laviron and Louis Gesbert, OCamlPro      *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018 OCamlPro SAS                                          *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Compile let-rec defining non-function values into separate allocation and
    assignments. *)

type dissected =
  | Dissected of Lambda.lambda
  | Unchanged

(** [dissect_letrec] assumes that bindings have not been dissected yet. In
    particular, that no arguments of function call are recursive. *)
val dissect_letrec :
  bindings:Lambda.rec_binding list ->
  body:Lambda.lambda ->
  free_vars_kind:(Ident.t -> Lambda.layout option) ->
  dissected
