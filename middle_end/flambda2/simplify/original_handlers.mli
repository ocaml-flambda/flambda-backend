(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2023--2024 OCamlPro SAS                                    *)
(*   Copyright 2023--2024 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t = private
  | Recursive of
      { invariant_params : Bound_parameters.t;
        lifted_params : Lifted_cont_params.t;
        continuation_handlers : One_recursive_handler.t Continuation.Map.t
      }
  | Non_recursive of Non_recursive_handler.t

val create_recursive :
  invariant_params:Bound_parameters.t ->
  lifted_params:Lifted_cont_params.t ->
  continuation_handlers:One_recursive_handler.t Continuation.Map.t ->
  t

val create_non_recursive : Non_recursive_handler.t -> t

val print : Format.formatter -> t -> unit

val add_params_to_lift : t -> Lifted_cont_params.t -> t
