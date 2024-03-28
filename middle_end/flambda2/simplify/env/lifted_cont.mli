(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2023--2023 OCamlPro SAS                                    *)
(*   Copyright 2023--2023 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** {1 Lifted continuations}

    Continuations that are being lifted are stored in the dacc, so that
    they can be placed at the correct spot. To simplify code, these lifted
    continuations share the same type as those defined in
    [Simplify_let_cont_expr] for continuations being simplified. This module
    is here to break what would otherwise be a cyclic dependency between
    [Simplify_let_cont_expr] and [Downwards_acc].

    Since this module must exists to break the cycle, we also define a few
    useful functions related to lifted continuations and their additional
    parameters.
*)

type one_recursive_handler =
  { params : Bound_parameters.t;
    handler : Flambda.Expr.t;
    is_cold : bool
  }

type non_recursive_handler =
  { cont : Continuation.t;
    params : Bound_parameters.t;
    lifted_params : Lifted_cont_params.t;
    handler : Flambda.Expr.t;
    is_exn_handler : bool;
    is_cold : bool
  }

(** These types are defined here only to avoid a cyclic dependency. *)
type original_handlers =
  | Recursive of
      { invariant_params : Bound_parameters.t;
        lifted_params : Lifted_cont_params.t;
        continuation_handlers : one_recursive_handler Continuation.Map.t
      }
  | Non_recursive of non_recursive_handler (**)

val print_one_recursive_handler :
  Format.formatter -> one_recursive_handler -> unit

val print_non_recursive_handler :
  Format.formatter -> non_recursive_handler -> unit

(** Printing functions *)
val print_original_handlers : Format.formatter -> original_handlers -> unit

(** Add lifted cont params to some continuations. Note that this must happen only
    once per continuation at most: when it is lifted and additional parameters are
    added to replace those that were in scope at the original location of the continuation
    but are not in scope at its destination.

    @raise Fatal_error if at least one of the continuations in [original_handlers]
    already had some lifted cont params. *)
val add_params_to_lift :
  original_handlers -> Lifted_cont_params.t -> original_handlers
