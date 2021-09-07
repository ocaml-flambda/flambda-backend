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

(** Values of type [t] represent alpha-equivalence classes of the definitions *
    of continuations: * let_cont [name] [args] = [handler] in [body] * or using
    an alternative notation: * [body] * where [name] [args] = [handler] * * -
    Continuations are second-class. * - Continuations do not capture variables.
    * - Continuations may be (mutually-)recursive. *)

(* CR mshinwell: ensure the statement about [Flambda_to_cmm] still holds. *)

(** It is an error to mark a continuation that might be recursive as
    non-recursive. The converse is safe.

    Note: any continuation used as an exception handler must be non-recursive by
    the point it reaches [Flambda_to_cmm]. (This means that it is permissible to
    introduce mutual recursion through stubs associated with such continuations,
    so long as [Simplify] is run afterwards to inline them out and turn the
    resulting single [Recursive] handler into a [Non_recursive] one. *)
type t = private
  | Non_recursive of
      { handler : Non_recursive_let_cont_handler.t;
        num_free_occurrences : Num_occurrences.t Or_unknown.t;
            (** [num_free_occurrences] can be used, for example, to decide
                whether to inline out a linearly-used continuation. *)
        is_applied_with_traps : bool
            (** [is_applied_with_traps] is used to prevent inlining of
                continuations that are applied with a trap action *)
      }
  | Recursive of Recursive_let_cont_handlers.t

(** Printing, invariant checks, name manipulation, etc. *)
include Expr_std.S with type t := t

include Contains_ids.S with type t := t

(** Create a definition of a non-recursive continuation. If the continuation
    does not occur free in the [body], then just the [body] is returned, without
    any enclosing [Let_cont]. *)
val create_non_recursive :
  Continuation.t ->
  Continuation_handler.t ->
  body:Expr.t ->
  free_names_of_body:Name_occurrences.t Or_unknown.t ->
  Expr.t

val create_non_recursive' :
  cont:Continuation.t ->
  Continuation_handler.t ->
  body:Expr.t ->
  num_free_occurrences_of_cont_in_body:Num_occurrences.t Or_unknown.t ->
  is_applied_with_traps:bool ->
  Expr.t

(** Create a definition of a set of possibly-recursive continuations. *)
val create_recursive :
  Continuation_handler.t Continuation.Map.t -> body:Expr.t -> Expr.t
