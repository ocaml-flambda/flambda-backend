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

(** The type of alpha-equivalence classes of expressions. *)
type t

(** Printing, invariant checks, name manipulation, etc. *)
include Expr_std.S with type t := t

include Contains_ids.S with type t := t

type descr = private
  | Let of Let_expr.t
  (** Bind variable(s) or symbol(s).  There can be no effect on control flow
      (save for asynchronous operations such as the invocation of finalisers
      or signal handlers as a result of reaching a safe point). *)
  | Let_cont of Let_cont_expr.t
  (** Define one or more continuations. *)
  | Apply of Apply.t
  (** Call an OCaml function, external function or method. *)
  | Apply_cont of Apply_cont.t
  (** Call a continuation, optionally adding or removing exception trap
      frames from the stack, which thus allows for the raising of
      exceptions. *)
  | Switch of Switch.t
  (** Conditional control flow. *)
  | Invalid of Invalid_term_semantics.t
  (** Code proved type-incorrect and therefore unreachable. *)

(** Extract the description of an expression. *)
val descr : t -> descr

val create_let : Let_expr.t -> t

val create_let_cont : Let_cont_expr.t -> t

(** Create an application expression. *)
val create_apply : Apply.t -> t

(** Create a continuation application (in the zero-arity case, "goto"). *)
val create_apply_cont : Apply_cont.t -> t

val create_switch : Switch_expr.t -> t

(** Create an expression indicating type-incorrect or unreachable code. *)
val create_invalid : ?semantics:Invalid_term_semantics.t -> unit -> t

val bind_parameters_to_args_no_simplification
   : params:Kinded_parameter.t list
  -> args:Simple.t list
  -> body:Expr.t
  -> Expr.t
