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

(** Actions affecting exception traps on the stack.  These are always
    associated with an [Apply_cont] node; the trap action is executed before
    the application of the continuation.

    Beware: continuations cannot be used both as an exception handler and as
    a normal continuation (since continuations used as exception handlers
    use a calling convention that may differ from normal).
*)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type raise_kind =
  | Regular
  | Reraise
  | No_trace

type t =
  | Push of { exn_handler : Continuation.t; }
  | Pop of {
      exn_handler : Continuation.t;
      (** Note that even for [Pop], [exn_handler] might not match the
          target continuation in the enclosing [Apply_cont_expr].  One
          example is when a value is being returned from the end of the
          non-exceptional block of a try...with. *)
      raise_kind : raise_kind option;
    }

include Expr_std.S with type t := t

module Option : sig
  type nonrec t = t option

  val print : Format.formatter -> t -> unit

  val all_ids_for_export : t -> Ids_for_export.t

  val apply_renaming : t -> Renaming.t -> t
end

val compare : t -> t -> int

val all_ids_for_export : t -> Ids_for_export.t

val apply_renaming : t -> Renaming.t -> t
