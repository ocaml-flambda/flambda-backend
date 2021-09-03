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

(** Exception continuations for function calls, etc.

    We allow exception handlers that have not only the exception bucket
    argument but also a sequence of "extra arguments". *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

(** Printing, invariant checks, name manipulation, etc. *)
include Container_types.S with type t := t
include Expr_std.S with type t := t
include Contains_ids.S with type t := t

(** Create an exception continuation. *)
val create
   : exn_handler:Continuation.t
  -> extra_args:(Simple.t * Flambda_kind.With_subkind.t) list
  -> t

(** The exception handler itself. *)
val exn_handler : t -> Continuation.t

(** Any extra arguments together with their kinds. *)
val extra_args : t -> (Simple.t * Flambda_kind.With_subkind.t) list

(** The arity of the given exception continuation, taking into account both
    the exception bucket argument and any [extra_args]. *)
val arity : t -> Flambda_arity.With_subkinds.t

val with_exn_handler : t -> Continuation.t -> t

val without_extra_args : t -> t
