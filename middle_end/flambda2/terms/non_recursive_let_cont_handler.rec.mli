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

(** The representation of the alpha-equivalence class of the binding of a single
    non-recursive continuation handler over a body. *)
type t

(** Printing, invariant checks, name manipulation, etc. *)
include Expr_std.S with type t := t

include Contains_ids.S with type t := t

(** Deconstruct a continuation binding to get the name of the bound continuation
    and the expression over which it is scoped. *)
val pattern_match : t -> f:(Continuation.t -> body:Expr.t -> 'a) -> 'a

(** Deconstruct two continuation bindings using the same name. *)
val pattern_match_pair :
  t -> t -> f:(Continuation.t -> body1:Expr.t -> body2:Expr.t -> 'a) -> 'a

(** Obtain the continuation itself (rather than the body over which it is
    scoped). *)
val handler : t -> Continuation_handler.t

val create : Continuation.t -> body:Expr.t -> Continuation_handler.t -> t
