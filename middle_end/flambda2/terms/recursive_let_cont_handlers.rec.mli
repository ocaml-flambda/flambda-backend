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

(** The representation of the alpha-equivalence class of a group of possibly
    (mutually-) recursive continuation handlers that are bound both over a
    body and their own handler code. *)
type t

(** Printing, invariant checks, name manipulation, etc. *)
include Expr_std.S with type t := t

include Contains_ids.S with type t := t

(** Deconstruct a continuation binding to get the bound continuations,
    together with the expressions and handlers over which they are scoped. *)
val pattern_match
   : t
  -> f:(body:Expr.t -> Continuation_handlers.t -> 'a)
  -> 'a

(** Deconstruct two continuation bindings using the same bound continuations. *)
val pattern_match_pair
   : t
  -> t
  -> f:(body1:Expr.t -> body2:Expr.t
    -> Continuation_handlers.t -> Continuation_handlers.t -> 'a)
  -> 'a

val create
   : body:Expr.t
  -> Continuation_handlers.t
  -> t
