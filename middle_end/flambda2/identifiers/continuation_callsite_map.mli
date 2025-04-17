(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*    Guillaume Bury and Nathanaëlle Courant, OCamlPro                    *)
(*                                                                        *)
(*   Copyright 2024--2024 OCamlPro SAS                                    *)
(*   Copyright 2024--2024 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type 'a t = 'a Apply_cont_rewrite_id.Map.t Continuation.Map.t
(** This type represents maps from continuations to (maps from) rewrite ids
    to values. *)

val print : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
(** Print function. *)

val empty : 'a t
(** The empty map *)

val find : Continuation.t -> Apply_cont_rewrite_id.t -> 'a t -> 'a
(** Find the value bound to a pair of a continuation and rewrite id.
    @raise Not_found if the continuation *and* rewrite ids are not bound. *)

val add : Continuation.t -> Apply_cont_rewrite_id.t -> 'a -> 'a t -> 'a t
(** Add a binding to the callsite map.
    @raise Misc.Fatal_error if there is a pre-existing binding for the keys. *)

