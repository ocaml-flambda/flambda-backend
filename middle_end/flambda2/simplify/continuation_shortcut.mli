(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Basile Clément, OCamlPro                         *)
(*                                                                        *)
(*   Copyright 2013--2025 OCamlPro SAS                                    *)
(*   Copyright 2014--2025 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** A continuation shortcut is an anonymous continuation whose body is a
    single [Apply_cont] expression to another continuation. *)

type t

val print : Format.formatter -> t -> unit

val create : params:Bound_parameters.t -> Continuation.t -> Simple.t list -> t

val apply : t -> Simple.t list -> Continuation.t * Simple.t list

val continuation : t -> Continuation.t

val to_alias : t -> Continuation.t option
