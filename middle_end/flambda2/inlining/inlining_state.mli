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

(** Abstracts the state used during inlining. *)

type t

val print : Format.formatter -> t -> unit

val equal : t -> t -> bool

val default : round:int -> t

val create : arguments:Inlining_arguments.t -> depth:int -> t

val depth : t -> int

val increment_depth : t -> t

val is_depth_exceeded : t -> bool

val meet : t -> t -> t

val invariant : t -> unit

val with_arguments : Inlining_arguments.t -> t -> t

val arguments : t -> Inlining_arguments.t
