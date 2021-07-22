(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2017--2018 OCamlPro SAS                                    *)
(*   Copyright 2017--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Numbering of the nesting depth of continuations. *)

type t

include Container_types.S with type t := t

val initial : t

val prev : t -> t
val next : t -> t

val (<=): t -> t -> bool
val (<): t -> t -> bool
val (>): t -> t -> bool
val (>=): t -> t -> bool

val max : t -> t -> t

val to_int : t -> int
