(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Values of type ['a] must not contain names! *)

type 'a t =
  | Const of 'a
  | Var of Variable.t * Debuginfo.t
      (** The [Debuginfo.t] will be used to give correct debugging information
          at the point in the code where the corresponding statically-allocated
          block is patched. It would typically identify the place where the
          original allocation occurred in the source code. *)

val print : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

val hash : ('a -> int) -> 'a t -> int

val value_map : 'a t -> default:'b -> f:('a -> 'b) -> 'b

val free_names : _ t -> Name_occurrences.t

val apply_renaming : 'a t -> Renaming.t -> 'a t
