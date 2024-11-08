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

type t =
  | Must_stay_here
  | May_be_pushed_down
      (** The uniqueness mode allows overwriting and freeing unique
      allocations even if they are marked as immutable. The frontend
      identifies reads from unique allocations and marks them as
      [Must_stay_here]. Those reads have coeffects and are not allowed
      to be pushed under function calls or effectful primitives.
      Other reads are [May_be_pushed_down], although you might still have
      to check the mutability of the allocation to see if the read can
      actually be pushed down. *)

val print : Format.formatter -> t -> unit

val compare : t -> t -> int

val join : t -> t -> t

val from_lambda : Lambda.unique_barrier -> t
