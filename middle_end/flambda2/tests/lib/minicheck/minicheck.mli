(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2015--2022 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Splittable_random = Splittable_random
module Type = Type

(** Check whether the function [f] returns [true] for randomly-generated inputs.
    The number and types of the arguments to [f] are determined by [types],
    which is (syntactically) a list of [Type.t]s. For example:

    [check ~types:Type.[int; int] ~f:(fun i j -> i + j = j + i) ~name:"+ comm"]

    If [f] returns [false], testing stops and the failing arguments are printed
    on standard error. If [f] returns true for all [n] cases, a success message
    is printed on standard error. *)
val check :
  types:('a, bool) Type.Tuple.t ->
  f:'a ->
  ?n:int (** Number of runs (default is 1000) *) ->
  ?verbose:bool
    (** Whether to print each argument of each run (default is false) *) ->
  name:string ->
  unit ->
  unit

(** Returns [true] if any invocation of [check] has failed since startup. *)
val something_has_failed : unit -> bool
