(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Compilation of pattern-matching *)

open Typedtree
open Lambda
open Debuginfo.Scoped_location

(* An action is a right-hand side of a case. *)
type action

(* Creates a guarded action.
   
   If a guard fails, a guarded action must fallthrough to the remaining cases.
   To facilitate this, guarded actions are constructed using a continuation.

   [mk_guarded ~patch_guarded] produces a guarded action with a lambda
   representation given by [patch_guarded ~patch], where [patch] contains an
   expression that falls through to the remaining cases.
*)
val mk_guarded: patch_guarded:(patch:lambda -> lambda) -> action

(* Creates an unguarded action from its lambda representation. *)
val mk_unguarded: lambda -> action

val is_guarded: action -> bool
val lambda_of_action: action -> lambda
val map_action: f:(lambda -> lambda) -> action -> action

(* Entry points to match compiler *)
val for_function:
        scopes:scopes ->
        arg_sort:Layouts.sort -> arg_layout:layout -> return_layout:layout ->
        Location.t -> int ref option -> lambda -> (pattern * action) list ->
        partial ->
        lambda
val for_trywith:
        scopes:scopes -> return_layout:layout -> Location.t ->
        lambda -> (pattern * action) list ->
        lambda
val for_let:
        scopes:scopes -> arg_sort:Layouts.sort -> return_layout:layout ->
        Location.t -> lambda -> pattern -> lambda ->
        lambda
val for_multiple_match:
        scopes:scopes -> return_layout:layout -> Location.t ->
        (lambda * Layouts.sort * layout) list -> alloc_mode ->
        (pattern * action) list -> partial ->
        lambda

val for_tupled_function:
        scopes:scopes -> return_layout:layout -> Location.t ->
        Ident.t list -> (pattern list * action) list -> partial ->
        lambda

exception Cannot_flatten

val flatten_pattern: int -> pattern -> pattern list

(* Expand stringswitch to  string test tree *)
val expand_stringswitch:
    scoped_location -> layout -> lambda -> (string * lambda) list ->
    lambda option -> lambda

val inline_lazy_force : lambda -> region_close -> scoped_location -> lambda
