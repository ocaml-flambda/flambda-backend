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

(* An action is a right-hand side of a case (as in a match, function, try) *)
type action =
| Guarded of
    { patch_guarded: patch:lambda -> lambda
    ; unpatched: lambda }
(* Guarded actions must allow for fallthrough if the guard fails.

   When translating a guarded action, the code to execute on fallthrough must
   be "patched" in. Because the fallthrough code is translated after the
   action is created, guarded actions carry a function [patch_guarded], which
   generates a lambda term for the action from the fallthrough code.

   Some translation functionality requires us to check syntactic properties
   of actions. Rather than recomputing [patch_guarded] at the time of these
   checks, we keep track of [unpatched], a lambda term which contains
   [staticfail] in the position to be patched.
*)
| Unguarded of lambda

val mk_guarded: patch_guarded:(patch:lambda -> lambda) -> action
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
