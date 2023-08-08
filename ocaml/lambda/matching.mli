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

(* Right-hand side of a case. *)
type rhs

(* Creates a guarded rhs.

   If a guard fails, a guarded rhs must fallthrough to the remaining cases.
   To facilitate this, guarded rhs's are constructed using a continuation.

   [mk_pattern_guarded_rhs ~patch_guarded] produces a guarded rhs with a
   lambda representation given by [patch_guarded ~patch], where [patch] contains
   an expression that falls through to the remaining cases.

   [mk_boolean_guarded_rhs ~patch_guarded ~free_variables] produces a similar
   rhs where [free_variables] contains the free variables of the rhs.
*)
(* CR-soon rgodse: This function is unused for now. Let's remove it after we
   implement the free variable optimization for pattern guards, merging the two
   functions below.
*)
val mk_boolean_guarded_rhs:
        patch_guarded:(patch:lambda -> lambda) ->
        free_variables:Ident.Set.t ->
        rhs

val mk_pattern_guarded_rhs:
        patch_guarded:(patch:lambda -> lambda) ->
        rhs

(* [add_guard_to_rhs ~patch_guarded ~guard_free_variables rhs0] produces an rhs
   with lambda representation [patch_guarded ~patch ~rhs], where [patch]
   contains an expression that falls through to the remaining cases, and [rhs]
   is the translation of [rhs0], and [guard_free_variables] are the free
   variables of the new guard. *)
val add_guard_to_rhs:
        patch_guarded:(patch:lambda -> rhs:lambda -> lambda) ->
        guard_free_variables:Ident.Set.t ->
        rhs ->
        rhs


(* Creates an unguarded rhs from its lambda representation. *)
val mk_unguarded_rhs: lambda -> rhs

(* Entry points to match compiler *)
val for_function:
        scopes:scopes ->
        arg_sort:Layouts.sort -> arg_layout:layout -> return_layout:layout ->
        Location.t -> int ref option -> lambda -> (pattern * rhs) list ->
        partial ->
        lambda
val for_trywith:
        scopes:scopes -> return_layout:layout -> Location.t ->
        lambda -> (pattern * rhs) list ->
        lambda
val for_let:
        scopes:scopes -> arg_sort:Layouts.sort -> return_layout:layout ->
        Location.t -> lambda -> pattern -> lambda ->
        lambda
val for_multiple_match:
        scopes:scopes -> return_layout:layout -> Location.t ->
        (lambda * Layouts.sort * layout) list -> alloc_mode ->
        (pattern * rhs) list -> partial ->
        lambda

val for_tupled_function:
        scopes:scopes -> return_layout:layout -> Location.t ->
        Ident.t list -> (pattern list * rhs) list -> partial ->
        lambda

exception Cannot_flatten

val flatten_pattern: int -> pattern -> pattern list

(* Expand stringswitch to  string test tree *)
val expand_stringswitch:
    scoped_location -> layout -> lambda -> (string * lambda) list ->
    lambda option -> lambda

val inline_lazy_force : lambda -> region_close -> scoped_location -> lambda
