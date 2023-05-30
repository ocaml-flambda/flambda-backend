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

(* Translation from typed abstract syntax to lambda terms,
   for the core language *)

open Asttypes
open Typedtree
open Lambda
open Debuginfo.Scoped_location

val pure_module : module_expr -> let_kind

(* Used for translating Alloc_heap values in classes and modules.  [transl_exp]
   and [transl_scoped_exp] must be called on expressions whose types have sort
   value. *)
val transl_exp: scopes:scopes -> expression -> lambda
val transl_apply: scopes:scopes
                  -> ?tailcall:tailcall_attribute
                  -> ?inlined:inlined_attribute
                  -> ?specialised:specialise_attribute
                  -> ?position:region_close
                  -> ?mode:alloc_mode
                  -> result_layout:Lambda.layout
                  -> lambda
                  -> (arg_label * apply_arg) list
                  -> scoped_location -> lambda
val transl_let: scopes:scopes -> ?in_structure:bool
                  -> rec_flag -> value_binding list -> layout -> lambda -> lambda

val transl_extension_constructor: scopes:scopes ->
  Env.t -> Longident.t option ->
  extension_constructor -> lambda

val transl_scoped_exp : scopes:scopes -> expression -> lambda

type error =
    Free_super_var
  | Unreachable_reached
  | Bad_probe_layout of Ident.t
  | Non_value_layout of Layouts.Layout.Violation.t

exception Error of Location.t * error

open Format

val report_error: formatter -> error -> unit

(* Forward declaration -- to be filled in by Translmod.transl_module *)
val transl_module :
      (scopes:scopes -> module_coercion -> Longident.t option ->
       module_expr -> lambda) ref
val transl_object :
      (scopes:scopes -> Ident.t -> string list ->
       class_expr -> lambda) ref

(* Declarations to be wrapped around the entire body *)
val clear_probe_handlers : unit -> unit
val declare_probe_handlers : lambda -> lambda
