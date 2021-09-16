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

open Misc
open Compile_common

type _ language =
  | Parse_tree_intf : Parsetree.signature language
  | Parse_tree_impl : Parsetree.structure language
  | Typecheck_intf : Typedtree.signature language
  | Typecheck_impl : (Typedtree.structure * Typedtree.module_coercion) language
  | Raw_lambda : Lambda.program language
  | Lambda : Lambda.program language
  | Raw_flambda2 : Flambda2_terms.Flambda_unit.t language
  | Flambda2 : Flambda2_terms.Flambda_unit.t language
  | Raw_flambda1 : Flambda.program language
  | Flambda1 : Flambda.program language
  | Raw_clambda : Clambda.ulambda language
  | Clambda : Clambda.ulambda language

  | Mach_combine : Mach.fundecl language
  | Mach_cse : Mach.fundecl language
  | Mach_spill : Mach.fundecl language
  | Mach_live : Mach.fundecl language
  | Mach_reload : Mach.fundecl language
  | Mach_sel : Mach.fundecl language
  | Mach_split : Mach.fundecl language
  | Linear : Linear.fundecl language
  | Cmm : Cmm.phrase list language

type t = {
  mutable parse_tree_intf : (Parsetree.signature -> unit) list;
  mutable parse_tree_impl : (Parsetree.structure -> unit) list;
  mutable typecheck_intf : (Typedtree.signature -> unit) list;
  mutable typecheck_impl : ((Typedtree.structure * Typedtree.module_coercion) -> unit) list;
  mutable raw_lambda : (Lambda.program -> unit) list;
  mutable lambda : (Lambda.program -> unit) list;
  mutable raw_flambda2 : (Flambda2_terms.Flambda_unit.t -> unit) list;
  mutable flambda2 : (Flambda2_terms.Flambda_unit.t -> unit) list;
  mutable raw_flambda1 : (Flambda.program -> unit) list;
  mutable flambda1 : (Flambda.program -> unit) list;
  mutable raw_clambda : (Clambda.ulambda -> unit) list;
  mutable clambda : (Clambda.ulambda -> unit) list;
  mutable mach_combine : (Mach.fundecl -> unit) list;
  mutable mach_cse : (Mach.fundecl -> unit) list;
  mutable mach_spill : (Mach.fundecl -> unit) list;
  mutable mach_live : (Mach.fundecl -> unit) list;
  mutable mach_reload : (Mach.fundecl -> unit) list;
  mutable mach_sel : (Mach.fundecl -> unit) list;
  mutable mach_split : (Mach.fundecl -> unit) list;
  mutable linear : (Linear.fundecl -> unit) list;
  mutable cmm : (Cmm.phrase list -> unit) list
}
let hooks : t = {
  parse_tree_intf = [];
  parse_tree_impl = [];
  typecheck_intf = [];
  typecheck_impl = [];
  raw_lambda = [];
  lambda = [];
  raw_flambda2 = [];
  flambda2 = [];
  raw_flambda1 = [];
  flambda1 = [];
  raw_clambda = [];
  clambda = [];
  mach_combine = [];
  mach_cse = [];
  mach_spill = [];
  mach_live = [];
  mach_reload = [];
  mach_sel = [];
  mach_split = [];
  linear = [];
  cmm = [];
}

let execute_hooks : type a. (a -> unit) list -> a -> unit = fun hooks arg ->
  List.iter (fun f -> f arg) hooks

let register : type a. a language -> (a -> unit) -> unit =
  fun representation f ->
  match representation with
  | Parse_tree_intf -> hooks.parse_tree_intf <- f :: hooks.parse_tree_intf
  | Parse_tree_impl -> hooks.parse_tree_impl <- f :: hooks.parse_tree_impl
  | Typecheck_intf -> hooks.typecheck_intf <- f :: hooks.typecheck_intf
  | Typecheck_impl -> hooks.typecheck_impl <- f :: hooks.typecheck_impl
  | Raw_lambda -> hooks.raw_lambda <- f :: hooks.raw_lambda
  | Lambda -> hooks.lambda <- f :: hooks.lambda
  | Raw_flambda2 -> hooks.raw_flambda2 <- f :: hooks.raw_flambda2
  | Flambda2 -> hooks.flambda2 <- f :: hooks.flambda2
  | Raw_flambda1 -> hooks.raw_flambda1 <- f :: hooks.raw_flambda1
  | Flambda1 -> hooks.flambda1 <- f :: hooks.flambda1
  | Raw_clambda -> hooks.clambda <- f :: hooks.clambda
  | Clambda -> hooks.clambda <- f :: hooks.clambda

  | Mach_combine -> hooks.mach_combine <- f :: hooks.mach_combine
  | Mach_cse -> hooks.mach_cse <- f :: hooks.mach_cse
  | Mach_spill -> hooks.mach_spill <- f :: hooks.mach_spill
  | Mach_live -> hooks.mach_live <- f :: hooks.mach_live
  | Mach_reload -> hooks.mach_reload <- f :: hooks.mach_reload
  | Mach_sel -> hooks.mach_sel <- f :: hooks.mach_sel
  | Mach_split -> hooks.mach_split <- f :: hooks.mach_split
  | Linear -> hooks.linear <- f :: hooks.linear
  | Cmm -> hooks.cmm <- f :: hooks.cmm

let execute : type a. a language -> a -> unit =
  fun representation arg ->
  match representation with
  | Parse_tree_intf -> execute_hooks hooks.parse_tree_intf arg
  | Parse_tree_impl -> execute_hooks hooks.parse_tree_impl arg
  | Typecheck_intf -> execute_hooks hooks.typecheck_intf arg
  | Typecheck_impl -> execute_hooks hooks.typecheck_impl arg
  | Raw_lambda -> execute_hooks hooks.raw_lambda arg
  | Lambda -> execute_hooks hooks.lambda arg
  | Raw_flambda2 -> execute_hooks hooks.raw_flambda2 arg
  | Flambda2 -> execute_hooks hooks.flambda2 arg
  | Raw_flambda1 -> execute_hooks hooks.raw_flambda1 arg
  | Flambda1 -> execute_hooks hooks.flambda1 arg
  | Raw_clambda -> execute_hooks hooks.raw_clambda arg
  | Clambda -> execute_hooks hooks.clambda arg
  | Mach_combine -> execute_hooks hooks.mach_combine arg
  | Mach_cse -> execute_hooks hooks.mach_cse arg
  | Mach_spill -> execute_hooks hooks.mach_spill arg
  | Mach_live -> execute_hooks hooks.mach_live arg
  | Mach_reload -> execute_hooks hooks.mach_reload arg
  | Mach_sel -> execute_hooks hooks.mach_sel arg
  | Mach_split -> execute_hooks hooks.mach_split arg
  | Linear -> execute_hooks hooks.linear arg
  | Cmm -> execute_hooks hooks.cmm arg

let execute_and_pipe r a = execute r a; a
