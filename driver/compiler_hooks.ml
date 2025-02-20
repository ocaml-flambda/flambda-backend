(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Copyright 2021 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type _ pass =
  | Parse_tree_intf : Parsetree.signature pass
  | Parse_tree_impl : Parsetree.structure pass
  | Typed_tree_intf : Typedtree.signature pass
  | Typed_tree_impl : Typedtree.implementation pass
  | Raw_lambda : Lambda.program pass
  | Lambda : Lambda.program pass
  | Raw_flambda2 : Flambda2_terms.Flambda_unit.t pass
  | Flambda2 : Flambda2_terms.Flambda_unit.t pass
  | Reaped_flambda2 : Flambda2_terms.Flambda_unit.t pass

  | Linear : Linear.fundecl pass
  | Cfg_combine : Cfg_with_layout.t pass
  | Cfg_cse : Cfg_with_layout.t pass
  | Cfg : Cfg_with_layout.t pass
  | Cmm : Cmm.phrase list pass

  | Inlining_tree : Flambda2_simplify_shared.Inlining_report.Inlining_tree.t pass
  | Check_allocations : Zero_alloc_checker.iter_witnesses pass

type t = {
  mutable parse_tree_intf : (Parsetree.signature -> unit) list;
  mutable parse_tree_impl : (Parsetree.structure -> unit) list;
  mutable typed_tree_intf : (Typedtree.signature -> unit) list;
  mutable typed_tree_impl : (Typedtree.implementation -> unit) list;
  mutable raw_lambda : (Lambda.program -> unit) list;
  mutable lambda : (Lambda.program -> unit) list;
  mutable raw_flambda2 : (Flambda2_terms.Flambda_unit.t -> unit) list;
  mutable flambda2 : (Flambda2_terms.Flambda_unit.t -> unit) list;
  mutable reaped_flambda2 : (Flambda2_terms.Flambda_unit.t -> unit) list;
  mutable linear : (Linear.fundecl -> unit) list;
  mutable cfg_combine : (Cfg_with_layout.t -> unit) list;
  mutable cfg_cse : (Cfg_with_layout.t -> unit) list;
  mutable cfg : (Cfg_with_layout.t -> unit) list;
  mutable cmm : (Cmm.phrase list -> unit) list;
  mutable inlining_tree : (Flambda2_simplify_shared.Inlining_report.Inlining_tree.t -> unit) list;
  mutable check_allocations : (Zero_alloc_checker.iter_witnesses -> unit) list
}
let hooks : t = {
  parse_tree_intf = [];
  parse_tree_impl = [];
  typed_tree_intf = [];
  typed_tree_impl = [];
  raw_lambda = [];
  lambda = [];
  raw_flambda2 = [];
  flambda2 = [];
  reaped_flambda2 = [];
  linear = [];
  cfg_combine = [];
  cfg_cse = [];
  cfg = [];
  cmm = [];
  inlining_tree = [];
  check_allocations = [];
}

let execute_hooks : type a. (a -> unit) list -> a -> unit = fun hooks arg ->
  List.iter (fun f -> f arg) hooks

let register : type a. a pass -> (a -> unit) -> unit =
  fun representation f ->
  match representation with
  | Parse_tree_intf -> hooks.parse_tree_intf <- f :: hooks.parse_tree_intf
  | Parse_tree_impl -> hooks.parse_tree_impl <- f :: hooks.parse_tree_impl
  | Typed_tree_intf -> hooks.typed_tree_intf <- f :: hooks.typed_tree_intf
  | Typed_tree_impl -> hooks.typed_tree_impl <- f :: hooks.typed_tree_impl
  | Raw_lambda -> hooks.raw_lambda <- f :: hooks.raw_lambda
  | Lambda -> hooks.lambda <- f :: hooks.lambda
  | Raw_flambda2 -> hooks.raw_flambda2 <- f :: hooks.raw_flambda2
  | Flambda2 -> hooks.flambda2 <- f :: hooks.flambda2
  | Reaped_flambda2 -> hooks.reaped_flambda2 <- f :: hooks.reaped_flambda2

  | Linear -> hooks.linear <- f :: hooks.linear
  | Cfg_combine -> hooks.cfg_combine <- f :: hooks.cfg_combine
  | Cfg_cse -> hooks.cfg_cse <- f :: hooks.cfg_cse
  | Cfg -> hooks.cfg <- f :: hooks.cfg
  | Cmm -> hooks.cmm <- f :: hooks.cmm
  | Inlining_tree -> hooks.inlining_tree <- f :: hooks.inlining_tree
  | Check_allocations ->
    hooks.check_allocations <- f :: hooks.check_allocations

let execute : type a. a pass -> a -> unit =
  fun representation arg ->
  match representation with
  | Parse_tree_intf -> execute_hooks hooks.parse_tree_intf arg
  | Parse_tree_impl -> execute_hooks hooks.parse_tree_impl arg
  | Typed_tree_intf -> execute_hooks hooks.typed_tree_intf arg
  | Typed_tree_impl -> execute_hooks hooks.typed_tree_impl arg
  | Raw_lambda -> execute_hooks hooks.raw_lambda arg
  | Lambda -> execute_hooks hooks.lambda arg
  | Raw_flambda2 -> execute_hooks hooks.raw_flambda2 arg
  | Flambda2 -> execute_hooks hooks.flambda2 arg
  | Reaped_flambda2 -> execute_hooks hooks.reaped_flambda2 arg
  | Linear -> execute_hooks hooks.linear arg
  | Cfg_combine -> execute_hooks hooks.cfg_combine arg
  | Cfg_cse -> execute_hooks hooks.cfg_cse arg
  | Cfg -> execute_hooks hooks.cfg arg
  | Cmm -> execute_hooks hooks.cmm arg
  | Inlining_tree -> execute_hooks hooks.inlining_tree arg
  | Check_allocations -> execute_hooks hooks.check_allocations arg

let execute_and_pipe r a = execute r a; a

let clear : type a. a pass -> unit =
  function
  | Parse_tree_intf -> hooks.parse_tree_intf <- []
  | Parse_tree_impl -> hooks.parse_tree_impl <- []
  | Typed_tree_intf -> hooks.typed_tree_intf <- []
  | Typed_tree_impl -> hooks.typed_tree_impl <- []
  | Raw_lambda -> hooks.raw_lambda <- []
  | Lambda -> hooks.lambda <- []
  | Raw_flambda2 -> hooks.raw_flambda2 <- []
  | Flambda2 -> hooks.flambda2 <- []
  | Reaped_flambda2 -> hooks.reaped_flambda2 <- []
  | Linear -> hooks.linear <- []
  | Cfg_combine -> hooks.cfg_combine <- []
  | Cfg_cse -> hooks.cfg_cse <- []
  | Cfg -> hooks.cfg <- []
  | Cmm -> hooks.cmm <- []
  | Inlining_tree -> hooks.inlining_tree <- []
  | Check_allocations -> hooks.check_allocations <- []
