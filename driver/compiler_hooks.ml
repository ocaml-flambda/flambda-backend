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

  | Mach_polling : Mach.fundecl pass
  | Mach_combine : Mach.fundecl pass
  | Mach_cse : Mach.fundecl pass
  | Mach_spill : Mach.fundecl pass
  | Mach_live : Mach.fundecl pass
  | Mach_reload : Mach.fundecl pass
  | Mach_sel : Mach.fundecl pass
  | Mach_split : Mach.fundecl pass
  | Linear : Linear.fundecl pass
  | Cfg : Cfg_with_layout.t pass
  | Cmm : Cmm.phrase list pass

  | Inlining_tree : Flambda2_simplify_shared.Inlining_report.Inlining_tree.t pass
  | Check_allocations : Checkmach_types.iter_witnesses pass

  | Imported_compilation_unit : (Compilation_unit.t * string) pass

type t = {
  mutable parse_tree_intf : (Parsetree.signature -> unit) list;
  mutable parse_tree_impl : (Parsetree.structure -> unit) list;
  mutable typed_tree_intf : (Typedtree.signature -> unit) list;
  mutable typed_tree_impl : (Typedtree.implementation -> unit) list;
  mutable raw_lambda : (Lambda.program -> unit) list;
  mutable lambda : (Lambda.program -> unit) list;
  mutable raw_flambda2 : (Flambda2_terms.Flambda_unit.t -> unit) list;
  mutable flambda2 : (Flambda2_terms.Flambda_unit.t -> unit) list;
  mutable mach_polling : (Mach.fundecl -> unit) list;
  mutable mach_combine : (Mach.fundecl -> unit) list;
  mutable mach_cse : (Mach.fundecl -> unit) list;
  mutable mach_spill : (Mach.fundecl -> unit) list;
  mutable mach_live : (Mach.fundecl -> unit) list;
  mutable mach_reload : (Mach.fundecl -> unit) list;
  mutable mach_sel : (Mach.fundecl -> unit) list;
  mutable mach_split : (Mach.fundecl -> unit) list;
  mutable linear : (Linear.fundecl -> unit) list;
  mutable cfg : (Cfg_with_layout.t -> unit) list;
  mutable cmm : (Cmm.phrase list -> unit) list;
  mutable inlining_tree : (Flambda2_simplify_shared.Inlining_report.Inlining_tree.t -> unit) list;
  mutable check_allocations : (Checkmach_types.iter_witnesses -> unit) list;
  mutable imported_compilation_unit : (Compilation_unit.t * string -> unit) list
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
  mach_polling = [];
  mach_combine = [];
  mach_cse = [];
  mach_spill = [];
  mach_live = [];
  mach_reload = [];
  mach_sel = [];
  mach_split = [];
  linear = [];
  cfg = [];
  cmm = [];
  inlining_tree = [];
  check_allocations = [];
  imported_compilation_unit = [];
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

  | Mach_combine -> hooks.mach_combine <- f :: hooks.mach_combine
  | Mach_polling -> hooks.mach_polling <- f :: hooks.mach_polling
  | Mach_cse -> hooks.mach_cse <- f :: hooks.mach_cse
  | Mach_spill -> hooks.mach_spill <- f :: hooks.mach_spill
  | Mach_live -> hooks.mach_live <- f :: hooks.mach_live
  | Mach_reload -> hooks.mach_reload <- f :: hooks.mach_reload
  | Mach_sel -> hooks.mach_sel <- f :: hooks.mach_sel
  | Mach_split -> hooks.mach_split <- f :: hooks.mach_split
  | Linear -> hooks.linear <- f :: hooks.linear
  | Cfg -> hooks.cfg <- f :: hooks.cfg
  | Cmm -> hooks.cmm <- f :: hooks.cmm
  | Inlining_tree -> hooks.inlining_tree <- f :: hooks.inlining_tree
  | Check_allocations ->
    hooks.check_allocations <- f :: hooks.check_allocations
  | Imported_compilation_unit -> hooks.imported_compilation_unit <- f :: hooks.imported_compilation_unit

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
  | Mach_polling -> execute_hooks hooks.mach_polling arg
  | Mach_combine -> execute_hooks hooks.mach_combine arg
  | Mach_cse -> execute_hooks hooks.mach_cse arg
  | Mach_spill -> execute_hooks hooks.mach_spill arg
  | Mach_live -> execute_hooks hooks.mach_live arg
  | Mach_reload -> execute_hooks hooks.mach_reload arg
  | Mach_sel -> execute_hooks hooks.mach_sel arg
  | Mach_split -> execute_hooks hooks.mach_split arg
  | Linear -> execute_hooks hooks.linear arg
  | Cfg -> execute_hooks hooks.cfg arg
  | Cmm -> execute_hooks hooks.cmm arg
  | Inlining_tree -> execute_hooks hooks.inlining_tree arg
  | Check_allocations -> execute_hooks hooks.check_allocations arg
  | Imported_compilation_unit -> execute_hooks hooks.imported_compilation_unit arg

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
  | Mach_polling -> hooks.mach_polling <- []
  | Mach_combine -> hooks.mach_combine <- []
  | Mach_cse -> hooks.mach_cse <- []
  | Mach_spill -> hooks.mach_spill <- []
  | Mach_live -> hooks.mach_live <- []
  | Mach_reload -> hooks.mach_reload <- []
  | Mach_sel -> hooks.mach_sel <- []
  | Mach_split -> hooks.mach_split <- []
  | Linear -> hooks.linear <- []
  | Cfg -> hooks.cfg <- []
  | Cmm -> hooks.cmm <- []
  | Inlining_tree -> hooks.inlining_tree <- []
  | Check_allocations -> hooks.check_allocations <- []
  | Imported_compilation_unit -> hooks.imported_compilation_unit <- []
