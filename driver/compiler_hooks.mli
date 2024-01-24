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

(* Hooks allow to inspect the IR produced by a pass without altering
   the compilation pipeline.

   Hooks are allowed to inspect the data but are prohibited from
   altering it. If one hook were to mutate the data there's no guarantee
   of how the compiler would behave.
   Several hooks can be registered for the same pass. There's no guarantees
   on the order of execution of hooks.
   When one IR is the output of several passes, the hooks are usually called
   on the latest version of the IR (the exception being passes marked as "raw",
   where corresponding hooks are called on the earliest version of the IR).
*)

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

(* Register a new hook for [pass]. *)
val register : 'a pass -> ('a -> unit) -> unit

(* Execute the hooks registered for [pass]. *)
val execute : 'a pass -> 'a -> unit

val execute_and_pipe : 'a pass -> 'a -> 'a

(* Remove all hooks registered for [pass] *)
val clear : 'a pass -> unit
