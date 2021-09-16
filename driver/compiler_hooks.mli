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
  (* CR-someday poechsel: use flambda2 *)
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

val register : 'a language -> ('a -> unit) -> unit

val execute : 'a language -> 'a -> unit

val execute_and_pipe : 'a language -> 'a -> 'a
