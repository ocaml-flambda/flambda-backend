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

(** From Lambda to assembly code *)

(** The type of converters straight from Lambda to Cmm. This is how Flambda 2
    operates. *)
type direct_to_cmm =
  ppf_dump:Format.formatter
  -> prefixname:string
  -> filename:string
  -> Lambda.program
  -> Cmm.phrase list

(** The one true way to get from Lambda to Cmm. *)
type pipeline =
  | Direct_to_cmm of direct_to_cmm

(** Compile an implementation from Lambda using the given middle end. *)
val compile_implementation
   : (module Compiler_owee.Unix_intf.S)
  -> ?toplevel:(string -> bool)
  -> pipeline:pipeline
  -> filename:string
  -> prefixname:string
  -> ppf_dump:Format.formatter
  -> Lambda.program
  -> unit

(** [compile_implementation_linear] reads Linear IR from [progname] file
    produced by previous compilation stages (for example,
    using "ocamlopt -save-ir-after" and "ocamlfdo opt")
    and continues compilation from [Emit].

    Correctness: carefully consider any use of [Config], [Clflags],
    [Flambda_backend_flags] and shared variables during or after [Emit].
    A mismatch between between their values in different compilation stages
    might lead to a miscompilation or compilation failures during
    [compile_implementation_linear]. Mismatches can also be due to
    marshaling of Linear IR (for example, if physical equality is used).

    In particular, compiler configuration settings and compilation flags used by
    [ocamlopt] must match the ones used by [ocamlfdo].
    Currently, [ocamlfdo] uses compiler-libs, so it must be compiled with the same
    configuration as [ocamlopt] from stages 1 and 2. For example, builds with frame
    pointers enabled require a compatible [ocamlfdo].
    There is currently no way to pass compilation flags to [ocamlfdo] so transformations
    perfomed by [ocamlfdo] must not depend on such compilation flags.  For example, SIMD
    is disabled in [ocamlfdo].

    Note that it is not safe to call functions that access variables whose values depend
    on previous compilation stages. For example, calling [Reg.create] may return a
    register that clashes with existing ones, because of the shared stamp counter in [Reg]
    that is not recorded in Linear IR files.
*)
val compile_implementation_linear
  : (module Compiler_owee.Unix_intf.S)
  -> string
  -> progname:string
  -> ppf_dump:Format.formatter
  -> unit

val compile_phrase
  : ppf_dump:Format.formatter
  -> Cmm.phrase
  -> unit

type error =
  | Assembler_error of string
  | Mismatched_for_pack of Compilation_unit.Prefix.t
  | Asm_generation of string * Emitaux.error

exception Error of error
val report_error: Format.formatter -> error -> unit

val compile_unit
   : output_prefix:string
   -> asm_filename:string
   -> keep_asm:bool
   -> obj_filename:string
   -> may_reduce_heap:bool
   -> ppf_dump:Format.formatter
   -> (unit -> unit)
   -> unit
