(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2022-2022 Jane Street Group LLC                                  *
 *                                                                                *
 * Permission is hereby granted, free of charge, to any person obtaining a copy   *
 * of this software and associated documentation files (the "Software"), to deal  *
 * in the Software without restriction, including without limitation the rights   *
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      *
 * copies of the Software, and to permit persons to whom the Software is          *
 * furnished to do so, subject to the following conditions:                       *
 *                                                                                *
 * The above copyright notice and this permission notice shall be included in all *
 * copies or substantial portions of the Software.                                *
 *                                                                                *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    *
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  *
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  *
 * SOFTWARE.                                                                      *
 *                                                                                *
 **********************************************************************************)
[@@@ocaml.warning "+a-30-40-41-42"]

(** Check that functions do not allocate on the heap (local allocations are ignored). *)

(** Maintains shared state per compilation unit *)

(** Removes all information *)
val reset_unit_info : unit -> unit

(** Records the result in [Compilenv] to be saved to [cmx]. *)
val record_unit_info : Format.formatter -> unit

(** Analyzes the function, performs all checks that are enabled, and accumulates
    the results. *)
val fundecl :
  Format.formatter ->
  future_funcnames:Misc.Stdlib.String.Set.t ->
  Mach.fundecl ->
  Mach.fundecl

(** When the check fails, [Witness.t] represents an instruction that does
    not satisfy the property. *)
module Witness : sig
  type kind =
    | Alloc of
        { bytes : int;
          dbginfo : Debuginfo.alloc_dbginfo
        }
    | Indirect_call
    | Indirect_tailcall
    | Direct_call of { callee : string }
    | Direct_tailcall of { callee : string }
    | Extcall of { callee : string }
    | Arch_specific
    | Probe of
        { name : string;
          handler_code_sym : string
        }

  type t =
    { dbg : Debuginfo.t;
      kind : kind
    }
end

module Witnesses : sig
  type t

  val is_empty : t -> bool

  val iter : t -> f:(Witness.t -> unit) -> unit

  (** The witnesses are classified into which path they may appear on. If a witness
      appears on both a path to a normal and an excpetional return, it will only appear in
      [nor] component. *)
  type components =
    { nor : t;  (** on a path from function entry to a normal return  *)
      exn : t;  (** on a path from function entry to an exceptionall return  *)
      div : t  (** on a path from function entry that may diverge *)
    }
end

(**   Iterate over all function symbols with their witnesses. This function can be called
      at any time, but the complete information is only available after a call to
      [record_unit_info].  To get all witnesses for all functions, and not only for
      functions annotated with [@zero_alloc], set
      [Flambda_backend_flags.zero_alloc_checker_details_cutoff]
      to a negative value before calls to
      [fundecl].  Used by compiler_hooks. *)
type iter_witnesses = (string -> Witnesses.components -> unit) -> unit

val iter_witnesses : iter_witnesses

val is_check_enabled : Cmm.codegen_option list -> string -> Debuginfo.t -> bool
