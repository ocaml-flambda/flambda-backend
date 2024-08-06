(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2024 Jane Street Group LLC                                       *
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

(** Tailcall analysis: constructs a call graph whose vertices are functions and whose
    edges are calls in syntactic tail position. Edges are labeled with whether they are
    explicitly TCO'd ([@tail] or [@tail hint]), implicitly TCO'd, or implicitly not TCO'd.
    (Whether a function is implicitly TCO'd or not depends on the heuristic we are
    developing as part of the less-tco project.) *)

module Global_state : sig
  (** Reset the shared state for the compilation unit *)
  val reset_unit_info : unit -> unit

  (** Analyzes a single function's CFG. This should be called on every function in a
    compilation unit. *)
  val cfg : Cfg_with_layout.t -> Cfg_with_layout.t

  val print_dot : Format.formatter -> unit

  val emit_warnings : unit -> unit
end

val fixup_inlined_tailcalls : Cmm.fundecl -> Cmm.fundecl
