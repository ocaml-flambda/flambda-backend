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
  val cfg :
    future_funcnames:Misc.Stdlib.String.Set.t ->
    Cfg_with_layout.t ->
    Cfg_with_layout.t

  val print_dot : Format.formatter -> unit

  val emit_warnings : unit -> unit

  val record_unit_info : unit -> unit
end

(* When we inline, we don't merge the position_and_tail_attribute. This means
   that we might report false positives when warning about inferred tails in
   TCO'd cycles: if A calls B in Tail_position Default_tail, and A is inlined
   into C, the calls to B keep their Tail_position Default_tail. Then, when we
   see these calls in the backend, they are not tail calls (because they cannot
   actually be optimized to tail calls because they are not in tail position),
   and we incorrectly deduce that C has inferred nontail calls. Furthermore, if
   these (deduced wrong) nontail calls participate in a TCO'd cycle, we will
   raise supurious inferred-nontail-in-tcod-cycle warnings.

   In the future should properly merge the attributes, but a workaround we do
   here is to traverse the CMM to replace position_and_tail_attributes in
   non-tail CMM position with an Inlined_into_not_tail_position attribute. *)
val fixup_inlined_tailcalls : Cmm.fundecl -> Cmm.fundecl
