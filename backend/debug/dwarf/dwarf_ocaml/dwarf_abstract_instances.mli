(******************************************************************************
 *                             flambda-backend                                *
 *                       Mark Shinwell, Jane Street                           *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2024 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

(** Management of DWARF "abstract instances" for functions. *)

open! Asm_targets
open! Dwarf_low
open! Dwarf_high

val attributes : string -> Dwarf_attribute_values.Attribute_value.t list

(** Add an abstract instance root. *)
val add_root :
  Dwarf_state.t ->
  parent:Proto_die.t ->
  demangled_name:string ->
  Asm_symbol.t ->
  location_attributes:Dwarf_attribute_values.Attribute_value.t list ->
  Proto_die.t * Asm_symbol.t

type find_result = private
  | Ok of Asm_symbol.t
  | External_unit of
      { demangled_name : string;
        fun_symbol : Asm_symbol.t
      }

val find :
  Dwarf_state.t ->
  compilation_unit_proto_die:Proto_die.t ->
  Debuginfo.t ->
  find_result
(* val find_maybe_in_another_unit_or_add : Dwarf_state.t ->
   function_proto_die:Proto_die.t -> Linear.fundecl -> Asm_symbol.t option *)
