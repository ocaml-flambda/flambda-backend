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

(** Maintains shared state per compilation unit *)

(** Removes all information *)
val reset_unit_info : unit -> unit

(** Records the result in [Compilenv] to be saved to [cmx]. *)
val record_unit_info : Format.formatter -> unit

(** Analyzes the function, performs all checks that are enabled, and accumulates
    the results. *)
val fundecl : Format.formatter -> Mach.fundecl -> Mach.fundecl

type error =
  | Annotation of
      { fun_name : string;
        check : string
      }

exception Error of Location.t * error

val report_error : Format.formatter -> error -> unit

module Value : sig
  type t =
    | Pass
    | Fail
    | Unknown
end

module Detail : sig
  type context =
    | In_raise (* The current allocation occurred in a raise*)
    | In_catch (* The current allocation occurred in a catch block *)
    | Somewhere_else
  (* The current allocation occurred somewhere else in the code *)

  type kind =
    | Caml_alloc
    | Indirect_call
    | Indirect_tailcall
    | Caml_apply
    | Caml_checkbound
    | Probe of
        { name : string;
          handler_code_sym : string
        }
    | Direct_call of string
    | Direct_tailcall of string
    | Direct_call_unknown of string
    | Extcall of string
    | Arch_specific

  type t =
    { dbg : Debuginfo.t;
      kind : kind;
      context : context
    }
end

val keep_all_details : bool ref

type details = (string, Detail.t list) Hashtbl.t

(* Asserts that [keep_all_details] has been set to true and returns details about
   all allocations in this compilation unit. *)
val details : unit -> details
