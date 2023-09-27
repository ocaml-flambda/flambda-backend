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
    | Missing_summary of { callee : string }
    | Forward_call of { callee : string }
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

  val get_alloc_dbginfo : kind -> Debuginfo.alloc_dbginfo option

  val print_kind : Format.formatter -> kind -> unit
end

module Witnesses : sig
  type t

  val create : Witness.kind -> Debuginfo.t -> t

  val is_empty : t -> bool

  val join : t -> t -> t

  val empty : t

  val print : Format.formatter -> t -> unit

  val iter : t -> f:(Witness.t -> unit) -> unit

  (** The witnesses are classified into which path they may appear on. If a witness
          appears on both a path to a normal and an excpetional return, it will only appear in
          [nor] component. *)
  type components =
    { nor : t;  (** on a path from function entry to a normal return  *)
      exn : t;  (** on a path from function entry to an exceptionall return  *)
      div : t  (** on a path from function entry that may diverge *)
    }

  val simplify : components -> components

  val elements : t -> Witness.t list
end

type iter_witnesses = (string -> Witnesses.components -> unit) -> unit
