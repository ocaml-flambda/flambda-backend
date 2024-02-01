(***********************************************************************)
(*                                                                     *)
(*                              OCaml                                  *)
(*                                                                     *)
(*  Copyright 2014, OCamlPro. All rights reserved.                     *)
(*  All rights reserved. This file is distributed under the terms of   *)
(*  the GNU Lesser General Public License version 2.1                  *)
(*                                                                     *)
(***********************************************************************)
(*
  Contributors:
  * Fabrice LE FESSANT (INRIA/OCamlPro)
*)

open X86_ast
module String = Misc.Stdlib.String

type section = { sec_name : string; mutable sec_instrs : asm_line array }

type data_size = B8 | B16 | B32 | B64

type symbol = {
  sy_name : string;
  mutable sy_type : string option;
  mutable sy_size : int option;
  mutable sy_global : bool;
  mutable sy_protected : bool;
  mutable sy_sec : section;
  mutable sy_pos : int option;
  mutable sy_num : int option; (* position in .symtab *)
}

module Relocation : sig
  module Kind : sig
    type t =
      (* 32 bits offset usually in data section *)
      | REL32 of string * int64
      | DIR32 of string * int64
      | DIR64 of string * int64
  end

  type t = { offset_from_section_beginning : int; kind : Kind.t }
end

module StringMap : Map.S with type key = string

type buffer

val size : buffer -> int

val relocations : buffer -> Relocation.t list

val assemble_section : arch -> section -> buffer

val get_symbol : buffer -> StringMap.key -> symbol

val contents_mut : buffer -> bytes

val contents : buffer -> string

val add_patch : offset:int -> size:data_size -> data:int64 -> buffer -> unit

val labels : buffer -> symbol String.Tbl.t
