(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*         Fabrice Le Fessant, projet Gallium, INRIA Rocquencourt         *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Helpers for Intel code generators *)

(* The DSL* modules expose functions to emit x86/x86_64 instructions
   using a syntax close to the official Intel syntax, except that
   source and destination operands are reversed as in the AT&T
   syntax:

     mov src dst
*)


open X86_ast

val sym: string -> arg
val nat: nativeint -> arg
val int: int -> arg
val const_32: int32 -> constant
val const_nat: nativeint -> constant
val const: int -> constant
val al: arg
val ah: arg
val cl: arg
val ax: arg
val rax: arg
val r10: arg
val r11: arg
val r13: arg
val r14: arg
val r15: arg
val rsp: arg
val rbp: arg
val xmm15: arg
val eax: arg
val ebx: arg
val ecx: arg
val edx: arg
val ebp: arg
val esp: arg

val mem32:
  data_type -> ?scale:int -> ?base:reg64 -> ?sym:string ->
  int -> reg64 -> arg

val mem64:
  data_type -> ?scale:int -> ?base:reg64 -> ?sym:string ->
  int -> reg64 -> arg

val mem64_rip: data_type -> ?ofs:int -> string -> arg


module D : sig
  (** Directives *)

  (* If data is true then null bytes are used for padding,
     otherwise nops are used *)
  val align: data:bool -> int -> unit
  val byte: constant -> unit
  val bytes: string -> unit
  val cfi_adjust_cfa_offset: int -> unit
  val cfi_endproc: unit -> unit
  val cfi_startproc: unit -> unit
  val comment: string -> unit
  val data: unit -> unit
  val direct_assignment : string -> constant -> unit
  val extrn: string -> data_type -> unit
  val file: file_num:int -> file_name:string -> unit
  val global: string -> unit
  val hidden: string -> unit
  val indirect_symbol: string -> unit
  val label: ?typ:data_type -> string -> unit
  val loc: file_num:int -> line:int -> col:int -> ?discriminator:int -> unit
    -> unit
  val long: constant -> unit
  val mode386: unit -> unit
  val model: string -> unit
  val new_line : unit -> unit
  val private_extern: string -> unit
  val qword: constant -> unit
  val reloc: offset:constant -> name:reloc_type -> expr:constant -> unit
  val section: ?delayed:bool -> string list -> string option -> string list -> unit
  val setvar: string * constant -> unit
  val size: string -> constant -> unit
  val sleb128 : constant -> unit
  val space: int -> unit
  val text: unit -> unit
  val type_: string -> string -> unit
  val uleb128 : constant -> unit
  val weak: string -> unit
  val word: constant -> unit
end

module I : sig
  (* Instructions *)

  val add: arg -> arg -> unit
  val addsd: arg -> arg -> unit
  val and_: arg -> arg -> unit
  val andpd: arg -> arg -> unit
  val bsf : arg -> arg -> unit
  val bsr : arg -> arg -> unit
  val bswap: arg -> unit
  val call: arg -> unit
  val cdq: unit -> unit
  val cmov : condition -> arg -> arg -> unit
  val cmp: arg -> arg -> unit
  val cmpsd : float_condition -> arg -> arg -> unit
  val comisd: arg -> arg -> unit
  val cqo: unit -> unit
  val cvtsd2si: arg -> arg -> unit
  val cvtsd2ss: arg -> arg -> unit
  val cvtsi2sd: arg -> arg -> unit
  val cvtss2sd: arg -> arg -> unit
  val cvttsd2si: arg -> arg -> unit
  val dec: arg -> unit
  val divsd: arg -> arg -> unit
  val hlt: unit -> unit
  val idiv: arg -> unit
  val imul: arg -> arg option -> unit
  val mul: arg -> unit
  val inc: arg -> unit
  val j: condition -> arg -> unit
  val ja: arg -> unit
  val jae: arg -> unit
  val jb: arg -> unit
  val jbe: arg -> unit
  val je: arg -> unit
  val jg: arg -> unit
  val jmp: arg -> unit
  val jne: arg -> unit
  val jp: arg -> unit
  val lea: arg -> arg -> unit
  val lock_cmpxchg: arg -> arg -> unit
  val lock_xadd: arg -> arg -> unit
  val maxsd: arg -> arg -> unit
  val minsd: arg -> arg -> unit
  val mov: arg -> arg -> unit
  val movapd: arg -> arg -> unit
  val movupd: arg -> arg -> unit
  val movd: arg -> arg -> unit
  val movq: arg -> arg -> unit
  val movsd: arg -> arg -> unit
  val movss: arg -> arg -> unit
  val movsx: arg -> arg -> unit
  val movsxd: arg -> arg -> unit
  val movzx: arg -> arg -> unit
  val mulsd: arg -> arg -> unit
  val neg : arg -> unit
  val nop: unit -> unit
  val or_: arg -> arg -> unit
  val pause: unit -> unit
  val pop: arg -> unit
  val popcnt : arg -> arg -> unit
  val prefetch : bool -> prefetch_temporal_locality_hint -> arg -> unit
  val push: arg -> unit
  val rdtsc: unit -> unit
  val rdpmc: unit -> unit
  val lfence: unit -> unit
  val sfence: unit -> unit
  val mfence: unit -> unit
  val ret: unit -> unit
  val roundsd : rounding -> arg -> arg -> unit
  val sal: arg -> arg -> unit
  val sar: arg -> arg -> unit
  val set: condition -> arg -> unit
  val shr: arg -> arg -> unit
  val sqrtsd: arg -> arg -> unit
  val sub: arg -> arg -> unit
  val subsd: arg -> arg -> unit
  val test: arg -> arg -> unit
  val ucomisd: arg -> arg -> unit
  val xchg: arg -> arg -> unit
  val xor: arg -> arg -> unit
  val xorpd: arg -> arg -> unit

  (* SSE instructions *)

  val cmpps: float_condition -> arg -> arg -> unit
  val shufps: int -> arg -> arg -> unit
  val addps: arg -> arg -> unit
  val subps: arg -> arg -> unit
  val mulps: arg -> arg -> unit
  val divps: arg -> arg -> unit
  val maxps: arg -> arg -> unit
  val minps: arg -> arg -> unit
  val rcpps: arg -> arg -> unit
  val sqrtps: arg -> arg -> unit
  val rsqrtps: arg -> arg -> unit
  val movhlps: arg -> arg -> unit
  val movlhps: arg -> arg -> unit
  val unpckhps: arg -> arg -> unit
  val unpcklps: arg -> arg -> unit
  val movmskps: arg -> arg -> unit

  (* SSE4.2 instructions *)

  val crc32 : arg -> arg -> unit
end
