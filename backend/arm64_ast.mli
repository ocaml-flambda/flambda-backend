[@@@ocaml.warning "+a"]

(* The same physical register has different names in the assembly encoding of
   instructions. The name determines the type of data the instruction operates
   on. *)

(* Float/SIMD register description *)
module Neon_reg_name : sig
  module Vector : sig
    type t =
      | V8B
      | V16B
      | V4H
      | V8H
      | V2S
      | V4S
      | V1D
      | V2D
  end

  module Scalar : sig
    type t =
      | B
      | H
      | S
      | D
      | Q
  end

  type t =
    | Vector of Vector.t
    | Scalar of Scalar.t
end

(* General-purpose register description *)
module GP_reg_name : sig
  type t =
    | W
    | X
    | WZR
    | XZR
    | WSP
    | SP
end

(* Register representation *)
module Reg_name : sig
  type t =
    | GP of GP_reg_name.t
    | Neon of Neon_reg_name.t
end

(* CR sspies: rename Reg.t, since it conflicts with the registers of the linear
   IR. *)
module Reg : sig
  type t

  val create : Reg_name.t -> int -> t

  val reg_x : int -> t

  val reg_w : int -> t

  val reg_d : int -> t

  val reg_s : int -> t

  val reg_q : int -> t

  val reg_v2d : int -> t

  val wzr : t

  val xzr : t

  val wsp : t

  val sp : t
end

module Symbol : sig
  type t

  type reloc_directive =
    | LOWER_TWELVE
    | GOT_PAGE
    | GOT_PAGE_OFF
    | GOT
    | GOT_LOWER_TWELVE
    | PAGE
    | PAGE_OFF

  val create : ?reloc:reloc_directive -> ?offset:int -> string -> t
end

module Operand : sig
  type t

  module Shift : sig
    module Kind : sig
      type t =
        | LSL
        | ASR
        | LSR
    end
  end
end

module Instruction_name : sig
  module Float_cond : sig
    type t =
      | EQ
      | GT
      | LE
      | GE
      | LT
      | NE
      | CC
      | CS
      | LS
      | HI
  end

  module Cond : sig
    type t =
      | EQ
      | NE
      | CS (* alias HS *)
      | CC (* alias LO *)
      | MI
      | PL
      | VS
      | VC
      | HI
      | LS
      | GE
      | LT
      | GT
      | LE
    (* The following are not supported, because NV means AL, but has a different
       encoding. *)
    (* Use unconditional branching instead. *)
    (* | AL *)
    (* | NV *)
  end

  module Rounding_mode : sig
    type t =
      | M
      | P
      | Z
      | X
      | N
  end

  module Memory_barrier : sig
    type t =
      | SY
        (* full system barrier operation; the default; use this for [dmb]/[dsb]
           without arguments *)
      | LD (* waits only for loads to complete *)
      | ST (* waits only for stores to complete *)
      | ISH (* waits only for the inner sharable domain *)
      | ISHLD (* waits only for loads and only for the inner sharable domain *)
      | ISHST (* waits only for stores and only for the inner sharable domain *)
      | NSH (* only out to the point of unification *)
      | NSHLD
        (* waits only for loads and only out to the point of unification *)
      | NSHST
        (* only for stores to complete and only out to the point of
           unification *)
      | OSH (* only to the outer shareable domain *)
      | OSHLD (* waits only for loads and only to the outer shareable domain *)
      | OSHST (* waits only for stores and only to the outer shareable domain *)
  end

  (** mnemonic *)
  type t =
    (* base *)
    | NOP
    | ADD
    | SUB
    | MUL
    | DIV
    | AND
    | OR
    | XOR
    | LSL
    | LSR
    | ASR
    | CLZ
    | CTZ
    | RBIT
    | CNT
    | SMULH
    | UMULH
    | ORR
    | EOR
    | B
    | BR
    | B_cond of Cond.t
    | B_cond_float of Float_cond.t
    | BL
    | BLR
    | CMP
    | CMN
    | CBNZ
    | CBZ
    | CSEL
    | CSET
    | SXTB
    | SXTH
    | SXTW
    | UXTB
    | UXTH
    | LDR
    | LDRB
    | LDRSB
    | LDRH
    | LDRSH
    | LDRSW
    | LDP
    | LDAR
    | STR
    | STRB
    | STRH
    | DMB of Memory_barrier.t
    | DSB of Memory_barrier.t
    | ISB
    | SDIV
    | MSUB
    | MADD
    | REV
    | REV16
    | UBFM
    | SBFM
    | TST
    | TBNZ
    | TBZ
    | ADR
    | ADRP
    | STP
    | RET
    (* neon *)
    | MOV
    | MOVI
    | MOVN
    | MOVK
    | MOVZ
    | FMOV
    | FADD
    | FSUB
    | FMUL
    | FDIV
    | FNMUL
    | FMADD
    | FNMADD
    | FMSUB
    | FNMSUB
    | FNEG
    | FABS
    | FSQRT
    | FCVT
    | FCVTZS
    | FCVTNS
    | SCVTF
    | FRINT of Rounding_mode.t
    | FRINT64 of Rounding_mode.t
    | FMIN
    | FMAX
    | ZIP1
    | ZIP2
    | FCMP
    | FCSEL
    | FRECPE
    | FRSQRTE
    | FADDP
    | FCM of Float_cond.t
    | CM of Cond.t
    | FCVTL
    | ADDV
end

module DSL : sig
  val reg_op : Reg.t -> Operand.t

  val imm : int -> Operand.t

  val imm_float : float -> Operand.t

  val imm_nativeint : nativeint -> Operand.t

  (* ARM symbol operand; string must be converted to the OS specific
     representation first *)
  (* An ARM symbol can be used for both labels and symbols from the symbol
     table; the respective conversion must be applied first *)
  val symbol : Symbol.t -> Operand.t

  val shift : kind:Operand.Shift.Kind.t -> amount:int -> Operand.t

  (* Note: Memory accesses are only allowed on X-registers and the stack
     pointer *)
  val mem : base:Reg.t -> Operand.t

  val mem_offset : base:Reg.t -> offset:int -> Operand.t

  val mem_symbol : base:Reg.t -> symbol:Symbol.t -> Operand.t

  val mem_pre : base:Reg.t -> offset:int -> Operand.t

  val mem_post : base:Reg.t -> offset:int -> Operand.t

  val cond : Instruction_name.Cond.t -> Operand.t

  val float_cond : Instruction_name.Float_cond.t -> Operand.t

  (* The functions below are shorthands for composing [reg_op] and the
     respective function from [Reg] *)
  val reg_v2d : int -> Operand.t

  val reg_v2s : int -> Operand.t

  val reg_v4s : int -> Operand.t

  val reg_v8b : int -> Operand.t

  val reg_b : int -> Operand.t

  val reg_s : int -> Operand.t

  val reg_d : int -> Operand.t

  val reg_q : int -> Operand.t

  val reg_x : int -> Operand.t

  val reg_w : int -> Operand.t

  val sp : Operand.t

  val xzr : Operand.t

  val wzr : Operand.t

  (* CR gyorsh: [print_*] functions below are exposed temporarily to use DSL for
     some but not all instructions in [emit.ml]. They can eventually*)
  val print_ins : Instruction_name.t -> Operand.t array -> string

  val print_ins_cond :
    Instruction_name.t -> Instruction_name.Cond.t -> Operand.t array -> string

  module Acc : sig
    val ins : Instruction_name.t -> Operand.t array -> unit

    val ins_cond :
      Instruction_name.t -> Instruction_name.Cond.t -> Operand.t array -> unit

    val generate_asm : out_channel -> unit
  end
end
