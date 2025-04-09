[@@@ocaml.warning "+a-37-40-41-42"]

open! Int_replace_polymorphic_compare

let check_index first last index =
  if index < first || index > last
  then Misc.fatal_errorf "Illegal register index %d" index ()

(* Float/SIMD register description *)
module Neon_reg_name = struct
  module Vector = struct
    type t =
      | V8B
      | V16B
      | V4H
      | V8H
      | V2S
      | V4S
      | V1D
      | V2D

    let to_string t =
      match t with
      | V8B -> "8B"
      | V16B -> "16B"
      | V4H -> "4H"
      | V8H -> "8H"
      | V2S -> "2S"
      | V4S -> "4S"
      | V1D -> "1D"
      | V2D -> "2D"

    let name t index = Printf.sprintf "V%d.%s" index (to_string t)
  end

  module Scalar = struct
    type t =
      | B
      | H
      | S
      | D
      | Q

    let to_string t =
      match t with B -> "b" | H -> "h" | S -> "s" | D -> "d" | Q -> "q"

    let name t index = Printf.sprintf "%s%d" (to_string t) index
  end

  type t =
    | Vector of Vector.t
    | Scalar of Scalar.t

  let last = 31

  let check_index _t index = check_index 0 last index

  let name t index =
    match t with
    | Vector v -> Vector.name v index
    | Scalar s -> Scalar.name s index
end

(* General-purpose register description *)
module GP_reg_name = struct
  type t =
    | W
    | X
    | WZR
    | XZR
    | WSP
    | SP

  let last_numbered = 30

  let last = 31

  let check_index t index =
    match t with
    | W | X -> check_index 0 last_numbered index
    | WZR | XZR | WSP | SP -> check_index last last index

  let name t index =
    match t with
    | W -> Printf.sprintf "w%d" index
    | X -> Printf.sprintf "x%d" index
    | WZR -> "wzr"
    | XZR -> "xzr"
    | WSP -> "wsp"
    | SP -> "sp"
end

(* Register representation *)
module Reg_name = struct
  type t =
    | GP of GP_reg_name.t
    | Neon of Neon_reg_name.t

  let check_index t index =
    match t with
    | GP rn -> GP_reg_name.check_index rn index
    | Neon rn -> Neon_reg_name.check_index rn index

  let name t index =
    match t with
    | GP rn -> GP_reg_name.name rn index
    | Neon rn -> Neon_reg_name.name rn index
end

module Reg = struct
  type t =
    { reg_name : Reg_name.t;
      index : int
    }

  let create reg_name index =
    Reg_name.check_index reg_name index;
    { reg_name; index }

  let name t = Reg_name.name t.reg_name t.index

  (* preallocate the registers *)
  let reg_array ~last name = Array.init (last + 1) (fun i -> create name i)

  let reg_x_array =
    reg_array ~last:GP_reg_name.last_numbered (Reg_name.GP GP_reg_name.X)

  let reg_w_array =
    reg_array ~last:GP_reg_name.last_numbered (Reg_name.GP GP_reg_name.W)

  let reg_s_array =
    reg_array ~last:Neon_reg_name.last (Reg_name.Neon (Neon_reg_name.Scalar S))

  let reg_d_array =
    reg_array ~last:Neon_reg_name.last (Reg_name.Neon (Neon_reg_name.Scalar D))

  let reg_q_array =
    reg_array ~last:Neon_reg_name.last (Reg_name.Neon (Neon_reg_name.Scalar Q))

  let reg_v2d_array =
    reg_array ~last:Neon_reg_name.last
      (Reg_name.Neon (Neon_reg_name.Vector V2D))

  (* for special GP registers we use the last index *)
  let sp = create (GP SP) GP_reg_name.last

  let wsp = create (GP WSP) GP_reg_name.last

  let xzr = create (GP XZR) GP_reg_name.last

  let wzr = create (GP WZR) GP_reg_name.last

  let reg_x i = reg_x_array.(i)

  let reg_w i = reg_w_array.(i)

  let reg_s i = reg_s_array.(i)

  let reg_d i = reg_d_array.(i)

  let reg_q i = reg_q_array.(i)

  let reg_v2d i = reg_v2d_array.(i)
end

module Instruction_name = struct
  module Float_cond = struct
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

    let to_string t =
      match t with
      | EQ -> "eq"
      | GT -> "gt"
      | LE -> "le"
      | GE -> "ge"
      | LT -> "lt"
      | NE -> "ne"
      | CC -> "cc"
      | CS -> "cs"
      | LS -> "ls"
      | HI -> "hi"
  end

  module Cond = struct
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
    (* | AL *)
    (* | NV *)

    let to_string t =
      match t with
      | EQ -> "eq"
      | NE -> "ne"
      | CS -> "cs"
      | CC -> "cc"
      | MI -> "mi"
      | PL -> "pl"
      | VS -> "vs"
      | VC -> "vc"
      | HI -> "hi"
      | LS -> "ls"
      | GE -> "ge"
      | LT -> "lt"
      | GT -> "gt"
      | LE -> "le"
  end

  module Rounding_mode = struct
    type t =
      | M
      | P
      | Z
      | X
      | N

    let to_string t =
      match t with M -> "m" | P -> "p" | Z -> "z" | X -> "x" | N -> "n"
  end

  module Memory_barrier = struct
    type t =
      | SY
      | LD
      | ST
      | ISH
      | ISHLD
      | ISHST
      | NSH
      | NSHLD
      | NSHST
      | OSH
      | OSHLD
      | OSHST

    let to_string b =
      match b with
      | SY -> "sy"
      | LD -> "ld"
      | ST -> "st"
      | ISH -> "ish"
      | ISHLD -> "ishld"
      | ISHST -> "ishst"
      | NSH -> "nsh"
      | NSHLD -> "nshld"
      | NSHST -> "nshst"
      | OSH -> "osh"
      | OSHLD -> "oshld"
      | OSHST -> "oshst"
  end

  (* Don't split the type into base and neon to make emit easier to read and
     avoid the need to copy all the instructions in the DSL. *)
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

  (* CR gyorsh: can some of this be automatically generated from the type? *)
  let to_string t =
    match t with
    | NOP -> "nop"
    | ADD -> "add"
    | SUB -> "sub"
    | MUL -> "mul"
    | DIV -> "div"
    | AND -> "and"
    | OR -> "or"
    | XOR -> "xor"
    | LSL -> "lsl"
    | LSR -> "lsr"
    | ASR -> "asr"
    | CLZ -> "clz"
    | RBIT -> "rbit"
    | CNT -> "cnt"
    | SMULH -> "smulh"
    | UMULH -> "umulh"
    | ORR -> "orr"
    | EOR -> "eor"
    | B -> "b"
    | BR -> "br"
    | B_cond c -> "b." ^ Cond.to_string c
    | B_cond_float c -> "b." ^ Float_cond.to_string c
    | BL -> "bl"
    | BLR -> "blr"
    | CMP -> "cmp"
    | CMN -> "cmn"
    | CBNZ -> "cbnz"
    | CBZ -> "cbz"
    | CSEL -> "csel"
    | CSET -> "cset"
    | SXTB -> "sxtb"
    | SXTH -> "sxth"
    | SXTW -> "sxtw"
    | UXTB -> "uxtb"
    | UXTH -> "uxth"
    | LDR -> "ldr"
    | LDRB -> "ldrb"
    | LDRSB -> "ldrsb"
    | LDRH -> "ldrh"
    | LDRSH -> "ldrsh"
    | LDRSW -> "ldrsw"
    | LDP -> "ldp"
    | LDAR -> "ldar"
    | STR -> "str"
    | STRB -> "strb"
    | STRH -> "strh"
    | DMB b -> "dmb\t" ^ Memory_barrier.to_string b
    | DSB b -> "dsb\t" ^ Memory_barrier.to_string b
    | ISB -> "isb"
    | SDIV -> "sdiv"
    | MSUB -> "msub"
    | MADD -> "madd"
    | REV -> "rev"
    | REV16 -> "rev16"
    | UBFM -> "ubfm"
    | SBFM -> "sbfm"
    | TST -> "tst"
    | TBNZ -> "tbnz"
    | TBZ -> "tbz"
    | ADR -> "adr"
    | ADRP -> "adrp"
    | STP -> "stp"
    | RET -> "ret"
    (* neon *)
    | MOV -> "mov"
    | MOVI -> "movi"
    | MOVN -> "movn"
    | MOVK -> "movk"
    | MOVZ -> "movz"
    | FMOV -> "fmov"
    | FADD -> "fadd"
    | FSUB -> "fsub"
    | FMUL -> "fmul"
    | FDIV -> "fdiv"
    | FNMUL -> "fnmul"
    | FMADD -> "fmadd"
    | FNMADD -> "fnmadd"
    | FMSUB -> "fmsub"
    | FNMSUB -> "fnmsub"
    | FNEG -> "fneg"
    | FABS -> "fabs"
    | FSQRT -> "fsqrt"
    | FCVT -> "fcvt"
    | FCVTZS -> "fcvtzs"
    | FCVTNS -> "fcvtns"
    | SCVTF -> "scvtf"
    | FRINT rm -> "frint" ^ Rounding_mode.to_string rm
    | FRINT64 rm -> "frint64" ^ Rounding_mode.to_string rm
    | FMIN -> "fmin"
    | FMAX -> "fmax"
    | ZIP1 -> "zip1"
    | ZIP2 -> "zip2"
    | FCMP -> "fcmp"
    | FCSEL -> "fcsel"
    | FRECPE -> "frecpe"
    | FRSQRTE -> "frsqrte"
    | FADDP -> "faddp"
    | FCM cond -> "fcm" ^ Float_cond.to_string cond
    | CM cond -> "cm" ^ Cond.to_string cond
    | FCVTL -> "fcvtl"
    | ADDV -> "addv"
end

module Symbol = struct
  type reloc_directive =
    | LOWER_TWELVE
    | GOT_PAGE
    | GOT_PAGE_OFF
    | GOT
    | GOT_LOWER_TWELVE
    | PAGE
    | PAGE_OFF

  type t =
    { name : string;
      offset : int;
      reloc : reloc_directive option
    }

  let create ?reloc ?(offset = 0) name : t = { name; offset; reloc }

  let print_with_reloc_directive ppf (s, reloc) =
    match reloc with
    | None -> Format.pp_print_string ppf s
    | Some LOWER_TWELVE -> Format.fprintf ppf ":lo12:%s" s
    | Some GOT -> Format.fprintf ppf ":got:%s" s
    | Some GOT_LOWER_TWELVE -> Format.fprintf ppf ":got_lo12:%s" s
    | Some GOT_PAGE -> Format.fprintf ppf "%s%@GOTPAGE" s
    | Some GOT_PAGE_OFF -> Format.fprintf ppf "%s%@GOTPAGEOFF" s
    | Some PAGE -> Format.fprintf ppf "%s%@PAGE" s
    | Some PAGE_OFF -> Format.fprintf ppf "%s%@PAGEOFF" s

  let print_int_offset ppf ofs =
    if ofs > 0
    then Format.fprintf ppf "+%d" ofs
    else if ofs < 0
    then Format.fprintf ppf "-%d" (-ofs)
    else ()

  let print ppf ({ name; offset; reloc } : t) =
    Format.fprintf ppf "%a%a" print_with_reloc_directive (name, reloc)
      print_int_offset offset
end

module Operand = struct
  module Imm = struct
    (* int is big enough for all instruction encodings *)
    type t = int

    let print ppf t = Format.fprintf ppf "#%d" t
  end

  module Extend = struct
    module Kind = struct
      type t =
        | UXTB (* 000 *)
        | UXTH (* 001 *)
        | UXTW (* 010 *)
        | UXTX (* 011 *)
        (* alias for LSL, requires shift *)
        | SXTB (* 100 *)
        | SXTH (* 101 *)
        | SXTW (* 110 *)
        | SXTX (* 111 *)

      let to_string t =
        match t with
        | UXTB -> "uxtb"
        | UXTH -> "uxth"
        | UXTW -> "uxtw"
        | UXTX -> "uxtx"
        | SXTB -> "sxtb"
        | SXTH -> "sxth"
        | SXTW -> "sxtw"
        | SXTX -> "sxtx"
    end

    type t =
      { kind : Kind.t;
        shift : Imm.t option
      }

    let print ppf t =
      let { kind; shift } = t in
      Format.fprintf ppf "%s" (Kind.to_string kind);
      Option.iter (fun s -> Format.fprintf ppf " %a" Imm.print s) shift
  end

  module Shift = struct
    module Kind = struct
      type t =
        | LSL
        | ASR
        | LSR

      let to_string t =
        match t with LSL -> "lsl" | ASR -> "asr" | LSR -> "lsr"
    end

    type t =
      { kind : Kind.t;
        amount : Imm.t
      }

    let print ppf t =
      let { kind; amount } = t in
      Format.fprintf ppf "%s %a" (Kind.to_string kind) Imm.print amount
  end

  let print_separator ppf () = Format.fprintf ppf ", "

  type label = string

  module Addressing_mode = struct
    module Offset = struct
      type t =
        | Imm of Imm.t
        | Symbol of Symbol.t

      let print ppf t =
        match t with Imm i -> Imm.print ppf i | Symbol s -> Symbol.print ppf s
    end

    type t =
      | Reg of Reg.t
      | Offset of Reg.t * Offset.t
      | Pre of Reg.t * Offset.t
      | Post of Reg.t * Offset.t
      | Literal of label

    let print ppf t =
      let open Format in
      match t with
      | Reg r -> fprintf ppf "[%s]" (Reg.name r)
      | Offset (r, off) -> fprintf ppf "[%s, %a]" (Reg.name r) Offset.print off
      | Pre (r, off) -> fprintf ppf "[%s, %a]!" (Reg.name r) Offset.print off
      | Post (r, off) -> fprintf ppf "[%s], %a" (Reg.name r) Offset.print off
      | Literal l -> fprintf ppf "%s" l
  end

  type t =
    | Imm of Imm.t
    | Imm_float of float
    | Imm_nativeint of nativeint
    | Reg of Reg.t
    | Extend of Extend.t
    | Shift of Shift.t
    | Sym of Symbol.t
    | Cond of Instruction_name.Cond.t
    | FloatCond of Instruction_name.Float_cond.t
    | Mem of Addressing_mode.t

  let print ppf t =
    match t with
    | Imm imm -> Imm.print ppf imm
    | Imm_float f -> Format.fprintf ppf "#%.7f" f
    | Imm_nativeint n -> Format.fprintf ppf "#%s" (Nativeint.to_string n)
    | Reg r -> Format.fprintf ppf "%s" (Reg.name r)
    | Extend e -> Extend.print ppf e
    | Shift s -> Shift.print ppf s
    | Sym s -> Symbol.print ppf s
    | Cond c -> Format.fprintf ppf "%s" (Instruction_name.Cond.to_string c)
    | FloatCond c ->
      Format.fprintf ppf "%s" (Instruction_name.Float_cond.to_string c)
    | Mem m -> Format.fprintf ppf "%a" Addressing_mode.print m

  (* We preallocate operand arrays for common registers *)
  let reg_x = Array.map (fun x -> Reg x) Reg.reg_x_array

  let reg_w = Array.map (fun x -> Reg x) Reg.reg_w_array

  let reg_s = Array.map (fun x -> Reg x) Reg.reg_s_array

  let reg_d = Array.map (fun x -> Reg x) Reg.reg_d_array

  let reg_q = Array.map (fun x -> Reg x) Reg.reg_q_array

  let reg_v2d = Array.map (fun x -> Reg x) Reg.reg_v2d_array
end

module Instruction = struct
  type t =
    { name : Instruction_name.t;
      operands : Operand.t array
    }

  let create name operands = { name; operands }

  let print ppf t =
    let { name; operands } = t in
    let pp_sep = Operand.print_separator in
    if Array.length operands = 0
    then Format.fprintf ppf "%s" (Instruction_name.to_string name)
    else
      Format.fprintf ppf "%s\t%a"
        (Instruction_name.to_string name)
        (Format.pp_print_seq ~pp_sep Operand.print)
        (Array.to_seq operands)
end

module Asm = struct
  (** Emit assembly instructions for gas. *)

  (* CR-soon gyorsh: The directives defined below duplicate [X86_ast]. Another
     version of these directives is in [backend/asm_targets]. Refactor and
     remove duplication. *)

  type constant =
    | Const of int64
    | ConstThis
    | ConstLabel of string
    | ConstLabelOffset of string * int
    | ConstAdd of constant * constant
    | ConstSub of constant * constant

  (* CR gyorsh: use inline record for Section and File constructors. *)
  type line =
    | Ins of Instruction.t
    | Align of bool * int
    | Byte of constant
    | Bytes of string
    | Comment of string
    | Global of string
    | Protected of string
    | Hidden of string
    | Weak of string
    | Long of constant
    | NewLabel of string
    | NewLine
    | Quad of constant
    | Section of string list * string option * string list * bool
    | Sleb128 of constant
    | Space of int
    | Uleb128 of constant
    | Word of constant
    (* gas only (the masm emitter will fail on them) *)
    | Cfi_adjust_cfa_offset of int
    | Cfi_endproc
    | Cfi_startproc
    | Cfi_remember_state
    | Cfi_restore_state
    | Cfi_def_cfa_register of string
    | Cfi_def_cfa_offset of int
    | File of int * string (* (file_num, file_name) *)
    | Indirect_symbol of string
    | Loc of
        { file_num : int;
          line : int;
          col : int;
          discriminator : int option
        }
    | Private_extern of string
    | Set of string * constant
    | Size of string * constant
    | Type of string * string
    (* MacOS only *)
    | Direct_assignment of string * constant

  let print_line ppf line =
    match line with
    | Ins i -> Format.fprintf ppf "\t%a\n" Instruction.print i
    | NewLine | Cfi_endproc | Cfi_startproc | Cfi_remember_state
    | Cfi_restore_state | Byte _ | Bytes _ | Comment _ | Global _ | Protected _
    | Hidden _ | Weak _ | Long _ | NewLabel _ | Quad _
    | Section (_, _, _, _)
    | Sleb128 _ | Space _ | Uleb128 _ | Word _ | Cfi_adjust_cfa_offset _
    | Cfi_def_cfa_register _ | Cfi_def_cfa_offset _
    | File (_, _)
    | Indirect_symbol _ | Loc _ | Private_extern _
    | Set (_, _)
    | Size (_, _)
    | Type (_, _)
    | Direct_assignment (_, _)
    | Align _ ->
      Misc.fatal_error "Directive not implemented yet"

  let print_program ppf lines = List.iter (print_line ppf) lines

  let generate_asm oc lines =
    let ppf = Format.formatter_of_out_channel oc in
    print_program ppf lines
end

module DSL = struct
  let symbol (s : Symbol.t) = Operand.Sym s

  let mem ~base = Operand.(Mem (Addressing_mode.Reg base))

  let mem_offset ~base ~offset =
    Operand.(Mem (Addressing_mode.Offset (base, Imm offset)))

  let mem_symbol ~base ~symbol =
    Operand.(Mem (Addressing_mode.Offset (base, Symbol symbol)))

  let mem_pre ~base ~offset =
    Operand.(Mem (Addressing_mode.Pre (base, Imm offset)))

  let mem_post ~base ~offset =
    Operand.(Mem (Addressing_mode.Post (base, Imm offset)))

  let shift ~kind ~amount = Operand.Shift { kind; amount }

  let reg_v2s index =
    Operand.Reg (Reg.create (Reg_name.Neon (Vector V2S)) index)

  let reg_v4s index =
    Operand.Reg (Reg.create (Reg_name.Neon (Vector V4S)) index)

  let reg_v2d index = Operand.reg_v2d.(index)

  let reg_s index = Operand.reg_s.(index)

  let reg_d index = Operand.reg_d.(index)

  let reg_q index = Operand.reg_q.(index)

  let reg_w index = Operand.reg_w.(index)

  let reg_x index = Operand.reg_x.(index)

  let sp = Operand.Reg Reg.sp

  let xzr = Operand.Reg Reg.xzr

  let wzr = Operand.Reg Reg.wzr

  let reg_op reg = Operand.Reg reg

  let imm n = Operand.Imm n

  let imm_float f = Operand.Imm_float f

  let imm_nativeint n = Operand.Imm_nativeint n

  let cond c = Operand.Cond c

  (* CR sspies: probably these should be part of the instruction name instead *)
  let float_cond c = Operand.FloatCond c

  let ins name operands = Asm.Ins (Instruction.create name operands)

  let ins_cond name cond operands =
    let operands = Array.append operands [| Operand.Cond cond |] in
    ins name operands

  let print_line line = Format.asprintf "%a" Asm.print_line line

  let print_ins name operands = print_line (ins name operands)

  let print_ins_cond name cond operands =
    print_line (ins_cond name cond operands)

  module Acc = struct
    (* collect instructions *)
    let asm_code = ref []

    let reset_asm_code () = asm_code := []

    let append line = asm_code := line :: !asm_code

    let ins name operands = append (ins name operands)

    let ins_cond name cond operands = append (ins_cond name cond operands)

    let generate_asm oc =
      let asm_code = List.rev !asm_code in
      reset_asm_code ();
      Asm.generate_asm oc asm_code
  end
end
