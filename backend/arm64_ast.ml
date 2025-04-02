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
end

module Instruction_name = struct
  module Float_cond = struct
    type t =
      | EQ
      | GT
      | LE
      | LT

    let to_string t =
      match t with EQ -> "eq" | GT -> "gt" | LE -> "le" | LT -> "lt"
  end

  module Cond = struct
    type t =
      | EQ
      | NE
      | CS
      | CC
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
      | AL

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
      | AL -> "al"
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
    | B of Cond.t
    | BL
    | BLR
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
    | STR
    (* neon *)
    | MOV
    | MOVI
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
    | B c -> "b." ^ Cond.to_string c
    | BL -> "bl"
    | BLR -> "blr"
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
    | STR -> "str"
    (* neon *)
    | MOV -> "mov"
    | MOVI -> "movi"
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

module Operand = struct
  module Imm = struct
    (* int is big enough for all instruction encodings *)
    type t = int

    let print ppf t = Format.fprintf ppf "#%d" t
  end

  module SymbolOffset = struct
    type t = string * int

    let print ppf ((sym, ofs): t) =
      if ofs > 0 then
        Format.fprintf ppf "#:lo12:%s+%d" sym ofs
      else
        Format.fprintf ppf "#:lo12:%s-%d" sym (-ofs)
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
    (* CR gyorsh: only immediate offsets implemented. *)
    type t =
      | Offset of Reg.t * Imm.t
      | SymbolOffset of Reg.t * string * Imm.t
      | Pre of Reg.t * Imm.t
      | Post of Reg.t * Imm.t
      | Literal of label

    let print ppf t =
      let open Format in
      match t with
      | Offset (r, imm) -> fprintf ppf "[%s, %a]" (Reg.name r) Imm.print imm
      | SymbolOffset (r, s, imm) -> fprintf ppf "[%s, %a]" (Reg.name r) SymbolOffset.print (s, imm)
      | Pre (r, imm) -> fprintf ppf "[%s, %a]!" (Reg.name r) Imm.print imm
      | Post (r, imm) -> fprintf ppf "[%s], %a" (Reg.name r) Imm.print imm
      | Literal l -> fprintf ppf "%s" l
  end

  type t =
    | Imm of Imm.t
    | Reg of Reg.t
    | Extend of Extend.t
    | Shift of Shift.t
    | Sym of string
    | Cond of Instruction_name.Cond.t
    | Mem of Addressing_mode.t

  let print ppf t =
    let open Format in
    match t with
    | Imm imm -> Imm.print ppf imm
    | Reg r -> Format.fprintf ppf "%s" (Reg.name r)
    | Extend e -> Extend.print ppf e
    | Shift s -> Shift.print ppf s
    | Sym s -> fprintf ppf "%s" s
    | Cond c -> Format.fprintf ppf "%s" (Instruction_name.Cond.to_string c)
    | Mem m -> Format.fprintf ppf "%a" Addressing_mode.print m
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
  (* Statically allocate some common combinations *)
  let reg_array size name = Array.init size (fun i -> Reg.create name i)

  let reg_and_operand_array ~last name =
    let size = last + 1 in
    let reg_array = reg_array size name in
    let op_array = Array.init size (fun i -> Operand.Reg reg_array.(i)) in
    reg_array, op_array

  let operand_array ~last name =
    let _reg_array, op_array = reg_and_operand_array ~last name in
    op_array

  let neon_operand_array name =
    operand_array ~last:Neon_reg_name.last (Reg_name.Neon name)

  let gp_reg_and_operand_array name =
    reg_and_operand_array ~last:GP_reg_name.last_numbered (Reg_name.GP name)

  let reg_x, reg_x_operands = gp_reg_and_operand_array GP_reg_name.X

  let _, reg_w_operands = gp_reg_and_operand_array GP_reg_name.W

  let reg_v2d_operands = neon_operand_array Neon_reg_name.(Vector V2D)

  let reg_s_operands = neon_operand_array Neon_reg_name.(Scalar S)

  let reg_d_operands = neon_operand_array Neon_reg_name.(Scalar D)

  let reg_q_operands = neon_operand_array Neon_reg_name.(Scalar Q)

  let mem ~base ~offset =
    Operand.(Mem (Addressing_mode.Offset (reg_x.(base), offset)))

  let mem_symbol ~base ~symbol ~offset =
    Operand.(Mem (Addressing_mode.SymbolOffset (reg_x.(base), symbol, offset)))

  let mem_pre ~base ~offset =
    Operand.(Mem (Addressing_mode.Pre (reg_x.(base), offset)))

  let mem_post ~base ~offset =
    Operand.(Mem (Addressing_mode.Post (reg_x.(base), offset)))

  let literal l = Operand.(Mem (Addressing_mode.Literal l))

  let reg_v2s index =
    Operand.Reg (Reg.create (Reg_name.Neon (Vector V2S)) index)

  let reg_v4s index =
    Operand.Reg (Reg.create (Reg_name.Neon (Vector V4S)) index)

  let reg_v2d index = reg_v2d_operands.(index)

  let reg_s index = reg_s_operands.(index)

  let reg_d index = reg_d_operands.(index)

  let reg_q index = reg_q_operands.(index)

  let reg_w index = reg_w_operands.(index) [@@warning "-32"]

  let reg_x index = reg_x_operands.(index)

  let reg_w index = reg_w_operands.(index)

  let imm n = Operand.Imm n

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
