(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                 Benedikt Meurer, University of Siegen                  *)
(*                                                                        *)
(*   Copyright 2013 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2012 Benedikt Meurer.                                      *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
[@@@ocaml.warning "+a-40-41-42"]
(* Emission of ARM assembly code, 64-bit mode *)

(* Correctness: carefully consider any use of [Config], [Clflags],
   [Flambda_backend_flags] and shared variables. For details, see
   [asmgen.mli]. *)

(* CR-soon mshinwell/mslater: needs updating for locals + effects *)

open Misc
open Arch
open Proc
open Reg
open! Operation
open Linear
open Emitaux
module I = Arm64_ast.Instruction_name
module D = Asm_targets.Asm_directives_new
open! Int_replace_polymorphic_compare

(* Tradeoff between code size and code speed *)

let fastcode_flag = ref true

(* Names for special regs *)

let reg_domain_state_ptr = phys_reg Int 25 (* x28 *)

let reg_trap_ptr = phys_reg Int 23 (* x26 *)

let reg_alloc_ptr = phys_reg Int 24 (* x27 *)

let reg_tmp1 = phys_reg Int 26 (* x16 *)

let reg_x8 = phys_reg Int 8 (* x8 *)

let reg_stack_arg_begin = phys_reg Int 17 (* x20 *)

let reg_stack_arg_end = phys_reg Int 18 (* x21 *)

(* Output a label *)
let label_prefix = if macosx then "L" else ".L"

let femit_label out lbl =
  femit_string out label_prefix;
  femit_string out (Label.to_string lbl)

(* Symbols *)

(* CR sdolan: Support local symbol definitions & references on arm64 *)

let femit_symbol out s =
  if macosx then femit_string out "_";
  Emitaux.femit_symbol out s

(* Object types *)

let emit_symbol_type emit_lbl_or_sym lbl_or_sym ty =
  if not macosx
  then
    emit_printf "\t.type\t%a, %%%a\n" emit_lbl_or_sym lbl_or_sym femit_string ty

let emit_symbol_size sym =
  if not macosx
  then emit_printf "\t.size\t%a, .-%a\n" femit_symbol sym femit_symbol sym

(* Likewise, but with the 32-bit name of the register *)

(* Layout of the stack frame *)

let stack_offset = ref 0

let num_stack_slots = Stack_class.Tbl.make 0

let prologue_required = ref false

let contains_calls = ref false

let initial_stack_offset () =
  Proc.initial_stack_offset ~contains_calls:!contains_calls ~num_stack_slots

let frame_size () =
  Proc.frame_size ~stack_offset:!stack_offset ~contains_calls:!contains_calls
    ~num_stack_slots

let slot_offset loc stack_class =
  let offset =
    Proc.slot_offset loc ~stack_class ~stack_offset:!stack_offset
      ~fun_contains_calls:!contains_calls ~fun_num_stack_slots:num_stack_slots
  in
  match offset with
  | Bytes_relative_to_stack_pointer n -> n
  | Bytes_relative_to_domainstate_pointer _ ->
    Misc.fatal_errorf "Not a stack slot"

(* Output an addressing mode *)

module DSL : sig
  val check_reg : Cmm.machtype_component -> Reg.t -> unit

  val mem : base:Arm64_ast.Reg.t -> Arm64_ast.Operand.t

  val mem_pre : base:Arm64_ast.Reg.t -> offset:int -> Arm64_ast.Operand.t

  val mem_post : base:Arm64_ast.Reg.t -> offset:int -> Arm64_ast.Operand.t

  val cond : Arm64_ast.Instruction_name.Cond.t -> Arm64_ast.Operand.t

  val float_cond :
    Arm64_ast.Instruction_name.Float_cond.t -> Arm64_ast.Operand.t

  val emit_reg : Reg.t -> Arm64_ast.Operand.t

  val emit_reg_d : Reg.t -> Arm64_ast.Operand.t

  val emit_reg_s : Reg.t -> Arm64_ast.Operand.t

  val emit_reg_w : Reg.t -> Arm64_ast.Operand.t

  val emit_reg_v2d : Reg.t -> Arm64_ast.Operand.t

  val imm : int -> Arm64_ast.Operand.t

  val imm_float : float -> Arm64_ast.Operand.t

  val imm_nativeint : nativeint -> Arm64_ast.Operand.t

  (* some common registers *)
  val sp : Arm64_ast.Operand.t

  val xzr : Arm64_ast.Operand.t

  val wzr : Arm64_ast.Operand.t

  val reg_x_30 : Arm64_ast.Operand.t

  val reg_x_29 : Arm64_ast.Operand.t

  val reg_s_7 : Arm64_ast.Operand.t

  (* translate registers before producing the operand *)
  val emit_mem : Reg.t -> Arm64_ast.Operand.t

  val emit_mem_offset : Reg.t -> int -> Arm64_ast.Operand.t [@@warning "-32"]

  val emit_mem_pre : Reg.t -> int -> Arm64_ast.Operand.t [@@warning "-32"]

  val emit_mem_post : Reg.t -> int -> Arm64_ast.Operand.t [@@warning "-32"]

  val emit_mem_sp_offset : int -> Arm64_ast.Operand.t

  val emit_addressing : addressing_mode -> Reg.t -> Arm64_ast.Operand.t

  val emit_mem_symbol :
    ?offset:int ->
    ?reloc:Arm64_ast.Symbol.reloc_directive ->
    Reg.t ->
    string ->
    Arm64_ast.Operand.t

  val emit_mem_label :
    ?offset:int ->
    ?reloc:Arm64_ast.Symbol.reloc_directive ->
    Reg.t ->
    label ->
    Arm64_ast.Operand.t

  val emit_shift : Arm64_ast.Operand.Shift.Kind.t -> int -> Arm64_ast.Operand.t

  (* Output a stack reference *)
  val emit_stack : Reg.t -> Arm64_ast.Operand.t

  val emit_label :
    ?offset:int ->
    ?reloc:Arm64_ast.Symbol.reloc_directive ->
    label ->
    Arm64_ast.Operand.t

  val emit_symbol :
    ?offset:int ->
    ?reloc:Arm64_ast.Symbol.reloc_directive ->
    string ->
    Arm64_ast.Operand.t

  val emit_immediate_symbol :
    ?offset:int ->
    ?reloc:Arm64_ast.Symbol.reloc_directive ->
    string ->
    Arm64_ast.Operand.t

  val ins : I.t -> Arm64_ast.Operand.t array -> unit

  val labeled_ins : label -> I.t -> Arm64_ast.Operand.t array -> unit

  val simd_instr : Simd.operation -> Linear.instruction -> unit

  val simd_instr_size : Simd.operation -> int
end = struct
  include Arm64_ast.DSL

  let check_reg typ reg =
    (* same type and not on stack *)
    assert (Cmm.equal_machtype_component typ reg.typ);
    assert (Reg.is_reg reg);
    ()

  (* See [Proc.int_reg_name]. *)
  let[@ocamlformat "disable"] int_reg_name_to_arch_index =
  [| 0;  1;  2; 3; 4; 5; 6 ; 7;    (* 0 - 7 *)
     8; 9; 10; 11; 12; 13; 14; 15; (* 8 - 15 *)
     19; 20; 21; 22; 23; 24; 25;   (* 16 - 22 *)
     26; 27; 28;                   (* 23 - 25 *)
     16; 17; |]
  (* 26 - 27 *)

  let reg_name_to_arch_index reg_class name_index =
    match reg_class with
    | 0 (* general-purpose registers *) ->
      int_reg_name_to_arch_index.(name_index)
    | 1 (* neon registers *) -> name_index
    | _ -> assert false

  let reg_index reg =
    match reg with
    | { loc = Reg r; _ } ->
      let reg_class = Proc.register_class reg in
      let name_index = r - Proc.first_available_register.(reg_class) in
      reg_name_to_arch_index reg_class name_index
    | { loc = Stack _ | Unknown; _ } -> fatal_error "Emit.reg"

  let translate_reg reg =
    let index = reg_index reg in
    (* use machtype to select register name *)
    match reg.typ with
    | Val | Int | Addr -> Arm64_ast.Reg.reg_x index
    | Float -> Arm64_ast.Reg.reg_d index
    | Float32 -> Arm64_ast.Reg.reg_s index
    | Vec128 | Valx2 -> Arm64_ast.Reg.reg_q index

  let emit_reg_v2s reg = reg_v2s (reg_index reg)

  let emit_reg_v4s reg = reg_v4s (reg_index reg)

  let emit_reg_v2d reg = reg_v2d (reg_index reg)

  let emit_reg_w reg = reg_w (reg_index reg)

  let emit_reg_s reg = reg_s (reg_index reg)

  let emit_reg_d reg = reg_d (reg_index reg)

  let emit_reg reg =
    let index = reg_index reg in
    (* use machtype to select register name *)
    match reg.typ with
    | Val | Int | Addr -> reg_x index
    | Float -> reg_d index
    | Float32 -> reg_s index
    | Vec128 | Valx2 -> reg_q index

  let reg_x_30 = reg_x 30

  let reg_x_29 = reg_x 29

  let reg_s_7 = reg_s 7

  let convert_symbol_representation (s : string) =
    if macosx
    then "_" ^ Emitaux.symbol_to_string s
    else Emitaux.symbol_to_string s

  let convert_label_representation (lbl : label) =
    label_prefix ^ Label.to_string lbl

  let emit_label ?offset ?reloc (s : label) =
    let l = convert_label_representation s in
    symbol (Arm64_ast.Symbol.create ?offset ?reloc l)

  let emit_symbol ?offset ?reloc (s : string) =
    let sym = convert_symbol_representation s in
    symbol (Arm64_ast.Symbol.create ?offset ?reloc sym)

  let emit_immediate_symbol ?offset ?reloc (s : string) =
    let sym = convert_symbol_representation s in
    symbol (Arm64_ast.Symbol.create ?offset ?reloc sym)

  let emit_mem r = mem ~base:(translate_reg r)

  let emit_mem_offset r ofs = mem_offset ~base:(translate_reg r) ~offset:ofs

  let emit_mem_pre r ofs = mem_pre ~base:(translate_reg r) ~offset:ofs

  let emit_mem_post r ofs = mem_post ~base:(translate_reg r) ~offset:ofs

  let emit_mem_sp_offset ofs = mem_offset ~base:Arm64_ast.Reg.sp ~offset:ofs

  let emit_addressing addr r =
    match addr with
    | Iindexed ofs -> mem_offset ~base:(translate_reg r) ~offset:ofs
    | Ibased (s, ofs) ->
      assert (not !Clflags.dlcode);
      (* see selection.ml *)
      let sym =
        Arm64_ast.Symbol.create ~reloc:LOWER_TWELVE ~offset:ofs
          (convert_symbol_representation s)
      in
      mem_symbol ~base:(translate_reg r) ~symbol:sym

  let emit_mem_symbol ?offset ?reloc r s =
    let sym = convert_symbol_representation s in
    let base = translate_reg r in
    mem_symbol ~base ~symbol:(Arm64_ast.Symbol.create ?offset ?reloc sym)

  let emit_mem_label ?offset ?reloc r lbl =
    let sym = convert_label_representation lbl in
    let base = translate_reg r in
    mem_symbol ~base ~symbol:(Arm64_ast.Symbol.create ?offset ?reloc sym)

  let emit_stack (r : Reg.t) =
    match r.loc with
    | Stack (Domainstate n) ->
      let ofs = n + (Domainstate.(idx_of_field Domain_extra_params) * 8) in
      mem_offset ~base:(translate_reg reg_domain_state_ptr) ~offset:ofs
    | Stack ((Local _ | Incoming _ | Outgoing _) as s) ->
      let ofs = slot_offset s (Stack_class.of_machtype r.typ) in
      emit_mem_sp_offset ofs
    | Reg _ | Unknown -> fatal_error "Emit.emit_stack"

  let emit_shift kind amount = shift ~kind ~amount

  let check_instr (register_behavior : Simd_proc.register_behavior) i =
    (* Ensure that operation size and register size match. On arm64, operation
       size is encoded solely into the operands (unlike amd64 where the opcode
       itself usually indicates operation size). *)
    match register_behavior with
    | Rf32x2_Rf32x2_to_Rf32x2 ->
      (* float32x2 is represented as Float machtype *)
      check_reg Float i.arg.(0);
      check_reg Float i.arg.(1);
      check_reg Float i.res.(0)
    | Rf32x4_Rf32x4_to_Ri32x4 | Rf32x4_Rf32x4_to_Rf32x4
    | Rf64x2_Rf64x2_to_Rf64x2 | Ri64x2_Ri64x2_to_Ri64x2 ->
      check_reg Vec128 i.arg.(0);
      check_reg Vec128 i.arg.(1);
      check_reg Vec128 i.res.(0)
    | Ri32x4_to_Ri32x4 | Rf32x2_to_Rf64x2 | Rf32x4_to_Rf32x4 | Rf32x4_to_Ri32x4
    | Ri32x4_to_Rf32x4 ->
      check_reg Vec128 i.arg.(0);
      check_reg Vec128 i.res.(0)
    | Rf32_Rf32_to_Rf32 ->
      check_reg Float32 i.arg.(0);
      check_reg Float32 i.arg.(1);
      check_reg Float32 i.res.(0)
    | Rf64_Rf64_to_Rf64 ->
      check_reg Float i.arg.(0);
      check_reg Float i.arg.(1);
      check_reg Float i.res.(0)
    | Rf32_to_Rf32 ->
      check_reg Float32 i.arg.(0);
      check_reg Float32 i.res.(0)
    | Rf64_to_Rf64 ->
      check_reg Float i.arg.(0);
      check_reg Float i.res.(0)
    | Rf32_to_Ri64 ->
      check_reg Float32 i.arg.(0);
      check_reg Int i.res.(0)

  let src_operands ops =
    (* returns a copy of [ops] without the first operand, which is assumed to be
       the destination operand. *)
    Array.sub ops 1 (Array.length ops - 1)

  let emit_regs_binary i =
    [| emit_reg i.res.(0); emit_reg i.arg.(0); emit_reg i.arg.(1) |]

  let emit_regs_unary i = [| emit_reg i.res.(0); emit_reg i.arg.(0) |]

  let ins name ops = print_ins name ops |> Emitaux.emit_string

  let labeled_ins lbl name ops =
    Emitaux.emit_printf "%s:\n" (convert_label_representation lbl);
    print_ins name ops |> Emitaux.emit_string

  let ins_cond name cond ops =
    print_ins_cond name cond ops |> Emitaux.emit_string

  let emit_operands (register_behavior : Simd_proc.register_behavior) i =
    match register_behavior with
    | Rf32x2_Rf32x2_to_Rf32x2 ->
      (* Special case: f32 argument is represented by machtype Float (to avoid
         classifying it as a reinterpret cast), and uses vector f32x2 register
         in the instruction encoding. *)
      [| emit_reg_v2s i.res.(0);
         emit_reg_v2s i.arg.(0);
         emit_reg_v2s i.arg.(1)
      |]
    | Rf32x4_Rf32x4_to_Rf32x4 ->
      [| emit_reg_v4s i.res.(0);
         emit_reg_v4s i.arg.(0);
         emit_reg_v4s i.arg.(1)
      |]
    | Ri64x2_Ri64x2_to_Ri64x2 | Rf64x2_Rf64x2_to_Rf64x2 ->
      [| emit_reg_v2d i.res.(0);
         emit_reg_v2d i.arg.(0);
         emit_reg_v2d i.arg.(1)
      |]
    | Rf32x4_Rf32x4_to_Ri32x4 ->
      [| emit_reg_v4s i.res.(0);
         emit_reg_v4s i.arg.(0);
         emit_reg_v4s i.arg.(1)
      |]
    | Ri32x4_to_Ri32x4 | Rf32x4_to_Rf32x4 | Rf32x4_to_Ri32x4 | Ri32x4_to_Rf32x4
      ->
      [| emit_reg_v4s i.res.(0); emit_reg_v4s i.arg.(0) |]
    | Rf32x2_to_Rf64x2 -> [| emit_reg_v2d i.res.(0); emit_reg_v2s i.arg.(0) |]
    | Rf32_Rf32_to_Rf32 | Rf64_Rf64_to_Rf64 -> emit_regs_binary i
    | Rf64_to_Rf64 | Rf32_to_Rf32 | Rf32_to_Ri64 -> emit_regs_unary i

  let simd_instr_size (op : Simd.operation) =
    match op with
    | Min_scalar_f64 | Max_scalar_f64 -> 2
    | Min_scalar_f32 | Max_scalar_f32 -> 2
    | Round_f32 _ | Round_f64 _ | Round_f32x4 _ | Round_f32_i64 | Zip1_f32
    | Zip1q_f32 | Zip1q_f64 | Zip2q_f64 | Addq_f32 | Subq_f32 | Mulq_f32
    | Divq_f32 | Minq_f32 | Maxq_f32 | Recpeq_f32 | Sqrtq_f32 | Rsqrteq_f32
    | Cvtq_s32_of_f32 | Cvtq_f32_of_s32 | Cvt_f64_f32 | Paddq_f32 | Fmin_f32
    | Fmax_f32 | Addq_i64 | Subq_i64 | Cmp_f32 _ | Cmpz_s32 _ ->
      1

  let emit_rounding_mode (rm : Simd.Rounding_mode.t) : I.Rounding_mode.t =
    match rm with
    | Neg_inf -> I.Rounding_mode.M
    | Pos_inf -> I.Rounding_mode.P
    | Zero -> I.Rounding_mode.Z
    | Current -> I.Rounding_mode.X
    | Nearest -> I.Rounding_mode.N

  let emit_float_cond (cond : Simd.Float_cond.t) : I.Float_cond.t =
    match cond with
    | EQ -> EQ
    | GT -> GT
    | LE -> LE
    | GE -> GE
    | LT -> LT
    | NE -> NE
    | CC -> CC
    | CS -> CS
    | LS -> LS
    | HI -> HI

  let emit_cond (cond : Simd.Cond.t) : I.Cond.t =
    match cond with EQ -> EQ | GT -> GT | GE -> GE | LE -> LE | LT -> LT

  let simd_instr (op : Simd.operation) i =
    let b = Simd_proc.register_behavior op in
    check_instr b i;
    let operands = emit_operands b i in
    match op with
    (* min/max: generate a sequence that matches the weird semantics of amd64
       instruction "minss", even when the flag [FPCR.AH] is not set. A separate
       intrinsics generates fmin/fmax arm64 instructions directly. *)
    | Min_scalar_f32 | Min_scalar_f64 ->
      ins I.FCMP (src_operands operands);
      ins_cond I.FCSEL I.Cond.MI operands
    | Max_scalar_f32 | Max_scalar_f64 ->
      ins I.FCMP (src_operands operands);
      ins_cond I.FCSEL I.Cond.GT operands
    | Round_f32 rm | Round_f64 rm | Round_f32x4 rm ->
      ins (I.FRINT (emit_rounding_mode rm)) operands
    | Round_f32_i64 -> ins I.FCVTNS operands
    | Fmin_f32 -> ins I.FMIN operands
    | Fmax_f32 -> ins I.FMAX operands
    | Zip1_f32 | Zip1q_f32 | Zip1q_f64 -> ins I.ZIP1 operands
    | Zip2q_f64 -> ins I.ZIP2 operands
    | Addq_i64 -> ins I.ADD operands
    | Subq_i64 -> ins I.SUB operands
    | Addq_f32 -> ins I.FADD operands
    | Subq_f32 -> ins I.FSUB operands
    | Mulq_f32 -> ins I.FMUL operands
    | Divq_f32 -> ins I.FDIV operands
    | Minq_f32 -> ins I.FMIN operands
    | Maxq_f32 -> ins I.FMAX operands
    | Recpeq_f32 -> ins I.FRECPE operands
    | Sqrtq_f32 -> ins I.FSQRT operands
    | Rsqrteq_f32 -> ins I.FRSQRTE operands
    | Cvtq_s32_of_f32 -> ins I.FCVT operands
    | Cvtq_f32_of_s32 -> ins I.FCVT operands
    | Cvt_f64_f32 -> ins I.FCVTL operands
    | Paddq_f32 -> ins I.FADDP operands
    | Cmp_f32 c -> ins (I.FCM (emit_float_cond c)) operands
    | Cmpz_s32 c -> ins (I.CM (emit_cond c)) (Array.append operands [| imm 0 |])
end

(* Record live pointers at call points *)

let record_frame_label live dbg =
  let lbl = Cmm.new_label () in
  let live_offset = ref [] in
  Reg.Set.iter
    (function
      | { typ = Val; loc = Reg r; _ } ->
        live_offset := ((r lsl 1) + 1) :: !live_offset
      | { typ = Val; loc = Stack s; _ } as reg ->
        live_offset
          := slot_offset s (Stack_class.of_machtype reg.typ) :: !live_offset
      | { typ = Addr; _ } as r ->
        Misc.fatal_errorf "bad GC root %a" Printreg.reg r
      | { typ = Valx2; _ } as r ->
        (* CR mslater: (SIMD) arm64 *)
        Misc.fatal_errorf "Unexpected Valx2 type of reg %a" Printreg.reg r
      | { typ = Val; loc = Unknown; _ } as r ->
        Misc.fatal_errorf "Unknown location %a" Printreg.reg r
      | { typ = Int | Float | Float32 | Vec128; _ } -> ())
    live;
  record_frame_descr ~label:lbl ~frame_size:(frame_size ())
    ~live_offset:!live_offset dbg;
  lbl

let record_frame live dbg =
  let lbl = record_frame_label live dbg in
  emit_printf "%a:\n" femit_label lbl

(* Record calls to the GC -- we've moved them out of the way *)

type gc_call =
  { gc_lbl: label;                      (* Entry label *)
    gc_return_lbl: label;               (* Where to branch after GC *)
    gc_frame_lbl: label }               (* Label of frame descriptor *)
[@@ocamlformat "disable"]

let call_gc_sites = ref ([] : gc_call list)

let emit_call_gc gc =
  DSL.labeled_ins gc.gc_lbl I.BL [| DSL.emit_symbol "caml_call_gc" |];
  DSL.labeled_ins gc.gc_frame_lbl I.B [| DSL.emit_label gc.gc_return_lbl |]

(* Record calls to local stack reallocation *)

type local_realloc_call =
  { lr_lbl : label;
    lr_return_lbl : label;
    lr_dbg : Debuginfo.t
  }

let local_realloc_sites = ref ([] : local_realloc_call list)

let emit_local_realloc lr =
  emit_printf "%a:\n" femit_label lr.lr_lbl;
  emit_debug_info lr.lr_dbg;
  DSL.ins I.BL [| DSL.emit_symbol "caml_call_local_realloc" |];
  DSL.ins I.B [| DSL.emit_label lr.lr_return_lbl |]

(* Local stack reallocation *)

type stack_realloc =
  { sc_label : Label.t; (* Label of the reallocation code. *)
    sc_return : Label.t; (* Label to return to after reallocation. *)
    sc_max_frame_size_in_bytes : int (* Size for reallocation. *)
  }

let stack_realloc = ref (None : stack_realloc option)

let clear_stack_realloc () = stack_realloc := None

let emit_stack_realloc () =
  match !stack_realloc with
  | None -> ()
  | Some { sc_label; sc_return; sc_max_frame_size_in_bytes } ->
    emit_printf "%a:\n" femit_label sc_label;
    (* Pass the desired frame size on the stack, since all of the
       argument-passing registers may be in use. *)
    DSL.ins I.MOV
      [| DSL.emit_reg reg_tmp1; DSL.imm sc_max_frame_size_in_bytes |];
    DSL.ins I.STP
      [| DSL.emit_reg reg_tmp1;
         DSL.reg_x_30;
         DSL.mem_pre ~base:Arm64_ast.Reg.sp ~offset:(-16)
      |];
    DSL.ins I.BL [| DSL.emit_symbol "caml_call_realloc_stack" |];
    DSL.ins I.LDP
      [| DSL.emit_reg reg_tmp1;
         DSL.reg_x_30;
         DSL.mem_post ~base:Arm64_ast.Reg.sp ~offset:16
      |];
    DSL.ins I.B [| DSL.emit_label sc_return |]

(* Names of various instructions *)

let cond_for_comparison :
    integer_comparison -> Arm64_ast.Instruction_name.Cond.t = function
  | Isigned Ceq -> EQ
  | Isigned Cne -> NE
  | Isigned Cle -> LE
  | Isigned Cge -> GE
  | Isigned Clt -> LT
  | Isigned Cgt -> GT
  | Iunsigned Ceq -> EQ
  | Iunsigned Cne -> NE
  | Iunsigned Cle -> LS
  | Iunsigned Cge -> CS
  | Iunsigned Clt -> CC
  | Iunsigned Cgt -> HI

let instr_for_int_operation = function
  | Iadd -> I.ADD
  | Isub -> I.SUB
  | Imul -> I.MUL
  | Idiv -> I.SDIV
  | Iand -> I.AND
  | Ior -> I.ORR
  | Ixor -> I.EOR
  | Ilsl -> I.LSL
  | Ilsr -> I.LSR
  | Iasr -> I.ASR
  | Iclz { arg_is_non_zero = _ } -> I.CLZ
  | Ipopcnt -> I.CNT
  | Ictz _ | Icomp _ | Imod | Imulh _ -> assert false

(* Decompose an integer constant into four 16-bit shifted fragments. Omit the
   fragments that are equal to "default" (16 zeros or 16 ones). *)

let decompose_int default n =
  let rec decomp n pos =
    if pos >= 64
    then []
    else
      let frag = Nativeint.logand n 0xFFFFn
      and rem = Nativeint.shift_right_logical n 16 in
      if Nativeint.equal frag default
      then decomp rem (pos + 16)
      else (frag, pos) :: decomp rem (pos + 16)
  in
  decomp n 0

(* Load an integer constant into a register *)

let emit_movk dst (f, p) =
  DSL.ins I.MOVK
    [| DSL.emit_reg dst; DSL.imm_nativeint f; DSL.emit_shift LSL p |]

let emit_intconst dst n =
  if is_logical_immediate n
  then DSL.ins I.ORR [| DSL.emit_reg dst; DSL.xzr; DSL.imm_nativeint n |]
  else
    let dz = decompose_int 0x0000n n and dn = decompose_int 0xFFFFn n in
    if List.length dz <= List.length dn
    then (
      match dz with
      | [] -> DSL.ins I.MOV [| DSL.emit_reg dst; DSL.xzr |]
      | (f, p) :: l ->
        DSL.ins I.MOVZ
          [| DSL.emit_reg dst; DSL.imm_nativeint f; DSL.emit_shift LSL p |];
        List.iter (emit_movk dst) l)
    else
      match dn with
      | [] -> DSL.ins I.MOVN [| DSL.emit_reg dst; DSL.imm 0 |]
      | (f, p) :: l ->
        let nf = Nativeint.logxor f 0xFFFFn in
        DSL.ins I.MOVN
          [| DSL.emit_reg dst; DSL.imm_nativeint nf; DSL.emit_shift LSL p |];
        List.iter (emit_movk dst) l

let num_instructions_for_intconst n =
  if is_logical_immediate n
  then 1
  else
    let dz = decompose_int 0x0000n n and dn = decompose_int 0xFFFFn n in
    max 1 (min (List.length dz) (List.length dn))

(* Recognize float constants appropriate for FMOV dst, #fpimm instruction: "a
   normalized binary floating point encoding with 1 sign bit, 4 bits of fraction
   and a 3-bit exponent" *)

let is_immediate_float bits =
  let exp = (Int64.(to_int (shift_right_logical bits 52)) land 0x7FF) - 1023 in
  let mant = Int64.logand bits 0xF_FFFF_FFFF_FFFFL in
  exp >= -3 && exp <= 4
  && Int64.equal (Int64.logand mant 0xF_0000_0000_0000L) mant

let is_immediate_float32 bits =
  let exp = (Int32.(to_int (shift_right_logical bits 23)) land 0x7F) - 63 in
  let mant = Int32.logand bits 0x7F_FFFFl in
  exp >= -3 && exp <= 4 && Int32.equal (Int32.logand mant 0x78_0000l) mant

(* Adjust sp (up or down) by the given byte amount *)

let emit_stack_adjustment n =
  let instr = if n < 0 then I.SUB else I.ADD in
  let m = abs n in
  assert (m < 0x1_000_000);
  let ml = m land 0xFFF and mh = m land 0xFFF_000 in
  if mh <> 0 then DSL.ins instr [| DSL.sp; DSL.sp; DSL.imm mh |];
  if ml <> 0 then DSL.ins instr [| DSL.sp; DSL.sp; DSL.imm ml |];
  if n <> 0 then cfi_adjust_cfa_offset (-n)

(* Deallocate the stack frame and reload the return address before a return or
   tail call *)

let output_epilogue f =
  let n = frame_size () in
  if !contains_calls
  then DSL.ins I.LDR [| DSL.reg_x_30; DSL.emit_mem_sp_offset (n - 8) |];
  if n > 0 then emit_stack_adjustment n;
  f ();
  (* reset CFA back because function body may continue *)
  if n > 0 then cfi_adjust_cfa_offset n

(* Output add-immediate / sub-immediate / cmp-immediate instructions *)

let rec emit_addimm rd rs n =
  if n < 0
  then emit_subimm rd rs (-n)
  else if n <= 0xFFF
  then DSL.ins I.ADD [| DSL.emit_reg rd; DSL.emit_reg rs; DSL.imm n |]
  else (
    assert (n <= 0xFFF_FFF);
    let nl = n land 0xFFF and nh = n land 0xFFF_000 in
    DSL.ins I.ADD [| DSL.emit_reg rd; DSL.emit_reg rs; DSL.imm nh |];
    if nl <> 0
    then DSL.ins I.ADD [| DSL.emit_reg rd; DSL.emit_reg rd; DSL.imm nl |])

and emit_subimm rd rs n =
  if n < 0
  then emit_addimm rd rs (-n)
  else if n <= 0xFFF
  then DSL.ins I.SUB [| DSL.emit_reg rd; DSL.emit_reg rs; DSL.imm n |]
  else (
    assert (n <= 0xFFF_FFF);
    let nl = n land 0xFFF and nh = n land 0xFFF_000 in
    DSL.ins I.SUB [| DSL.emit_reg rd; DSL.emit_reg rs; DSL.imm nh |];
    if nl <> 0
    then DSL.ins I.SUB [| DSL.emit_reg rd; DSL.emit_reg rd; DSL.imm nl |])

let emit_cmpimm rs n =
  if n >= 0
  then DSL.ins I.CMP [| DSL.emit_reg rs; DSL.imm n |]
  else DSL.ins I.CMN [| DSL.emit_reg rs; DSL.imm (-n) |]

(* Name of current function *)
let function_name = ref ""

(* Entry point for tail recursive calls *)
let tailrec_entry_point = ref None

(* Pending floating-point literals *)
let float_literals = ref ([] : (int64 * label) list)

let vec128_literals = ref ([] : (Cmm.vec128_bits * label) list)

(* Label a floating-point literal *)
let add_literal p f =
  try List.assoc f !p
  with Not_found ->
    let lbl = Cmm.new_label () in
    p := (f, lbl) :: !p;
    lbl

let float_literal f = add_literal float_literals f

let vec128_literal f = add_literal vec128_literals f

(* Emit all pending literals *)
let emit_literals p align emit_literal =
  if not (Misc.Stdlib.List.is_empty !p)
  then (
    if macosx
    then
      emit_printf "\t.section __TEXT,__literal%a,%abyte_literals\n" femit_int
        align femit_int align;
    emit_printf "\t.align\t%a\n" femit_int (Misc.log2 align);
    List.iter emit_literal !p;
    p := [])

let emit_float64_directive_with_comment f =
  let comment = Printf.sprintf "\t/* %.12g */" (Int64.float_of_bits f) in
  emit_printf "\t.8byte\t%Ld%s\n" f comment

let emit_float32_directive_with_comment f =
  let comment = Printf.sprintf "\t/* %.12f */" (Int32.float_of_bits f) in
  emit_printf "\t.4byte\t%ld%s\n" f comment

let emit_float_literal (f, lbl) =
  emit_printf "%a:\n" femit_label lbl;
  emit_float64_directive_with_comment f

let emit_vec128_literal (({ high; low } : Cmm.vec128_bits), lbl) =
  emit_printf "%a:\n" femit_label lbl;
  emit_float64_directive_with_comment low;
  emit_float64_directive_with_comment high

let emit_literals () =
  emit_literals float_literals size_float emit_float_literal;
  emit_literals vec128_literals size_vec128 emit_vec128_literal

(* Emit code to load the address of a symbol *)

let emit_load_symbol_addr dst s =
  if macosx
  then (
    DSL.ins I.ADRP [| DSL.emit_reg dst; DSL.emit_symbol s ~reloc:GOT_PAGE |];
    DSL.ins I.LDR
      [| DSL.emit_reg dst; DSL.emit_mem_symbol ~reloc:GOT_PAGE_OFF dst s |])
  else if not !Clflags.dlcode
  then (
    DSL.ins I.ADRP [| DSL.emit_reg dst; DSL.emit_symbol s |];
    DSL.ins I.ADD
      [| DSL.emit_reg dst;
         DSL.emit_reg dst;
         DSL.emit_immediate_symbol ~reloc:LOWER_TWELVE s
      |])
  else (
    DSL.ins I.ADRP [| DSL.emit_reg dst; DSL.emit_symbol ~reloc:GOT s |];
    DSL.ins I.LDR
      [| DSL.emit_reg dst; DSL.emit_mem_symbol ~reloc:GOT_LOWER_TWELVE dst s |])

(* The following functions are used for calculating the sizes of the call GC and
   bounds check points emitted out-of-line from the function body. See
   branch_relaxation.mli. *)

let num_call_gc_points instr =
  let rec loop instr call_gc =
    match instr.desc with
    | Lend -> call_gc
    | Lop (Alloc { mode = Heap; _ }) when !fastcode_flag ->
      loop instr.next (call_gc + 1)
    | Lop Poll -> loop instr.next (call_gc + 1)
    (* The following four should never be seen, since this function is run
       before branch relaxation. *)
    | Lop (Specific (Ifar_alloc _)) | Lop (Specific Ifar_poll) -> assert false
    | Lop (Alloc { mode = Local | Heap; _ })
    | Lop
        (Specific
          ( Imuladd | Imulsub | Inegmulf | Imuladdf | Inegmuladdf | Imulsubf
          | Inegmulsubf | Isqrtf | Imove32
          | Ishiftarith (_, _)
          | Ibswap _ | Isignext _ | Isimd _ ))
    | Lop
        ( Move | Spill | Reload | Opaque | Begin_region | End_region | Dls_get
        | Const_int _ | Const_float32 _ | Const_float _ | Const_symbol _
        | Const_vec128 _ | Stackoffset _ | Load _
        | Store (_, _, _)
        | Intop _
        | Intop_imm (_, _)
        | Intop_atomic _
        | Floatop (_, _)
        | Csel _ | Reinterpret_cast _ | Static_cast _ | Probe_is_enabled _
        | Name_for_debugger _ )
    | Lprologue | Lreloadretaddr | Lreturn | Lentertrap | Lpoptrap | Lcall_op _
    | Llabel _ | Lbranch _
    | Lcondbranch (_, _)
    | Lcondbranch3 (_, _, _)
    | Lswitch _ | Ladjust_stack_offset _ | Lpushtrap _ | Lraise _
    | Lstackcheck _ ->
      loop instr.next call_gc
  in
  loop instr 0

let max_out_of_line_code_offset ~num_call_gc =
  if num_call_gc < 1
  then 0
  else
    let size_of_call_gc = 2 in
    let size_of_last_thing = size_of_call_gc in
    let total_size = size_of_call_gc * num_call_gc in
    let max_offset = total_size - size_of_last_thing in
    assert (max_offset >= 0);
    max_offset

module BR = Branch_relaxation.Make (struct
  (* CR-someday mshinwell: B and BL have +/- 128Mb ranges; for the moment we
     assume we will never exceed this. It would seem to be most likely to occur
     for branches between functions; in this case, the linker should be able to
     insert veneers anyway. (See section 4.6.7 of the document "ELF for the ARM
     64-bit architecture (AArch64)".) *)

  type distance = int

  module Cond_branch = struct
    type t =
      | TB
      | CB
      | Bcc

    let all = [TB; CB; Bcc]

    (* AArch64 instructions are 32 bits wide, so [distance] in this module means
       units of 32-bit words. *)
    let max_displacement = function
      | TB -> 32 * 1024 / 4 (* +/- 32Kb *)
      | CB | Bcc -> 1 * 1024 * 1024 / 4 (* +/- 1Mb *)

    let classify_instr = function
      | Lop (Alloc _) | Lop Poll -> Some Bcc
      (* The various "far" variants in [specific_operation] don't need to return
         [Some] here, since their code sequences never contain any conditional
         branches that might need relaxing. *)
      | Lcondbranch (Itruetest, _) | Lcondbranch (Ifalsetest, _) -> Some CB
      | Lcondbranch (Iinttest _, _)
      | Lcondbranch (Iinttest_imm _, _)
      | Lcondbranch (Ifloattest _, _) ->
        Some Bcc
      | Lcondbranch (Ioddtest, _) | Lcondbranch (Ieventest, _) -> Some TB
      | Lcondbranch3 _ -> Some Bcc
      | Lop
          ( Specific _ | Move | Spill | Reload | Opaque | Begin_region
          | End_region | Dls_get | Const_int _ | Const_float32 _ | Const_float _
          | Const_symbol _ | Const_vec128 _ | Stackoffset _ | Load _
          | Store (_, _, _)
          | Intop _
          | Intop_imm (_, _)
          | Intop_atomic _
          | Floatop (_, _)
          | Csel _ | Reinterpret_cast _ | Static_cast _ | Probe_is_enabled _
          | Name_for_debugger _ )
      | Lprologue | Lend | Lreloadretaddr | Lreturn | Lentertrap | Lpoptrap
      | Lcall_op _ | Llabel _ | Lbranch _ | Lswitch _ | Ladjust_stack_offset _
      | Lpushtrap _ | Lraise _ | Lstackcheck _ ->
        None
  end

  let offset_pc_at_branch = 0

  let prologue_size () =
    (if initial_stack_offset () > 0 then 2 else 0)
    + if !contains_calls then 1 else 0

  let epilogue_size () = if !contains_calls then 3 else 2

  let memory_access_size (memory_chunk : Cmm.memory_chunk) =
    match memory_chunk with
    | Single { reg = Float64 } -> 2
    | Single { reg = Float32 } -> 1
    | Byte_unsigned | Byte_signed | Sixteen_unsigned | Sixteen_signed
    | Thirtytwo_unsigned | Thirtytwo_signed | Word_int | Word_val | Double
    | Onetwentyeight_unaligned | Onetwentyeight_aligned ->
      1

  let instr_size = function
    | Lend -> 0
    | Lprologue -> prologue_size ()
    | Lop (Move | Spill | Reload) -> 1
    | Lop (Const_int n) -> num_instructions_for_intconst n
    | Lop (Const_float32 _) -> 2
    | Lop (Const_float _) -> 2
    | Lop (Const_vec128 _) -> 2
    | Lop (Const_symbol _) -> 2
    | Lop (Intop_atomic _) ->
      (* Never generated; builtins are not yet translated to atomics *)
      assert false
    | Lcall_op Lcall_ind -> 1
    | Lcall_op (Lcall_imm _) -> 1
    | Lcall_op Ltailcall_ind -> epilogue_size ()
    | Lcall_op (Ltailcall_imm { func; _ }) ->
      if String.equal func.sym_name !function_name then 1 else epilogue_size ()
    | Lcall_op
        (Lextcall
          { alloc; stack_ofs; func = _; ty_res = _; ty_args = _; returns = _ })
      ->
      if Config.runtime5 && stack_ofs > 0 then 5 else if alloc then 3 else 5
    | Lop (Stackoffset _) -> 2
    | Lop (Load { memory_chunk; addressing_mode; is_atomic; mutability = _ }) ->
      let based = match addressing_mode with Iindexed _ -> 0 | Ibased _ -> 1
      and barrier = if is_atomic then 1 else 0
      and single = memory_access_size memory_chunk in
      based + barrier + single
    | Lop (Store (memory_chunk, addressing_mode, assignment)) ->
      let based = match addressing_mode with Iindexed _ -> 0 | Ibased _ -> 1
      and barrier =
        match memory_chunk, assignment with
        | (Word_int | Word_val), true -> 1
        | (Word_int | Word_val), false -> 0
        | ( ( Byte_unsigned | Byte_signed | Sixteen_unsigned | Sixteen_signed
            | Thirtytwo_unsigned | Thirtytwo_signed | Single _ | Double
            | Onetwentyeight_unaligned | Onetwentyeight_aligned ),
            _ ) ->
          0
      and single = memory_access_size memory_chunk in
      based + barrier + single
    | Lop (Alloc { mode = Local; _ }) -> 9
    | Lop (Alloc { mode = Heap; _ }) when !fastcode_flag -> 5
    | Lop (Specific (Ifar_alloc _)) when !fastcode_flag -> 6
    | Lop Poll -> 3
    | Lop (Specific Ifar_poll) -> 4
    | Lop (Alloc { mode = Heap; bytes = num_bytes; _ })
    | Lop (Specific (Ifar_alloc { bytes = num_bytes; _ })) -> (
      match num_bytes with
      | 16 | 24 | 32 -> 1
      | _ -> 1 + num_instructions_for_intconst (Nativeint.of_int num_bytes))
    | Lop (Csel _) -> 4
    | Lop (Begin_region | End_region) -> 1
    | Lop (Intop (Icomp _)) -> 2
    | Lop (Floatop (Float64, Icompf _)) -> 2
    | Lop (Floatop (Float32, Icompf _)) -> 2
    | Lop (Intop_imm (Icomp _, _)) -> 2
    | Lop (Intop Imod) -> 2
    | Lop (Intop (Imulh _)) -> 1
    | Lop (Intop (Iclz _)) -> 1
    | Lop (Intop (Ictz _)) -> 2
    | Lop
        (Intop
          ( Iadd | Isub | Imul | Idiv | Iand | Ior | Ixor | Ilsl | Ilsr | Iasr
          | Ipopcnt )) ->
      1
    | Lop
        (Intop_imm
          ( ( Iadd | Isub | Imul | Idiv | Imod | Imulh _ | Iand | Ior | Ixor
            | Ilsl | Ilsr | Iasr | Iclz _ | Ictz _ | Ipopcnt ),
            _ )) ->
      1
    | Lop (Floatop (Float64, (Iabsf | Inegf))) -> 1
    | Lop (Floatop (Float32, (Iabsf | Inegf))) -> 1
    | Lop (Specific Isqrtf) -> 1
    | Lop
        (Reinterpret_cast
          (Value_of_int | Int_of_value | Float_of_int64 | Int64_of_float)) ->
      1
    | Lop
        (Reinterpret_cast
          ( Float32_of_float | Float_of_float32 | Float32_of_int32
          | Int32_of_float32 )) ->
      1
    | Lop (Reinterpret_cast V128_of_v128) -> 1
    | Lop (Static_cast (Float_of_int Float64 | Int_of_float Float64)) -> 1
    | Lop
        (Static_cast
          ( Float_of_int Float32
          | Int_of_float Float32
          | Float_of_float32 | Float32_of_float )) ->
      1
    | Lop (Static_cast (Scalar_of_v128 (Int8x16 | Int16x8))) -> 2
    | Lop
        (Static_cast
          (Scalar_of_v128 (Int32x4 | Int64x2 | Float32x4 | Float64x2))) ->
      1
    | Lop (Static_cast (V128_of_scalar _)) -> 1
    | Lop (Floatop (Float64, (Iaddf | Isubf | Imulf | Idivf))) -> 1
    | Lop (Floatop (Float32, (Iaddf | Isubf | Imulf | Idivf))) -> 1
    | Lop (Specific Inegmulf) -> 1
    | Lop Opaque -> 0
    | Lop (Specific (Imuladdf | Inegmuladdf | Imulsubf | Inegmulsubf)) -> 1
    | Lop (Specific (Ishiftarith _)) -> 1
    | Lop (Specific (Imuladd | Imulsub)) -> 1
    | Lop (Specific (Ibswap { bitwidth = Sixteen })) -> 2
    | Lop (Specific (Ibswap { bitwidth = Thirtytwo | Sixtyfour })) -> 1
    | Lop (Specific Imove32) -> 1
    | Lop (Specific (Isignext _)) -> 1
    | Lop (Name_for_debugger _) -> 0
    | Lcall_op (Lprobe _) | Lop (Probe_is_enabled _) ->
      fatal_error "Probes not supported."
    | Lop Dls_get -> 1
    | Lreloadretaddr -> 0
    | Lreturn -> epilogue_size ()
    | Llabel _ -> 0
    | Lbranch _ -> 1
    | Lcondbranch (tst, _) -> (
      match tst with
      | Itruetest -> 1
      | Ifalsetest -> 1
      | Iinttest _ -> 2
      | Iinttest_imm _ -> 2
      | Ifloattest _ -> 2
      | Ioddtest -> 1
      | Ieventest -> 1)
    | Lcondbranch3 (lbl0, lbl1, lbl2) -> (
      1
      + (match lbl0 with None -> 0 | Some _ -> 1)
      + (match lbl1 with None -> 0 | Some _ -> 1)
      + match lbl2 with None -> 0 | Some _ -> 1)
    | Lswitch jumptbl -> 3 + Array.length jumptbl
    | Lentertrap -> 0
    | Ladjust_stack_offset _ -> 0
    | Lpushtrap _ -> 4
    | Lpoptrap -> 1
    | Lraise k -> (
      match k with
      | Lambda.Raise_regular -> 1
      | Lambda.Raise_reraise -> 1
      | Lambda.Raise_notrace -> 4)
    | Lstackcheck _ -> 5
    | Lop (Specific (Isimd simd)) -> DSL.simd_instr_size simd

  let relax_poll () = Lop (Specific Ifar_poll)

  let relax_allocation ~num_bytes ~dbginfo =
    Lop (Specific (Ifar_alloc { bytes = num_bytes; dbginfo }))
end)

let cond_for_float_comparison :
    Cmm.float_comparison -> Arm64_ast.Instruction_name.Float_cond.t = function
  | CFeq -> EQ
  | CFneq -> NE
  | CFlt -> CC
  | CFnlt -> CS
  | CFle -> LS
  | CFnle -> HI
  | CFgt -> GT
  | CFngt -> LE
  | CFge -> GE
  | CFnge -> LT

(* Output the assembly code for allocation. *)

let assembly_code_for_allocation i ~local ~n ~far ~dbginfo =
  if local
  then (
    let r = i.res.(0) in
    let module DS = Domainstate in
    let domain_local_sp_offset = DS.(idx_of_field Domain_local_sp) * 8 in
    let domain_local_limit_offset = DS.(idx_of_field Domain_local_limit) * 8 in
    let domain_local_top_offset = DS.(idx_of_field Domain_local_top) * 8 in
    DSL.ins I.LDR
      [| DSL.emit_reg reg_tmp1;
         DSL.emit_addressing (Iindexed domain_local_limit_offset)
           reg_domain_state_ptr
      |];
    DSL.ins I.LDR
      [| DSL.emit_reg r;
         DSL.emit_addressing (Iindexed domain_local_sp_offset)
           reg_domain_state_ptr
      |];
    emit_subimm r r n;
    DSL.ins I.STR
      [| DSL.emit_reg r;
         DSL.emit_addressing (Iindexed domain_local_sp_offset)
           reg_domain_state_ptr
      |];
    DSL.ins I.CMP [| DSL.emit_reg r; DSL.emit_reg reg_tmp1 |];
    let lbl_call = Cmm.new_label () in
    DSL.ins (I.B_cond LT) [| DSL.emit_label lbl_call |];
    let lbl_after_alloc = Cmm.new_label () in
    emit_printf "%a:\n" femit_label lbl_after_alloc;
    DSL.ins I.LDR
      [| DSL.emit_reg reg_tmp1;
         DSL.emit_addressing (Iindexed domain_local_top_offset)
           reg_domain_state_ptr
      |];
    DSL.ins I.ADD [| DSL.emit_reg r; DSL.emit_reg r; DSL.emit_reg reg_tmp1 |];
    DSL.ins I.ADD [| DSL.emit_reg r; DSL.emit_reg r; DSL.imm 8 |];
    local_realloc_sites
      := { lr_lbl = lbl_call; lr_dbg = i.dbg; lr_return_lbl = lbl_after_alloc }
         :: !local_realloc_sites)
  else
    let lbl_frame = record_frame_label i.live (Dbg_alloc dbginfo) in
    if !fastcode_flag
    then (
      let lbl_after_alloc = Cmm.new_label () in
      let lbl_call_gc = Cmm.new_label () in
      (*= n is at most Max_young_whsize * 8, i.e. currently 0x808,
         so it is reasonable to assume n < 0x1_000.  This makes
         the generated code simpler. *)
      assert (16 <= n && n < 0x1_000 && n land 0x7 = 0);
      let offset = Domainstate.(idx_of_field Domain_young_limit) * 8 in
      DSL.ins I.LDR
        [| DSL.emit_reg reg_tmp1;
           DSL.emit_addressing (Iindexed offset) reg_domain_state_ptr
        |];
      emit_subimm reg_alloc_ptr reg_alloc_ptr n;
      DSL.ins I.CMP [| DSL.emit_reg reg_alloc_ptr; DSL.emit_reg reg_tmp1 |];
      (if not far
      then DSL.ins (I.B_cond CC) [| DSL.emit_label lbl_call_gc |]
      else
        let lbl = Cmm.new_label () in
        DSL.ins (I.B_cond CS) [| DSL.emit_label lbl |];
        DSL.ins I.B [| DSL.emit_label lbl_call_gc |];
        emit_printf "%a:\n" femit_label lbl);
      DSL.labeled_ins lbl_after_alloc I.ADD
        [| DSL.emit_reg i.res.(0); DSL.emit_reg reg_alloc_ptr; DSL.imm 8 |];
      call_gc_sites
        := { gc_lbl = lbl_call_gc;
             gc_return_lbl = lbl_after_alloc;
             gc_frame_lbl = lbl_frame
           }
           :: !call_gc_sites)
    else (
      (match n with
      | 16 -> DSL.ins I.BL [| DSL.emit_symbol "caml_alloc1" |]
      | 24 -> DSL.ins I.BL [| DSL.emit_symbol "caml_alloc2" |]
      | 32 -> DSL.ins I.BL [| DSL.emit_symbol "caml_alloc3" |]
      | _ ->
        emit_intconst reg_x8 (Nativeint.of_int n);
        DSL.ins I.BL [| DSL.emit_symbol "caml_allocN" |]);
      DSL.labeled_ins lbl_frame I.ADD
        [| DSL.emit_reg i.res.(0); DSL.emit_reg reg_alloc_ptr; DSL.imm 8 |])

let assembly_code_for_poll i ~far ~return_label =
  let lbl_frame = record_frame_label i.live (Dbg_alloc []) in
  let lbl_call_gc = Cmm.new_label () in
  let lbl_after_poll =
    match return_label with None -> Cmm.new_label () | Some lbl -> lbl
  in
  let offset = Domainstate.(idx_of_field Domain_young_limit) * 8 in
  DSL.ins I.LDR
    [| DSL.emit_reg reg_tmp1;
       DSL.emit_addressing (Iindexed offset) reg_domain_state_ptr
    |];
  DSL.ins I.CMP [| DSL.emit_reg reg_alloc_ptr; DSL.emit_reg reg_tmp1 |];
  (if not far
  then (
    match return_label with
    | None ->
      DSL.ins (I.B_cond LS) [| DSL.emit_label lbl_call_gc |];
      emit_printf "%a:\n" femit_label lbl_after_poll
    | Some return_label ->
      DSL.ins (I.B_cond HI) [| DSL.emit_label return_label |];
      DSL.ins I.B [| DSL.emit_label lbl_call_gc |])
  else
    match return_label with
    | None ->
      DSL.ins (I.B_cond HI) [| DSL.emit_label lbl_after_poll |];
      DSL.ins I.B [| DSL.emit_label lbl_call_gc |];
      emit_printf "%a:\n" femit_label lbl_after_poll
    | Some return_label ->
      let lbl = Cmm.new_label () in
      DSL.ins (I.B_cond LS) [| DSL.emit_label lbl |];
      DSL.ins I.B [| DSL.emit_label return_label |];
      DSL.labeled_ins lbl I.B [| DSL.emit_label lbl_call_gc |]);
  call_gc_sites
    := { gc_lbl = lbl_call_gc;
         gc_return_lbl = lbl_after_poll;
         gc_frame_lbl = lbl_frame
       }
       :: !call_gc_sites

(* Output .text section directive, or named .text.caml.<name> if enabled. *)

let emit_named_text_section func_name =
  if !Clflags.function_sections
  then
    emit_printf "\t.section .text.caml.%a,%a,%%progbits\n" femit_symbol
      func_name femit_string_literal "ax"
  else emit_printf "\t.text\n"

(* Emit code to load an emitted literal *)

let emit_load_literal dst lbl =
  if macosx
  then (
    DSL.ins I.ADRP [| DSL.emit_reg reg_tmp1; DSL.emit_label ~reloc:PAGE lbl |];
    DSL.ins I.LDR
      [| DSL.emit_reg dst; DSL.emit_mem_label ~reloc:PAGE_OFF reg_tmp1 lbl |])
  else (
    DSL.ins I.ADRP [| DSL.emit_reg reg_tmp1; DSL.emit_label lbl |];
    DSL.ins I.LDR
      [| DSL.emit_reg dst;
         DSL.emit_mem_label ~reloc:LOWER_TWELVE reg_tmp1 lbl
      |])

let move (src : Reg.t) (dst : Reg.t) =
  let distinct = not (Reg.same_loc src dst) in
  if distinct
  then
    match src.typ, src.loc, dst.typ, dst.loc with
    | Float, Reg _, Float, Reg _ | Float32, Reg _, Float32, Reg _ ->
      DSL.ins I.FMOV [| DSL.emit_reg dst; DSL.emit_reg src |]
    | (Vec128 | Valx2), Reg _, (Vec128 | Valx2), Reg _ ->
      DSL.ins I.MOV [| DSL.emit_reg_v2d dst; DSL.emit_reg_v2d src |]
    | (Int | Val | Addr), Reg _, (Int | Val | Addr), Reg _ ->
      DSL.ins I.MOV [| DSL.emit_reg dst; DSL.emit_reg src |]
    | _, Reg _, _, Stack _ ->
      DSL.ins I.STR [| DSL.emit_reg src; DSL.emit_stack dst |]
    | _, Stack _, _, Reg _ ->
      DSL.ins I.LDR [| DSL.emit_reg dst; DSL.emit_stack src |]
    | _, Stack _, _, Stack _ ->
      Misc.fatal_errorf "Illegal move between registers (%a to %a)\n"
        Printreg.reg src Printreg.reg dst
    | _, Unknown, _, (Reg _ | Stack _ | Unknown)
    | _, (Reg _ | Stack _), _, Unknown ->
      Misc.fatal_errorf
        "Illegal move with an unknown register location (%a to %a)\n"
        Printreg.reg src Printreg.reg dst
    | (Float | Float32 | Vec128 | Int | Val | Addr | Valx2), Reg _, _, _ ->
      Misc.fatal_errorf
        "Illegal move between registers of differing types (%a to %a)\n"
        Printreg.reg src Printreg.reg dst

let emit_reinterpret_cast (cast : Cmm.reinterpret_cast) i =
  let src = i.arg.(0) in
  let dst = i.res.(0) in
  let distinct = not (Reg.same_loc src dst) in
  match cast with
  | Int64_of_float ->
    DSL.check_reg Float src;
    DSL.ins I.FMOV [| DSL.emit_reg dst; DSL.emit_reg src |]
  | Float_of_int64 ->
    DSL.check_reg Float dst;
    DSL.ins I.FMOV [| DSL.emit_reg dst; DSL.emit_reg src |]
  | Float32_of_int32 ->
    DSL.check_reg Float32 dst;
    DSL.ins I.FMOV [| DSL.emit_reg dst; DSL.emit_reg_w src |]
  | Int32_of_float32 ->
    DSL.check_reg Float32 src;
    DSL.ins I.FMOV [| DSL.emit_reg_w dst; DSL.emit_reg src |]
  | Float32_of_float ->
    if distinct
    then (
      DSL.check_reg Float src;
      DSL.check_reg Float32 dst;
      DSL.ins I.MOV [| DSL.emit_reg_d dst; DSL.emit_reg_d src |])
  | Float_of_float32 ->
    if distinct
    then (
      DSL.check_reg Float32 src;
      DSL.check_reg Float dst;
      DSL.ins I.MOV [| DSL.emit_reg_d dst; DSL.emit_reg_d src |])
  | V128_of_v128 ->
    if distinct
    then (
      DSL.check_reg Vec128 src;
      DSL.check_reg Vec128 dst;
      DSL.ins I.FMOV [| DSL.emit_reg dst; DSL.emit_reg src |])
  | Int_of_value | Value_of_int -> move src dst

let emit_static_cast (cast : Cmm.static_cast) i =
  let dst = i.res.(0) in
  let src = i.arg.(0) in
  let distinct = not (Reg.same_loc src dst) in
  match cast with
  | Int_of_float Float64 ->
    DSL.check_reg Float src;
    DSL.ins I.FCVTZS [| DSL.emit_reg dst; DSL.emit_reg src |]
  | Int_of_float Float32 ->
    DSL.check_reg Float32 src;
    DSL.ins I.FCVTZS [| DSL.emit_reg dst; DSL.emit_reg src |]
  | Float_of_int Float64 ->
    DSL.check_reg Float dst;
    DSL.ins I.SCVTF [| DSL.emit_reg dst; DSL.emit_reg src |]
  | Float_of_int Float32 ->
    DSL.check_reg Float32 dst;
    DSL.ins I.SCVTF [| DSL.emit_reg dst; DSL.emit_reg src |]
  | Float_of_float32 ->
    DSL.check_reg Float dst;
    DSL.check_reg Float32 src;
    DSL.ins I.FCVT [| DSL.emit_reg dst; DSL.emit_reg src |]
  | Float32_of_float ->
    DSL.check_reg Float32 dst;
    DSL.check_reg Float src;
    DSL.ins I.FCVT [| DSL.emit_reg dst; DSL.emit_reg src |]
  | Scalar_of_v128 v -> (
    DSL.check_reg Vec128 src;
    match v with
    | Int8x16 ->
      DSL.ins I.FMOV [| DSL.emit_reg_w dst; DSL.emit_reg_s src |];
      DSL.ins I.UXTB [| DSL.emit_reg dst; DSL.emit_reg_w dst |]
    | Int16x8 ->
      DSL.ins I.FMOV [| DSL.emit_reg_w dst; DSL.emit_reg_s src |];
      DSL.ins I.UXTH [| DSL.emit_reg dst; DSL.emit_reg_w dst |]
    | Int32x4 -> DSL.ins I.FMOV [| DSL.emit_reg_w dst; DSL.emit_reg_s src |]
    | Int64x2 -> DSL.ins I.FMOV [| DSL.emit_reg dst; DSL.emit_reg_d src |]
    | Float32x4 ->
      if distinct
      then (
        DSL.check_reg Float32 dst;
        DSL.ins I.FMOV [| DSL.emit_reg dst; DSL.emit_reg_s src |])
    | Float64x2 ->
      if distinct
      then (
        DSL.check_reg Float dst;
        DSL.ins I.FMOV [| DSL.emit_reg dst; DSL.emit_reg_d src |]))
  | V128_of_scalar v -> (
    DSL.check_reg Vec128 dst;
    match v with
    | Int8x16 -> DSL.ins I.FMOV [| DSL.emit_reg_s dst; DSL.emit_reg_w src |]
    | Int16x8 -> DSL.ins I.FMOV [| DSL.emit_reg_s dst; DSL.emit_reg_w src |]
    | Int32x4 -> DSL.ins I.FMOV [| DSL.emit_reg_s dst; DSL.emit_reg_w src |]
    | Int64x2 -> DSL.ins I.FMOV [| DSL.emit_reg_d dst; DSL.emit_reg src |]
    | Float32x4 ->
      if distinct
      then (
        DSL.check_reg Float32 src;
        DSL.ins I.FMOV [| DSL.emit_reg_s dst; DSL.emit_reg src |])
    | Float64x2 ->
      if distinct
      then (
        DSL.check_reg Float src;
        DSL.ins I.FMOV [| DSL.emit_reg_d dst; DSL.emit_reg src |]))

(* Output the assembly code for an instruction *)

let emit_instr i =
  emit_debug_info i.dbg;
  match i.desc with
  | Lend -> ()
  | Lprologue ->
    assert !prologue_required;
    let n = frame_size () in
    if n > 0 then emit_stack_adjustment (-n);
    if !contains_calls
    then (
      cfi_offset ~reg:30 (* return address *) ~offset:(-8);
      DSL.ins I.STR [| DSL.reg_x_30; DSL.emit_mem_sp_offset (n - 8) |])
  | Lop (Intop_atomic _) ->
    (* Never generated; builtins are not yet translated to atomics *)
    assert false
  | Lop (Reinterpret_cast cast) -> emit_reinterpret_cast cast i
  | Lop (Static_cast cast) -> emit_static_cast cast i
  | Lop (Move | Spill | Reload) -> move i.arg.(0) i.res.(0)
  | Lop (Specific Imove32) -> (
    let src = i.arg.(0) and dst = i.res.(0) in
    if not (Reg.same_loc src dst)
    then
      match src.loc, dst.loc with
      | Reg _, Reg _ ->
        DSL.ins I.MOV [| DSL.emit_reg_w dst; DSL.emit_reg_w src |]
      | Reg _, Stack _ ->
        DSL.ins I.STR [| DSL.emit_reg_w src; DSL.emit_stack dst |]
      | Stack _, Reg _ ->
        DSL.ins I.LDR [| DSL.emit_reg_w dst; DSL.emit_stack src |]
      | Stack _, Stack _ | _, Unknown | Unknown, _ -> assert false)
  | Lop (Const_int n) -> emit_intconst i.res.(0) n
  | Lop (Const_float32 f) ->
    DSL.check_reg Float32 i.res.(0);
    if Int32.equal f 0l
    then DSL.ins I.FMOV [| DSL.emit_reg i.res.(0); DSL.wzr |]
    else if is_immediate_float32 f
    then
      DSL.ins I.FMOV
        [| DSL.emit_reg i.res.(0); DSL.imm_float (Int32.float_of_bits f) |]
    else
      (* float32 constants still take up 8 bytes; we load the lower half. *)
      let lbl = float_literal (Int64.of_int32 f) in
      emit_load_literal i.res.(0) lbl
  | Lop (Const_float f) ->
    if Int64.equal f 0L
    then DSL.ins I.FMOV [| DSL.emit_reg i.res.(0); DSL.xzr |]
    else if is_immediate_float f
    then
      DSL.ins I.FMOV
        [| DSL.emit_reg i.res.(0); DSL.imm_float (Int64.float_of_bits f) |]
    else
      let lbl = float_literal f in
      emit_load_literal i.res.(0) lbl
  | Lop (Const_vec128 ({ high; low } as l)) -> (
    DSL.check_reg Vec128 i.res.(0);
    match high, low with
    | 0x0000_0000_0000_0000L, 0x0000_0000_0000_0000L ->
      let dst = DSL.emit_reg_v2d i.res.(0) in
      DSL.ins I.MOVI [| dst; DSL.imm 0 |]
    | _ ->
      let lbl = vec128_literal l in
      emit_load_literal i.res.(0) lbl)
  | Lop (Const_symbol s) -> emit_load_symbol_addr i.res.(0) s.sym_name
  | Lcall_op Lcall_ind ->
    DSL.ins I.BLR [| DSL.emit_reg i.arg.(0) |];
    record_frame i.live (Dbg_other i.dbg)
  | Lcall_op (Lcall_imm { func }) ->
    DSL.ins I.BL [| DSL.emit_symbol func.sym_name |];
    record_frame i.live (Dbg_other i.dbg)
  | Lcall_op Ltailcall_ind ->
    output_epilogue (fun () -> DSL.ins I.BR [| DSL.emit_reg i.arg.(0) |])
  | Lcall_op (Ltailcall_imm { func }) ->
    if String.equal func.sym_name !function_name
    then
      match !tailrec_entry_point with
      | None -> Misc.fatal_error "jump to missing tailrec entry point"
      | Some tailrec_entry_point ->
        DSL.ins I.B [| DSL.emit_label tailrec_entry_point |]
    else
      output_epilogue (fun () ->
          DSL.ins I.B [| DSL.emit_symbol func.sym_name |])
  | Lcall_op (Lextcall { func; alloc; stack_ofs; _ }) ->
    if Config.runtime5 && stack_ofs > 0
    then (
      DSL.ins I.MOV [| DSL.emit_reg reg_stack_arg_begin; DSL.sp |];
      DSL.ins I.ADD
        [| DSL.emit_reg reg_stack_arg_end;
           DSL.sp;
           DSL.imm (Misc.align stack_ofs 16)
        |];
      emit_load_symbol_addr reg_x8 func;
      DSL.ins I.BL [| DSL.emit_symbol "caml_c_call_stack_args" |];
      record_frame i.live (Dbg_other i.dbg))
    else if alloc
    then (
      emit_load_symbol_addr reg_x8 func;
      DSL.ins I.BL [| DSL.emit_symbol "caml_c_call" |];
      record_frame i.live (Dbg_other i.dbg))
    else (
      (*= store ocaml stack in the frame pointer register
             NB: no need to store previous x29 because OCaml frames don't
             maintain frame pointer *)
      if Config.runtime5
      then (
        DSL.ins I.MOV [| DSL.reg_x_29; DSL.sp |];
        cfi_remember_state ();
        cfi_def_cfa_register ~reg:29;
        let offset = Domainstate.(idx_of_field Domain_c_stack) * 8 in
        DSL.ins I.LDR
          [| DSL.emit_reg reg_tmp1;
             DSL.emit_addressing (Iindexed offset) reg_domain_state_ptr
          |];
        DSL.ins I.MOV [| DSL.sp; DSL.emit_reg reg_tmp1 |]);
      DSL.ins I.BL [| DSL.emit_symbol func |];
      if Config.runtime5 then DSL.ins I.MOV [| DSL.sp; DSL.reg_x_29 |];
      cfi_restore_state ())
  | Lop (Stackoffset n) ->
    assert (n mod 16 = 0);
    emit_stack_adjustment (-n);
    stack_offset := !stack_offset + n
  | Lop (Load { memory_chunk; addressing_mode; is_atomic; _ }) -> (
    assert (
      Cmm.equal_memory_chunk memory_chunk Cmm.Word_int
      || Cmm.equal_memory_chunk memory_chunk Cmm.Word_val
      || not is_atomic);
    let dst = i.res.(0) in
    let base =
      match addressing_mode with
      | Iindexed _ -> i.arg.(0)
      | Ibased (s, ofs) ->
        assert (not !Clflags.dlcode);
        (* see selection_utils.ml *)
        DSL.ins I.ADRP
          [| DSL.emit_reg reg_tmp1; DSL.emit_symbol ~offset:ofs s |];
        reg_tmp1
    in
    match memory_chunk with
    | Byte_unsigned ->
      DSL.ins I.LDRB
        [| DSL.emit_reg_w dst; DSL.emit_addressing addressing_mode base |]
    | Byte_signed ->
      DSL.ins I.LDRSB
        [| DSL.emit_reg dst; DSL.emit_addressing addressing_mode base |]
    | Sixteen_unsigned ->
      DSL.ins I.LDRH
        [| DSL.emit_reg_w dst; DSL.emit_addressing addressing_mode base |]
    | Sixteen_signed ->
      DSL.ins I.LDRSH
        [| DSL.emit_reg dst; DSL.emit_addressing addressing_mode base |]
    | Thirtytwo_unsigned ->
      DSL.ins I.LDR
        [| DSL.emit_reg_w dst; DSL.emit_addressing addressing_mode base |]
    | Thirtytwo_signed ->
      DSL.ins I.LDRSW
        [| DSL.emit_reg dst; DSL.emit_addressing addressing_mode base |]
    | Single { reg = Float64 } ->
      DSL.check_reg Float dst;
      DSL.ins I.LDR [| DSL.reg_s_7; DSL.emit_addressing addressing_mode base |];
      DSL.ins I.FCVT [| DSL.emit_reg dst; DSL.reg_s_7 |]
    | Word_int | Word_val ->
      if is_atomic
      then (
        assert (Arch.equal_addressing_mode addressing_mode (Iindexed 0));
        DSL.ins (I.DMB ISHLD) [||];
        DSL.ins I.LDAR [| DSL.emit_reg dst; DSL.emit_mem i.arg.(0) |])
      else
        DSL.ins I.LDR
          [| DSL.emit_reg dst; DSL.emit_addressing addressing_mode base |]
    | Double ->
      DSL.ins I.LDR
        [| DSL.emit_reg dst; DSL.emit_addressing addressing_mode base |]
    | Single { reg = Float32 } ->
      DSL.check_reg Float32 dst;
      DSL.ins I.LDR
        [| DSL.emit_reg dst; DSL.emit_addressing addressing_mode base |]
    | Onetwentyeight_aligned | Onetwentyeight_unaligned ->
      (* CR gyorsh: check alignment *)
      DSL.check_reg Vec128 dst;
      DSL.ins I.LDR
        [| DSL.emit_reg dst; DSL.emit_addressing addressing_mode base |])
  | Lop (Store (size, addr, assignment)) -> (
    (* NB: assignments other than Word_int and Word_val do not follow the
       Multicore OCaml memory model and so do not emit a barrier *)
    let src = i.arg.(0) in
    let base =
      match addr with
      | Iindexed _ -> i.arg.(1)
      | Ibased (s, ofs) ->
        assert (not !Clflags.dlcode);
        DSL.ins I.ADRP
          [| DSL.emit_reg reg_tmp1; DSL.emit_symbol ~offset:ofs s |];
        reg_tmp1
    in
    match size with
    | Byte_unsigned | Byte_signed ->
      DSL.ins I.STRB [| DSL.emit_reg_w src; DSL.emit_addressing addr base |]
    | Sixteen_unsigned | Sixteen_signed ->
      DSL.ins I.STRH [| DSL.emit_reg_w src; DSL.emit_addressing addr base |]
    | Thirtytwo_unsigned | Thirtytwo_signed ->
      DSL.ins I.STR [| DSL.emit_reg_w src; DSL.emit_addressing addr base |]
    | Single { reg = Float64 } ->
      DSL.check_reg Float src;
      DSL.ins I.FCVT [| DSL.reg_s_7; DSL.emit_reg src |];
      DSL.ins I.STR [| DSL.reg_s_7; DSL.emit_addressing addr base |]
    | Word_int | Word_val ->
      (* memory model barrier for non-initializing store *)
      if assignment then DSL.ins (I.DMB ISHLD) [||];
      DSL.ins I.STR [| DSL.emit_reg src; DSL.emit_addressing addr base |]
    | Double ->
      DSL.ins I.STR [| DSL.emit_reg src; DSL.emit_addressing addr base |]
    | Single { reg = Float32 } ->
      DSL.check_reg Float32 src;
      DSL.ins I.STR [| DSL.emit_reg src; DSL.emit_addressing addr base |]
    | Onetwentyeight_aligned | Onetwentyeight_unaligned ->
      (* CR gyorsh: check alignment *)
      DSL.check_reg Vec128 src;
      DSL.ins I.STR [| DSL.emit_reg src; DSL.emit_addressing addr base |])
  | Lop (Alloc { bytes = n; dbginfo; mode = Heap }) ->
    assembly_code_for_allocation i ~n ~local:false ~far:false ~dbginfo
  | Lop (Specific (Ifar_alloc { bytes = n; dbginfo })) ->
    assembly_code_for_allocation i ~n ~local:false ~far:true ~dbginfo
  | Lop (Alloc { bytes = n; dbginfo; mode = Local }) ->
    assembly_code_for_allocation i ~n ~local:true ~far:false ~dbginfo
  | Lop Begin_region ->
    let offset = Domainstate.(idx_of_field Domain_local_sp) * 8 in
    DSL.ins I.LDR
      [| DSL.emit_reg i.res.(0);
         DSL.emit_addressing (Iindexed offset) reg_domain_state_ptr
      |]
  | Lop End_region ->
    let offset = Domainstate.(idx_of_field Domain_local_sp) * 8 in
    DSL.ins I.STR
      [| DSL.emit_reg i.arg.(0);
         DSL.emit_addressing (Iindexed offset) reg_domain_state_ptr
      |]
  | Lop Poll -> assembly_code_for_poll i ~far:false ~return_label:None
  | Lop (Specific Ifar_poll) ->
    assembly_code_for_poll i ~far:true ~return_label:None
  | Lop (Intop_imm (Iadd, n)) -> emit_addimm i.res.(0) i.arg.(0) n
  | Lop (Intop_imm (Isub, n)) -> emit_subimm i.res.(0) i.arg.(0) n
  | Lop (Intop (Icomp cmp)) ->
    DSL.ins I.CMP [| DSL.emit_reg i.arg.(0); DSL.emit_reg i.arg.(1) |];
    DSL.ins I.CSET
      [| DSL.emit_reg i.res.(0); DSL.cond (cond_for_comparison cmp) |]
  | Lop (Floatop (Float64, Icompf cmp)) ->
    DSL.check_reg Float i.arg.(0);
    DSL.check_reg Float i.arg.(1);
    let comp = cond_for_float_comparison cmp in
    DSL.ins I.FCMP [| DSL.emit_reg i.arg.(0); DSL.emit_reg i.arg.(1) |];
    DSL.ins I.CSET [| DSL.emit_reg i.res.(0); DSL.float_cond comp |]
  | Lop (Floatop (Float32, Icompf cmp)) ->
    DSL.check_reg Float32 i.arg.(0);
    DSL.check_reg Float32 i.arg.(1);
    let comp = cond_for_float_comparison cmp in
    DSL.ins I.FCMP [| DSL.emit_reg i.arg.(0); DSL.emit_reg i.arg.(1) |];
    DSL.ins I.CSET [| DSL.emit_reg i.res.(0); DSL.float_cond comp |]
  | Lop (Intop_imm (Icomp cmp, n)) ->
    emit_cmpimm i.arg.(0) n;
    DSL.ins I.CSET
      [| DSL.emit_reg i.res.(0); DSL.cond (cond_for_comparison cmp) |]
  | Lop (Intop Imod) ->
    DSL.ins I.SDIV
      [| DSL.emit_reg reg_tmp1;
         DSL.emit_reg i.arg.(0);
         DSL.emit_reg i.arg.(1)
      |];
    DSL.ins I.MSUB
      [| DSL.emit_reg i.res.(0);
         DSL.emit_reg reg_tmp1;
         DSL.emit_reg i.arg.(1);
         DSL.emit_reg i.arg.(0)
      |]
  | Lop (Intop (Imulh { signed = true })) ->
    DSL.ins I.SMULH
      [| DSL.emit_reg i.res.(0);
         DSL.emit_reg i.arg.(0);
         DSL.emit_reg i.arg.(1)
      |]
  | Lop (Intop (Imulh { signed = false })) ->
    DSL.ins I.UMULH
      [| DSL.emit_reg i.res.(0);
         DSL.emit_reg i.arg.(0);
         DSL.emit_reg i.arg.(1)
      |]
  | Lop (Intop (Ictz _)) ->
    (* emit_printf "ctz Rd, Rn" is optionally supported from Armv8.7, but rbit
       and clz are supported in all ARMv8 CPUs. *)
    DSL.ins I.RBIT [| DSL.emit_reg i.res.(0); DSL.emit_reg i.arg.(0) |];
    DSL.ins I.CLZ [| DSL.emit_reg i.res.(0); DSL.emit_reg i.res.(0) |]
  | Lop (Intop (Iclz _ as op)) ->
    let instr = instr_for_int_operation op in
    DSL.ins instr [| DSL.emit_reg i.res.(0); DSL.emit_reg i.arg.(0) |]
  | Lop
      (Intop
        (( Iadd | Isub | Imul | Idiv | Iand | Ior | Ixor | Ilsl | Ilsr | Iasr
         | Ipopcnt ) as op)) ->
    let instr = instr_for_int_operation op in
    DSL.ins instr
      [| DSL.emit_reg i.res.(0);
         DSL.emit_reg i.arg.(0);
         DSL.emit_reg i.arg.(1)
      |]
  | Lop (Intop_imm (op, n)) ->
    let instr = instr_for_int_operation op in
    DSL.ins instr
      [| DSL.emit_reg i.res.(0); DSL.emit_reg i.arg.(0); DSL.imm n |]
  | Lop (Specific Isqrtf) ->
    DSL.ins I.FSQRT [| DSL.emit_reg i.res.(0); DSL.emit_reg i.arg.(0) |]
  | Lop (Floatop ((Float32 | Float64), Iabsf)) ->
    DSL.ins I.FABS [| DSL.emit_reg i.res.(0); DSL.emit_reg i.arg.(0) |]
  | Lop (Floatop ((Float32 | Float64), Inegf)) ->
    DSL.ins I.FNEG [| DSL.emit_reg i.res.(0); DSL.emit_reg i.arg.(0) |]
  | Lop (Floatop ((Float32 | Float64), Iaddf)) ->
    DSL.ins I.FADD
      [| DSL.emit_reg i.res.(0);
         DSL.emit_reg i.arg.(0);
         DSL.emit_reg i.arg.(1)
      |]
  | Lop (Floatop ((Float32 | Float64), Isubf)) ->
    DSL.ins I.FSUB
      [| DSL.emit_reg i.res.(0);
         DSL.emit_reg i.arg.(0);
         DSL.emit_reg i.arg.(1)
      |]
  | Lop (Floatop ((Float32 | Float64), Imulf)) ->
    DSL.ins I.FMUL
      [| DSL.emit_reg i.res.(0);
         DSL.emit_reg i.arg.(0);
         DSL.emit_reg i.arg.(1)
      |]
  | Lop (Floatop ((Float32 | Float64), Idivf)) ->
    DSL.ins I.FDIV
      [| DSL.emit_reg i.res.(0);
         DSL.emit_reg i.arg.(0);
         DSL.emit_reg i.arg.(1)
      |]
  | Lop (Specific Inegmulf) ->
    DSL.ins I.FNMUL
      [| DSL.emit_reg i.res.(0);
         DSL.emit_reg i.arg.(0);
         DSL.emit_reg i.arg.(1)
      |]
  | Lop (Specific Imuladdf) ->
    DSL.ins I.FMADD
      [| DSL.emit_reg i.res.(0);
         DSL.emit_reg i.arg.(1);
         DSL.emit_reg i.arg.(2);
         DSL.emit_reg i.arg.(0)
      |]
  | Lop (Specific Inegmuladdf) ->
    DSL.ins I.FNMADD
      [| DSL.emit_reg i.res.(0);
         DSL.emit_reg i.arg.(1);
         DSL.emit_reg i.arg.(2);
         DSL.emit_reg i.arg.(0)
      |]
  | Lop (Specific Imulsubf) ->
    DSL.ins I.FMSUB
      [| DSL.emit_reg i.res.(0);
         DSL.emit_reg i.arg.(1);
         DSL.emit_reg i.arg.(2);
         DSL.emit_reg i.arg.(0)
      |]
  | Lop (Specific Inegmulsubf) ->
    DSL.ins I.FNMSUB
      [| DSL.emit_reg i.res.(0);
         DSL.emit_reg i.arg.(1);
         DSL.emit_reg i.arg.(2);
         DSL.emit_reg i.arg.(0)
      |]
  | Lop Opaque -> assert (Reg.equal_location i.arg.(0).loc i.res.(0).loc)
  | Lop (Specific (Ishiftarith (op, shift))) ->
    let instr = match op with Ishiftadd -> I.ADD | Ishiftsub -> I.SUB in
    let shift =
      if shift >= 0
      then DSL.emit_shift LSL shift
      else DSL.emit_shift ASR (-shift)
    in
    DSL.ins instr
      [| DSL.emit_reg i.res.(0);
         DSL.emit_reg i.arg.(0);
         DSL.emit_reg i.arg.(1);
         shift
      |]
  | Lop (Specific Imuladd) ->
    DSL.ins I.MADD
      [| DSL.emit_reg i.res.(0);
         DSL.emit_reg i.arg.(0);
         DSL.emit_reg i.arg.(1);
         DSL.emit_reg i.arg.(2)
      |]
  | Lop (Specific Imulsub) ->
    DSL.ins I.MSUB
      [| DSL.emit_reg i.res.(0);
         DSL.emit_reg i.arg.(0);
         DSL.emit_reg i.arg.(1);
         DSL.emit_reg i.arg.(2)
      |]
  | Lop (Specific (Ibswap { bitwidth })) -> (
    match bitwidth with
    | Sixteen ->
      DSL.ins I.REV16 [| DSL.emit_reg_w i.res.(0); DSL.emit_reg_w i.arg.(0) |];
      DSL.ins I.UBFM
        [| DSL.emit_reg i.res.(0);
           DSL.emit_reg i.res.(0);
           DSL.imm 0;
           DSL.imm 15
        |]
    | Thirtytwo ->
      DSL.ins I.REV [| DSL.emit_reg_w i.res.(0); DSL.emit_reg_w i.arg.(0) |]
    | Sixtyfour ->
      DSL.ins I.REV [| DSL.emit_reg i.res.(0); DSL.emit_reg i.arg.(0) |])
  | Lop (Specific (Isignext size)) ->
    DSL.ins I.SBFM
      [| DSL.emit_reg i.res.(0);
         DSL.emit_reg i.arg.(0);
         DSL.imm 0;
         DSL.imm (size - 1)
      |]
  | Lop (Specific (Isimd simd)) -> DSL.simd_instr simd i
  | Lop (Name_for_debugger _) -> ()
  | Lcall_op (Lprobe _) | Lop (Probe_is_enabled _) ->
    fatal_error "Probes not supported."
  | Lop Dls_get ->
    if Config.runtime5
    then
      let offset = Domainstate.(idx_of_field Domain_dls_root) * 8 in
      DSL.ins I.LDR
        [| DSL.emit_reg i.res.(0);
           DSL.emit_addressing (Iindexed offset) reg_domain_state_ptr
        |]
    else Misc.fatal_error "Dls is not supported in runtime4."
  | Lop (Csel tst) -> (
    let len = Array.length i.arg in
    let ifso = i.arg.(len - 2) in
    let ifnot = i.arg.(len - 1) in
    if Reg.same_loc ifso ifnot
    then move ifso i.res.(0)
    else
      match tst with
      | Itruetest ->
        DSL.ins I.CMP [| DSL.emit_reg i.arg.(0); DSL.imm 0 |];
        DSL.ins I.CSEL
          [| DSL.emit_reg i.res.(0);
             DSL.emit_reg i.arg.(1);
             DSL.emit_reg i.arg.(2);
             DSL.cond NE
          |]
      | Ifalsetest ->
        DSL.ins I.CMP [| DSL.emit_reg i.arg.(0); DSL.imm 0 |];
        DSL.ins I.CSEL
          [| DSL.emit_reg i.res.(0);
             DSL.emit_reg i.arg.(1);
             DSL.emit_reg i.arg.(2);
             DSL.cond EQ
          |]
      | Iinttest cmp ->
        let comp = cond_for_comparison cmp in
        DSL.ins I.CMP [| DSL.emit_reg i.arg.(0); DSL.emit_reg i.arg.(1) |];
        DSL.ins I.CSEL
          [| DSL.emit_reg i.res.(0);
             DSL.emit_reg i.arg.(2);
             DSL.emit_reg i.arg.(3);
             DSL.cond comp
          |]
      | Iinttest_imm (cmp, n) ->
        let comp = cond_for_comparison cmp in
        emit_cmpimm i.arg.(0) n;
        DSL.ins I.CSEL
          [| DSL.emit_reg i.res.(0);
             DSL.emit_reg i.arg.(1);
             DSL.emit_reg i.arg.(2);
             DSL.cond comp
          |]
      | Ifloattest ((Float32 | Float64), cmp) ->
        let comp = cond_for_float_comparison cmp in
        DSL.ins I.FCMP [| DSL.emit_reg i.arg.(0); DSL.emit_reg i.arg.(1) |];
        DSL.ins I.CSEL
          [| DSL.emit_reg i.res.(0);
             DSL.emit_reg i.arg.(2);
             DSL.emit_reg i.arg.(3);
             DSL.float_cond comp
          |]
      | Ioddtest ->
        DSL.ins I.TST [| DSL.emit_reg i.arg.(0); DSL.imm 1 |];
        DSL.ins I.CSEL
          [| DSL.emit_reg i.res.(0);
             DSL.emit_reg i.arg.(1);
             DSL.emit_reg i.arg.(2);
             DSL.cond NE
          |]
      | Ieventest ->
        DSL.ins I.TST [| DSL.emit_reg i.arg.(0); DSL.imm 1 |];
        DSL.ins I.CSEL
          [| DSL.emit_reg i.res.(0);
             DSL.emit_reg i.arg.(1);
             DSL.emit_reg i.arg.(2);
             DSL.cond EQ
          |])
  | Lreloadretaddr -> ()
  | Lreturn -> output_epilogue (fun () -> DSL.ins I.RET [||])
  | Llabel { label = lbl; _ } -> emit_printf "%a:\n" femit_label lbl
  | Lbranch lbl -> DSL.ins I.B [| DSL.emit_label lbl |]
  | Lcondbranch (tst, lbl) -> (
    match tst with
    | Itruetest ->
      DSL.ins I.CBNZ [| DSL.emit_reg i.arg.(0); DSL.emit_label lbl |]
    | Ifalsetest ->
      DSL.ins I.CBZ [| DSL.emit_reg i.arg.(0); DSL.emit_label lbl |]
    | Iinttest cmp ->
      DSL.ins I.CMP [| DSL.emit_reg i.arg.(0); DSL.emit_reg i.arg.(1) |];
      let comp = cond_for_comparison cmp in
      DSL.ins (I.B_cond comp) [| DSL.emit_label lbl |]
    | Iinttest_imm (cmp, n) ->
      emit_cmpimm i.arg.(0) n;
      let comp = cond_for_comparison cmp in
      DSL.ins (I.B_cond comp) [| DSL.emit_label lbl |]
    | Ifloattest ((Float32 | Float64), cmp) ->
      let comp = cond_for_float_comparison cmp in
      DSL.ins I.FCMP [| DSL.emit_reg i.arg.(0); DSL.emit_reg i.arg.(1) |];
      DSL.ins (I.B_cond_float comp) [| DSL.emit_label lbl |]
    | Ioddtest ->
      DSL.ins I.TBNZ [| DSL.emit_reg i.arg.(0); DSL.imm 0; DSL.emit_label lbl |]
    | Ieventest ->
      DSL.ins I.TBZ [| DSL.emit_reg i.arg.(0); DSL.imm 0; DSL.emit_label lbl |])
  | Lcondbranch3 (lbl0, lbl1, lbl2) -> (
    DSL.ins I.CMP [| DSL.emit_reg i.arg.(0); DSL.imm 1 |];
    (match lbl0 with
    | None -> ()
    | Some lbl -> DSL.ins (I.B_cond LT) [| DSL.emit_label lbl |]);
    (match lbl1 with
    | None -> ()
    | Some lbl -> DSL.ins (I.B_cond EQ) [| DSL.emit_label lbl |]);
    match lbl2 with
    | None -> ()
    | Some lbl -> DSL.ins (I.B_cond GT) [| DSL.emit_label lbl |])
  | Lswitch jumptbl ->
    let lbltbl = Cmm.new_label () in
    DSL.ins I.ADR [| DSL.emit_reg reg_tmp1; DSL.emit_label lbltbl |];
    DSL.ins I.ADD
      [| DSL.emit_reg reg_tmp1;
         DSL.emit_reg reg_tmp1;
         DSL.emit_reg i.arg.(0);
         DSL.emit_shift LSL 2
      |];
    DSL.ins I.BR [| DSL.emit_reg reg_tmp1 |];
    emit_printf "%a:\n" femit_label lbltbl;
    for j = 0 to Array.length jumptbl - 1 do
      DSL.ins I.B [| DSL.emit_label jumptbl.(j) |]
    done
  (*= Alternative:
        let lbltbl = Cmm.new_label() in
        emit_printf "	adr	%a, %a\n" femit_reg reg_tmp1 femit_label lbltbl;
        emit_printf "	ldr	%a, [%a, %a, lsl #2]\n" femit_wreg reg_tmp2 femit_reg reg_tmp1 femit_reg i.arg.(0);
        emit_printf "	add	%a, %a, sxtb\n" femit_reg reg_tmp1 femit_wreg reg_tmp2;
        emit_printf "	br	%a\n" femit_reg reg_tmp1;
        emit_printf "%a:\n" femit_label lbltbl;
        for j = 0 to Array.length jumptbl - 1 do
            emit_printf "	.4byte	%a - %a\n" femit_label jumptbl.(j) femit_label lbltbl
        done
*)
  | Lentertrap -> ()
  | Ladjust_stack_offset { delta_bytes } ->
    cfi_adjust_cfa_offset delta_bytes;
    stack_offset := !stack_offset + delta_bytes
  | Lpushtrap { lbl_handler } ->
    DSL.ins I.ADR [| DSL.emit_reg reg_tmp1; DSL.emit_label lbl_handler |];
    stack_offset := !stack_offset + 16;
    DSL.ins I.STP
      [| DSL.emit_reg reg_trap_ptr;
         DSL.emit_reg reg_tmp1;
         DSL.mem_pre ~base:Arm64_ast.Reg.sp ~offset:(-16)
      |];
    cfi_adjust_cfa_offset 16;
    DSL.ins I.MOV [| DSL.emit_reg reg_trap_ptr; DSL.sp |]
  | Lpoptrap ->
    DSL.ins I.LDR
      [| DSL.emit_reg reg_trap_ptr;
         DSL.mem_post ~base:Arm64_ast.Reg.sp ~offset:16
      |];
    cfi_adjust_cfa_offset (-16);
    stack_offset := !stack_offset - 16
  | Lraise k -> (
    match k with
    | Lambda.Raise_regular ->
      DSL.ins I.BL [| DSL.emit_symbol "caml_raise_exn" |];
      record_frame Reg.Set.empty (Dbg_raise i.dbg)
    | Lambda.Raise_reraise ->
      if Config.runtime5
      then DSL.ins I.BL [| DSL.emit_symbol "caml_reraise_exn" |]
      else DSL.ins I.BL [| DSL.emit_symbol "caml_raise_exn" |];
      record_frame Reg.Set.empty (Dbg_raise i.dbg)
    | Lambda.Raise_notrace ->
      DSL.ins I.MOV [| DSL.sp; DSL.emit_reg reg_trap_ptr |];
      DSL.ins I.LDP
        [| DSL.emit_reg reg_trap_ptr;
           DSL.emit_reg reg_tmp1;
           DSL.mem ~base:Arm64_ast.Reg.sp;
           DSL.imm 16
        |];
      DSL.ins I.BR [| DSL.emit_reg reg_tmp1 |])
  | Lstackcheck { max_frame_size_bytes } ->
    let overflow = Cmm.new_label () and ret = Cmm.new_label () in
    let threshold_offset =
      (Domainstate.stack_ctx_words * 8) + Stack_check.stack_threshold_size
    in
    let f = max_frame_size_bytes + threshold_offset in
    let offset = Domainstate.(idx_of_field Domain_current_stack) * 8 in
    DSL.ins I.LDR
      [| DSL.emit_reg reg_tmp1;
         DSL.emit_addressing (Iindexed offset) reg_domain_state_ptr
      |];
    emit_addimm reg_tmp1 reg_tmp1 f;
    DSL.ins I.CMP [| DSL.sp; DSL.emit_reg reg_tmp1 |];
    DSL.ins (I.B_cond CC) [| DSL.emit_label overflow |];
    emit_printf "%a:\n" femit_label ret;
    stack_realloc
      := Some
           { sc_label = overflow;
             sc_return = ret;
             sc_max_frame_size_in_bytes = max_frame_size_bytes
           }

let emit_instr i =
  try emit_instr i
  with exn ->
    Format.eprintf "Exception whilst emitting instruction:@ %a\n"
      Printlinear.instr i;
    raise exn

(* Emission of an instruction sequence *)

let rec emit_all i =
  (* CR-soon xclerc for xclerc: get rid of polymorphic compare. *)
  if Stdlib.compare i.desc Lend = 0
  then ()
  else (
    emit_instr i;
    emit_all i.next)

(* Emission of a function declaration *)

let fundecl fundecl =
  let fun_end_label, fundecl =
    match Emitaux.Dwarf_helpers.record_dwarf_for_fundecl fundecl with
    | None -> None, fundecl
    | Some { fun_end_label; fundecl } -> Some fun_end_label, fundecl
  in
  function_name := fundecl.fun_name;
  fastcode_flag := fundecl.fun_fast;
  tailrec_entry_point := fundecl.fun_tailrec_entry_point_label;
  float_literals := [];
  stack_offset := 0;
  call_gc_sites := [];
  local_realloc_sites := [];
  clear_stack_realloc ();
  Stack_class.Tbl.copy_values ~from:fundecl.fun_num_stack_slots
    ~to_:num_stack_slots;
  prologue_required := fundecl.fun_prologue_required;
  contains_calls := fundecl.fun_contains_calls;
  emit_named_text_section !function_name;
  emit_printf "\t.align\t3\n";
  emit_printf "\t.globl\t%a\n" femit_symbol fundecl.fun_name;
  emit_symbol_type femit_symbol fundecl.fun_name "function";
  emit_printf "%a:\n" femit_symbol fundecl.fun_name;
  emit_debug_info fundecl.fun_dbg;
  cfi_startproc ();
  let num_call_gc = num_call_gc_points fundecl.fun_body in
  let max_out_of_line_code_offset = max_out_of_line_code_offset ~num_call_gc in
  BR.relax fundecl.fun_body ~max_out_of_line_code_offset;
  emit_all fundecl.fun_body;
  List.iter emit_call_gc !call_gc_sites;
  List.iter emit_local_realloc !local_realloc_sites;
  emit_stack_realloc ();
  assert (List.length !call_gc_sites = num_call_gc);
  (match fun_end_label with
  | None -> ()
  | Some fun_end_label -> emit_printf "%a:\n" femit_label fun_end_label);
  cfi_endproc ();
  emit_symbol_type femit_symbol fundecl.fun_name "function";
  emit_symbol_size fundecl.fun_name;
  emit_literals ()

(* Emission of data *)

let emit_item (d : Cmm.data_item) =
  match d with
  | Cdefine_symbol s ->
    if !Clflags.dlcode || Cmm.equal_is_global s.sym_global Cmm.Global
    then
      (* GOT relocations against non-global symbols don't seem to work properly:
         GOT entries are not created for the symbols and the relocations
         evaluate to random other GOT entries. For the moment force all symbols
         to be global. *)
      emit_printf "\t.globl\t%a\n" femit_symbol s.sym_name;
    emit_printf "%a:\n" femit_symbol s.sym_name
  | Cint8 n -> emit_printf "\t.byte\t%d\n" n
  | Cint16 n -> emit_printf "\t.2byte\t%d\n" n
  | Cint32 n -> emit_printf "\t.4byte\t%Ld\n" (Int64.of_nativeint n)
  | Cint n -> emit_printf "\t.8byte\t%Ld\n" (Int64.of_nativeint n)
  | Csingle f -> emit_float32_directive_with_comment (Int32.bits_of_float f)
  | Cdouble f -> emit_float64_directive_with_comment (Int64.bits_of_float f)
  | Cvec128 { high; low } ->
    emit_float64_directive_with_comment low;
    emit_float64_directive_with_comment high
  | Csymbol_address s -> emit_printf "\t.8byte\t%a\n" femit_symbol s.sym_name
  | Csymbol_offset (s, o) ->
    emit_printf "\t.8byte\t%a+%a\n" femit_symbol s.sym_name femit_int o
  | Cstring s ->
    if String.length s = 0
    then emit_string "\n"
    else emit_string_directive "\t.ascii\t" s
  | Cskip n -> if n > 0 then emit_printf "\t.space\t%a\n" femit_int n
  | Calign n -> emit_printf "\t.align\t%a\n" femit_int (Misc.log2 n)

let data l =
  emit_printf "\t.data\n";
  emit_printf "\t.align\t3\n";
  List.iter emit_item l

let emit_line str = emit_string (str ^ "\n")

let file_emitter ~file_num ~file_name =
  D.file ~file_num:(Some file_num) ~file_name

let build_asm_directives () : (module Asm_targets.Asm_directives_intf.S) =
  (module Asm_targets.Asm_directives.Make (struct
    let emit_line = emit_line

    let get_file_num file_name = Emitaux.get_file_num ~file_emitter file_name

    let debugging_comments_in_asm_files = !Flambda_backend_flags.dasm_comments

    module D = struct
      type constant = D.Directive.Constant.t

      let const_int64 num = D.Directive.Constant.Signed_int num

      let const_label str = D.Directive.Constant.Named_thing str

      let const_add c1 c2 = D.Directive.Constant.Add (c1, c2)

      let const_sub c1 c2 = D.Directive.Constant.Sub (c1, c2)

      (* CR sspies: The functions depending on [emit_directive] below break
         abstractions. This is intensional at the moment, because this is only
         the first step of getting rid of the first-class module entirely. *)
      let emit_directive dir =
        let buf = Buffer.create 80 in
        D.Directive.print buf dir;
        Buffer.add_string buf "\n";
        Buffer.output_buffer !output_channel buf

      let emit_constant const size =
        emit_directive
          (Const
             { constant = D.Directive.Constant_with_width.create const size;
               comment = None
             })

      type data_type =
        | NONE
        | DWORD
        | QWORD
        | VEC128

      let file = file_emitter

      let loc ~file_num ~line ~col ?discriminator () =
        ignore discriminator;
        D.loc ~file_num ~line ~col ?discriminator ()

      let comment str = D.comment str

      let label ?data_type:_ str = emit_directive (New_label (str, Code))

      let section ?delayed:_ name flags args =
        match name, flags, args with
        | [".data"], _, _ -> D.data ()
        | [".text"], _, _ -> D.text ()
        | name, flags, args -> D.switch_to_section_raw ~names:name ~flags ~args

      let text () = D.text ()

      let new_line () = D.new_line ()

      let global sym = emit_directive (Global sym)

      let protected sym = if not macosx then emit_directive (Protected sym)

      let type_ sym typ_ =
        let typ_ : D.symbol_type =
          match typ_ with
          | "@function" -> Function
          | "@object" -> Object
          | "STT_FUNC" -> Function
          | "STT_OBJECT" -> Object
          | _ -> Misc.fatal_errorf "Unsupported assembly type %s" typ_
        in
        emit_directive (Type (sym, typ_))

      let byte const = emit_constant const Eight

      let word const = emit_constant const Sixteen

      let long const = emit_constant const Thirty_two

      let qword const = emit_constant const Sixty_four

      let bytes str = D.string str

      let uleb128 const =
        emit_directive (Uleb128 { constant = const; comment = None })

      let sleb128 const =
        emit_directive (Sleb128 { constant = const; comment = None })

      let direct_assignment var const =
        emit_directive (Direct_assignment (var, const))
    end
  end))

(* Beginning / end of an assembly file *)

let begin_assembly _unix =
  reset_debug_info ();
  Asm_targets.Asm_label.initialize ~new_label:(fun () ->
      Cmm.new_label () |> Label.to_int);
  let asm_line_buffer = Buffer.create 200 in
  D.initialize ~big_endian:Arch.big_endian ~emit_assembly_comments:!Flambda_backend_flags.dasm_comments ~emit:(fun d ->
      Buffer.clear asm_line_buffer;
      D.Directive.print asm_line_buffer d;
      Buffer.add_string asm_line_buffer "\n";
      Buffer.output_buffer !output_channel asm_line_buffer);
  emit_printf "\t.file\t\"\"\n";
  (* PR#7037 *)
  let lbl_begin = Cmm_helpers.make_symbol "data_begin" in
  emit_printf "\t.data\n";
  emit_printf "\t.globl\t%a\n" femit_symbol lbl_begin;
  emit_printf "%a:\n" femit_symbol lbl_begin;
  let lbl_begin = Cmm_helpers.make_symbol "code_begin" in
  emit_named_text_section lbl_begin;
  emit_printf "\t.globl\t%a\n" femit_symbol lbl_begin;
  emit_printf "%a:\n" femit_symbol lbl_begin;
  (* we need to pad here to avoid collision for the unwind test between the
     code_begin symbol and the first function. (See also #4690) Alignment is
     needed to avoid linker warnings for shared_startup__code_{begin,end} (e.g.
     tests/lib-dynlink-pr4839). *)
  if macosx
  then (
    DSL.ins I.NOP [||];
    emit_printf "\t.align\t3\n");
  let lbl_end = Cmm_helpers.make_symbol "code_end" in
  Emitaux.Dwarf_helpers.begin_dwarf ~build_asm_directives ~code_begin:lbl_begin
    ~code_end:lbl_end ~file_emitter

let end_assembly () =
  let lbl_end = Cmm_helpers.make_symbol "code_end" in
  emit_named_text_section lbl_end;
  emit_printf "\t.globl\t%a\n" femit_symbol lbl_end;
  emit_printf "%a:\n" femit_symbol lbl_end;
  let lbl_end = Cmm_helpers.make_symbol "data_end" in
  emit_printf "\t.data\n";
  emit_printf "\t.8byte\t0\n";
  (* PR#6329 *)
  emit_printf "\t.globl\t%a\n" femit_symbol lbl_end;
  emit_printf "%a:\n" femit_symbol lbl_end;
  emit_printf "\t.8byte\t0\n";
  emit_printf "\t.align\t3\n";
  (* #7887 *)
  let lbl = Cmm_helpers.make_symbol "frametable" in
  emit_printf "\t.globl\t%a\n" femit_symbol lbl;
  emit_printf "%a:\n" femit_symbol lbl;
  emit_frames
    { efa_code_label =
        (fun lbl ->
          emit_symbol_type femit_label lbl "function";
          emit_printf "\t.8byte\t%a\n" femit_label lbl);
      efa_data_label =
        (fun lbl ->
          emit_symbol_type femit_label lbl "object";
          emit_printf "\t.8byte\t%a\n" femit_label lbl);
      efa_8 = (fun n -> emit_printf "\t.byte\t%d\n" n);
      efa_16 = (fun n -> emit_printf "\t.2byte\t%d\n" n);
      efa_32 = (fun n -> emit_printf "\t.4byte\t%ld\n" n);
      efa_word = (fun n -> emit_printf "\t.8byte\t%d\n" n);
      efa_align =
        (fun n -> emit_printf "\t.align\t%a\n" femit_int (Misc.log2 n));
      efa_label_rel =
        (fun lbl ofs ->
          emit_printf "\t.4byte\t(%a + %ld) - .\n" femit_label lbl ofs);
      efa_def_label = (fun lbl -> emit_printf "%a:\n" femit_label lbl);
      efa_string = (fun s -> emit_string_directive "\t.ascii\t" (s ^ "\000"))
    };
  emit_symbol_type femit_symbol lbl "object";
  emit_symbol_size lbl;
  if not !Flambda_backend_flags.internal_assembler
  then Emitaux.Dwarf_helpers.emit_dwarf ();
  match Config.system with
  | "linux" ->
    (* Mark stack as non-executable *)
    emit_printf "\t.section\t.note.GNU-stack,\"\",%%progbits\n"
  | _ -> ()
