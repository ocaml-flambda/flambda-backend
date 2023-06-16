# 2 "backend/amd64/proc.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2000 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
[@@@ocaml.warning "+4"]

(* Description of the AMD64 processor *)

open Misc
open Arch
open Cmm
open Reg
open Mach

let fp = Config.with_frame_pointers

(* Which ABI to use *)

let win64 = Arch.win64

(* Registers available for register allocation *)

(* Register map:
    rax         0
    rbx         1
    rdi         2
    rsi         3
    rdx         4
    rcx         5
    r8          6
    r9          7
    r12         8
    r13         9
    r10         10
    r11         11
    rbp         12
    r14         domain state pointer
    r15         allocation pointer

  xmm0 - xmm15  100 - 115  *)

(* Conventions:
     rax - r13: OCaml function arguments
     rax: OCaml and C function results
     xmm0 - xmm9: OCaml function arguments
     xmm0: OCaml and C function results
   Under Unix:
     rdi, rsi, rdx, rcx, r8, r9: C function arguments
     xmm0 - xmm7: C function arguments
     rbx, rbp, r12-r15 are preserved by C
     xmm registers are not preserved by C
   Under Win64:
     rcx, rdx, r8, r9: C function arguments
     xmm0 - xmm3: C function arguments
     rbx, rbp, rsi, rdi r12-r15 are preserved by C
     xmm6-xmm15 are preserved by C
   Note (PR#5707, GPR#1304): PLT stubs (used for dynamic resolution of symbols
     on Unix-like platforms) may clobber any register except those used for:
       1. C parameter passing;
       2. C return values;
       3. C callee-saved registers.
     This translates to the set { r10, r11 }.  These registers hence cannot
     be used for OCaml parameter passing and must also be marked as
     destroyed across [Ialloc] and [Ipoll] (otherwise a call to
     caml_call_gc@PLT might clobber these two registers before the assembly
     stub saves them into the GC regs block).
*)

let int_reg_name =
  match Config.ccomp_type with
  | "msvc" ->
      [| "rax"; "rbx"; "rdi"; "rsi"; "rdx"; "rcx"; "r8"; "r9";
         "r12"; "r13"; "r10"; "r11"; "rbp" |]
  | _ ->
      [| "%rax"; "%rbx"; "%rdi"; "%rsi"; "%rdx"; "%rcx"; "%r8"; "%r9";
         "%r12"; "%r13"; "%r10"; "%r11"; "%rbp" |]

let float_reg_name =
  match Config.ccomp_type with
  | "msvc" ->
      [| "xmm0"; "xmm1"; "xmm2"; "xmm3"; "xmm4"; "xmm5"; "xmm6"; "xmm7";
         "xmm8"; "xmm9"; "xmm10"; "xmm11";
         "xmm12"; "xmm13"; "xmm14"; "xmm15" |]
  | _ ->
      [| "%xmm0"; "%xmm1"; "%xmm2"; "%xmm3"; "%xmm4"; "%xmm5"; "%xmm6"; "%xmm7";
         "%xmm8"; "%xmm9"; "%xmm10"; "%xmm11";
         "%xmm12"; "%xmm13"; "%xmm14"; "%xmm15" |]

let num_register_classes = 2

let register_class r =
  match r.typ with
  | Val | Int | Addr -> 0
  | Float -> 1

let register_class_tag c =
  match c with
  | 0 -> "i"
  | 1 -> "f"
  | c -> Misc.fatal_errorf "Unspecified register class %d" c

let num_available_registers = [| 13; 16 |]

let first_available_register = [| 0; 100 |]

let register_name r =
  if r < 100 then int_reg_name.(r) else float_reg_name.(r - 100)

(* Pack registers starting at %rax so as to reduce the number of REX
   prefixes and thus improve code density *)
let rotate_registers = false

(* Representation of hard registers by pseudo-registers *)

let hard_int_reg =
  let v = Array.make 13 Reg.dummy in
  for i = 0 to 12 do v.(i) <- Reg.at_location Int (Reg i) done;
  v

let hard_float_reg =
  let v = Array.make 16 Reg.dummy in
  for i = 0 to 15 do v.(i) <- Reg.at_location Float (Reg (100 + i)) done;
  v

let all_phys_regs =
  Array.append hard_int_reg hard_float_reg

let phys_reg n =
  if n < 100 then hard_int_reg.(n) else hard_float_reg.(n - 100)

let rax = phys_reg 0
let rdx = phys_reg 4
let r10 = phys_reg 10
let r11 = phys_reg 11
let rbp = phys_reg 12
let rxmm15 = phys_reg 115

let destroyed_by_plt_stub =
  if not X86_proc.use_plt then [| |] else [| r10; r11 |]

let num_destroyed_by_plt_stub = Array.length destroyed_by_plt_stub

let destroyed_by_plt_stub_set = Reg.set_of_array destroyed_by_plt_stub

let stack_slot slot ty =
  Reg.at_location ty (Stack slot)

(* Instruction selection *)

let word_addressed = false

(* Calling conventions *)

let size_domainstate_args = 64 * size_int

let calling_conventions first_int last_int first_float last_float
                        make_stack first_stack
                        arg =
  let loc = Array.make (Array.length arg) Reg.dummy in
  let int = ref first_int in
  let float = ref first_float in
  let ofs = ref first_stack in
  for i = 0 to Array.length arg - 1 do
    match arg.(i) with
    | Val | Int | Addr as ty ->
        if !int <= last_int then begin
          loc.(i) <- phys_reg !int;
          incr int
        end else begin
          loc.(i) <- stack_slot (make_stack !ofs) ty;
          ofs := !ofs + size_int
        end;
        assert (not (Reg.Set.mem loc.(i) destroyed_by_plt_stub_set))
    | Float ->
        if !float <= last_float then begin
          loc.(i) <- phys_reg !float;
          incr float
        end else begin
          loc.(i) <- stack_slot (make_stack !ofs) Float;
          ofs := !ofs + size_float
        end
  done;
  (loc, Misc.align (max 0 !ofs) 16)  (* keep stack 16-aligned *)

let incoming ofs =
  if ofs >= 0
  then Incoming ofs
  else Domainstate (ofs + size_domainstate_args)
let outgoing ofs =
  if ofs >= 0
  then Outgoing ofs
  else Domainstate (ofs + size_domainstate_args)
let not_supported _ofs = fatal_error "Proc.loc_results: cannot call"

let loc_arguments arg =
  calling_conventions 0 9 100 109 outgoing (- size_domainstate_args) arg
let loc_parameters arg =
  let (loc, _ofs) =
    calling_conventions 0 9 100 109 incoming (- size_domainstate_args) arg
  in
  loc

let loc_results_call res =
  calling_conventions 0 9 100 109 outgoing (- size_domainstate_args) res
let loc_results_return res =
  let (loc, _ofs) =
    calling_conventions 0 9 100 109 incoming (- size_domainstate_args) res
  in loc

let max_arguments_for_tailcalls = 10 (* in regs *) + 64 (* in domain state *)

(* C calling conventions under Unix:
     first integer args in rdi, rsi, rdx, rcx, r8, r9
     first float args in xmm0 ... xmm7
     remaining args on stack
     return value in rax or xmm0.
  C calling conventions under Win64:
     first integer args in rcx, rdx, r8, r9
     first float args in xmm0 ... xmm3
     each integer arg consumes a float reg, and conversely
     remaining args on stack
     always 32 bytes reserved at bottom of stack.
     Return value in rax or xmm0. *)

let loc_external_results res =
  let (loc, _ofs) = calling_conventions 0 0 100 100 not_supported 0 res in loc

let unix_loc_external_arguments arg =
  calling_conventions 2 7 100 107 outgoing 0 arg

let win64_int_external_arguments =
  [| 5 (*rcx*); 4 (*rdx*); 6 (*r8*); 7 (*r9*) |]
let win64_float_external_arguments =
  [| 100 (*xmm0*); 101 (*xmm1*); 102 (*xmm2*); 103 (*xmm3*) |]

let win64_loc_external_arguments arg =
  let loc = Array.make (Array.length arg) Reg.dummy in
  let reg = ref 0
  and ofs = ref 32 in
  for i = 0 to Array.length arg - 1 do
    match arg.(i) with
    | Val | Int | Addr as ty ->
        if !reg < 4 then begin
          loc.(i) <- phys_reg win64_int_external_arguments.(!reg);
          incr reg
        end else begin
          loc.(i) <- stack_slot (Outgoing !ofs) ty;
          ofs := !ofs + size_int
        end
    | Float ->
        if !reg < 4 then begin
          loc.(i) <- phys_reg win64_float_external_arguments.(!reg);
          incr reg
        end else begin
          loc.(i) <- stack_slot (Outgoing !ofs) Float;
          ofs := !ofs + size_float
        end
  done;
  (loc, Misc.align !ofs 16)  (* keep stack 16-aligned *)

let loc_external_arguments ty_args =
  let arg = Cmm.machtype_of_exttype_list ty_args in
  let loc, stack_ofs =
    if win64
    then win64_loc_external_arguments arg
    else unix_loc_external_arguments arg
  in
  Array.map (fun reg -> [|reg|]) loc, stack_ofs

let loc_exn_bucket = rax

(** See "System V Application Binary Interface, AMD64 Architecture Processor
    Supplement" (www.x86-64.org/documentation/abi.pdf) page 57, fig. 3.36. *)
let int_dwarf_reg_numbers =
  [| 0; 3; 5; 4; 1; 2; 8; 9; 12; 13; 10; 11; 6 |]

let float_dwarf_reg_numbers =
  [| 17; 18; 19; 20; 21; 22; 23; 24; 25; 26; 27; 28; 29; 30; 31; 32 |]

let dwarf_register_numbers ~reg_class =
  match reg_class with
  | 0 -> int_dwarf_reg_numbers
  | 1 -> float_dwarf_reg_numbers
  | _ -> Misc.fatal_errorf "Bad register class %d" reg_class

let stack_ptr_dwarf_register_number = 7

(* Volatile registers: none *)

let regs_are_volatile _rs = false

(* Registers destroyed by operations *)

let destroyed_at_c_call =
  if win64 then
    (* Win64: rbx, rbp, rsi, rdi, r12-r15, xmm6-xmm15 preserved *)
    Array.of_list(List.map phys_reg
      [0;4;5;6;7;10;11;
       100;101;102;103;104;105])
  else
    (* Unix: rbp, rbx, r12-r15 preserved *)
    Array.of_list(List.map phys_reg
      [0;2;3;4;5;6;7;10;11;
       100;101;102;103;104;105;106;107;
       108;109;110;111;112;113;114;115])

let destroyed_at_alloc_or_poll =
  if X86_proc.use_plt then
    destroyed_by_plt_stub
  else
    [| r11 |]

let destroyed_at_pushtrap =
  [| r11 |]

let has_pushtrap traps =
  List.exists (function Cmm.Push _ -> true | Pop _ -> false) traps

(* note: keep this function in sync with `destroyed_at_{basic,terminator}` below. *)
let destroyed_at_oper = function
    Iop(Icall_ind | Icall_imm _ | Iextcall { alloc = true; }) ->
    all_phys_regs
  | Iop(Iextcall { alloc = false; }) -> destroyed_at_c_call
  | Iop(Iintop(Idiv | Imod)) | Iop(Iintop_imm((Idiv | Imod), _))
        -> [| rax; rdx |]
  | Iop(Istore(Single, _, _)) -> [| rxmm15 |]
  | Iop(Ialloc _ | Ipoll _) -> destroyed_at_alloc_or_poll
  | Iop(Iintop(Imulh _ | Icomp _) | Iintop_imm((Icomp _), _))
        -> [| rax |]
  | Iswitch(_, _) -> [| rax; rdx |]
  | Itrywith _ -> destroyed_at_pushtrap
  | Iexit (_, traps) when has_pushtrap traps -> destroyed_at_pushtrap
  | Ireturn traps when has_pushtrap traps -> assert false
  | Iop(Ispecific (Irdtsc | Irdpmc)) -> [| rax; rdx |]
  | Iop(Ispecific(Ilfence | Isfence | Imfence)) -> [||]
  | Iop(Ispecific(Isqrtf | Isextend32 | Izextend32 | Icrc32q | Ilea _
                 | Istore_int (_, _, _) | Ioffset_loc (_, _)
                 | Ipause
                 | Iprefetch _
                 | Ifloat_round _
                 | Ifloat_iround | Ifloat_min | Ifloat_max
                 | Ifloatarithmem (_, _) | Ibswap _ | Ifloatsqrtf _))
  | Iop(Iintop(Iadd | Isub | Imul | Iand | Ior | Ixor | Ilsl | Ilsr | Iasr
              | Ipopcnt | Iclz _ | Ictz _ | Icheckbound))
  | Iop(Iintop_imm((Iadd | Isub | Imul | Imulh _ | Iand | Ior | Ixor | Ilsl
                   | Ilsr | Iasr | Ipopcnt | Iclz _ | Ictz _
                   | Icheckbound),_))
  | Iop(Iintop_atomic _)
  | Iop(Istore((Byte_unsigned | Byte_signed | Sixteen_unsigned | Sixteen_signed
               | Thirtytwo_unsigned | Thirtytwo_signed | Word_int | Word_val
               | Double ), _, _))
  | Iop(Imove | Ispill | Ireload | Inegf | Iabsf | Iaddf | Isubf | Imulf | Idivf
       | Icompf _
       | Icsel _
       | Ifloatofint | Iintoffloat
       | Ivalueofint | Iintofvalue
       | Iconst_int _ | Iconst_float _ | Iconst_symbol _
       | Itailcall_ind | Itailcall_imm _ | Istackoffset _ | Iload (_, _, _)
       | Iname_for_debugger _ | Iprobe _| Iprobe_is_enabled _ | Iopaque)
  | Iend | Ireturn _ | Iifthenelse (_, _, _) | Icatch (_, _, _, _)
  | Iexit _ | Iraise _
  | Iop(Ibeginregion | Iendregion)
    ->
    if fp then
(* prevent any use of the frame pointer ! *)
      [| rbp |]
    else
      [||]


let destroyed_at_raise = all_phys_regs

let destroyed_at_reloadretaddr = [| |]

(* note: keep this function in sync with `destroyed_at_oper` above. *)
let destroyed_at_basic (basic : Cfg_intf.S.basic) =
  match basic with
  | Reloadretaddr ->
    destroyed_at_reloadretaddr
  | Pushtrap _ ->
    destroyed_at_pushtrap
  | Op (Intop (Idiv | Imod)) | Op (Intop_imm ((Idiv | Imod), _)) ->
    [| rax; rdx |]
  | Op(Store(Single, _, _)) ->
    [| rxmm15 |]
  | Op(Intop(Imulh _ | Icomp _) | Intop_imm((Icomp _), _)) ->
    [| rax |]
  | Op (Specific (Irdtsc | Irdpmc)) ->
    [| rax; rdx |]
  | Op (Intop Icheckbound | Intop_imm (Icheckbound, _)) ->
    assert false
  | Op (Move | Spill | Reload
       | Const_int _ | Const_float _ | Const_symbol _
       | Stackoffset _
       | Load _ | Store ((Byte_unsigned | Byte_signed | Sixteen_unsigned
                         | Sixteen_signed | Thirtytwo_unsigned
                         | Thirtytwo_signed | Word_int | Word_val
                         | Double ), _, _)
       | Intop (Iadd | Isub | Imul | Iand | Ior | Ixor | Ilsl | Ilsr
               | Iasr | Ipopcnt | Iclz _ | Ictz _)
       | Intop_imm ((Iadd | Isub | Imul | Imulh _ | Iand | Ior | Ixor
                    | Ilsl | Ilsr | Iasr | Ipopcnt | Iclz _ | Ictz _),_)
       | Intop_atomic _
       | Negf | Absf | Addf | Subf | Mulf | Divf
       | Compf _
       | Csel _
       | Floatofint | Intoffloat
       | Valueofint | Intofvalue
       | Probe_is_enabled _
       | Opaque
       | Begin_region
       | End_region
       | Specific (Ilea _ | Istore_int _ | Ioffset_loc _
                  | Ifloatarithmem _ | Ibswap _ | Isqrtf
                  | Ifloatsqrtf _ | Ifloat_iround
                  | Ifloat_round _ | Ifloat_min | Ifloat_max
                  | Isextend32 | Izextend32 | Icrc32q | Ipause
                  | Iprefetch _ | Ilfence | Isfence | Imfence)
       | Name_for_debugger _)
  | Poptrap | Prologue ->
    if fp then [| rbp |] else [||]

(* note: keep this function in sync with `destroyed_at_oper` above,
   and `is_destruction_point` below. *)
let destroyed_at_terminator (terminator : Cfg_intf.S.terminator) =
  match terminator with
  | Never -> assert false
  | Prim {op = Alloc _; _} ->
    destroyed_at_alloc_or_poll
  | Always _ | Parity_test _ | Truth_test _ | Float_test _ | Int_test _
  | Return | Raise _ | Tailcall_self  _ | Tailcall_func _
  | Prim {op = Checkbound _ | Probe _; _}
  ->
    if fp then [| rbp |] else [||]
  | Switch _ ->
    [| rax; rdx |]
  | Call_no_return { func_symbol = _; alloc; ty_res = _; ty_args = _; }
  | Prim {op = External { func_symbol = _; alloc; ty_res = _; ty_args = _; }; _} ->
    if alloc then all_phys_regs else destroyed_at_c_call
  | Call {op = Indirect | Direct _; _} ->
    all_phys_regs
  | Specific_can_raise { op = (Ilea _ | Ibswap _ | Isqrtf | Isextend32 | Izextend32
                       | Ifloatarithmem _ | Ifloatsqrtf _
                       | Ifloat_iround | Ifloat_round _ | Ifloat_min | Ifloat_max
                       | Icrc32q | Irdtsc | Irdpmc | Ipause
                       | Ilfence | Isfence | Imfence
                       | Istore_int (_, _, _) | Ioffset_loc (_, _)
                              | Iprefetch _); _ } ->
    Misc.fatal_error "no instructions specific for this architecture can raise"
  | Poll_and_jump _ -> destroyed_at_alloc_or_poll

(* CR-soon xclerc for xclerc: consider having more destruction points.
   We current return `true` when `destroyed_at_terminator` returns
   `all_phys_regs` (which means we are conservative in the sense we will
   spill registers that would spill anyway); we could also return `true`
   when `destroyed_at_terminator` returns `destroyed_at_c_call` for instance. *)
(* note: keep this function in sync with `destroyed_at_terminator` above. *)
let is_destruction_point (terminator : Cfg_intf.S.terminator) =
  match terminator with
  | Never -> assert false
  | Prim {op = Alloc _; _} ->
    false
  | Always _ | Parity_test _ | Truth_test _ | Float_test _ | Int_test _
  | Return | Raise _ | Tailcall_self  _ | Tailcall_func _
  | Prim {op = Checkbound _ | Probe _; _} ->
    false
  | Switch _ ->
    false
  | Call_no_return { func_symbol = _; alloc; ty_res = _; ty_args = _; }
  | Prim {op = External { func_symbol = _; alloc; ty_res = _; ty_args = _; }; _} ->
    if alloc then true else false
  | Call {op = Indirect | Direct _; _} ->
    true
  | Specific_can_raise { op = (Ilea _ | Ibswap _ | Isqrtf | Isextend32 | Izextend32
                       | Ifloatarithmem _ | Ifloatsqrtf _
                       | Ifloat_iround | Ifloat_round _ | Ifloat_min | Ifloat_max
                       | Icrc32q | Irdtsc | Irdpmc | Ipause
                       | Ilfence | Isfence | Imfence
                       | Istore_int (_, _, _) | Ioffset_loc (_, _)
                              | Iprefetch _); _ } ->
    Misc.fatal_error "no instructions specific for this architecture can raise"
  | Poll_and_jump _ -> false

(* Maximal register pressure *)


let safe_register_pressure = function
    Iextcall _ -> if win64 then if fp then 7 else 8 else 0
  | Ialloc _ | Ipoll _ | Imove | Ispill | Ireload
  | Inegf | Iabsf | Iaddf | Isubf | Imulf | Idivf
  | Ifloatofint | Iintoffloat | Ivalueofint | Iintofvalue
  | Icompf _
  | Icsel _
  | Iconst_int _ | Iconst_float _ | Iconst_symbol _
  | Icall_ind | Icall_imm _ | Itailcall_ind | Itailcall_imm _
  | Istackoffset _ | Iload (_, _, _) | Istore (_, _, _)
  | Iintop _ | Iintop_imm (_, _) | Iintop_atomic _
  | Ispecific _ | Iname_for_debugger _
  | Iprobe _ | Iprobe_is_enabled _ | Iopaque
  | Ibeginregion | Iendregion
    -> if fp then 10 else 11

let max_register_pressure =
  let consumes ~int ~float =
    if fp
    then [| 12 - int; 16 - float |]
    else [| 13 - int; 16 - float |]
  in function
    Iextcall _ ->
    if win64
      then consumes ~int:5 ~float:6
      else consumes ~int:9 ~float:16
  | Iintop(Idiv | Imod) | Iintop_imm((Idiv | Imod), _) ->
    consumes ~int:2 ~float:0
  | Ialloc _ | Ipoll _ ->
    consumes ~int:(1 + num_destroyed_by_plt_stub) ~float:0
  | Iintop(Icomp _) | Iintop_imm((Icomp _), _) ->
    consumes ~int:1 ~float:0
  | Istore(Single, _, _) | Icompf _ ->
    consumes ~int:0 ~float:1
  | Iintop(Iadd | Isub | Imul | Imulh _ | Iand | Ior | Ixor | Ilsl | Ilsr | Iasr
           | Ipopcnt|Iclz _| Ictz _|Icheckbound)
  | Iintop_imm((Iadd | Isub | Imul | Imulh _ | Iand | Ior | Ixor | Ilsl | Ilsr
                | Iasr | Ipopcnt | Iclz _| Ictz _|Icheckbound), _)
  | Iintop_atomic _
  | Istore((Byte_unsigned | Byte_signed | Sixteen_unsigned | Sixteen_signed
            | Thirtytwo_unsigned | Thirtytwo_signed | Word_int | Word_val
            | Double ),
            _, _)
  | Imove | Ispill | Ireload | Inegf | Iabsf | Iaddf | Isubf | Imulf | Idivf
  | Icsel _
  | Ifloatofint | Iintoffloat | Ivalueofint | Iintofvalue
  | Iconst_int _ | Iconst_float _ | Iconst_symbol _
  | Icall_ind | Icall_imm _ | Itailcall_ind | Itailcall_imm _
  | Istackoffset _ | Iload (_, _, _)
  | Ispecific(Ilea _ | Isextend32 | Izextend32 | Iprefetch _ | Ipause
             | Irdtsc | Irdpmc | Icrc32q | Istore_int (_, _, _)
             | Ilfence | Isfence | Imfence
             | Ifloat_round _
             | Ifloat_iround | Ifloat_min | Ifloat_max
             | Ioffset_loc (_, _) | Ifloatarithmem (_, _)
             | Ibswap _ | Ifloatsqrtf _ | Isqrtf)
  | Iname_for_debugger _ | Iprobe _ | Iprobe_is_enabled _ | Iopaque
  | Ibeginregion | Iendregion
    -> consumes ~int:0 ~float:0

(* Layout of the stack frame *)

let frame_required ~fun_contains_calls ~fun_num_stack_slots =
  fp || fun_contains_calls ||
  fun_num_stack_slots.(0) > 0 || fun_num_stack_slots.(1) > 0

let prologue_required ~fun_contains_calls ~fun_num_stack_slots =
  frame_required ~fun_contains_calls ~fun_num_stack_slots

(* Calling the assembler *)

let assemble_file infile outfile =
  X86_proc.assemble_file infile outfile

let init () =
  if fp then begin
    num_available_registers.(0) <- 12
  end else
    num_available_registers.(0) <- 13

let operation_supported = function
  | Cpopcnt -> !popcnt_support
  | Cprefetch _ | Catomic _
  | Capply _ | Cextcall _ | Cload _ | Calloc _ | Cstore _
  | Caddi | Csubi | Cmuli | Cmulhi _ | Cdivi | Cmodi
  | Cand | Cor | Cxor | Clsl | Clsr | Casr
  | Ccsel _
  | Cbswap _
  | Cclz _ | Cctz _
  | Ccmpi _ | Caddv | Cadda | Ccmpa _
  | Cnegf | Cabsf | Caddf | Csubf | Cmulf | Cdivf
  | Cfloatofint | Cintoffloat
  | Cvalueofint | Cintofvalue
  | Ccmpf _
  | Craise _
  | Ccheckbound
  | Cprobe _ | Cprobe_is_enabled _ | Copaque | Cbeginregion | Cendregion
    -> true

let trap_size_in_bytes = 16
