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

  xmm0 - xmm15  100 - 115 *)

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
  | Vec128 -> assert_simd_enabled (); 1
  | Float32 -> assert_float32_enabled (); 1

let num_stack_slot_classes = 3

let stack_slot_class typ =
  match (typ : machtype_component) with
  | Val | Addr | Int -> 0
  | Float -> 1
  | Vec128 -> assert_simd_enabled (); 2
  | Float32 -> assert_float32_enabled (); 1

let stack_class_tag c =
  match c with
  | 0 -> "i"
  | 1 -> "f"
  | 2 -> assert_simd_enabled (); "x"
  | c -> Misc.fatal_errorf "Unspecified stack slot class %d" c

let num_available_registers = [| 13; 16 |]

let first_available_register = [| 0; 100 |]

let register_name ty r =
  (* If the ID doesn't match the type, the array access will raise. *)
  match (ty : machtype_component) with
  | Int | Addr | Val ->
    int_reg_name.(r - first_available_register.(0))
  | Float ->
    float_reg_name.(r - first_available_register.(1))
  | Vec128 ->
    assert_simd_enabled ();
    float_reg_name.(r - first_available_register.(1))
  | Float32 ->
    assert_float32_enabled ();
    float_reg_name.(r - first_available_register.(1))

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

let hard_vec128_reg =
  let v = Array.make 16 Reg.dummy in
  for i = 0 to 15 do v.(i) <- Reg.at_location Vec128 (Reg (100 + i)) done;
  fun () -> assert_simd_enabled (); v

let hard_float32_reg =
  let v = Array.make 16 Reg.dummy in
  for i = 0 to 15 do v.(i) <- Reg.at_location Float32 (Reg (100 + i)) done;
  fun () -> assert_float32_enabled (); v

let extension_regs (type a) ?prefix (ext : a Language_extension.t) () =
  if not (Language_extension.is_enabled ext) then [||]
  else let regs =
    match ext with
    | SIMD -> hard_vec128_reg ()
    | Small_numbers -> hard_float32_reg ()
    | Mode | Unique | Include_functor | Comprehensions
    | Polymorphic_parameters | Immutable_arrays
    | Module_strengthening | Layouts | Labeled_tuples -> [||]
  in match prefix with
  | None -> regs
  | Some p -> Array.sub regs 0 (Int.min p (Array.length regs))

let all_phys_regs =
  let basic_regs = Array.append hard_int_reg hard_float_reg in
  let simd_regs = extension_regs SIMD in
  let f32_regs = extension_regs Small_numbers in
  fun () -> Array.append basic_regs
            (Array.append (simd_regs ()) (f32_regs ()))

let phys_reg ty n =
  match (ty : machtype_component) with
  | Int | Addr | Val -> hard_int_reg.(n)
  | Float -> hard_float_reg.(n - 100)
  | Float32 -> (hard_float32_reg ()).(n - 100)
  | Vec128 -> (hard_vec128_reg ()).(n - 100)

let rax = phys_reg Int 0
let rdx = phys_reg Int 4
let rcx = phys_reg Int 5
let r10 = phys_reg Int 10
let r11 = phys_reg Int 11
let rbp = phys_reg Int 12

(* CSE needs to know that all versions of xmm15 are destroyed. *)
let destroy_xmm n =
  let f64 = [| phys_reg Float (100 + n) |] in
  let f32 =
    if Language_extension.is_enabled Small_numbers
    then [| phys_reg Float32 (100 + n) |]
    else [||]
  in
  let v128 =
    if Language_extension.is_enabled SIMD
    then [| phys_reg Vec128 (100 + n) |]
    else [||]
  in
  Array.append f64 (Array.append f32 v128)

let destroyed_by_plt_stub =
  if not X86_proc.use_plt then [| |] else [| r10; r11 |]

let num_destroyed_by_plt_stub = Array.length destroyed_by_plt_stub

let destroyed_by_plt_stub_set = Reg.set_of_array destroyed_by_plt_stub

let stack_slot slot ty =
  (match (ty : machtype_component) with
   | Float | Int | Addr | Val -> ()
   | Float32 -> assert_float32_enabled ()
   | Vec128 -> assert_simd_enabled ());
  Reg.at_location ty (Stack slot)

(* Instruction selection *)

let word_addressed = false

(* Calling conventions *)

let size_domainstate_args = 64 * size_int

let calling_conventions first_int last_int first_float last_float make_stack first_stack
                        arg =
  let loc = Array.make (Array.length arg) Reg.dummy in
  let int = ref first_int in
  let float = ref first_float in
  let ofs = ref first_stack in
  for i = 0 to Array.length arg - 1 do
    match (arg.(i) : machtype_component) with
    | Val | Int | Addr as ty ->
        if !int <= last_int then begin
          loc.(i) <- phys_reg ty !int;
          incr int
        end else begin
          loc.(i) <- stack_slot (make_stack !ofs) ty;
          ofs := !ofs + size_int
        end;
        assert (not (Reg.Set.mem loc.(i) destroyed_by_plt_stub_set))
    | Float ->
        if !float <= last_float then begin
          loc.(i) <- phys_reg Float !float;
          incr float
        end else begin
          loc.(i) <- stack_slot (make_stack !ofs) Float;
          ofs := !ofs + size_float
        end
    | Vec128 ->
      if !float <= last_float then begin
        loc.(i) <- phys_reg Vec128 !float;
        incr float
      end else begin
        ofs := Misc.align !ofs 16;
        loc.(i) <- stack_slot (make_stack !ofs) Vec128;
        ofs := !ofs + size_vec128
      end
    | Float32 ->
        if !float <= last_float then begin
          loc.(i) <- phys_reg Float32 !float;
          incr float
        end else begin
          loc.(i) <- stack_slot (make_stack !ofs) Float32;
          (* float32 slots still take up a full word *)
          ofs := !ofs + size_float
        end
  done;
  (* CR mslater: (SIMD) will need to be 32/64 if vec256/512 are used. *)
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
  and ofs = ref (if Config.runtime5 then 0 else 32) in
  for i = 0 to Array.length arg - 1 do
    match (arg.(i) : machtype_component) with
    | Val | Int | Addr as ty ->
        if !reg < 4 then begin
          loc.(i) <- phys_reg ty win64_int_external_arguments.(!reg);
          incr reg
        end else begin
          loc.(i) <- stack_slot (Outgoing !ofs) ty;
          ofs := !ofs + size_int
        end
    | Float ->
        if !reg < 4 then begin
          loc.(i) <- phys_reg Float win64_float_external_arguments.(!reg);
          incr reg
        end else begin
          loc.(i) <- stack_slot (Outgoing !ofs) Float;
          ofs := !ofs + size_float
        end
    | Float32 ->
        if !reg < 4 then begin
          loc.(i) <- phys_reg Float32 win64_float_external_arguments.(!reg);
          incr reg
        end else begin
          loc.(i) <- stack_slot (Outgoing !ofs) Float32;
          (* float32 slots still take up a full word *)
          ofs := !ofs + size_float
        end
    | Vec128 ->
        (* CR mslater: (SIMD) win64 calling convention requires pass by reference *)
        Misc.fatal_error "SIMD external arguments are not supported on Win64"
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
let domainstate_ptr_dwarf_register_number = 14

(* Registers destroyed by operations *)

let int_regs_destroyed_at_c_call_win64 =
  if Config.runtime5 then [|0;1;4;5;6;7;10;11;12|] else [|0;4;5;6;7;10;11|]

let int_regs_destroyed_at_c_call =
  if Config.runtime5 then [|0;1;2;3;4;5;6;7;10;11|] else [|0;2;3;4;5;6;7;10;11|]

let destroyed_at_c_call_win64 =
  (* Win64: rbx, rbp, rsi, rdi, r12-r15, xmm6-xmm15 preserved *)
  let basic_regs = Array.append
    (Array.map (phys_reg Int) int_regs_destroyed_at_c_call_win64)
    (Array.sub hard_float_reg 0 6)
  in
  let v128_regs = extension_regs ~prefix:6 SIMD in
  let f32_regs = extension_regs ~prefix:6 Small_numbers in
  fun () -> Array.append basic_regs
              (Array.append (v128_regs ()) (f32_regs ()))

let destroyed_at_c_call_unix =
  (* Unix: rbx, rbp, r12-r15 preserved *)
  let basic_regs = Array.append
      (Array.map (phys_reg Int) int_regs_destroyed_at_c_call)
    hard_float_reg
  in
  let v128_regs = extension_regs SIMD in
  let f32_regs = extension_regs Small_numbers in
  fun () -> Array.append basic_regs
              (Array.append (v128_regs ()) (f32_regs ()))

let destroyed_at_c_call =
  (* C calling conventions preserve rbx, but it is clobbered
     by the code sequence used for C calls in emit.mlp, so it
     is marked as destroyed. *)
  if win64 then destroyed_at_c_call_win64 else destroyed_at_c_call_unix

let destroyed_at_alloc_or_poll =
  if X86_proc.use_plt then
    destroyed_by_plt_stub
  else
    [| r11 |]

let destroyed_at_pushtrap =
  [| r11 |]

let has_pushtrap traps =
  List.exists (function Cmm.Push _ -> true | Pop _ -> false) traps

let destroyed_by_simd_op op =
  match Simd_proc.register_behavior op with
  | R_RM_rax_rdx_to_xmm0
  | R_RM_to_xmm0 -> destroy_xmm 0
  | R_RM_rax_rdx_to_rcx
  | R_RM_to_rcx -> [| rcx |]
  | R_to_fst
  | R_to_R
  | R_to_RM
  | RM_to_R
  | R_R_to_fst
  | R_RM_to_fst
  | R_RM_to_R
  | R_RM_xmm0_to_fst -> [||]

(* note: keep this function in sync with `destroyed_at_{basic,terminator}` below. *)
let destroyed_at_oper = function
    Iop(Icall_ind | Icall_imm _) ->
      all_phys_regs ()
  | Iop(Iextcall {alloc; stack_ofs; }) ->
      assert (stack_ofs >= 0);
      if alloc || stack_ofs > 0 then all_phys_regs ()
      else destroyed_at_c_call ()
  | Iop(Iintop(Idiv | Imod)) | Iop(Iintop_imm((Idiv | Imod), _))
        -> [| rax; rdx |]
  | Iop(Istore(Single { reg = Float64 }, _, _))
        -> destroy_xmm 15
  | Iop(Ialloc _ | Ipoll _) -> destroyed_at_alloc_or_poll
  | Iop(Iintop(Imulh _ | Icomp _) | Iintop_imm((Icomp _), _))
        -> [| rax |]
  | Iswitch(_, _) -> [| rax; rdx |]
  | Itrywith _ -> destroyed_at_pushtrap
  | Iexit (_, traps) when has_pushtrap traps -> destroyed_at_pushtrap
  | Ireturn traps when has_pushtrap traps -> assert false
  | Iop(Ispecific (Irdtsc | Irdpmc)) -> [| rax; rdx |]
  | Iop(Ispecific(Ilfence | Isfence | Imfence)) -> [||]
  | Iop(Ispecific(Isimd op)) -> destroyed_by_simd_op op
  | Iop(Ispecific(Isextend32 | Izextend32 | Ilea _
                 | Istore_int (_, _, _) | Ioffset_loc (_, _)
                 | Ipause | Iprefetch _
                 | Ifloatarithmem (_, _) | Ifloatsqrtf _ | Ibswap _))
  | Iop(Iintop(Iadd | Isub | Imul | Iand | Ior | Ixor | Ilsl | Ilsr | Iasr
              | Ipopcnt | Iclz _ | Ictz _ ))
  | Iop(Iintop_imm((Iadd | Isub | Imul | Imulh _ | Iand | Ior | Ixor | Ilsl
                   | Ilsr | Iasr | Ipopcnt | Iclz _ | Ictz _),_))
  | Iop(Iintop_atomic _)
  | Iop(Istore((Byte_unsigned | Byte_signed | Sixteen_unsigned | Sixteen_signed
               | Thirtytwo_unsigned | Thirtytwo_signed | Word_int | Word_val
               | Single { reg = Float32 } | Double
               | Onetwentyeight_aligned | Onetwentyeight_unaligned), _, _))
  | Iop(Imove | Ispill | Ireload | Ifloatop _
       | Icsel _
       | Ivalueofint | Iintofvalue
       | Ivectorcast _ | Iscalarcast _
       | Iconst_int _ | Iconst_float32 _ | Iconst_float _
       | Iconst_symbol _ | Iconst_vec128 _
       | Itailcall_ind | Itailcall_imm _ | Istackoffset _ | Iload _
       | Iname_for_debugger _ | Iprobe _| Iprobe_is_enabled _ | Iopaque | Idls_get)
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
  | Op(Store(Single { reg = Float64 }, _, _)) ->
    destroy_xmm 15
  | Op(Intop(Imulh _ | Icomp _) | Intop_imm((Icomp _), _)) ->
    [| rax |]
  | Op (Specific (Irdtsc | Irdpmc)) ->
    [| rax; rdx |]
  | Op Poll -> destroyed_at_alloc_or_poll
  | Op (Alloc _) ->
    destroyed_at_alloc_or_poll
  | Op (Specific (Isimd op)) -> destroyed_by_simd_op op
  | Op (Move | Spill | Reload
       | Const_int _ | Const_float _ | Const_float32 _ | Const_symbol _
       | Const_vec128 _
       | Stackoffset _
       | Load _ | Store ((Byte_unsigned | Byte_signed | Sixteen_unsigned
                         | Sixteen_signed | Thirtytwo_unsigned
                         | Thirtytwo_signed | Word_int | Word_val
                         | Double | Single { reg = Float32 }
                         | Onetwentyeight_aligned | Onetwentyeight_unaligned), _, _)
       | Intop (Iadd | Isub | Imul | Iand | Ior | Ixor | Ilsl | Ilsr
               | Iasr | Ipopcnt | Iclz _ | Ictz _)
       | Intop_imm ((Iadd | Isub | Imul | Imulh _ | Iand | Ior | Ixor
                    | Ilsl | Ilsr | Iasr | Ipopcnt | Iclz _ | Ictz _),_)
       | Intop_atomic _
       | Floatop _
       | Csel _
       | Valueofint | Intofvalue
       | Vectorcast _
       | Scalarcast _
       | Probe_is_enabled _
       | Opaque
       | Begin_region
       | End_region
       | Specific (Ilea _ | Istore_int _ | Ioffset_loc _
                  | Ifloatarithmem _ | Ifloatsqrtf _ | Ibswap _
                  | Isextend32 | Izextend32 | Ipause
                  | Iprefetch _ | Ilfence | Isfence | Imfence)
       | Name_for_debugger _ | Dls_get)
  | Poptrap | Prologue ->
    if fp then [| rbp |] else [||]
  | Stack_check _ ->
    assert false (* the instruction is added after register allocation *)

(* note: keep this function in sync with `destroyed_at_oper` above,
   and `is_destruction_point` below. *)
let destroyed_at_terminator (terminator : Cfg_intf.S.terminator) =
  match terminator with
  | Never -> assert false
  | Always _ | Parity_test _ | Truth_test _ | Float_test _ | Int_test _
  | Return | Raise _ | Tailcall_self  _ | Tailcall_func _
  | Prim {op = Probe _; _}
  ->
    if fp then [| rbp |] else [||]
  | Switch _ ->
    [| rax; rdx |]
  | Call_no_return { func_symbol = _; alloc; ty_res = _; ty_args = _; stack_ofs; }
  | Prim {op = External { func_symbol = _; alloc; ty_res = _; ty_args = _; stack_ofs; }; _} ->
    assert (stack_ofs >= 0);
    if alloc || stack_ofs > 0 then all_phys_regs () else destroyed_at_c_call ()
  | Call {op = Indirect | Direct _; _} ->
    all_phys_regs ()
  | Specific_can_raise { op = (Ilea _ | Ibswap _ | Isextend32 | Izextend32
                       | Ifloatarithmem _ | Ifloatsqrtf _ | Irdtsc | Irdpmc | Ipause
                       | Isimd _ | Ilfence | Isfence | Imfence
                       | Istore_int (_, _, _) | Ioffset_loc (_, _)
                       | Iprefetch _); _ } ->
    Misc.fatal_error "no instructions specific for this architecture can raise"

(* CR-soon xclerc for xclerc: consider having more destruction points.
   We current return `true` when `destroyed_at_terminator` returns
   `all_phys_regs` (which means we are conservative in the sense we will
   spill registers that would spill anyway); we could also return `true`
   when `destroyed_at_terminator` returns `destroyed_at_c_call` for instance. *)
(* note: keep this function in sync with `destroyed_at_terminator` above. *)
let is_destruction_point ~(more_destruction_points : bool) (terminator : Cfg_intf.S.terminator) =
  match terminator with
  | Never -> assert false
  | Always _ | Parity_test _ | Truth_test _ | Float_test _ | Int_test _
  | Return | Raise _ | Tailcall_self  _ | Tailcall_func _
  | Prim {op = Probe _; _} ->
    false
  | Switch _ ->
    false
  | Call_no_return { func_symbol = _; alloc; ty_res = _; ty_args = _; }
  | Prim {op = External { func_symbol = _; alloc; ty_res = _; ty_args = _; }; _} ->
    if more_destruction_points then
      true
    else
      if alloc then true else false
  | Call {op = Indirect | Direct _; _} ->
    true
  | Specific_can_raise { op = (Ilea _ | Ibswap _ | Isextend32 | Izextend32
                       | Ifloatarithmem _ | Ifloatsqrtf _ | Irdtsc | Irdpmc | Ipause
                       | Isimd _ | Ilfence | Isfence | Imfence
                       | Istore_int (_, _, _) | Ioffset_loc (_, _)
                       | Iprefetch _); _ } ->
    Misc.fatal_error "no instructions specific for this architecture can raise"

(* Maximal register pressure *)


let safe_register_pressure = function
    Iextcall _ -> if win64 then if fp then 7 else 8 else 0
  | Ialloc _ | Ipoll _ | Imove | Ispill | Ireload
  | Ivalueofint | Iintofvalue | Ivectorcast _
  | Ifloatop _ | Iscalarcast _
  | Icsel _
  | Iconst_int _ | Iconst_float32 _ | Iconst_float _
  | Iconst_symbol _ | Iconst_vec128 _
  | Icall_ind | Icall_imm _ | Itailcall_ind | Itailcall_imm _
  | Istackoffset _ | Iload _ | Istore (_, _, _)
  | Iintop _ | Iintop_imm (_, _) | Iintop_atomic _
  | Ispecific _ | Iname_for_debugger _
  | Iprobe _ | Iprobe_is_enabled _ | Iopaque
  | Ibeginregion | Iendregion | Idls_get
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
  | Istore(Single { reg = Float64 }, _, _) | Ifloatop (Icompf _) ->
    consumes ~int:0 ~float:1
  | Ispecific(Isimd op) ->
    (match Simd_proc.register_behavior op with
    | R_RM_rax_rdx_to_xmm0
    | R_RM_to_xmm0 -> consumes ~int:0 ~float:1
    | R_RM_rax_rdx_to_rcx
    | R_RM_to_rcx -> consumes ~int:1 ~float:0
    | R_to_fst
    | R_to_R
    | R_to_RM
    | RM_to_R
    | R_R_to_fst
    | R_RM_to_fst
    | R_RM_to_R
    | R_RM_xmm0_to_fst -> consumes ~int:0 ~float:0)
  | Iintop(Iadd | Isub | Imul | Imulh _ | Iand | Ior | Ixor | Ilsl | Ilsr | Iasr
           | Ipopcnt|Iclz _| Ictz _)
  | Iintop_imm((Iadd | Isub | Imul | Imulh _ | Iand | Ior | Ixor | Ilsl | Ilsr
                | Iasr | Ipopcnt | Iclz _| Ictz _), _)
  | Iintop_atomic _
  | Istore((Byte_unsigned | Byte_signed | Sixteen_unsigned | Sixteen_signed
            | Thirtytwo_unsigned | Thirtytwo_signed | Word_int | Word_val
            | Single { reg = Float32 } | Double
            | Onetwentyeight_aligned | Onetwentyeight_unaligned),
            _, _)
  | Imove | Ispill | Ireload | Ifloatop (Inegf | Iabsf | Iaddf | Isubf | Imulf | Idivf)
  | Icsel _
  | Ivalueofint | Iintofvalue | Ivectorcast _ | Iscalarcast _
  | Iconst_int _ | Iconst_float _ | Iconst_float32 _
  | Iconst_symbol _ | Iconst_vec128 _
  | Icall_ind | Icall_imm _ | Itailcall_ind | Itailcall_imm _
  | Istackoffset _ | Iload _
  | Ispecific(Ilea _ | Isextend32 | Izextend32 | Iprefetch _ | Ipause
             | Irdtsc | Irdpmc | Istore_int (_, _, _)
             | Ilfence | Isfence | Imfence
             | Ioffset_loc (_, _) | Ifloatarithmem (_, _) | Ifloatsqrtf _
             | Ibswap _)
  | Iname_for_debugger _ | Iprobe _ | Iprobe_is_enabled _ | Iopaque
  | Ibeginregion | Iendregion | Idls_get
    -> consumes ~int:0 ~float:0

(* Layout of the stack frame *)

let initial_stack_offset ~num_stack_slots:_ ~contains_calls:_ = 0

let trap_frame_size_in_bytes = 16

let frame_required ~fun_contains_calls ~fun_num_stack_slots =
  fp || fun_contains_calls ||
  fun_num_stack_slots.(0) > 0 ||
  fun_num_stack_slots.(1) > 0 ||
  fun_num_stack_slots.(2) > 0

let prologue_required ~fun_contains_calls ~fun_num_stack_slots =
  frame_required ~fun_contains_calls ~fun_num_stack_slots

(* returned size includes return address *)
let frame_size ~stack_offset ~contains_calls ~num_stack_slots =
  if frame_required ~fun_contains_calls:contains_calls
     ~fun_num_stack_slots:num_stack_slots
  then begin
    let sz =
      (stack_offset
       + 8
       + 8 * num_stack_slots.(0)
       + 8 * num_stack_slots.(1)
       + 16 * num_stack_slots.(2)
       + (if fp then 8 else 0))
    in Misc.align sz 16
  end else
    stack_offset + 8

type slot_offset =
  | Bytes_relative_to_stack_pointer of int
  | Bytes_relative_to_domainstate_pointer of int

(* CR mshinwell: standardise everywhere on e.g. [contains_calls] instead
   of [fun_contains_calls] *)

let slot_offset loc ~stack_class ~stack_offset ~fun_contains_calls
      ~fun_num_stack_slots =
  match loc with
  | Incoming n ->
      Bytes_relative_to_stack_pointer (
        frame_size ~stack_offset ~contains_calls:fun_contains_calls
          ~num_stack_slots:fun_num_stack_slots
        + n)
  | Local n ->
      if fun_num_stack_slots.(2) > 0 || stack_class >= 2
      then assert_simd_enabled ();
      Bytes_relative_to_stack_pointer (
        stack_offset +
          (* Preserves original ordering (int -> float) *)
          match stack_class with
          | 2 -> n * 16
          | 0 -> fun_num_stack_slots.(2) * 16 + n * 8
          | 1 -> fun_num_stack_slots.(2) * 16
                 + fun_num_stack_slots.(0) * 8 + n * 8
          | _ -> Misc.fatal_errorf "Unknown register class %d" stack_class)
  | Outgoing n -> Bytes_relative_to_stack_pointer n
  | Domainstate n ->
      Bytes_relative_to_domainstate_pointer (
        n + Domainstate.(idx_of_field Domain_extra_params) * 8)

(* Calling the assembler *)

let assemble_file infile outfile =
  X86_proc.assemble_file infile outfile

let init () =
  if fp then begin
    num_available_registers.(0) <- 12
  end else
    num_available_registers.(0) <- 13

(* Precolored_regs is not always the same as [all_phys_regs], as some physical registers
   may not be allocatable (e.g. rbp when frame pointers are enabled). *)
let precolored_regs () =
  let phys_regs = Reg.set_of_array (all_phys_regs ()) in
  if fp then Reg.Set.remove rbp phys_regs else phys_regs

let operation_supported = function
  | Cpopcnt -> Arch.Extension.enabled POPCNT
  | Cprefetch _ | Catomic _
  | Capply _ | Cextcall _ | Cload _ | Calloc _ | Cstore _
  | Caddi | Csubi | Cmuli | Cmulhi _ | Cdivi | Cmodi
  | Cand | Cor | Cxor | Clsl | Clsr | Casr
  | Ccsel _
  | Cbswap _
  | Cclz _ | Cctz _
  | Ccmpi _ | Caddv | Cadda | Ccmpa _
  | Cnegf | Cabsf | Caddf | Csubf | Cmulf | Cdivf
  | Cvalueofint | Cintofvalue
  | Ccmpf _
  | Craise _
  | Cvectorcast _ | Cscalarcast _
  | Cprobe _ | Cprobe_is_enabled _ | Copaque | Cbeginregion | Cendregion
  | Ctuple_field _
  | Cdls_get
    -> true

let trap_size_in_bytes = 16
