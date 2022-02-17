(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Pretty-printing of pseudo machine code *)

open Format
open Cmm
open Reg
open Mach
open Interval

module V = Backend_var

let reg ppf r =
  if not (Reg.anonymous r) then
    fprintf ppf "%s" (Reg.name r)
  else
    fprintf ppf "%s"
      (match r.typ with Val -> "V" | Addr -> "A" | Int -> "I" | Float -> "F");
  fprintf ppf "/%i" r.stamp;
  begin match r.loc with
  | Unknown -> ()
  | Reg r ->
      fprintf ppf "[%s]" (Proc.register_name r)
  | Stack(Local s) ->
      fprintf ppf "[s%i]" s
  | Stack(Incoming s) ->
      fprintf ppf "[si%i]" s
  | Stack(Outgoing s) ->
      fprintf ppf "[so%i]" s
  end

let regs ppf v =
  match Array.length v with
  | 0 -> ()
  | 1 -> reg ppf v.(0)
  | n -> reg ppf v.(0);
         for i = 1 to n-1 do fprintf ppf " %a" reg v.(i) done

let operand ppf = function
  | Ireg r -> fprintf ppf "reg %a" reg r
  | Iimm i -> fprintf ppf "imm %a" Targetint.print i
  | Iimmf f -> fprintf ppf "immf %F" (Int64.float_of_bits f)
  | Imem { chunk; addr; reg=rv } ->
    let c =
      match chunk with
      | Some chunk -> (Printcmm.chunk chunk)
      | None -> ""
    in
    fprintf ppf "mem %s[%a]" c (Arch.print_addressing reg addr) rv

let operands ppf v =
  match Array.length v with
  | 0 -> ()
  | 1 -> operand ppf v.(0)
  | n -> operand ppf v.(0);
         for i = 1 to n-1 do fprintf ppf ", %a" operand v.(i) done

let regset ppf s =
  let first = ref true in
  Reg.Set.iter
    (fun r ->
      if !first then begin first := false; fprintf ppf "%a" reg r end
      else fprintf ppf "@ %a" reg r)
    s

let regsetaddr ppf s =
  let first = ref true in
  Reg.Set.iter
    (fun r ->
      if !first then begin first := false; fprintf ppf "%a" reg r end
      else fprintf ppf "@ %a" reg r;
      match r.typ with
      | Val -> fprintf ppf "*"
      | Addr -> fprintf ppf "!"
      | _ -> ())
    s

let intcomp = function
  | Isigned c -> Printf.sprintf " %ss " (Printcmm.integer_comparison c)
  | Iunsigned c -> Printf.sprintf " %su " (Printcmm.integer_comparison c)

let floatcomp c =
    Printf.sprintf " %sf " (Printcmm.float_comparison c)

let is_unary_op = function
  | Iclz _
  | Ictz _
  | Ipopcnt -> true
  | Iadd | Isub | Imul | Imulh _ | Idiv | Imod
  | Iand | Ior | Ixor | Ilsl | Ilsr | Iasr
  | Icomp _
  | Icheckbound
    -> false

let is_unary_floatop = function
  | Inegf | Iabsf -> true
  | Iaddf | Isubf | Imulf | Idivf | Icompf _ -> false

let intop = function
  | Iadd -> " + "
  | Isub -> " - "
  | Imul -> " * "
  | Imulh { signed } -> " *h "^(if signed then "" else "u")
  | Idiv -> " div "
  | Imod -> " mod "
  | Iand -> " & "
  | Ior ->  " | "
  | Ixor -> " ^ "
  | Ilsl -> " << "
  | Ilsr -> " >>u "
  | Iasr -> " >>s "
  | Iclz { arg_is_non_zero; } -> Printf.sprintf "clz %B " arg_is_non_zero
  | Ictz { arg_is_non_zero; } -> Printf.sprintf "ctz %B " arg_is_non_zero
  | Ipopcnt -> "popcnt "
  | Icomp cmp -> intcomp cmp
  | Icheckbound -> Printf.sprintf "check > "

let floatop = function
  | Inegf -> "-f "
  | Iabsf -> "absf "
  | Iaddf -> " +f "
  | Isubf -> " -f "
  | Imulf -> " *f "
  | Idivf -> " /f "
  | Icompf cmp -> floatcomp cmp

let test tst ppf arg =
  match tst with
  | Itruetest -> operand ppf arg.(0)
  | Ifalsetest -> fprintf ppf "not %a" operand arg.(0)
  | Iinttest cmp ->
      fprintf ppf "%a%s%a" operand arg.(0) (intcomp cmp) operand arg.(1)
  | Ifloattest cmp ->
      fprintf ppf "%a%s%a" operand arg.(0) (floatcomp cmp) operand arg.(1)
  | Ieventest -> fprintf ppf "%a & 1 == 0" operand arg.(0)
  | Ioddtest -> fprintf ppf "%a & 1 == 1" operand arg.(0)

let operation op arg ppf res =
  if Array.length res > 0 then fprintf ppf "%a := " regs res;
  match op with
  | Imove -> operands ppf arg
  | Ispill -> fprintf ppf "%a (spill)" operands arg
  | Ireload -> fprintf ppf "%a (reload)" operands arg
  | Iconst_int n -> fprintf ppf "%s" (Nativeint.to_string n)
  | Iconst_float f -> fprintf ppf "%F" (Int64.float_of_bits f)
  | Iconst_symbol s -> fprintf ppf "\"%s\"" s
  | Icall_ind -> fprintf ppf "call %a" operands arg
  | Icall_imm { func; } -> fprintf ppf "call \"%s\" %a" func operands arg
  | Itailcall_ind -> fprintf ppf "tailcall %a" operands arg
  | Itailcall_imm { func; } -> fprintf ppf "tailcall \"%s\" %a" func operands arg
  | Iextcall { func; alloc; _ } ->
      fprintf ppf "extcall \"%s\" %a%s" func operands arg
      (if alloc then "" else " (noalloc)")
  | Istackoffset n ->
      fprintf ppf "offset stack %i" n
  | Iload(chunk, addr) ->
      fprintf ppf "%s[%a]"
       (Printcmm.chunk chunk) (Arch.print_addressing reg addr)
       (Array.map Mach.arg_reg arg)
  | Istore is_assign ->
      fprintf ppf "%a := %a %s" operand arg.(1) operand arg.(0)
       (if is_assign then "(assign)" else "(init)")
  | Ialloc { bytes = n; mode = Alloc_heap } ->
    fprintf ppf "alloc %i" n;
  | Ialloc { bytes = n; mode = Alloc_local } ->
    fprintf ppf "alloc_local %i" n;
  | Iintop(op) ->
    if is_unary_op op then begin
      assert (Array.length arg = 1);
      fprintf ppf "%s%a" (intop op) operand arg.(0)
    end else begin
      assert (Array.length arg = 2);
      fprintf ppf "%a%s%a" operand arg.(0) (intop op) operand arg.(1)
    end
  | Ifloatop(op) ->
     if is_unary_floatop op then
       begin
        assert (Array.length arg = 1);
        fprintf ppf "%s%a" (floatop op) operand arg.(0)
      end else begin
        assert (Array.length arg > 1);
        fprintf ppf "%a%s%a" operand arg.(0) (floatop op) operand arg.(1)
      end
  | Ifloatofint -> fprintf ppf "floatofint %a" operand arg.(0)
  | Iintoffloat -> fprintf ppf "intoffloat %a" operand arg.(0)
  | Iopaque -> fprintf ppf "opaque %a" operand arg.(0)
  | Iname_for_debugger { ident; which_parameter; } ->
    fprintf ppf "name_for_debugger %a%s=%a"
      V.print ident
      (match which_parameter with
        | None -> ""
        | Some index -> sprintf "[P%d]" index)
      operand arg.(0)
  | Ibeginregion -> fprintf ppf "beginregion"
  | Iendregion -> fprintf ppf "endregion %a" operand arg.(0)
  | Ispecific op ->
    Arch.print_specific_operation operand op ppf arg
  | Iprobe {name;handler_code_sym} ->
    fprintf ppf "probe \"%s\" %s %a" name handler_code_sym operands arg
  | Iprobe_is_enabled {name} -> fprintf ppf "probe_is_enabled \"%s\"" name

let rec instr ppf i =
  if !Clflags.dump_live then begin
    fprintf ppf "@[<1>{%a" regsetaddr i.live;
    if Array.length i.arg > 0 then
      fprintf ppf "@ +@ %a" regsetaddr (Mach.arg_regset i.arg);
    fprintf ppf "}@]@,";
    if !Clflags.dump_avail then begin
      let module RAS = Reg_availability_set in
      fprintf ppf "@[<1>AB={%a}" (RAS.print ~print_reg:reg) i.available_before;
      begin match i.available_across with
      | None -> ()
      | Some available_across ->
        fprintf ppf ",AA={%a}" (RAS.print ~print_reg:reg) available_across
      end;
      fprintf ppf "@]@,"
    end
  end;
  begin match i.desc with
  | Iend -> ()
  | Iop op ->
      operation op i.arg ppf i.res
  | Ireturn traps ->
      fprintf ppf "return%a %a" Printcmm.trap_action_list traps operands i.arg
  | Iifthenelse(tst, ifso, ifnot) ->
      fprintf ppf "@[<v 2>if %a then@,%a" (test tst) i.arg instr ifso;
      begin match ifnot.desc with
      | Iend -> ()
      | _ -> fprintf ppf "@;<0 -2>else@,%a" instr ifnot
      end;
      fprintf ppf "@;<0 -2>endif@]"
  | Iswitch(index, cases) ->
      fprintf ppf "switch %a" operand i.arg.(0);
      for i = 0 to Array.length cases - 1 do
        fprintf ppf "@,@[<v 2>@[";
        for j = 0 to Array.length index - 1 do
          if index.(j) = i then fprintf ppf "case %i:@," j
        done;
        fprintf ppf "@]@,%a@]" instr cases.(i)
      done;
      fprintf ppf "@,endswitch"
  | Icatch(flag, _ts, handlers, body) ->
      fprintf ppf "@[<v 2>catch%a@,%a@;<0 -2>with"
        Printcmm.rec_flag flag instr body;
      let h (nfail, _trap_stack, handler) =
        (* CR vlaviron: print the trap stacks ? *)
        fprintf ppf "(%d)@,%a@;" nfail instr handler in
      let rec aux = function
        | [] -> ()
        | [v] -> h v
        | v :: t ->
            h v;
            fprintf ppf "@ and";
            aux t
      in
      aux handlers;
      fprintf ppf "@;<0 -2>endcatch@]"
  | Iexit (i, traps) ->
      fprintf ppf "exit%a(%d)" Printcmm.trap_action_list traps i
  | Itrywith(body, kind, (_ts, handler)) ->
      fprintf ppf "@[<v 2>try%a@,%a@;<0 -2>with@,%a@;<0 -2>endtry@]"
             Printcmm.trywith_kind kind instr body instr handler
  | Iraise k ->
      fprintf ppf "%s %a" (Lambda.raise_kind k) operand i.arg.(0)
  end;
  if not (Debuginfo.is_none i.dbg) && !Clflags.locations then
    fprintf ppf "%s" (Debuginfo.to_string i.dbg);
  begin match i.next.desc with
    Iend -> ()
  | _ -> fprintf ppf "@,%a" instr i.next
  end

let fundecl ppf f =
  let dbg =
    if Debuginfo.is_none f.fun_dbg || not !Clflags.locations then
      ""
    else
      " " ^ Debuginfo.to_string f.fun_dbg in
  fprintf ppf "@[<v 2>%s(%a)%s@,%a@]"
    f.fun_name regs f.fun_args dbg instr f.fun_body

let phase msg ppf f =
  fprintf ppf "*** %s@.%a@." msg fundecl f

let interference ppf r =
  let interf ppf =
   List.iter
    (fun r -> fprintf ppf "@ %a" reg r)
    r.interf in
  fprintf ppf "@[<2>%a:%t@]@." reg r interf

let interferences ppf () =
  fprintf ppf "*** Interferences@.";
  List.iter (interference ppf) (Reg.all_registers())

let interval ppf i =
  let interv ppf =
    List.iter
      (fun r -> fprintf ppf "@ [%d;%d]" r.rbegin r.rend)
      i.ranges in
  fprintf ppf "@[<2>%a:%t@]@." reg i.reg interv

let intervals ppf () =
  fprintf ppf "*** Intervals@.";
  List.iter (interval ppf) (Interval.all_fixed_intervals());
  List.iter (interval ppf) (Interval.all_intervals())

let preference ppf r =
  let prefs ppf =
    List.iter
      (fun (r, w) -> fprintf ppf "@ %a weight %i" reg r w)
      r.prefer in
  fprintf ppf "@[<2>%a: %t@]@." reg r prefs

let preferences ppf () =
  fprintf ppf "*** Preferences@.";
  List.iter (preference ppf) (Reg.all_registers())
