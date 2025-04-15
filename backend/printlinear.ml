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

(* Pretty-printing of linearized machine code *)

[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare
open Format
open Linear

let label ppf l = Format.fprintf ppf "L%a" Label.format l

let section_name_to_string ppf = function
  | None -> ()
  | Some name -> fprintf ppf " in %s section" name

let call_operation ?(print_reg = Printreg.reg) ppf op arg =
  let regs = Printreg.regs' ~print_reg in
  match op with
  | Lcall_ind -> fprintf ppf "call %a" regs arg
  | Lcall_imm { func } -> fprintf ppf "call \"%s\" %a" func.sym_name regs arg
  | Ltailcall_ind -> fprintf ppf "tailcall %a" regs arg
  | Ltailcall_imm { func } ->
    fprintf ppf "tailcall \"%s\" %a" func.sym_name regs arg
  | Lextcall { func; alloc; _ } ->
    fprintf ppf "extcall \"%s\" %a%s" func regs arg
      (if alloc then "" else " (noalloc)")
  | Lprobe { name; handler_code_sym } ->
    fprintf ppf "probe \"%s\" %s %a" name handler_code_sym regs arg

let instr' ?(print_reg = Printreg.reg) ppf i =
  let reg = print_reg in
  let regs = Printreg.regs' ~print_reg in
  let regsetaddr = Printreg.regsetaddr' ~print_reg in
  let test = Operation.format_test ~print_reg in
  let operation = Printoperation.operation ~print_reg in
  (if !Flambda_backend_flags.davail
  then
    let module RAS = Reg_availability_set in
    let ras_is_nonempty (set : RAS.t) =
      match set with
      | Ok set -> not (Reg_with_debug_info.Set.is_empty set)
      | Unreachable -> true
    in
    if (match i.available_before with
       | None -> false
       | Some available_before -> ras_is_nonempty available_before)
       ||
       match i.available_across with
       | None -> false
       | Some available_across -> ras_is_nonempty available_across
    then
      if Option.equal RAS.equal i.available_before i.available_across
      then
        fprintf ppf "@[<1>AB=AA={%a}@]@,"
          (Misc.Stdlib.Option.print (RAS.print ~print_reg:reg))
          i.available_before
      else (
        fprintf ppf "@[<1>AB={%a}"
          (Misc.Stdlib.Option.print (RAS.print ~print_reg:reg))
          i.available_before;
        fprintf ppf ",AA={%a}"
          (Misc.Stdlib.Option.print (RAS.print ~print_reg:reg))
          i.available_across;
        fprintf ppf "@]@,"));
  (match i.desc with
  | Lend -> ()
  | Lprologue -> fprintf ppf "prologue"
  | Lop op ->
    (match op with
    | Alloc _ | Poll -> fprintf ppf "@[<1>{%a}@]@," regsetaddr i.live
    | _ -> ());
    operation op i.arg ppf i.res
  | Lcall_op op ->
    (match op with
    | Lcall_ind | Lcall_imm _ | Lextcall _ | Lprobe _ ->
      fprintf ppf "@[<1>{%a}@]@," regsetaddr i.live
    | _ -> ());
    call_operation ppf op i.arg
  | Lreloadretaddr -> fprintf ppf "reload retaddr"
  | Lreturn -> fprintf ppf "return %a" regs i.arg
  | Llabel { label = lbl; section_name } ->
    fprintf ppf "%a%a:" label lbl section_name_to_string section_name
  | Lbranch lbl -> fprintf ppf "goto %a" label lbl
  | Lcondbranch (tst, lbl) ->
    fprintf ppf "if %a goto %a" (test tst) i.arg label lbl
  | Lcondbranch3 (lbl0, lbl1, lbl2) ->
    fprintf ppf "switch3 %a" reg i.arg.(0);
    let case n = function
      | None -> ()
      | Some lbl -> fprintf ppf "@,case %i: goto %a" n label lbl
    in
    case 0 lbl0;
    case 1 lbl1;
    case 2 lbl2;
    fprintf ppf "@,endswitch"
  | Lswitch lblv ->
    fprintf ppf "switch %a" reg i.arg.(0);
    for i = 0 to Array.length lblv - 1 do
      fprintf ppf "case %i: goto %a" i label lblv.(i)
    done;
    fprintf ppf "@,endswitch"
  | Lentertrap -> fprintf ppf "enter trap"
  | Ladjust_stack_offset { delta_bytes } ->
    fprintf ppf "adjust pseudo stack offset by %d bytes" delta_bytes
  | Lpushtrap { lbl_handler } -> fprintf ppf "push trap %a" label lbl_handler
  | Lpoptrap -> fprintf ppf "pop trap"
  | Lraise k -> fprintf ppf "%s %a" (Lambda.raise_kind k) reg i.arg.(0)
  | Lstackcheck { max_frame_size_bytes } ->
    fprintf ppf "stack check (%d bytes)" max_frame_size_bytes);
  if (not (Debuginfo.is_none i.dbg)) && !Clflags.locations
  then fprintf ppf " %s" (Debuginfo.to_string i.dbg)

let instr ppf i = instr' ppf i

let rec all_instr ppf i =
  match i.desc with
  | Lend -> ()
  | _ -> fprintf ppf "%a@,%a" instr i all_instr i.next

let fundecl ppf f =
  let dbg =
    if Debuginfo.is_none f.fun_dbg || not !Clflags.locations
    then ""
    else " " ^ Debuginfo.to_string f.fun_dbg
  in
  fprintf ppf "@[<v 2>%s:%s%a@,%a@]" f.fun_name dbg section_name_to_string
    f.fun_section_name all_instr f.fun_body
