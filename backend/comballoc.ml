(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1999 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
[@@@ocaml.warning "+a-30-40-41-42"]

(* Combine heap allocations occurring in the same basic block *)

open Mach

type pending_alloc =
  { reg: Reg.t;         (* register holding the result of the last allocation *)
    dbginfos: Debuginfo.alloc_dbginfo;   (* debug info for each pending alloc *)
    totalsz: int;                     (* amount to be allocated in this block *)
    mode: Lambda.alloc_mode }                     (* heap or stack allocation *)

type allocation_state =
    No_alloc
  | Pending_alloc of pending_alloc

let dump_formatter : Format.formatter option Lazy.t = lazy (
    match !Flambda_backend_flags.dump_comballoc_path with
    | "" -> None
    | dir ->
      let file = Sys.argv |> Array.to_list |> String.concat " " |> Digest.string |> Digest.to_hex in
      let path = Filename.concat dir file in
      let chan = open_out path in
      let ppf = Format.formatter_of_out_channel chan in
      at_exit (fun () -> close_out_noerr chan);
      Some ppf)

let dump_alloc_info ~fun_name dbginfo =
  match dbginfo with
  | [] | [_] -> ()
  | _ ->
    match Lazy.force dump_formatter with
    | None -> ()
    | Some ppf ->
      Format.fprintf ppf "%s |" fun_name;
      List.iter
        (fun { Debuginfo.alloc_words; alloc_dbg } ->
           Format.fprintf ppf " %d@%a" alloc_words Debuginfo.print_compact alloc_dbg)
        dbginfo;
      Format.fprintf ppf "\n%!"

let rec combine ~fun_name i allocstate =
  match i.desc with
    Iend | Ireturn _ | Iexit _ | Iraise _ ->
      (i, allocstate)
  | Iop(Ialloc { bytes = sz; dbginfo; mode }) ->
      assert (List.length dbginfo = 1);
      begin match allocstate with
      | Pending_alloc {reg; dbginfos; totalsz; mode = prev_mode}
          when (mode = prev_mode) &&
              ((totalsz + sz <= (Config.max_young_wosize + 1) * Arch.size_addr)
               || Lambda.is_local_mode mode) ->
          let (next, state) =
           combine ~fun_name i.next
             (Pending_alloc { reg = i.res.(0);
                              dbginfos = dbginfo @ dbginfos;
                              totalsz = totalsz + sz;
                              mode }) in
         (instr_cons_debug (Iop(Iintop_imm(Iadd, -sz)))
            [| reg |] i.res i.dbg next,
           state)
      | No_alloc | Pending_alloc _ ->
         let (next, state) =
           combine ~fun_name i.next
             (Pending_alloc { reg = i.res.(0);
                              dbginfos = dbginfo;
                              totalsz = sz;
                              mode }) in
         let totalsz, dbginfo =
           match state with
           | No_alloc -> assert false
           | Pending_alloc { totalsz; dbginfos; mode = m; _ } ->
              assert (Lambda.eq_mode m mode);
              totalsz, dbginfos in
         let next =
           let offset = totalsz - sz in
           if offset = 0 then next
           else instr_cons_debug (Iop(Iintop_imm(Iadd, offset))) i.res
                i.res i.dbg next
         in
         dump_alloc_info ~fun_name dbginfo;
         (instr_cons_debug (Iop(Ialloc {bytes = totalsz; dbginfo; mode}))
          i.arg i.res i.dbg next, allocstate)
      end
  | Iop(Icall_ind | Icall_imm _ | Iextcall _ |
        Itailcall_ind | Itailcall_imm _ | Ipoll _ | Iprobe _) ->
      let newnext = combine_restart ~fun_name i.next in
      (instr_cons_debug i.desc i.arg i.res i.dbg newnext,
       allocstate)
  | Iop(Ibeginregion|Iendregion) -> begin
      match allocstate with
      | Pending_alloc { mode = Alloc_local; _ } ->
          let newnext = combine_restart ~fun_name i.next in
          (instr_cons_debug i.desc i.arg i.res i.dbg newnext, allocstate)
      | No_alloc | Pending_alloc { mode = Alloc_heap; _ } ->
          let newnext, s' = combine ~fun_name i.next allocstate in
          (instr_cons_debug i.desc i.arg i.res i.dbg newnext, s')
    end
  | Iop((Imove|Ispill|Ireload|Inegf|Iabsf|Iaddf|Isubf|Imulf|Idivf|Ifloatofint|
         Iintoffloat|Ivalueofint|Iintofvalue|Ivectorcast _|Iopaque|Iconst_int _|
         Iconst_float _|Iconst_vec128 _|Iconst_symbol _|Istackoffset _|Iload _|
         Istore (_, _, _)|Icompf _|Icsel _ |Ispecific _|Iname_for_debugger _|
         Iprobe_is_enabled _|Iscalarcast _|Idls_get))
  | Iop(Iintop(Iadd | Isub | Imul | Idiv | Imod | Iand | Ior | Ixor
              | Ilsl | Ilsr | Iasr | Ipopcnt | Imulh _
              | Iclz _ | Ictz _ | Icomp _))
  | Iop(Iintop_imm((Iadd | Isub | Imul | Idiv | Imod | Iand | Ior | Ixor
                   | Ilsl | Ilsr | Iasr | Ipopcnt | Imulh _
                   | Iclz _ | Ictz _ | Icomp _),_))
  | Iop(Iintop_atomic _) ->
      let (newnext, s') = combine ~fun_name i.next allocstate in
      (instr_cons_debug i.desc i.arg i.res i.dbg newnext, s')
  | Iifthenelse(test, ifso, ifnot) ->
      let newifso = combine_restart ~fun_name ifso in
      let newifnot = combine_restart ~fun_name ifnot in
      let newnext = combine_restart ~fun_name i.next in
      (instr_cons_debug (Iifthenelse(test, newifso, newifnot)) i.arg i.res
         i.dbg newnext,
       allocstate)
  | Iswitch(table, cases) ->
      let newcases = Array.map (combine_restart ~fun_name) cases in
      let newnext = combine_restart ~fun_name i.next in
      (instr_cons_debug (Iswitch(table, newcases)) i.arg i.res i.dbg newnext,
       allocstate)
  | Icatch(rec_flag, ts, handlers, body) ->
      let (newbody, s') = combine ~fun_name body allocstate in
      let newhandlers =
        List.map
          (fun (io, ts, handler, is_cold) -> io, ts, combine_restart ~fun_name handler, is_cold)
          handlers
      in
      let newnext = combine_restart ~fun_name i.next in
      (instr_cons_debug (Icatch(rec_flag, ts, newhandlers, newbody))
         i.arg i.res i.dbg newnext, s')
  | Itrywith(body, kind, (ts, handler)) ->
      let (newbody, s') = combine ~fun_name body allocstate in
      let newhandler = combine_restart ~fun_name handler in
      let newnext = combine_restart ~fun_name i.next in
      (instr_cons_debug (Itrywith(newbody, kind, (ts, newhandler)))
         i.arg i.res i.dbg newnext, s')

and combine_restart ~fun_name i =
  let (newi, _) = combine ~fun_name i No_alloc in newi

let fundecl f =
  {f with fun_body = combine_restart ~fun_name:f.fun_name f.fun_body}
