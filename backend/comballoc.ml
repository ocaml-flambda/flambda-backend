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

let rec combine i allocstate =
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
           combine i.next
             (Pending_alloc { reg = i.res.(0);
                              dbginfos = dbginfo @ dbginfos;
                              totalsz = totalsz + sz;
                              mode }) in
         (instr_cons_debug (Iop(Iintop_imm(Iadd, -sz)))
            [| reg |] i.res i.dbg next,
           state)
      | No_alloc | Pending_alloc _ ->
         let (next, state) =
           combine i.next
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
         (instr_cons_debug (Iop(Ialloc {bytes = totalsz; dbginfo; mode}))
          i.arg i.res i.dbg next, allocstate)
      end
  | Iop(Icall_ind | Icall_imm _ | Iextcall _ |
        Itailcall_ind | Itailcall_imm _ | Ipoll _ | Iprobe _ |
        Iintop (Icheckbound | Icheckalign _) |
        Iintop_imm ((Icheckbound | Icheckalign _), _)) ->
      let newnext = combine_restart i.next in
      (instr_cons_debug i.desc i.arg i.res i.dbg newnext,
       allocstate)
  | Iop(Ibeginregion|Iendregion) -> begin
      match allocstate with
      | Pending_alloc { mode = Alloc_local; _ } ->
          let newnext = combine_restart i.next in
          (instr_cons_debug i.desc i.arg i.res i.dbg newnext, allocstate)
      | No_alloc | Pending_alloc { mode = Alloc_heap; _ } ->
          let newnext, s' = combine i.next allocstate in
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
      let (newnext, s') = combine i.next allocstate in
      (instr_cons_debug i.desc i.arg i.res i.dbg newnext, s')
  | Iifthenelse(test, ifso, ifnot) ->
      let newifso = combine_restart ifso in
      let newifnot = combine_restart ifnot in
      let newnext = combine_restart i.next in
      (instr_cons_debug (Iifthenelse(test, newifso, newifnot)) i.arg i.res
         i.dbg newnext,
       allocstate)
  | Iswitch(table, cases) ->
      let newcases = Array.map combine_restart cases in
      let newnext = combine_restart i.next in
      (instr_cons_debug (Iswitch(table, newcases)) i.arg i.res i.dbg newnext,
       allocstate)
  | Icatch(rec_flag, ts, handlers, body) ->
      let (newbody, s') = combine body allocstate in
      let newhandlers =
        List.map
          (fun (io, ts, handler, is_cold) -> io, ts, combine_restart handler, is_cold)
          handlers
      in
      let newnext = combine_restart i.next in
      (instr_cons_debug (Icatch(rec_flag, ts, newhandlers, newbody))
         i.arg i.res i.dbg newnext, s')
  | Itrywith(body, kind, (ts, handler)) ->
      let (newbody, s') = combine body allocstate in
      let newhandler = combine_restart handler in
      let newnext = combine_restart i.next in
      (instr_cons_debug (Itrywith(newbody, kind, (ts, newhandler)))
         i.arg i.res i.dbg newnext, s')

and combine_restart i =
  let (newi, _) = combine i No_alloc in newi

let fundecl f =
  {f with fun_body = combine_restart f.fun_body}
