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

(* Insert load/stores for pseudoregs that got assigned to stack locations. *)

open Misc
open Reg
open Mach

let insert_move dbg src dst next =
  if src.loc = dst.loc
  then next
  else instr_cons_debug (Iop Imove) [|src|] [|dst|] dbg next

let insert_moves dbg src dst next =
  let rec insmoves i =
    if i >= Array.length src
    then next
    else insert_move dbg src.(i) dst.(i) (insmoves (i+1))
  in insmoves 0

class reload_generic = object (self)

val mutable redo_regalloc = false

method makereg r =
  match r.loc with
    Unknown -> fatal_error "Reload.makereg"
  | Reg _ -> r
  | Stack _ ->
      redo_regalloc <- true;
      let newr = Reg.clone r in
      (* Strongly discourage spilling this register *)
      newr.spill_cost <- 100000;
      newr

method private makeregs rv =
  let n = Array.length rv in
  let newv = Array.make n Reg.dummy in
  for i = 0 to n-1 do newv.(i) <- self#makereg rv.(i) done;
  newv

method private makereg1 rv =
  let newv = Array.copy rv in
  newv.(0) <- self#makereg rv.(0);
  newv

method reload_operation op arg res =
  (* By default, assume that arguments and results must reside
     in hardware registers. For moves, allow one arg or one
     res to be stack-allocated, but do something for
     stack-to-stack moves *)
  match op with
    (* Reinterpret casts are essentially moves, as they do not require transforming
       the value. That means regs with the same stack location can simply be aliased.
       However, the amd64 backend specializes casts other than int<->value, because moving
       values across gpr<->xmm can require register operands. *)
    | Imove | Ireload | Ispill | Ireinterpret_cast _ ->
      begin match arg.(0), res.(0) with
        {loc = Stack s1}, {loc = Stack s2} ->
          if s1 = s2
          && Proc.stack_slot_class arg.(0).typ = Proc.stack_slot_class res.(0).typ then
            (* nothing will be emitted later,
               not necessary to apply constraints *)
            (arg, res)
          else
            ([| self#makereg arg.(0) |], res)
      | _ ->
          (arg, res)
      end
  | Iprobe _ ->
    (* No constraints on where the arguments reside,
       so that the presence of a probe does not affect
       register allocation of the rest of the code. *)
    (arg, res)
  | Iopaque ->
      (* arg = result, can be on stack or register *)
      assert (arg.(0).stamp = res.(0).stamp);
      (arg, res)
  | _ -> (self#makeregs arg, self#makeregs res)

method reload_test _tst args =
  self#makeregs args

method private reload i k =
  match i.desc with
    (* For function calls, returns, etc: the arguments and results are
       already at the correct position (e.g. on stack for some arguments).
       However, something needs to be done for the function pointer in
       indirect calls. *)
    Iend | Ireturn _ | Iop(Itailcall_imm _) | Iraise _ -> k i
  | Iop(Itailcall_ind) ->
      let newarg = self#makereg1 i.arg in
      k (insert_moves i.dbg i.arg newarg
           {i with arg = newarg})
  | Iop(Icall_imm _ | Iextcall _) ->
      self#reload i.next (fun next -> k {i with next; })
  | Iop(Icall_ind) ->
      let newarg = self#makereg1 i.arg in
      self#reload i.next (fun next ->
        k (insert_moves i.dbg i.arg newarg
             {i with arg = newarg; next; }))
  | Iop op ->
      let (newarg, newres) = self#reload_operation op i.arg i.res in
      self#reload i.next (fun next ->
        k (insert_moves i.dbg i.arg newarg
             {i with arg = newarg; res = newres;
                     next = (insert_moves i.dbg newres i.res next); }))
  | Iifthenelse(tst, ifso, ifnot) ->
      let newarg = self#reload_test tst i.arg in
      self#reload ifso (fun ifso ->
        self#reload ifnot (fun ifnot ->
          self#reload i.next (fun next ->
            k (insert_moves i.dbg i.arg newarg
                 (instr_cons_debug (Iifthenelse(tst, ifso, ifnot))
                    newarg [||] i.dbg next)))))
  | Iswitch(index, cases) ->
      let newarg = self#makeregs i.arg in
      let cases = Array.map (fun case -> self#reload case Fun.id) cases in
      self#reload i.next (fun next ->
        k (insert_moves i.dbg i.arg newarg
             (instr_cons_debug (Iswitch(index, cases)) newarg [||] i.dbg next)))
  | Icatch(rec_flag, ts, handlers, body) ->
      let new_handlers = List.map
          (fun (nfail, ts, handler, is_cold) -> nfail, ts, self#reload handler Fun.id, is_cold)
          handlers in
      self#reload body (fun body ->
        self#reload i.next (fun next ->
          k (instr_cons_debug (Icatch(rec_flag, ts, new_handlers, body))
               [||] [||] i.dbg next)))
  | Iexit (cont, traps) ->
      k (instr_cons_debug (Iexit (cont, traps)) [||] [||] i.dbg dummy_instr)
  | Itrywith(body, kind, (ts, handler)) ->
      self#reload body (fun body ->
        self#reload handler (fun handler ->
          self#reload i.next (fun next ->
            k (instr_cons_debug (Itrywith(body, kind, (ts, handler)))
                 [||] [||] i.dbg next))))

method fundecl f num_stack_slots =
  redo_regalloc <- false;
  let new_body = self#reload f.fun_body Fun.id in
  ({fun_name = f.fun_name; fun_args = f.fun_args;
    fun_body = new_body; fun_codegen_options = f.fun_codegen_options;
    fun_dbg  = f.fun_dbg;
    fun_poll = f.fun_poll;
    fun_contains_calls = f.fun_contains_calls;
    fun_num_stack_slots = Array.copy num_stack_slots;
   },
   redo_regalloc)
end
