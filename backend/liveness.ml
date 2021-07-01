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

(* Liveness analysis.
   Annotate mach code with the set of regs live at each point. *)

open Mach

type liveness_env =
  { at_exit : (int * Reg.Set.t) list;
    at_raise : Reg.Set.t;
    last_regular_trywith_handler : Reg.Set.t;
  }

let initial_env =
  { at_exit = [];
    at_raise = Reg.Set.empty;
    last_regular_trywith_handler = Reg.Set.empty;
  }

let find_live_at_exit env k =
  try
    List.assoc k env.at_exit
  with
  | Not_found -> Misc.fatal_error "Liveness.find_live_at_exit"

let env_from_trap_stack env ts =
  let at_raise =
    match ts with
    | Uncaught -> Reg.Set.empty
    | Generic_trap _ -> env.last_regular_trywith_handler
    | Specific_trap (nfail, _) -> find_live_at_exit env nfail
  in
  { env with at_raise; }

let rec live env i finally =
  (* finally is the set of registers live after execution of the
     instruction sequence.
     The result of the function is the set of registers live just
     before the instruction sequence.
     The instruction i is annotated by the set of registers live across
     the instruction. *)
  match i.desc with
    Iend ->
      i.live <- finally;
      finally
  | Ireturn _ | Iop(Itailcall_ind) | Iop(Itailcall_imm _) ->
      i.live <- Reg.Set.empty; (* no regs are live across *)
      Reg.set_of_array i.arg
  | Iop op ->
      let after = live env i.next finally in
      if Proc.op_is_pure op                    (* no side effects *)
      && Reg.disjoint_set_array after i.res    (* results are not used after *)
      && not (Proc.regs_are_volatile i.arg)    (* no stack-like hard reg *)
      && not (Proc.regs_are_volatile i.res)    (*            is involved *)
      then begin
        (* This operation is dead code.  Ignore its arguments. *)
        i.live <- after;
        after
      end else begin
        let across_after = Reg.diff_set_array after i.res in
        let across =
          match op with
          | Icall_ind | Icall_imm _ | Iextcall _ | Ialloc _
          | Iprobe _
          | Iintop (Icheckbound) | Iintop_imm(Icheckbound, _) ->
              (* The function call may raise an exception, branching to the
                 nearest enclosing try ... with. Similarly for bounds checks,
                 probes and allocation (for the latter: finalizers may throw
                 exceptions, as may signal handlers).
                 Hence, everything that must be live at the beginning of
                 the exception handler must also be live across this instr. *)
               Reg.Set.union across_after env.at_raise
           | _ ->
               across_after in
        i.live <- across;
        Reg.add_set_array across i.arg
      end
  | Iifthenelse(_test, ifso, ifnot) ->
      let at_join = live env i.next finally in
      let at_fork =
        Reg.Set.union (live env ifso at_join) (live env ifnot at_join)
      in
      i.live <- at_fork;
      Reg.add_set_array at_fork i.arg
  | Iswitch(_index, cases) ->
      let at_join = live env i.next finally in
      let at_fork = ref Reg.Set.empty in
      for i = 0 to Array.length cases - 1 do
        at_fork := Reg.Set.union !at_fork (live env cases.(i) at_join)
      done;
      i.live <- !at_fork;
      Reg.add_set_array !at_fork i.arg
  | Icatch(rec_flag, ts, handlers, body) ->
      let at_join = live (env_from_trap_stack env ts) i.next finally in
      let aux env (nfail, ts, handler) (nfail', before_handler) =
        assert(nfail = nfail');
        let env = env_from_trap_stack env ts in
        let before_handler' = live env handler at_join in
        nfail, Reg.Set.union before_handler before_handler'
      in
      let aux_equal (nfail, before_handler) (nfail', before_handler') =
        assert(nfail = nfail');
        Reg.Set.equal before_handler before_handler'
      in
      let rec fixpoint before_handlers =
        let env = { env with at_exit = before_handlers @ env.at_exit; } in
        let before_handlers' = List.map2 (aux env) handlers before_handlers in
        match rec_flag with
        | Cmm.Nonrecursive ->
            before_handlers'
        | Cmm.Recursive ->
            if List.for_all2 aux_equal before_handlers before_handlers'
            then before_handlers'
            else fixpoint before_handlers'
      in
      let init_state =
        List.map (fun (nfail, _ts, _handler) -> nfail, Reg.Set.empty) handlers
      in
      let before_handler = fixpoint init_state in
      (* We could use handler.live instead of Reg.Set.empty as the initial
         value but we would need to clean the live field before doing the
         analysis (to remove remnants of previous passes). *)
      let env = { env with at_exit = before_handler @ env.at_exit; } in
      let before_body = live env body at_join in
      i.live <- before_body;
      before_body
  | Iexit (nfail, _traps) ->
      let this_live = find_live_at_exit env nfail in
      i.live <- this_live ;
      this_live
  | Itrywith(body, kind, (ts, handler)) ->
      let at_join = live env i.next finally in
      let env_handler = env_from_trap_stack env ts in
      let before_handler = live env_handler handler at_join in
      let live_at_raise = Reg.Set.remove Proc.loc_exn_bucket before_handler in
      let env =
        match kind with
        | Regular ->
          { env with at_raise = live_at_raise;
                     last_regular_trywith_handler = live_at_raise;
          }
        | Delayed nfail ->
          { env with at_exit = (nfail, live_at_raise) :: env.at_exit; }
      in
      let before_body = live env body at_join in
      i.live <- before_body;
      before_body
  | Iraise _ ->
      i.live <- env.at_raise;
      Reg.add_set_array env.at_raise i.arg

let fundecl f =
  let initially_live = live initial_env f.fun_body Reg.Set.empty in
  (* Sanity check: only function parameters can be live at entrypoint *)
  let wrong_live = Reg.Set.diff initially_live (Reg.set_of_array f.fun_args) in
  if not (Reg.Set.is_empty wrong_live) then begin
    Misc.fatal_errorf "@[Liveness.fundecl:@\n%a@]"
      Printmach.regset wrong_live
  end
