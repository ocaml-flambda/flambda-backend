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

(* CR-someday xclerc for vlaviron: we should double check that this version
   of the module is indeed equivalent to upstream's version, at least if we
   keep using this code (which is somewhat unlikely since we are switching
   to the CFG pipeline). *)

(* Insertion of moves to suggest possible spilling / reloading points
   before register allocation. *)

open Reg
open Mach

(* We say that a register is "destroyed" if it is live across a construct
   that potentially destroys all physical registers: function calls or
   try...with constructs.

   The "destroyed" registers must therefore reside in the stack during
   these instructions.. We will insert spills (stores) just after they
   are defined, and reloads just before their first use following a
   "destroying" construct.

   Instructions with more live registers than actual registers also
   "destroy" registers: we mark as "destroyed" the registers live
   across the instruction that haven't been used for the longest time.
   These registers will be spilled and reloaded as described above. *)

type reload_env =
  {
    (* Association of spill registers to registers.
       This is mostly a result of the reload pass, but
       it is also used during reload. *)
    spill_env : Reg.t Reg.Map.t;

    (* Record the position of last use of registers.
       This is used during reload to choose which registers to spill. *)
    use_date : int Reg.Map.t;

    (* The current date.
       This is used during reload to set the use_date. *)
    current_date : int;

    (* A-list recording what is destroyed at if-then-else points.
       This is computed during reload for use during spill. *)
    destroyed_at_fork: (instruction * Reg.Set.t) list;

    (* Map from continuation labels to their reload set.
       This is internal to reload, and flows both ways:
       - The catch construct creates empty bindings
       - The exit constructs add their reload set to the bindings
       - The catch construct then uses the reload sets to analyze
         the handlers
       In the case of recursive handlers, since the handlers can
       update the map, a fixpoint is needed. *)
    reload_at_exit : Reg.Set.t Numbers.Int.Map.t;

    free_conts_for_handlers : Numbers.Int.Set.t Numbers.Int.Map.t;
  }

let initial_reload_env fundecl =
  { spill_env = Reg.Map.empty;
    use_date = Reg.Map.empty;
    current_date = 0;
    destroyed_at_fork = [];
    reload_at_exit = Numbers.Int.Map.empty;
    free_conts_for_handlers = Mach.free_conts_for_handlers fundecl;
  }

type reload_cache_entry =
  { before : Reg.Set.t; (* last seen input *)
    result : instruction * Reg.Set.t; (* last computed result *)
    at_exit_result : Reg.Set.t Numbers.Int.Map.t;
      (* reload_at_exit after computing the handler, for free handlers *)
  }

let reload_cache : reload_cache_entry Numbers.Int.Map.t ref =
  ref Numbers.Int.Map.empty

let cache_reload_result nfail before handler after_handler env =
  let at_exit_result =
    Numbers.Int.Map.filter (fun n _at_exit ->
        Numbers.Int.Set.mem n
          (Numbers.Int.Map.find nfail env.free_conts_for_handlers))
      env.reload_at_exit
  in
  let entry =
    { before;
      result = (handler, after_handler);
      at_exit_result; }
  in
  reload_cache := Numbers.Int.Map.add nfail entry !reload_cache

let spill_reg env r =
  try
    env, Reg.Map.find r env.spill_env
  with Not_found ->
    let spill_r = Reg.create r.typ in
    spill_r.spill <- true;
    if not (Reg.anonymous r) then spill_r.raw_name <- r.raw_name;
    { env with spill_env = Reg.Map.add r spill_r env.spill_env; },
    spill_r

let record_use env regv =
  let use_date = Array.fold_left (fun use_date r ->
      let prev_date = try Reg.Map.find r use_date with Not_found -> 0 in
      if env.current_date > prev_date then
        Reg.Map.add r env.current_date use_date
      else
        use_date)
    env.use_date regv
  in
  { env with use_date; }

(* Check if the register pressure overflows the maximum pressure allowed
   at that point. If so, spill enough registers to lower the pressure. *)

let add_superpressure_regs env op live_regs res_regs spilled =
  let max_pressure = Proc.max_register_pressure op in
  let regs = Reg.add_set_array live_regs res_regs in
  (* Compute the pressure in each register class *)
  let pressure = Array.make Proc.num_register_classes 0 in
  Reg.Set.iter
    (fun r ->
      if Reg.Set.mem r spilled then () else begin
        match r.loc with
          Stack _ -> ()
        | _ -> let c = Proc.register_class r in
               pressure.(c) <- pressure.(c) + 1
      end)
    regs;
  (* Check if pressure is exceeded for each class. *)
  let rec check_pressure cl spilled =
    if cl >= Proc.num_register_classes then
      spilled
    else if pressure.(cl) <= max_pressure.(cl) then
      check_pressure (cl+1) spilled
    else begin
      (* Find the least recently used, unspilled, unallocated, live register
         in the class *)
      let lru_date = ref 1000000 and lru_reg = ref Reg.dummy in
      Reg.Set.iter
        (fun r ->
          if Proc.register_class r = cl &&
             not (Reg.Set.mem r spilled) &&
             r.loc = Unknown
          then begin
            try
              let d = Reg.Map.find r env.use_date in
              if d < !lru_date then begin
                lru_date := d;
                lru_reg := r
              end
            with Not_found ->                 (* Should not happen *)
              ()
          end)
        live_regs;
      if !lru_reg != Reg.dummy then begin
        pressure.(cl) <- pressure.(cl) - 1;
        check_pressure cl (Reg.Set.add !lru_reg spilled)
      end else
        (* Couldn't find any spillable register, give up for this class *)
        check_pressure (cl+1) spilled
    end in
  check_pressure 0 spilled

(* First pass: insert reload instructions based on an approximation of
   what is destroyed at pressure points. *)

let add_reloads env regset i =
  Reg.Set.fold
    (fun r (env, i) ->
       let env, r' = spill_reg env r in
       env,
       instr_cons_debug (Iop Ireload) [|r'|] [|r|] i.dbg i)
    regset (env, i)

let find_reload_at_exit env k =
  try
    Numbers.Int.Map.find k env.reload_at_exit
  with
  | Not_found -> Misc.fatal_error "Spill.find_reload_at_exit"

let find_in_reload_cache nfail env =
  try
    let { before; result; at_exit_result; } =
      Numbers.Int.Map.find nfail !reload_cache
    in
    if Reg.Set.subset (find_reload_at_exit env nfail) before
    then Some (result, at_exit_result)
    else None
  with Not_found -> None

let rec reload env i before =
  let env = { env with current_date = succ env.current_date; } in
  let env = record_use env i.arg in
  let env = record_use env i.res in
  match i.desc with
    Iend ->
      (i, before, env)
  | Ireturn _ | Iop Itailcall_ind | Iop(Itailcall_imm _) ->
      let env, i =
        add_reloads env (Reg.inter_set_array before i.arg) i
      in
       (i, Reg.Set.empty, env)
  | Iop(Icall_ind | Icall_imm _ | Iextcall { alloc = true; }) ->
      (* All regs live across must be spilled *)
      let (new_next, finally, env) = reload env i.next i.live in
      let env, i =
        add_reloads env (Reg.inter_set_array before i.arg)
          (instr_cons_debug i.desc i.arg i.res i.dbg new_next)
      in
       (i, finally, env)
  | Iop op ->
      let new_before =
        (* Quick check to see if the register pressure is below the maximum *)
        if !Clflags.use_linscan ||
           (Reg.Set.cardinal i.live + Array.length i.res <=
            Proc.safe_register_pressure op)
        then before
        else add_superpressure_regs env op i.live i.res before in
      let after =
        Reg.diff_set_array (Reg.diff_set_array new_before i.arg) i.res in
      let (new_next, finally, env) = reload env i.next after in
      let env, i =
        add_reloads env (Reg.inter_set_array new_before i.arg)
          (instr_cons_debug i.desc i.arg i.res i.dbg new_next)
      in
      (i, finally, env)
  | Iifthenelse(test, ifso, ifnot) ->
      let at_fork = Reg.diff_set_array before i.arg in
      let (new_ifso, after_ifso, env_ifso) = reload env ifso at_fork in
      let env =
        { env_ifso with current_date = env.current_date; }
      in
      let (new_ifnot, after_ifnot, env_ifnot) = reload env ifnot at_fork in
      let env =
        { env_ifnot with current_date =
                     max env_ifso.current_date env_ifnot.current_date;
        }
      in
      let (new_next, finally, env) =
        reload env i.next (Reg.Set.union after_ifso after_ifnot) in
      let new_i =
        instr_cons_debug (Iifthenelse(test, new_ifso, new_ifnot))
        i.arg i.res i.dbg new_next in
      let env =
        { env with destroyed_at_fork =
                     (new_i, at_fork) :: env.destroyed_at_fork;
        }
      in
      let env, i =
        add_reloads env (Reg.inter_set_array before i.arg) new_i
      in
      (i, finally, env)
  | Iswitch(index, cases) ->
      let at_fork = Reg.diff_set_array before i.arg in
      let date_fork = env.current_date in
      let new_cases_list, env, after_cases =
        Array.fold_left (fun (new_cases_list, env, after_cases) c ->
            let date_join = env.current_date in
            let env = { env with current_date = date_fork; } in
            let (new_c, after_c, env_c) = reload env c at_fork in
            let env =
              { env_c with current_date = max date_join env_c.current_date; }
            in
            (new_c :: new_cases_list, env, Reg.Set.union after_cases after_c))
          ([], env, Reg.Set.empty) cases
      in
      let new_cases =
        Array.of_list (List.rev new_cases_list)
      in
      let (new_next, finally, env) = reload env i.next after_cases in
      let env, i =
        add_reloads env (Reg.inter_set_array before i.arg)
          (instr_cons_debug (Iswitch(index, new_cases))
             i.arg i.res i.dbg new_next)
      in
      (i, finally, env)
  | Icatch(rec_flag, ts, handlers, body) ->
      let reload_at_exit = List.fold_left
          (fun reload_at_exit (nfail, _, _, _) ->
            Numbers.Int.Map.add nfail Reg.Set.empty reload_at_exit)
          env.reload_at_exit
          handlers
      in
      let env = { env with reload_at_exit; } in
      let (new_body, after_body, env) = reload env body before in
      let rec fixpoint env_fix =
        let new_handlers, after_union, env =
          List.fold_right
            (fun (nfail, ts, handler, is_cold) (handlers, after_union, env) ->
               let handler, after_handler, env =
                 match find_in_reload_cache nfail env with
                 | None ->
                   let before_handler = find_reload_at_exit env nfail in
                   let handler, after_handler, env =
                     reload env handler before_handler
                   in
                   cache_reload_result nfail before_handler
                     handler after_handler env;
                   handler, after_handler, env
                 | Some ((handler, after_handler), at_exit_result) ->
                   let reload_at_exit =
                     Numbers.Int.Map.union (fun _nfail from_env from_result ->
                         Some (Reg.Set.union from_env from_result))
                       env.reload_at_exit
                       at_exit_result
                   in
                   handler, after_handler, { env with reload_at_exit }
               in
               ((nfail, ts, handler, is_cold) :: handlers,
                Reg.Set.union after_union after_handler,
                env))
            handlers ([], after_body, env_fix)
        in
        match rec_flag with
        | Cmm.Nonrecursive ->
            new_handlers, after_union, env
        | Cmm.Recursive ->
            let equal =
              List.for_all (fun (nfail, _ts, _handler, _) ->
                Reg.Set.equal
                  (find_reload_at_exit env_fix nfail)
                  (find_reload_at_exit env nfail))
                handlers
            in
            if equal
            then new_handlers, after_union, env
            else fixpoint env
      in
      let new_handlers, after_union, env = fixpoint env in
      let reload_at_exit =
        List.fold_left (fun reload_at_exit (nfail, _, _, _) ->
            Numbers.Int.Map.remove nfail reload_at_exit)
          env.reload_at_exit handlers
      in
      let env = { env with reload_at_exit; } in
      let (new_next, finally, env) = reload env i.next after_union in
      (instr_cons_debug
         (Icatch(rec_flag, ts, new_handlers, new_body)) i.arg i.res i.dbg
         new_next,
       finally,
       env)
  | Iexit (nfail, _traps) ->
      let set = find_reload_at_exit env nfail in
      let env =
        { env with reload_at_exit =
                     Numbers.Int.Map.add nfail (Reg.Set.union set before)
                       env.reload_at_exit;
        }
      in
      (i, Reg.Set.empty, env)
  | Itrywith(body, kind, (ts, handler)) ->
      let (new_body, after_body, env) = reload env body before in
      (* All registers live at the beginning of the handler are destroyed,
         except the exception bucket *)
      let before_handler =
        Reg.Set.remove Proc.loc_exn_bucket
                       (Reg.add_set_array handler.live handler.arg) in
      let (new_handler, after_handler, env) =
        reload env handler before_handler
      in
      let (new_next, finally, env) =
        reload env i.next (Reg.Set.union after_body after_handler) in
      (instr_cons_debug (Itrywith(new_body, kind, (ts, new_handler))) i.arg
         i.res i.dbg new_next,
       finally,
       env)
  | Iraise _ ->
      let env, i = add_reloads env (Reg.inter_set_array before i.arg) i in
      (i, Reg.Set.empty, env)

(* Second pass: add spill instructions based on what we've decided to reload.
   That is, any register that may be reloaded in the future must be spilled
   just after its definition. *)

(*
   As an optimization, if a register needs to be spilled in one branch of
   a conditional but not in the other, then we spill it late on entrance
   in the branch that needs it spilled.
   NB: This strategy is turned off in loops, as it may prevent a spill from
   being lifted up all the way out of the loop.
   NB again: This strategy is also off in switch arms
   as it generates many useless spills inside switch arms
   NB ter: is it the same thing for catch bodies ?
*)

(* CR mshinwell for pchambart: Try to test the new algorithms for dealing
   with Icatch. *)

type spill_env =
  { at_exit : (int * Reg.Set.t) list;
    at_raise : Reg.Set.t;
    last_regular_trywith_handler : Reg.Set.t;
    spill_env : Reg.t Reg.Map.t;
    destroyed_at_fork : (instruction * Reg.Set.t) list;
    loop : bool;
    arm : bool;
    catch : bool;
    free_conts_for_handlers : Numbers.Int.Set.t Numbers.Int.Map.t;
  }

let initial_env (reload_env : reload_env) =
  { at_exit = [];
    at_raise = Reg.Set.empty;
    last_regular_trywith_handler = Reg.Set.empty;
    spill_env = reload_env.spill_env;
    destroyed_at_fork = reload_env.destroyed_at_fork;
    free_conts_for_handlers = reload_env.free_conts_for_handlers;
    loop = false;
    arm = false;
    catch = false;
  }

type spill_cache_entry =
  { at_exit_restricted : (int * Reg.Set.t) list;
    after_handler : Reg.Set.t;
    result : (instruction * Reg.Set.t);
  }

let spill_cache : spill_cache_entry Numbers.Int.Map.t ref =
  ref Numbers.Int.Map.empty

let cache_spill_result nfail env after_handler handler before_handler =
  let at_exit_restricted =
    List.filter (fun (n, _at_exit) ->
        Numbers.Int.Set.mem n
          (Numbers.Int.Map.find nfail env.free_conts_for_handlers))
      env.at_exit
  in
  let entry =
    { at_exit_restricted;
      after_handler;
      result = (handler, before_handler);
    }
  in
  spill_cache := Numbers.Int.Map.add nfail entry !spill_cache

let reset_cache () =
  spill_cache := Numbers.Int.Map.empty;
  reload_cache := Numbers.Int.Map.empty

let spill_reg_no_add (env : spill_env) r =
  try Reg.Map.find r env.spill_env
  with Not_found ->
    Misc.fatal_errorf "Spill: Register %s unknown" (Reg.name r)

let find_spill_at_exit env k =
  try
    List.assoc k env.at_exit
  with
  | Not_found -> Misc.fatal_error "Spill.find_spill_at_exit"

let at_raise_from_trap_stack env ts =
  match ts with
  | Uncaught -> Reg.Set.empty
  | Specific_trap (nfail, _) -> find_spill_at_exit env nfail

let find_in_spill_cache nfail at_join env =
  try
    let { at_exit_restricted; after_handler; result; } =
      Numbers.Int.Map.find nfail !spill_cache
    in
    if Reg.Set.subset at_join after_handler
    && List.for_all (fun (n, at_exit) ->
        Reg.Set.subset (find_spill_at_exit env n) at_exit)
      at_exit_restricted
    then Some result
    else None
  with Not_found -> None

let add_spills env regset i =
  let regset = Reg.Set.elements regset in
  (* Skip over any [Iname_for_debugger] operations so that we don't put a
     spill between a move into a register and the operation naming that
     register.  (Such a situation would cause the spilled register to be
     unnamed). *)
  (* CR mshinwell: this probably needs implementing for Cfg regalloc
     (see Regalloc_split.insert_spills). *)
  let rec add_spills i =
    match i.desc with
    | Iop (Iname_for_debugger _) ->
      let next = add_spills i.next in
      { i with next; }
    | _ ->
      List.fold_left (fun i r ->
          instr_cons_debug (Iop Ispill) [|r|] [|spill_reg_no_add env r|]
            i.dbg i)
        i regset
  in
  add_spills i

let rec spill :
  type a. spill_env -> Mach.instruction -> Reg.Set.t ->
  (Mach.instruction -> Reg.Set.t -> a) -> a
  = fun env i finally k ->
  match i.desc with
    Iend ->
      k i finally
  | Ireturn _ | Iop Itailcall_ind | Iop(Itailcall_imm _) ->
      k i Reg.Set.empty
  | Iop Ireload ->
    spill env i.next finally (fun new_next after ->
      let before1 = Reg.diff_set_array after i.res in
      k (instr_cons_debug i.desc i.arg i.res i.dbg new_next)
        (Reg.add_set_array before1 i.res))
  | Iop op ->
    spill env i.next finally (fun new_next after ->
      let before1 = Reg.diff_set_array after i.res in
      let before =
        if operation_can_raise op
        then Reg.Set.union before1 env.at_raise
        else before1 in
      k (instr_cons_debug i.desc i.arg i.res i.dbg
                  (add_spills env (Reg.inter_set_array after i.res) new_next))
        before)
  | Iifthenelse(test, ifso, ifnot) ->
    spill env i.next finally (fun new_next at_join ->
    spill env ifso at_join (fun new_ifso before_ifso ->
    spill env ifnot at_join (fun new_ifnot before_ifnot ->
      if
        env.loop || env.arm || env.catch
      then
        k (instr_cons_debug (Iifthenelse(test, new_ifso, new_ifnot))
                     i.arg i.res i.dbg new_next)
          (Reg.Set.union before_ifso before_ifnot)
      else begin
        let destroyed = List.assq i env.destroyed_at_fork in
        let spill_ifso_branch =
          Reg.Set.diff (Reg.Set.diff before_ifso before_ifnot) destroyed
        and spill_ifnot_branch =
          Reg.Set.diff (Reg.Set.diff before_ifnot before_ifso) destroyed in
        k (instr_cons_debug
            (Iifthenelse(test, add_spills env spill_ifso_branch new_ifso,
                               add_spills env spill_ifnot_branch new_ifnot))
            i.arg i.res i.dbg new_next)
          (Reg.Set.diff (Reg.Set.diff (Reg.Set.union before_ifso before_ifnot)
                                    spill_ifso_branch)
                       spill_ifnot_branch)
      end)))
  | Iswitch(index, cases) ->
    spill env i.next finally (fun new_next at_join ->
      let env = { env with arm = true; } in
      let before = ref Reg.Set.empty in
      let new_cases =
        Array.map
          (fun c ->
            spill env c at_join (fun new_c before_c ->
            before := Reg.Set.union !before before_c;
            new_c))
          cases in
      k (instr_cons_debug (Iswitch(index, new_cases)) i.arg i.res i.dbg
          new_next)
        !before)
  | Icatch(rec_flag, ts, handlers, body) ->
    let next_env = { env with at_raise = at_raise_from_trap_stack env ts } in
    spill next_env i.next finally (fun new_next at_join ->
      let spill_at_exit_add at_exits = List.map2
          (fun (nfail,_,_,_) at_exit -> nfail, at_exit)
          handlers at_exits
      in
      let rec fixpoint at_exits =
        let spill_at_exit_add = spill_at_exit_add at_exits in
        let new_at_exit = spill_at_exit_add @ env.at_exit in
        let res =
          List.map
            (fun (nfail, ts, handler, _) ->
               let env =
                 { env with at_exit = new_at_exit;
                            at_raise = at_raise_from_trap_stack env ts;
                            catch = true;
                 }
               in
               match find_in_spill_cache nfail at_join env with
               | None ->
                 spill env handler at_join (fun handler before_handler ->
                   cache_spill_result nfail env at_join handler before_handler;
                   handler, before_handler)
               | Some result -> result)
            handlers
        in
        match rec_flag with
        | Cmm.Nonrecursive ->
            res
        | Cmm.Recursive ->
            let equal =
              List.for_all2
                (fun (_new_handler, new_at_exit) (_, at_exit) ->
                   Reg.Set.equal at_exit new_at_exit)
                res spill_at_exit_add in
            if equal
            then res
            else fixpoint (List.map snd res)
      in
      let res = fixpoint (List.map (fun _ -> Reg.Set.empty) handlers) in
      let spill_at_exit_add = spill_at_exit_add (List.map snd res) in
      let env_body = { env with at_exit = spill_at_exit_add @ env.at_exit; } in
      spill env_body body at_join (fun new_body before ->
      let new_handlers = List.map2
          (fun (nfail, ts, _, is_cold) (handler, _) -> nfail, ts, handler, is_cold)
          handlers res in
      k (instr_cons_debug (Icatch(rec_flag, ts, new_handlers, new_body))
         i.arg i.res i.dbg new_next)
        before))
  | Iexit (nfail, _traps) ->
      k i (find_spill_at_exit env nfail)
  | Itrywith(body, kind, (ts, handler)) ->
    spill env i.next finally (fun new_next at_join ->
      let env_handler =
        { env with at_raise = at_raise_from_trap_stack env ts; }
      in
      spill env_handler handler at_join (fun new_handler before_handler ->
      let env_body =
        match kind with
        | Delayed nfail ->
            { env with at_exit =
                         (nfail, before_handler) :: env.at_exit;
            }
      in
      spill env_body body at_join (fun new_body before_body ->
      k (instr_cons_debug (Itrywith(new_body, kind, (ts, new_handler)))
         i.arg i.res i.dbg new_next)
        before_body)))
  | Iraise _ ->
      k i env.at_raise

(* Entry point *)

let fundecl f =
  reset_cache ();
  let (body1, _, reload_env) =
    reload (initial_reload_env f) f.fun_body Reg.Set.empty
  in
  let spill_env = initial_env reload_env in
  spill spill_env body1 Reg.Set.empty (fun body2 tospill_at_entry ->
  let new_body =
    add_spills spill_env (Reg.inter_set_array tospill_at_entry f.fun_args) body2
  in
  { fun_name = f.fun_name;
    fun_args = f.fun_args;
    fun_body = new_body;
    fun_codegen_options = f.fun_codegen_options;
    fun_poll = f.fun_poll;
    fun_dbg  = f.fun_dbg;
    fun_num_stack_slots = f.fun_num_stack_slots;
    fun_contains_calls = f.fun_contains_calls;
  })
