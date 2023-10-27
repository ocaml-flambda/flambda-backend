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

(* Transformation of Mach code into a list of pseudo-instructions. *)
open Linear

(* Cons a simple instruction (arg, res, live empty) *)

let cons_instr d n =
  { desc = d; next = n; arg = [||]; res = [||];
    dbg = Debuginfo.none; fdo = Fdo_info.none; live = Reg.Set.empty;
    (* CR mshinwell: this isn't good, but will suffice for now, since we're
       targetting Cfg anyway *)
    available_before = None; available_across = None }

(* Build an instruction with arg, res, dbg, live and availability taken from
   the given Mach.instruction *)

let copy_instr d i n =
  { desc = d; next = n;
    arg = i.Mach.arg; res = i.Mach.res;
    dbg = i.Mach.dbg; fdo = Fdo_info.none; live = i.Mach.live;
    available_before = Some i.Mach.available_before;
    available_across = i.Mach.available_across }

(*
   Label the beginning of the given instruction sequence.
   - If the sequence starts with a branch, jump over it.
   - If the sequence is the end, (tail call position), just do nothing

*)

let get_label n = match n.desc with
    Lbranch lbl -> (lbl, n)
  | Llabel { label = lbl; _ } -> (lbl, n)
  | Lend -> (-1, n)
  | _ ->
    let lbl = Cmm.new_label() in
    (* CR gyorsh: basic block sections are not supported in [linearize]. *)
    (lbl, cons_instr (Llabel { label = lbl; section_name = None; }) n)

(* Check the fallthrough label *)
let check_label n = match n.desc with
  | Lbranch lbl -> lbl
  | Llabel { label = lbl; _ } -> lbl
  | _ -> -1


(* Add pseudo-instruction Ladjust_stack_offset in front of a continuation
   to notify assembler generation about updates to the stack as a result
   of differences in exception trap depths
   and stack allocated outgoing arguments. *)

let rec adjust_stack_offset delta_bytes next =
  (* Simplify by merging and eliminating Ladjust_stack_offset instructions
     whenever possible. *)
  match next.desc with
  | Ladjust_stack_offset { delta_bytes = k } ->
    adjust_stack_offset (delta_bytes + k) next.next
  | _ ->
    if delta_bytes = 0 then next
    else cons_instr (Ladjust_stack_offset { delta_bytes }) next

let adjust_trap_depth delta_traps next =
  adjust_stack_offset (Linear.traps_to_bytes delta_traps) next

let delta_traps stack_before stack_after =
  let rec stack_depth acc stack =
    match (stack : Mach.trap_stack) with
    | Uncaught -> acc
    | Generic_trap t | Specific_trap (_, t) -> stack_depth (succ acc) t
  in
  (stack_depth 0 stack_after) - (stack_depth 0 stack_before)

(* Discard all instructions up to the next label.
   This function is to be called before adding a non-terminating
   instruction. *)

let rec discard_dead_code n =
  let adjust ~delta_bytes =
    adjust_stack_offset delta_bytes (discard_dead_code n.next)
  in
  let adjust_traps ~delta_traps =
    adjust ~delta_bytes:(Linear.traps_to_bytes delta_traps)
  in
  match n.desc with
    Lend -> n
  | Llabel _ -> n
    (* Do not discard Lpoptrap/Lpushtrap/Ladjust_stack_offset
       or Istackoffset instructions, as this may cause a stack imbalance
       later during assembler generation. Replace them
       with pseudo-instruction Ladjust_stack_offset with the corresponding
       stack offset and eliminate dead instructions after them. *)
  | Lpoptrap -> adjust_traps ~delta_traps:(-1)
  | Lpushtrap _ -> adjust_traps ~delta_traps:(+1)
  | Ladjust_stack_offset { delta_bytes } -> adjust ~delta_bytes
  | Lop(Istackoffset delta_bytes) -> adjust ~delta_bytes
  | _ -> discard_dead_code n.next

(*
   Add a branch in front of a continuation.
   Discard dead code in the continuation.
   Does not insert anything if we're just falling through
   or if we jump to dead code after the end of function (lbl=-1)
*)

let add_branch lbl n =
  if lbl >= 0 then
    let n1 = discard_dead_code n in
    match n1.desc with
    | Llabel { label = lbl1; _ } when lbl1 = lbl -> n1
    | _ -> cons_instr (Lbranch lbl) n1
  else
    discard_dead_code n

type linear_env =
  { trap_stack : Mach.trap_stack;
    (** The current trap stack *)
    exit_label : (int * label) list;
    (** Association list: exit handler -> handler label *)
  }

let initial_env =
  { trap_stack = Uncaught;
    exit_label = [];
  }

let find_exit_label env k =
  try
    List.assoc k env.exit_label
  with
  | Not_found -> Misc.fatal_error "Linearize.find_exit_label"

let is_next_catch env n = match env.exit_label with
  | (n0,_)::_  when n0=n -> true
| _ -> false

let rec add_traps env i traps =
  match traps with
  | [] -> i
  | Cmm.Pop _ :: traps ->
    add_traps env (cons_instr Lpoptrap i) traps
  | Cmm.Push handler :: traps ->
    let lbl_handler = find_exit_label env handler in
    add_traps env (cons_instr (Lpushtrap { lbl_handler; }) i) traps

let delta_traps_diff traps =
  let delta =
    List.fold_left
      (fun delta trap ->
         match trap with
         | Cmm.Pop _ -> delta - 1
         | Cmm.Push _ -> delta + 1)
      0 traps in
  -delta

(* Linearize an instruction [i]: add it in front of the continuation [n] *)
let linear i n contains_calls =
  let rec linear env i n =
    match i.Mach.desc with
      Iend -> n
    | Iop(Itailcall_ind | Itailcall_imm _ as op)
    | Iop((Iextcall { returns = false; _ }) as op) ->
        copy_instr (Lop op) i (discard_dead_code n)
    | Iop(Imove | Ireload | Ispill)
      when i.Mach.arg.(0).loc = i.Mach.res.(0).loc ->
        linear env i.Mach.next n
    | Iop((Icsel _) as op) ->
      (* CR gyorsh: this optimization can leave behind dead code
         from computing the condition and the arguments, because there
         is not dead code elimination after linearize. *)
      let len = Array.length i.Mach.arg in
      let ifso = i.Mach.arg.(len-2) in
      let ifnot = i.Mach.arg.(len-1) in
      if Reg.same_loc i.Mach.res.(0) ifso &&
         Reg.same_loc i.Mach.res.(0) ifnot
      then linear env i.Mach.next n
      else copy_instr (Lop op) i (linear env i.Mach.next n)
    | Iop((Ipoll { return_label = None; _ }) as op) ->
        (* If the poll call does not already specify where to jump to after
           the poll (the expected situation in the current implementation),
           absorb any branch after the poll call into the poll call itself.
           This, in particular, optimises polls at the back edges of loops. *)
        let n = linear env i.Mach.next n in
        let op, n =
          match n.desc with
          | Lbranch lbl ->
            Mach.Ipoll { return_label = Some lbl }, n.next
          | _ -> op, n
        in
        copy_instr (Lop op) i n
    | Iop op ->
        copy_instr (Lop op) i (linear env i.Mach.next n)
    | Ireturn traps ->
        let n = adjust_trap_depth (delta_traps_diff traps) n in
        let n1 = copy_instr Lreturn i (discard_dead_code n) in
        let n2 =
          if contains_calls
          then cons_instr Lreloadretaddr n1
          else n1
        in
        add_traps env n2 traps
    | Iifthenelse(test, ifso, ifnot) ->
      let n1 = linear env i.Mach.next n in
        begin match (ifso.Mach.desc, ifnot.Mach.desc, n1.desc) with
          Iend, _, Lbranch lbl ->
          copy_instr (Lcondbranch(test, lbl)) i (linear env ifnot n1)
        | _, Iend, Lbranch lbl ->
            copy_instr (Lcondbranch(invert_test test, lbl)) i
              (linear env ifso n1)
        | Iexit (nfail1, []), Iexit (nfail2, []), _
            when is_next_catch env nfail1 ->
            let lbl2 = find_exit_label env nfail2 in
            copy_instr
              (Lcondbranch (invert_test test, lbl2)) i (linear env ifso n1)
        | Iexit (nfail, []), _, _ ->
            let n2 = linear env ifnot n1
            and lbl = find_exit_label env nfail in
            copy_instr (Lcondbranch(test, lbl)) i n2
        | _,  Iexit (nfail, []), _ ->
            let n2 = linear env ifso n1 in
            let lbl = find_exit_label env nfail in
            copy_instr (Lcondbranch(invert_test test, lbl)) i n2
        | Iend, _, _ ->
            let (lbl_end, n2) = get_label n1 in
            copy_instr (Lcondbranch(test, lbl_end)) i (linear env ifnot n2)
        | _,  Iend, _ ->
            let (lbl_end, n2) = get_label n1 in
            copy_instr (Lcondbranch(invert_test test, lbl_end)) i
                       (linear env ifso n2)
        | _, _, _ ->
          (* Should attempt branch prediction here *)
            let (lbl_end, n2) = get_label n1 in
            let (lbl_else, nelse) = get_label (linear env ifnot n2) in
            copy_instr (Lcondbranch(invert_test test, lbl_else)) i
              (linear env ifso (add_branch lbl_end nelse))
        end
    | Iswitch(index, cases) ->
        let lbl_cases = Array.make (Array.length cases) 0 in
        let (lbl_end, n1) = get_label(linear env i.Mach.next n) in
        let n2 = ref (discard_dead_code n1) in
        for i = Array.length cases - 1 downto 0 do
          let (lbl_case, ncase) =
                  get_label(linear env cases.(i) (add_branch lbl_end !n2)) in
          lbl_cases.(i) <- lbl_case;
          n2 := discard_dead_code ncase
        done;
        (* Switches with 1 and 2 branches have been eliminated earlier.
           Here, we do something for switches with 3 branches. *)
        if Array.length index = 3 then begin
          let fallthrough_lbl = check_label !n2 in
          let find_label n =
            let lbl = lbl_cases.(index.(n)) in
            if lbl = fallthrough_lbl then None else Some lbl in
          copy_instr (Lcondbranch3(find_label 0, find_label 1, find_label 2))
                     i !n2
        end else
          copy_instr (Lswitch(Array.map (fun n -> lbl_cases.(n)) index)) i !n2
    | Icatch(_rec_flag, ts_next, handlers, body) ->
        let n0 = adjust_trap_depth (delta_traps ts_next env.trap_stack) n in
        let env_next = { env with trap_stack = ts_next; } in
        let (lbl_end, n1) = get_label(linear env_next i.Mach.next n0) in
        (* CR mshinwell for pchambart:
           1. rename "io"
           2. Make sure the test cases cover the "Iend" cases too *)
        let labels_at_entry_to_handlers = List.map (fun (_n, _ts, handler, _) ->
            match handler.Mach.desc with
            | Iend -> lbl_end
            | _ -> Cmm.new_label ())
            handlers in
        let exit_label_add = List.map2
            (fun (nfail, _ts, _, _) lbl -> (nfail, lbl))
            handlers labels_at_entry_to_handlers in
        let env = { env with exit_label = exit_label_add @ env.exit_label; } in
        let (n2, ts_n2) =
          List.fold_left2 (fun (n, ts_next) (_nfail, ts, handler, _) lbl_handler ->
            match handler.Mach.desc with
            | Iend -> n, ts_next
            | _ ->
                let delta = delta_traps ts ts_next in
                let n = adjust_trap_depth delta n in
                let env = { env with trap_stack = ts; } in
                let n =
                  cons_instr (Llabel { label = lbl_handler; section_name = None; } )
                    (linear env handler (add_branch lbl_end n))
                in
                n, ts)
            (n1, ts_next) handlers labels_at_entry_to_handlers
        in
        let n2 = adjust_trap_depth (delta_traps env.trap_stack ts_n2) n2 in
        let n3 = linear env body (add_branch lbl_end n2) in
        n3
    | Iexit (nfail, traps) ->
        let lbl = find_exit_label env nfail in
        assert (i.Mach.next.desc = Mach.Iend);
        let n1 = adjust_trap_depth (delta_traps_diff traps) n in
        add_traps env (add_branch lbl n1) traps
    | Itrywith(body, Regular, (ts, handler)) ->
        let (lbl_join, n1) = get_label (linear env i.Mach.next n) in
        assert (Mach.equal_trap_stack ts env.trap_stack);
        let (lbl_handler, n2) =
          get_label (cons_instr Lentertrap (linear env handler n1))
        in
        let env_body =
          { env with trap_stack = Mach.Generic_trap env.trap_stack; }
        in
        assert (i.Mach.arg = [| |]);
        let n3 = cons_instr (Lpushtrap { lbl_handler; })
                   (linear env_body body
                      (cons_instr
                         Lpoptrap
                         (add_branch lbl_join n2))) in
        n3
    | Itrywith(body, Delayed nfail, (ts, handler)) ->
        let (lbl_join, n1) = get_label (linear env i.Mach.next n) in
        let delta = delta_traps ts env.trap_stack in
        let n1' = adjust_trap_depth delta n1 in
        let env_handler = { env with trap_stack = ts; } in
        let (lbl_handler, n2) =
          get_label (cons_instr Lentertrap (linear env_handler handler n1'))
        in
        let n2' = adjust_trap_depth (-delta) n2 in
        let env_body =
          {env with exit_label = (nfail, lbl_handler) :: env.exit_label; }
        in
        let n3 = linear env_body body (add_branch lbl_join n2') in
        n3
    | Iraise k ->
        copy_instr (Lraise k) i (discard_dead_code n)
  in linear initial_env i n

let add_prologue first_insn prologue_required =
  (* The prologue needs to come after any [Iname_for_debugger] operations that
     refer to parameters.  (Such operations always come in a contiguous
     block, cf. [Selectgen].) *)
  let rec skip_naming_ops (insn : instruction) : label * instruction =
    match insn.desc with
    | Lop (Iname_for_debugger _) ->
      let tailrec_entry_point_label, next = skip_naming_ops insn.next in
      tailrec_entry_point_label, { insn with next; }
    | _ ->
      let tailrec_entry_point_label = Cmm.new_label () in
      let tailrec_entry_point =
        { desc = Llabel { label = tailrec_entry_point_label; section_name = None; };
          next = insn;
          arg = [| |];
          res = [| |];
          dbg = insn.dbg;
          fdo = insn.fdo;
          live = insn.live;
          available_before = None;
          available_across = None;
        }
      in
      (* We expect [Lprologue] to expand to at least one instruction---as such,
         if no prologue is required, we avoid adding the instruction here.
         The reason is subtle: an empty expansion of [Lprologue] can cause
         two labels, one either side of the [Lprologue], to point at the same
         location.  This means that we lose the property (cf. [Coalesce_labels])
         that we can check if two labels point at the same location by
         comparing them for equality.  This causes trouble when the function
         whose prologue is in question lands at the top of the object file
         and we are emitting DWARF debugging information:
           foo_code_begin:
           foo:
           .L1:
           ; empty prologue
           .L2:
           ...
         If we were to emit a location list entry from L1...L2, not realising
         that they point at the same location, then the beginning and ending
         points of the range would be both equal to each other and (relative to
         "foo_code_begin") equal to zero.  This appears to confuse objdump,
         which seemingly misinterprets the entry as an end-of-list entry
         (which is encoded with two zero words), then complaining about a
         "hole in location list" (as it ignores any remaining list entries
         after the misinterpreted entry). *)
      if prologue_required then
        let prologue =
          { desc = Lprologue;
            next = tailrec_entry_point;
            arg = [| |];
            res = [| |];
            dbg = tailrec_entry_point.dbg;
            fdo = tailrec_entry_point.fdo;
            live = Reg.Set.empty;  (* will not be used *)
            available_before = None;
            available_across = None;
          }
        in
        tailrec_entry_point_label, prologue
      else
        tailrec_entry_point_label, tailrec_entry_point
  in
  skip_naming_ops first_insn

let fundecl f =
  let fun_contains_calls = f.Mach.fun_contains_calls in
  let fun_num_stack_slots = f.Mach.fun_num_stack_slots in
  let fun_prologue_required =
    Proc.prologue_required ~fun_contains_calls ~fun_num_stack_slots in
  let fun_frame_required =
    Proc.frame_required ~fun_contains_calls ~fun_num_stack_slots in
  let fun_tailrec_entry_point_label, fun_body =
    add_prologue (linear f.Mach.fun_body end_instr fun_contains_calls)
      fun_prologue_required
  in
  { fun_name = f.Mach.fun_name;
    fun_body;
    fun_fast = not (List.mem Cmm.Reduce_code_size f.Mach.fun_codegen_options);
    fun_dbg  = f.Mach.fun_dbg;
    fun_tailrec_entry_point_label = Some fun_tailrec_entry_point_label;
    fun_contains_calls;
    fun_num_stack_slots;
    fun_frame_required;
    fun_prologue_required;
    fun_section_name = None;
  }
