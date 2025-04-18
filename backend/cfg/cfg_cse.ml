(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                                 et al.                                 *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2024--2025 Jane Street Group LLC.                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare
module DLL = Flambda_backend_utils.Doubly_linked_list
module List = ListLabels
module Array = ArrayLabels

type valnum = int

(* Classification of operations *)

type op_class = Cfg_cse_target_intf.op_class

module type Operation = sig
  type t
  (* CR-someday xclerc for xclerc: consider asking for a `compare` function. *)
end

module type S = sig
  type op

  type rhs = op * valnum array

  module Equations : sig
    module Rhs_map : Map.S with type key = rhs

    type 'a t =
      { mutable_load_equations : 'a Rhs_map.t;
        other_equations : 'a Rhs_map.t
      }
  end

  type numbering =
    { num_next : int; (* next fresh value number *)
      num_eqs : valnum array Equations.t; (* mapping rhs -> valnums *)
      num_reg : valnum Reg.Map.t
    }
  (* mapping register -> valnum *)

  val empty_numbering : numbering

  val valnum_regs : numbering -> Reg.t array -> numbering * valnum array

  val find_equation : op_class -> numbering -> rhs -> valnum array option

  val find_regs_containing : numbering -> valnum array -> Reg.t array option

  val set_known_regs : numbering -> Reg.t array -> valnum array -> numbering

  val set_move : numbering -> Reg.t -> Reg.t -> numbering

  val set_fresh_regs : numbering -> Reg.t array -> rhs -> op_class -> numbering

  val set_unknown_regs : numbering -> Reg.t array -> numbering

  val remove_mutable_load_numbering : numbering -> numbering

  val kill_addr_regs : numbering -> numbering
end

module Make (Op : Operation) : S with type op = Op.t = struct
  type op = Op.t

  (* We maintain sets of equations of the form valnums = operation(valnums) plus
     a mapping from registers to valnums (value numbers). *)

  type rhs = op * valnum array

  module Equations = struct
    module Rhs_map = Map.Make (struct
      type t = rhs

      let compare = Stdlib.compare
    end)

    type 'a t =
      { mutable_load_equations : 'a Rhs_map.t;
        other_equations : 'a Rhs_map.t
      }

    let empty =
      { mutable_load_equations = Rhs_map.empty;
        other_equations = Rhs_map.empty
      }

    let add (op_class : op_class) op v m =
      match op_class with
      | Op_load Mutable ->
        { m with
          mutable_load_equations = Rhs_map.add op v m.mutable_load_equations
        }
      | Op_pure | Op_load Immutable | Op_store _ | Op_other ->
        { m with other_equations = Rhs_map.add op v m.other_equations }

    let find (op_class : op_class) op m =
      match op_class with
      | Op_load Mutable -> Rhs_map.find op m.mutable_load_equations
      | Op_pure | Op_load Immutable | Op_store _ | Op_other ->
        Rhs_map.find op m.other_equations

    let remove_mutable_loads m =
      { mutable_load_equations = Rhs_map.empty;
        other_equations = m.other_equations
      }
  end

  type numbering =
    { num_next : int; (* next fresh value number *)
      num_eqs : valnum array Equations.t; (* mapping rhs -> valnums *)
      num_reg : valnum Reg.Map.t
    }
  (* mapping register -> valnum *)

  let empty_numbering =
    { num_next = 0; num_eqs = Equations.empty; num_reg = Reg.Map.empty }

  (** Generate a fresh value number [v] and associate it to register [r].
  Returns a pair [(n',v)] with the updated value numbering [n']. *)

  let fresh_valnum_reg n r =
    let v = n.num_next in
    { n with num_next = v + 1; num_reg = Reg.Map.add r v n.num_reg }, v

  (* Same, for a set of registers [rs]. *)

  let array_fold_transf (f : numbering -> 'a -> numbering * 'b) n (a : 'a array)
      : numbering * 'b array =
    match Array.length a with
    | 0 -> n, [||]
    | 1 ->
      let n', b = f n a.(0) in
      n', [| b |]
    | l ->
      let b = Array.make l 0 and n = ref n in
      for i = 0 to l - 1 do
        let n', x = f !n a.(i) in
        b.(i) <- x;
        n := n'
      done;
      !n, b

  let fresh_valnum_regs n rs = array_fold_transf fresh_valnum_reg n rs

  (** [valnum_reg n r] returns the value number for the contents of
  register [r].  If none exists, a fresh value number is returned
  and associated with register [r].  The possibly updated numbering
  is also returned.  [valnum_regs] is similar, but for an array of
  registers. *)

  let valnum_reg n r =
    try n, Reg.Map.find r n.num_reg with Not_found -> fresh_valnum_reg n r

  let valnum_regs n rs = array_fold_transf valnum_reg n rs

  (* Look up the set of equations for an equation with the given rhs. Return
     [Some res] if there is one, where [res] is the lhs. *)

  let find_equation op_class n rhs =
    try Some (Equations.find op_class rhs n.num_eqs) with Not_found -> None

  (* Find a register containing the given value number. *)

  let find_reg_containing n v =
    Reg.Map.fold (fun r v' res -> if v' = v then Some r else res) n.num_reg None

  (* Find a set of registers containing the given value numbers. *)

  let find_regs_containing n vs =
    match Array.length vs with
    | 0 -> Some [||]
    | 1 -> (
      match find_reg_containing n vs.(0) with
      | None -> None
      | Some r -> Some [| r |])
    | l -> (
      let rs = Array.make l Reg.dummy in
      try
        for i = 0 to l - 1 do
          match find_reg_containing n vs.(i) with
          | None -> raise Exit
          | Some r -> rs.(i) <- r
        done;
        Some rs
      with Exit -> None)

  (* Associate the given value number to the given result register, without
     adding new equations. *)

  let set_known_reg n r v = { n with num_reg = Reg.Map.add r v n.num_reg }

  (* Associate the given value numbers to the given result registers, without
     adding new equations. *)

  let set_known_regs n rs vs =
    Misc.Stdlib.Array.fold_left2 set_known_reg n rs vs

  (* Record the effect of a move: no new equations, but the result reg maps to
     the same value number as the argument reg. *)

  let set_move n src dst =
    let n1, v = valnum_reg n src in
    { n1 with num_reg = Reg.Map.add dst v n1.num_reg }

  (* Record the equation [fresh valnums = rhs] and associate the given result
     registers [rs] to [fresh valnums]. *)

  let set_fresh_regs n rs rhs op_class =
    let n1, vs = fresh_valnum_regs n rs in
    { n1 with num_eqs = Equations.add op_class rhs vs n.num_eqs }

  (* Forget everything we know about the given result registers, which are
     receiving unpredictable values at run-time. *)

  let set_unknown_regs n rs =
    { n with num_reg = Array.fold_right ~f:Reg.Map.remove rs ~init:n.num_reg }

  (* Keep only the equations satisfying the given predicate. *)

  let remove_mutable_load_numbering n =
    { n with num_eqs = Equations.remove_mutable_loads n.num_eqs }

  (* Forget everything we know about registers of type [Addr]. *)

  let kill_addr_regs n =
    { n with
      num_reg =
        Reg.Map.filter
          (fun r _n -> not (Cmm.equal_machtype_component r.Reg.typ Cmm.Addr))
          n.num_reg
    }
end

module Numbering = Make (struct
  type t = Operation.t
end)

open Numbering

let debug = false

module State : sig
  type t

  val make : last_used:InstructionId.t -> t

  val get_and_incr_instruction_id : t -> InstructionId.t
end = struct
  (* CR-soon xclerc for xclerc: factor out with the state of GI, IRC, LS. *)
  type t = { instruction_id : InstructionId.sequence }

  let make ~last_used =
    let instruction_id = InstructionId.make_sequence ~last_used () in
    { instruction_id }

  let get_and_incr_instruction_id state =
    InstructionId.get_and_incr state.instruction_id
end

let insert_single_move :
    State.t -> Reg.t -> Reg.t -> Cfg.basic Cfg.instruction DLL.cell -> unit =
 fun state src dst cell ->
  let instr = DLL.value cell in
  let move : Cfg.basic Cfg.instruction =
    { instr with
      desc = Op Move;
      arg = [| src |];
      res = [| dst |];
      id = State.get_and_incr_instruction_id state
    }
  in
  DLL.insert_after cell move

let insert_move :
    State.t ->
    Reg.t array ->
    Reg.t array ->
    Cfg.basic Cfg.instruction DLL.cell ->
    unit =
 fun state srcs dsts cell ->
  match Array.length srcs with
  | 0 -> ()
  | 1 -> insert_single_move state srcs.(0) dsts.(0) cell
  | _ ->
    let tmps = Reg.createv_like srcs in
    let insert_single_move src dst = insert_single_move state src dst cell in
    Array.iter2 tmps dsts ~f:insert_single_move;
    Array.iter2 srcs tmps ~f:insert_single_move

module Cse_generic (Target : Cfg_cse_target_intf.S) = struct
  let class_of_operation0 : Operation.t -> op_class = function
    | Move | Spill | Reload -> assert false (* treated specially *)
    | Const_int _ | Const_float32 _ | Const_float _ | Const_symbol _
    | Const_vec128 _ ->
      Op_pure
    | Opaque -> assert false (* treated specially *)
    | Stackoffset _ -> Op_other
    | Load { mutability; is_atomic; memory_chunk = _; addressing_mode = _ } ->
      (* #12173: disable CSE for atomic loads. *)
      if is_atomic
      then Op_other
      else
        Op_load
          (match mutability with Mutable -> Mutable | Immutable -> Immutable)
    | Store (_, _, asg) -> Op_store asg
    | Alloc _ | Poll -> assert false (* treated specially *)
    | Intop _ -> Op_pure
    | Intop_imm (_, _) -> Op_pure
    | Intop_atomic _ -> Op_store true
    | Floatop _ | Csel _ | Static_cast _ | Reinterpret_cast _ -> Op_pure
    | Specific _ -> Op_other
    | Name_for_debugger _ -> Op_other
    | Probe_is_enabled _ -> Op_other
    | Begin_region | End_region -> Op_other
    | Dls_get -> Op_load Mutable

  let class_of_operation op =
    match Target.class_of_operation op with
    | Class op_class -> op_class
    | Use_default -> class_of_operation0 op

  let is_cheap_operation : Operation.t -> bool = function
    | Const_int _ -> true
    | Move | Spill | Reload | Const_float32 _ | Const_float _ | Const_symbol _
    | Const_vec128 _ | Opaque | Stackoffset _ | Load _ | Store _ | Alloc _
    | Poll | Intop _
    | Intop_imm (_, _)
    | Intop_atomic _ | Floatop _ | Csel _ | Static_cast _ | Reinterpret_cast _
    | Specific _ | Name_for_debugger _ | Probe_is_enabled _ | Begin_region
    | End_region | Dls_get ->
      false

  let kill_loads (n : numbering) : numbering = remove_mutable_load_numbering n

  let cse_instruction :
      State.t -> numbering -> Cfg.basic Cfg.instruction DLL.cell -> numbering =
   fun state n cell ->
    let i = DLL.value cell in
    match i.desc with
    | Reloadretaddr | Pushtrap _ | Poptrap _ | Prologue | Stack_check _ -> n
    | Op (Move | Spill | Reload) ->
      (* For moves, we associate the same value number to the result reg as to
         the argument reg. *)
      let n1 = set_move n i.arg.(0) i.res.(0) in
      n1
    | Op Opaque ->
      (* Assume arbitrary side effects from Iopaque *)
      empty_numbering
    | Op (Alloc _) | Op Poll ->
      (* For allocations, we must avoid extending the live range of a
         pseudoregister across the allocation if this pseudoreg is a derived
         heap pointer (a pointer into the heap that does not point to the
         beginning of a Caml block). PR#6484 is an example of this situation.
         Such pseudoregs have type [Addr]. Pseudoregs with types other than
         [Addr] can be kept. Moreover, allocations and polls can trigger the
         asynchronous execution of arbitrary Caml code (finalizer, signal
         handler, context switch), which can contain non-initializing stores.
         Hence, all equations over mutable loads must be removed. *)
      let n1 = kill_addr_regs (kill_loads n) in
      let n2 = set_unknown_regs n1 i.res in
      n2
    | Op
        (( Const_int _ | Begin_region | End_region | Dls_get | Const_float32 _
         | Const_float _ | Const_symbol _ | Const_vec128 _ | Stackoffset _
         | Load _
         | Store (_, _, _)
         | Intop _
         | Intop_imm (_, _)
         | Intop_atomic _
         | Floatop (_, _)
         | Csel _ | Reinterpret_cast _ | Static_cast _ | Probe_is_enabled _
         | Specific _ | Name_for_debugger _ ) as op) -> (
      match class_of_operation op with
      | (Op_pure | Op_load _) as op_class -> (
        let n1, varg = valnum_regs n i.arg in
        let n2 = set_unknown_regs n1 (Proc.destroyed_at_basic i.desc) in
        match find_equation op_class n1 (op, varg) with
        | Some vres -> (
          (* This operation was computed earlier. *)
          (* Are there registers that hold the results computed earlier? *)
          match find_regs_containing n1 vres with
          | Some res when not (is_cheap_operation op) ->
            (* We can replace the operation with a move, provided the registers
               are stable (non-volatile). If the operation is very cheap to
               compute, e.g. an integer constant, don't bother.*)
            let n3 = set_known_regs n1 i.res vres in
            (* This is n1 above and not n2 because the move does not destroy any
               regs *)
            insert_move state res i.res cell;
            DLL.delete_curr cell;
            n3
          | _ ->
            (* We already computed the operation but lost its results. Associate
               the result registers to the result valnums of the previous
               operation. *)
            let n3 = set_known_regs n2 i.res vres in
            n3)
        | None ->
          (* This operation produces a result we haven't seen earlier. *)
          let n3 = set_fresh_regs n2 i.res (op, varg) op_class in
          n3)
      | Op_store false | Op_other ->
        (* An initializing store or an "other" operation do not invalidate any
           equations, but we do not know anything about the results. *)
        let n1 = set_unknown_regs n (Proc.destroyed_at_basic i.desc) in
        let n2 = set_unknown_regs n1 i.res in
        n2
      | Op_store true ->
        (* A non-initializing store can invalidate anything we know about prior
           mutable loads. *)
        let n1 = set_unknown_regs n (Proc.destroyed_at_basic i.desc) in
        let n2 = set_unknown_regs n1 i.res in
        let n3 = kill_loads n2 in
        n3)

  let cse_body :
      State.t -> numbering -> Cfg.basic Cfg.instruction DLL.t -> numbering =
   fun state numbering body ->
    let numbering = ref numbering in
    DLL.iter_cell body ~f:(fun (cell : Cfg.basic Cfg.instruction DLL.cell) ->
        numbering := cse_instruction state !numbering cell);
    !numbering

  let cse_terminator : numbering -> Cfg.terminator Cfg.instruction -> numbering
      =
   fun numbering terminator ->
    match terminator.desc with
    | Never -> assert false
    | Always _ | Parity_test _ | Truth_test _ | Float_test _ | Int_test _
    | Switch _ ->
      set_unknown_regs numbering (Proc.destroyed_at_terminator terminator.desc)
    | Return | Raise _ | Tailcall_self _ | Tailcall_func _ | Call_no_return _
    | Call _ | Prim _ ->
      (* For function calls and probes, we should at least forget: - equations
         involving memory loads, since the callee can perform arbitrary memory
         stores; - equations involving arithmetic operations that can produce
         [Addr]-typed derived pointers into the heap (see below for Ialloc); -
         mappings from hardware registers to value numbers, since the callee
         does not preserve these registers. That doesn't leave much usable
         information: checkbounds could be kept, but won't be usable for CSE as
         one of their arguments is always a memory load. For simplicity, we just
         forget everything. *)
      empty_numbering

  let cse_blocks : State.t -> Cfg.t -> unit =
   fun state cfg ->
    let visited = ref Label.Set.empty in
    let to_visit : (numbering * Cfg.basic_block) Queue.t = Queue.create () in
    (* For control structures, we set the numbering to empty at every join
       point, but propagate the current numbering across fork points. This is
       why blocks with zero or several predecessors and exception handlers start
       with an empty numbering: zero predecessors means we have an entry point
       (or dead code), and several predecessors means we have an entry point. *)
    Cfg.iter_blocks cfg ~f:(fun _label block ->
        let to_add =
          block.is_trap_handler
          ||
          match Label.Set.cardinal block.predecessors with
          | 0 -> true
          | 1 -> false
          | _ -> true
        in
        if to_add then Queue.add (empty_numbering, block) to_visit);
    while not (Queue.is_empty to_visit) do
      let numbering, block = Queue.take to_visit in
      if not (Label.Set.mem block.start !visited)
      then (
        if debug
        then Format.eprintf "[cse] visiting %a\n%!" Label.format block.start;
        visited := Label.Set.add block.start !visited;
        let numbering = cse_body state numbering block.body in
        let numbering = cse_terminator numbering block.terminator in
        let successor_labels =
          Cfg.successor_labels ~normal:true ~exn:false block
        in
        Label.Set.iter
          (fun successor_label ->
            let successor_block = Cfg.get_block_exn cfg successor_label in
            let to_add =
              (* This condition is defensive / redundant, but avoids thinking
                 too hard about weird corner cases like for instance an
                 unreachable loop. (That particular case would actually be fine
                 since the blocks from said loop would never be visited, meaning
                 there is no risk they would prevent the loop from
                 terminating.) *)
              (not (Label.Set.mem successor_label !visited))
              && Label.Set.cardinal successor_block.predecessors = 1
            in
            if debug
            then
              Format.eprintf "[cse] successor %a to_add=%B %d\n%!" Label.format
                successor_label to_add
                (Label.Set.cardinal successor_block.predecessors);
            if to_add then Queue.add (numbering, successor_block) to_visit)
          successor_labels)
    done;
    (* The following assertion may fail if there is an unreachable loop, since
       (as noted in the comment above), the blocks of such a loop would not be
       visited. *)
    if debug
    then assert (Label.Set.cardinal !visited = Label.Tbl.length cfg.blocks)

  let cfg_with_layout : Cfg_with_layout.t -> Cfg_with_layout.t =
   fun cfg_with_layout ->
    let cfg = Cfg_with_layout.cfg cfg_with_layout in
    (if not (List.mem ~set:cfg.fun_codegen_options Cfg.No_CSE)
    then
      let cfg_infos = Regalloc_utils.collect_cfg_infos cfg_with_layout in
      let state = State.make ~last_used:cfg_infos.max_instruction_id in
      cse_blocks state cfg);
    cfg_with_layout
end
