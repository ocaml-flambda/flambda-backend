[@@@ocaml.warning "+a-30-40-41-42"]

module DLL = Flambda_backend_utils.Doubly_linked_list
module Instruction = Regalloc_utils.Instruction
module List = ListLabels
module Array = ArrayLabels

module CSE_utils_numbering = CSE_utils.Make (struct
  type t = Cfg.operation
end)

open CSE_utils
open CSE_utils_numbering

let debug = false

module State : sig
  type t

  val make : next_instruction_id:Instruction.id -> t

  val get_and_incr_instruction_id : t -> Instruction.id
end = struct
  (* CR-soon xclerc for xclerc: factor out with the state of GI, IRC, LS. *)
  type t = { mutable next_instruction_id : Instruction.id }

  let make ~next_instruction_id = { next_instruction_id }

  let get_and_incr_instruction_id state =
    let res = state.next_instruction_id in
    state.next_instruction_id <- succ res;
    res
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

class cse_generic =
  object (self)
    method class_of_operation : Cfg.operation -> op_class =
      function
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
            (match mutability with
            | Mutable -> Mutable
            | Immutable -> Immutable)
      | Store (_, _, asg) -> Op_store asg
      | Alloc _ | Poll -> assert false (* treated specially *)
      | Intop _ -> Op_pure
      | Intop_imm (_, _) -> Op_pure
      | Intop_atomic _ -> Op_store true
      | Floatop _ | Csel _
      | Static_cast _ | Reinterpret_cast _ ->
        Op_pure
      | Specific _ -> Op_other
      | Name_for_debugger _ -> Op_other
      | Probe_is_enabled _ -> Op_other
      | Begin_region | End_region -> Op_other
      | Dls_get -> Op_load Mutable

    method is_cheap_operation : Cfg.operation -> bool =
      function Const_int _ -> true | _ -> false
    [@@warning "-4"]

    method private kill_loads (n : numbering) : numbering =
      remove_mutable_load_numbering n

    method private cse_instruction
        : State.t ->
          numbering ->
          Cfg.basic Cfg.instruction DLL.cell ->
          numbering =
      fun state n cell ->
        let i = DLL.value cell in
        match i.desc with
        | Reloadretaddr | Pushtrap _ | Poptrap | Prologue | Stack_check _ -> n
        | Op (Move | Spill | Reload) ->
          (* For moves, we associate the same value number to the result reg as
             to the argument reg. *)
          let n1 = set_move n i.arg.(0) i.res.(0) in
          n1
        | Op Opaque ->
          (* Assume arbitrary side effects from Iopaque *)
          empty_numbering
        | Op (Alloc _) | Op Poll ->
          (* For allocations, we must avoid extending the live range of a
             pseudoregister across the allocation if this pseudoreg is a derived
             heap pointer (a pointer into the heap that does not point to the
             beginning of a Caml block). PR#6484 is an example of this
             situation. Such pseudoregs have type [Addr]. Pseudoregs with types
             other than [Addr] can be kept. Moreover, allocations and polls can
             trigger the asynchronous execution of arbitrary Caml code
             (finalizer, signal handler, context switch), which can contain
             non-initializing stores. Hence, all equations over mutable loads
             must be removed. *)
          let n1 = kill_addr_regs (self#kill_loads n) in
          let n2 = set_unknown_regs n1 i.res in
          n2
        | Op op -> (
          match self#class_of_operation op with
          | (Op_pure | Op_load _) as op_class -> (
            let n1, varg = valnum_regs n i.arg in
            let n2 = set_unknown_regs n1 (Proc.destroyed_at_basic i.desc) in
            match find_equation op_class n1 (op, varg) with
            | Some vres -> (
              (* This operation was computed earlier. *)
              (* Are there registers that hold the results computed earlier? *)
              match find_regs_containing n1 vres with
              | Some res when not (self#is_cheap_operation op) ->
                (* We can replace the operation with a move, provided the
                   registers are stable (non-volatile). If the operation is very
                   cheap to compute, e.g. an integer constant, don't bother.*)
                let n3 = set_known_regs n1 i.res vres in
                (* This is n1 above and not n2 because the move does not destroy
                   any regs *)
                insert_move state res i.res cell;
                DLL.delete_curr cell;
                n3
              | _ ->
                (* We already computed the operation but lost its results.
                   Associate the result registers to the result valnums of the
                   previous operation. *)
                let n3 = set_known_regs n2 i.res vres in
                n3)
            | None ->
              (* This operation produces a result we haven't seen earlier. *)
              let n3 = set_fresh_regs n2 i.res (op, varg) op_class in
              n3)
          | Op_store false | Op_other ->
            (* An initializing store or an "other" operation do not invalidate
               any equations, but we do not know anything about the results. *)
            let n1 = set_unknown_regs n (Proc.destroyed_at_basic i.desc) in
            let n2 = set_unknown_regs n1 i.res in
            n2
          | Op_store true ->
            (* A non-initializing store can invalidate anything we know about
               prior mutable loads. *)
            let n1 = set_unknown_regs n (Proc.destroyed_at_basic i.desc) in
            let n2 = set_unknown_regs n1 i.res in
            let n3 = self#kill_loads n2 in
            n3)
    [@@warning "-4"]

    method private cse_body
        : State.t -> numbering -> Cfg.basic Cfg.instruction DLL.t -> numbering =
      fun state numbering body ->
        let numbering = ref numbering in
        DLL.iter_cell body
          ~f:(fun (cell : Cfg.basic Cfg.instruction DLL.cell) ->
            numbering := self#cse_instruction state !numbering cell);
        !numbering

    method private cse_terminator
        : numbering -> Cfg.terminator Cfg.instruction -> numbering =
      fun numbering terminator ->
        match terminator.desc with
        | Never -> assert false
        | Always _ | Parity_test _ | Truth_test _ | Float_test _ | Int_test _
        | Switch _ ->
          set_unknown_regs numbering
            (Proc.destroyed_at_terminator terminator.desc)
        | Return | Raise _ | Tailcall_self _ | Tailcall_func _
        | Call_no_return _ | Call _ | Prim _ ->
          (* For function calls and probes, we should at least forget: -
             equations involving memory loads, since the callee can perform
             arbitrary memory stores; - equations involving arithmetic
             operations that can produce [Addr]-typed derived pointers into the
             heap (see below for Ialloc); - mappings from hardware registers to
             value numbers, since the callee does not preserve these registers.
             That doesn't leave much usable information: checkbounds could be
             kept, but won't be usable for CSE as one of their arguments is
             always a memory load. For simplicity, we just forget everything. *)
          empty_numbering
        | Specific_can_raise _ ->
          (* CR-soon xclerc for xclerc: is it too conservative? *)
          empty_numbering

    method private cse_blocks : State.t -> Cfg.t -> unit =
      fun state cfg ->
        let visited = ref Label.Set.empty in
        let to_visit : (numbering * Cfg.basic_block) Queue.t =
          Queue.create ()
        in
        (* As in CSEgen], for control structures, we set the numbering to empty
           at every join point, but propagate the current numbering across fork
           points. This is why blocks with zero or several predecessors and
           exception handlers start with an empty numbering: zero predecessors
           means we have an entry point (or dead code), and several predecessors
           means we have an entry point. *)
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
            if debug then Format.eprintf "[cse] visiting %d\n%!" block.start;
            visited := Label.Set.add block.start !visited;
            let numbering = self#cse_body state numbering block.body in
            let numbering = self#cse_terminator numbering block.terminator in
            let successor_labels =
              Cfg.successor_labels ~normal:true ~exn:false block
            in
            Label.Set.iter
              (fun successor_label ->
                let successor_block = Cfg.get_block_exn cfg successor_label in
                let to_add =
                  (* This condition is defensive / redundant, but avoids
                     thinking too hard about weird corner cases like for
                     instance an unreachable loop. (That particular case would
                     actually be fine since the blocks from said loop would
                     never be visited, meaning there is no risk they would
                     prevent the loop from terminating.) *)
                  (not (Label.Set.mem successor_label !visited))
                  && Label.Set.cardinal successor_block.predecessors = 1
                in
                if debug
                then
                  Format.eprintf "[cse] successor %d to_add=%B %d\n%!"
                    successor_label to_add
                    (Label.Set.cardinal successor_block.predecessors);
                if to_add then Queue.add (numbering, successor_block) to_visit)
              successor_labels)
        done;
        (* The following assertion may fail if there is an unreachable loop,
           since (as noted in the comment above), the blocks of such a loop
           would not be visited. *)
        if debug
        then assert (Label.Set.cardinal !visited = Label.Tbl.length cfg.blocks)

    method cfg_with_layout : Cfg_with_layout.t -> Cfg_with_layout.t =
      fun cfg_with_layout ->
        let cfg = Cfg_with_layout.cfg cfg_with_layout in
        (if not (List.mem ~set:cfg.fun_codegen_options Cfg.No_CSE)
        then
          let cfg_infos = Regalloc_utils.collect_cfg_infos cfg_with_layout in
          let state =
            State.make ~next_instruction_id:(succ cfg_infos.max_instruction_id)
          in
          self#cse_blocks state cfg);
        cfg_with_layout
  end
