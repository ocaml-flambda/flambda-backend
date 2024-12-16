[@@@ocaml.warning "+a-40-41-42"]

(* Finds independent scalar operations within the same basic block and tries to
   use vector operations if possible *)
(* CR gyorsh: how does the info from [reg_map] flow between blocks? *)

module DLL = Flambda_backend_utils.Doubly_linked_list

let ( << ) f g x = f (g x)

module State : sig
  type t

  type live_regs = Reg.Set.t

  val create : Format.formatter -> Cfg_with_layout.t -> t

  val next_available_instruction : t -> int

  val liveness : t -> int -> live_regs

  val dump_debug : t -> ('a, Format.formatter, unit) format -> 'a

  val dump : t -> ('a, Format.formatter, unit) format -> 'a

  val max_block_size_to_vectorize : int

  val extra_debug : bool

  val fun_name : t -> string

  val fun_dbg : t -> Debuginfo.t
end = struct
  type t =
    { ppf_dump : Format.formatter;
      mutable max_instruction_id : int;
      cfg_with_infos : Cfg_with_infos.t Lazy.t;
      cfg_with_layout : Cfg_with_layout.t
    }

  type live_regs = Reg.Set.t

  (* CR gyorsh: add a compiler flag to control this constant? *)
  let max_block_size_to_vectorize = 1000

  let fun_name t = Cfg.fun_name (Cfg_with_layout.cfg t.cfg_with_layout)

  let fun_dbg t = (Cfg_with_layout.cfg t.cfg_with_layout).fun_dbg

  let next_available_instruction t =
    let id = t.max_instruction_id + 1 in
    t.max_instruction_id <- id;
    id

  let init_instructon_max_id cl =
    (* CR gyorsh: Duplicated from backend/regalloc/regalloc_utils.ml. Should
       probably move it to Cfg or Cfg_with_infos. *)
    let max_id = ref Int.min_int in
    let update_max_id (instr : _ Cfg.instruction) : unit =
      max_id := Int.max !max_id instr.id
    in
    Cfg_with_layout.iter_instructions cl ~instruction:update_max_id
      ~terminator:update_max_id;
    !max_id

  let create ppf_dump cl =
    (* CR-someday tip: the function may someday take a cfg_with_infos instead of
       creating a new one *)
    { ppf_dump;
      max_instruction_id = init_instructon_max_id cl;
      cfg_with_layout = cl;
      cfg_with_infos = lazy (Cfg_with_infos.make cl)
    }

  let liveness t id =
    Cfg_with_infos.(liveness_find (Lazy.force t.cfg_with_infos) id).before

  let extra_debug = true

  let dump_if c t =
    if c && !Flambda_backend_flags.dump_vectorize
    then Format.fprintf t.ppf_dump
    else Format.ifprintf t.ppf_dump

  let dump_debug t = dump_if extra_debug t

  let dump t = dump_if true t
end

module Substitution : sig
  (* CR-someday gyorsh: should be factored out with
     [Regalloc_utils.Substitution]. *)
  type t

  val create : int -> t

  val get_reg_exn : t -> Reg.t -> Reg.t

  val get_reg_opt : t -> Reg.t -> Reg.t option

  (* CR gyorsh: for SSA *)
  (* (** [fresh_reg t r typ] assumes that [r] is not mapped, creates a fresh register [r'] of
   *     type [typ] and maps [r] to [r']. *)
   * val fresh_reg : t -> Reg.t -> Cmm.machtype_component -> Reg.t *)

  (** [fresh_reg_for_pack t pack typ] assumes that none of the registers in [pack] are
      mapped, creates a fresh register [r'] of type [typ] and maps all registers in [pack]
      to [r]. *)
  val fresh_reg_for_pack : t -> Reg.t list -> Cmm.machtype_component -> unit
end = struct
  type t = Reg.t Reg.Tbl.t

  let create n = Reg.Tbl.create n

  let get_reg_exn t (reg : Reg.t) = Reg.Tbl.find t reg

  let get_reg_opt t (reg : Reg.t) = Reg.Tbl.find_opt t reg

  let fresh_reg t reg machtype_component =
    match get_reg_opt t reg with
    | None ->
      let new_reg = Reg.create machtype_component in
      Reg.Tbl.add t reg new_reg;
      new_reg
    | Some old_reg ->
      Misc.fatal_errorf "register %a is already mapped to %a" Printmach.reg reg
        Printmach.reg old_reg

  let fresh_reg_for_pack t regs machtype_component =
    match regs with
    | [] ->
      Misc.fatal_error "State.fresh_reg_for_group: expects a non-empty group."
    | hd :: tl ->
      let new_reg = fresh_reg t hd machtype_component in
      let add reg =
        match get_reg_opt t reg with
        | None -> Reg.Tbl.add t reg new_reg
        | Some old_reg ->
          if Reg.same new_reg old_reg
          then
            (* same register may appear multiple times in the group, this is
               fine here but may not be desirable in the client code and should
               be checked for there. *)
            Misc.fatal_errorf
              "fresh_reg_for_group: duplicate register %a in the group"
              Printmach.reg reg
          else
            Misc.fatal_errorf
              "fresh_reg_for_group: register %a is already mapped to %a"
              Printmach.reg reg Printmach.reg old_reg
      in
      List.iter add tl
end

module Instruction : sig
  (* CR-someday tip: consider moving this to cfg or at least have something
     similar there *)
  module Id : sig
    type t

    include Identifiable.S with type t := t
  end

  type t

  val basic : Cfg.basic Cfg.instruction -> t

  val terminator : Cfg.terminator Cfg.instruction -> t

  val id : t -> Id.t

  val equal_id : t -> t -> bool

  val arguments : t -> Reg.t Array.t

  val results : t -> Reg.t Array.t

  val destroyed : t -> Reg.t Array.t

  val op : t -> Cfg.operation option

  val have_isomorphic_op : t -> t -> bool

  val stack_offset : t -> int

  val print : Format.formatter -> t -> unit

  val print_id : Format.formatter -> t -> unit

  val copy :
    Cfg.basic Cfg.instruction ->
    arg:Reg.t array ->
    res:Reg.t array ->
    id:int ->
    desc:Cfg.basic ->
    Cfg.basic Cfg.instruction
end = struct
  module Id = struct
    include Numbers.Int

    let print ppf t = Format.fprintf ppf "(id:%d)" t
  end

  type t =
    | Basic of Cfg.basic Cfg.instruction
    | Terminator of Cfg.terminator Cfg.instruction

  let basic i = Basic i

  let terminator i = Terminator i

  let id (instruction : t) : Id.t =
    match instruction with
    | Basic instruction -> instruction.id
    | Terminator instruction -> instruction.id

  let equal_id t1 t2 = Id.equal (id t1) (id t2)

  let arguments (instruction : t) : Reg.t Array.t =
    match instruction with
    | Basic instruction -> instruction.arg
    | Terminator instruction -> instruction.arg

  let results (instruction : t) : Reg.t Array.t =
    match instruction with
    | Basic instruction -> instruction.res
    | Terminator instruction -> instruction.res

  let destroyed (instruction : t) : Reg.t Array.t =
    match instruction with
    | Basic instruction -> Proc.destroyed_at_basic instruction.desc
    | Terminator instruction -> Proc.destroyed_at_terminator instruction.desc

  let stack_offset (instruction : t) : int =
    match instruction with
    | Basic instruction -> instruction.stack_offset
    | Terminator instruction -> instruction.stack_offset

  let op (instruction : t) : Cfg.operation option =
    match instruction with
    | Basic basic_instruction -> (
      let desc = basic_instruction.desc in
      match desc with
      | Op op -> Some op
      | Reloadretaddr | Pushtrap _ | Poptrap | Prologue | Stack_check _ -> None)
    | Terminator _ -> None

  let copy (i : Cfg.basic Cfg.instruction) ~arg ~res ~id ~desc =
    { i with desc; arg; res; id }

  let op_isomorphic (op1 : Cfg.operation) (op2 : Cfg.operation) =
    match op1, op2 with
    | Move, Move | Spill, Spill | Reload, Reload -> true
    | Const_int _, Const_int _
    | Const_float32 _, Const_float32 _
    | Const_float _, Const_float _
    | Const_symbol _, Const_symbol _
    | Const_vec128 _, Const_vec128 _ ->
      true
    | ( Load
          { memory_chunk = memory_chunk1;
            addressing_mode = addressing_mode1;
            mutability = mutability1;
            is_atomic = is_atomic1
          },
        Load
          { memory_chunk = memory_chunk2;
            addressing_mode = addressing_mode2;
            mutability = mutability2;
            is_atomic = is_atomic2
          } ) ->
      Cmm.equal_memory_chunk memory_chunk1 memory_chunk2
      && Arch.compare_addressing_mode_without_displ addressing_mode1
           addressing_mode2
         = 0
      && mutability1 = mutability2 && is_atomic1 = is_atomic2
    | ( Store (memory_chunk1, addressing_mode1, is_assignment1),
        Store (memory_chunk2, addressing_mode2, is_assignment2) ) ->
      Cmm.equal_memory_chunk memory_chunk1 memory_chunk2
      && Arch.compare_addressing_mode_without_displ addressing_mode1
           addressing_mode2
         = 0
      && is_assignment1 = is_assignment2
    | Intop intop1, Intop intop2 -> Mach.equal_integer_operation intop1 intop2
    | Intop_imm (intop1, _), Intop_imm (intop2, _) ->
      Mach.equal_integer_operation intop1 intop2
    | Floatop (width1, floatop1), Floatop (width2, floatop2) ->
      Mach.equal_float_width width1 width2
      && Mach.equal_float_operation floatop1 floatop2
    | Specific specific_operation1, Specific specific_operation2 ->
      Arch.equal_specific_operation specific_operation1 specific_operation2
      (* CR-soon tip: [Arch.equal_specific_operation] may return false even if
         some operations are isomorphic (ie. when they have different
         constants) *)
    | Move, _
    | Spill, _
    | Reload, _
    | Const_int _, _
    | Const_float32 _, _
    | Const_float _, _
    | Const_symbol _, _
    | Const_vec128 _, _
    | Stackoffset _, _
    | Load _, _
    | Store _, _
    | Intop _, _
    | Intop_imm _, _
    | Intop_atomic _, _
    | Floatop _, _
    | Csel _, _
    | Reinterpret_cast _, _
    | Static_cast _, _
    | Probe_is_enabled _, _
    | Opaque, _
    | Begin_region, _
    | End_region, _
    | Specific _, _
    | Name_for_debugger _, _
    | Dls_get, _
    | Poll, _
    | Alloc _, _ ->
      false

  let have_isomorphic_op instruction1 instruction2 =
    match op instruction1, op instruction2 with
    | Some op1, Some op2 -> op_isomorphic op1 op2
    | _ -> false

  let print ppf t =
    match t with
    | Basic i -> Cfg.print_basic ppf i
    | Terminator i -> Cfg.print_terminator ppf i

  let print ppf t = Format.fprintf ppf "%a %a" Id.print (id t) print t

  let print_id ppf t = Format.fprintf ppf "%a" Id.print (id t)
end

module Block : sig
  type t

  val create : Cfg.basic_block -> State.t -> t

  val body : t -> Cfg.basic_instruction_list

  val terminator : t -> Instruction.t

  (** original size of the block before vectorization  *)
  val size : t -> int

  val find : t -> Instruction.Id.t -> Instruction.t

  (** [find_last_instruction t instrs] returns instruction [i]
      from [instrs] such that [i] appears after
      all other instructions from [instrs] according to the order of instructions
      in this basic block.  Raises if [instrs] is empty. *)
  val find_last_instruction : t -> Instruction.Id.t list -> Instruction.t

  val get_live_regs_before_terminator : t -> State.live_regs

  val state : t -> State.t

  val reg_map : t -> Substitution.t

  val start : t -> Label.t

  (** [pos t id] returns the original position of [id] instruction within the body of [t].
      Raises if [id] is not in the body. *)
  val pos : t -> Instruction.Id.t -> int
end = struct
  type t =
    { block : Cfg.basic_block;
      id_to_instructions : Instruction.t Instruction.Id.Tbl.t;
      id_to_body_pos : int Instruction.Id.Tbl.t;
      size : int;
      state : State.t;
      reg_map : Substitution.t
    }

  let state t = t.state

  let body t = t.block.body

  let[@inline] terminator t = Instruction.terminator t.block.terminator

  let size t = t.size

  let start (t : t) = t.block.start

  let reg_map t = t.reg_map

  let find t id = Instruction.Id.Tbl.find t.id_to_instructions id

  let pos t id = Instruction.Id.Tbl.find t.id_to_body_pos id

  let create (block : Cfg.basic_block) state =
    let size = DLL.length block.body + 1 in
    let id_to_instructions = Instruction.Id.Tbl.create size in
    let add i =
      Instruction.Id.Tbl.add id_to_instructions (Instruction.id i) i
    in
    DLL.iter block.body ~f:(fun i -> add (Instruction.basic i));
    add (Instruction.terminator block.terminator);
    let id_to_body_pos = Instruction.Id.Tbl.create size in
    DLL.iteri block.body ~f:(fun pos i ->
        Instruction.Id.Tbl.add id_to_body_pos
          (Instruction.id (Instruction.basic i))
          pos);
    let reg_map = Substitution.create 3 in
    { block; size; id_to_instructions; id_to_body_pos; state; reg_map }

  let get_live_regs_before_terminator t =
    State.liveness t.state t.block.terminator.id

  let find_last_instruction t instructions =
    let instruction_set = Instruction.Id.Set.of_list instructions in
    let terminator = terminator t in
    if Instruction.Id.Set.mem (Instruction.id terminator) instruction_set
    then terminator
    else
      let body = t.block.body in
      let rec find_last cell_option =
        match cell_option with
        | None ->
          Misc.fatal_errorf "Vectorizer.find_last_instruction in block %d"
            t.block.start ()
        | Some cell ->
          let current_instruction = Instruction.basic (DLL.value cell) in
          let current_instruction_id = Instruction.id current_instruction in
          if Instruction.Id.Set.exists
               (Instruction.Id.equal current_instruction_id)
               instruction_set
          then current_instruction
          else find_last (DLL.prev cell)
      in
      find_last (DLL.last_cell body)
end

(* CR-someday gyorsh: Dependencies computed below can be used for other
   optimizations, not only vectorization. For example, peephole optimizations
   within a basic block can replace reorder or replace instructions that are not
   consecutive, as long as the transformation preserves dependencies. *)

(**
   Construct an overapproximation of transitive dependencies between instructions, and
   use this information to identify instructions that can run in parallel and can be
   reordered.

   Approach:
   =========

   Record a direct dependency from instruction [i] to [j] if one of the following two
   conditions holds:

   (a) Dependency via registers: [i] may read from a register [r] a value that was
   previously written to [r] by instruction [j].

   (b) Dependency via memory: [i] may read to a memory location that [j] may
   write to (RAW memory dependency).

   (c) Ordering via memory: [i] and [j] may access the same memory location,
   and one of the accesses is a "write" (this covers WAR and WAW dependencies,
   as well as dependencies of reads and writes on allocation).

   Vectorizable computation
   ========================
   TODO add citation

   The method for constructing vectorizable computations.
   - Group: a sequence of scalar instructions that are independent of each other, and have
   an equivalent vector instructions. Most groups correspond to one vector instruction,
   but sometimes a sequence of vector instructions is needed, for example scalar addition
   of a register and a constant.
   - Current heuristic for identifying vectorizable instructions requires that memory
   accesses of scalar instructions are adjacent and have the same base address and
   addressing mode.
   - Seed: a group of store instructions.
   - Vectorizable computation is set of groups of scalar instructions that can be replaced
   with equivalent vector instructions.
   - Find vectorizable computations: starting from a seed group, traverse the dependencies
   backwards to construct groups for each of the arguments. Stop at load instructions (and
   constant arguments).
   - Use (a) during the backward traversal to find scalar instruction need to form
   a group.
   - Use both (a) and (b) dependencies to determine whether scalar instructions can run in
   parallel and therefore can form a group.
   - Use (c) to determine the placement of vector instructions for each group in the
   block, relative to other instructions. Order constrains detemine when scalar
   instructions can be executed.
   - Key: conservatively choose the position to the last scalar instruction of the group
   to place the vector instructions for the group.
   - Conservatively require that all ordering constraints from a scalar instruction in a
   vectorizable computation is to instructions that appear earlier in the block,
   before the first scalar instruction of the computation.
   - Conservatively require that nothing dependes (via a,b,c)
   on scalar instructions in the computation.

   Heuristics
   ==========
   Identify disjoint memory accesses to improve precision of memory dependency and
   ordering overapproximation. Use the following observations:

   (1) A freshly-allocated memory block is disjoint from all previously-allocated blocks.

   A crude version of points-to analysis with allocation site abstraction is sufficient to
   track this information.

   (2) Different offsets from the same base address.

   If both memory accesses use the same base address, and offsets from the base address
   can statically be shown to refer to non-overlapping address intervals.

   Two memory accesses have the same base address if they use the same register for the
   base address, and the register contains the same value, i.e., there are no writes to
   this register between the two memory accesses.

   (3) Two valid ocaml blocks are disjoint, except for closure blocks.

   A register points to a valid ocaml block iff [Reg.typ] is [Val].

   For two different registers that point to valid ocaml blocks, one of the following
   conditions holds:
    - the registers point to the same block, or
    - the registers point to disjoint blocks, or
    - the registers point to closure blocks.

   To rule out closure blocks, we use the following observation.

   Closures cannot be modified after initialization. All stores to closure blocks emitted
   by the ocaml compiler are initializing stores.  Therefore, if a register that points to
   a valid ocaml block is used in an address computation of a non-initializing store, then
   the block it points to is not a closure block.

   The type system guarantees that there is no pointer arithmetic between valid ocaml
   blocks: an address computed as an offset from a register that points to a valid ocaml
   block cannot point to a different valid ocaml block, except for closure blocks.

   If two registers (a) point to valid ocaml blocks, (b) one of the blocks is not a
   closure block, and (c) a memory access uses one of the registers as base address,
   then a memory access that uses the other base address is disjoint if the accesses
   refer to non-overlapping address intervals (similarly to (2)).

   Overview of the implementation
   ==============================

   - Partition: represents a set of memory blocks that are disjoint from memory blocks
   represented by other partitions. A memory block need not be on the ocaml heap.
   - Points-to graph: for each partition, track the set of partitions it may point to.
   - Initial partition [unknown] represents all other memory blocks, previously allocated on
   the ocaml heap or elsewhere, and it may point to itself.
   - Fresh allocations: Track that a partition represents exactly one ocaml block. Such a
   partion is a result of an allocation instruction, and known to be disjoint from all
   other partitions, and initially has no valid pointers to or from other partitions.
   - Aliases: For each register, track the set of partitions it may point to.
   - Memory accesses: For each instruction, identify the set of partitions that the
   instruction may access for read or write.
   - For each partition, track the set of instructions that may read from it or write
   to it.
   - For each instruction [i] that may write to partition [p], and each instruction [j] that
   appears before [i] in the basic block and may access the same partition [p] for read
   or write, if heuristic conditions (2) and (3) above do not hold, then add a direct
   dependency from [i] to [j]. Note that it will add dependency from Load to Store.

   Computing aliases and points-to graph
   =====================================

   - Initially, all registers may point to [unknown] partition.
   - If a register is clobbered, its partition is reset to empty, representing the fact
   that it does not contain a valid pointer and it is illegal for the program to access
   memory that this register may point to.
   - If a register is assigned a static constant, its partition is reset to [unknown],
   representing the fact that a constant may point to statically allocated memory, but it
   is illegal to use it to access any partitions freshly allocated on the OCaml heap.
   - If a register is used as RHS of a store (i.e., the value to store), add edges
   between the corresponding partitions: the partitions of the LHS of the store (i.e., the
   address arguments of the store) may point to the partitions of RHS.
   - If register [r] is used as a LHS of a load (i.e., result of the load), then after the
   load [r] may point to partition [p'] if [p] is an RHS of this load (i.e., address
   arguments of the load) may point to, and [p] may point to [p'].
   - For all other assignments, propagate points-to information from arguments to results.
   - Edges between partitions can be added but not removed (i.e., no strong update
   of the points-to graph). Strong updates of aliases within a basic block are safe.

   Complexity
   ========
   - The number of partitions is n+1 where n is the number of allocation instruction in
   the block.
   - Tracking of memory accesses of each partition is quadratic in [n] in the worst case,
   but in practice should be much smaller.
   - Single pass on the block's body to construct memory dependencies.

*)
module Dependencies : sig
  (** Dependencies between basic instructions within the same basic block *)
  type t

  val from_block : Block.t -> t

  val get_direct_dependency_of_reg :
    t -> Instruction.Id.t -> Reg.t -> Instruction.Id.t option

  val get_direct_dependency_of_arg :
    t -> Instruction.Id.t -> arg_i:int -> Instruction.Id.t option

  val get_direct_dependencies : t -> Instruction.Id.t -> Instruction.Id.Set.t

  val independent : t -> Instruction.t -> Instruction.t -> bool

  (** [all_independent t l] returns true when all instructions in [l] are pairwise
      independent.  *)
  val all_independent : t -> Instruction.t list -> bool

  (** [all_adjacent t l] raises if [l] are not all memory operations. *)
  val all_adjacent : t -> Instruction.t list -> bool

  val for_all_memory_dependencies :
    t -> f:(Instruction.Id.t -> Instruction.Id.t -> bool) -> bool

  (* (** [width_in_bits t i] raises if [i] is not a memory operation or width is not known,
   *     e.g., Arbitrary or Alloc operations. *)
   * val width_in_bits : t -> Instruction.t -> int *)

  (* CR gyorsh: find a better way to pass the state around. it's mostly used for
     debug printing, so we could just pass the [ppf] around, but not only - the
     other two use is for reg_map. It's not yet needed across blocks, so need
     not be passed around. Maybe we should split printing util out of state and
     pass ppf around, then pass the state to only the places that actually need
     it. *)
  val state : t -> State.t

  module Memory : sig
    module Operation : sig
      type t

      val first_memory_arg_index : t -> int
    end
  end

  val get_memory_operation : t -> Instruction.t -> Memory.Operation.t option

  (* CR gyorsh: output assorted dependency graphs and points-to graph in dot
     format, it'll be very useful for debugging. Not clear how to control it on
     a per-block basis. *)
end = struct
  module Reaching_definitions : sig
    type t

    val from_block : Block.t -> t

    (** [get t cur_id reg] For register [r], return [id] of instruction that defines [r]
        and there is no other instruction that defines [r] or destroys/clobbers [r] on any
        path from [id] to the current program point, i.e., the program point immediately
        before [cur_id]. An instruction defines [r] means that [r] is a "result" register
        that the instruction writes to, not clobbers it.  Returns [None] if there is no
        definition of [r] in the block.  *)
    val get : t -> Instruction.Id.t -> Reg.t -> Instruction.Id.t option

    val dump : Format.formatter -> block:Block.t -> t -> unit
  end = struct
    module D = struct
      type t = Instruction.Id.t Reg.Map.t

      let get t reg = Reg.Map.find_opt reg t

      let dump ppf t =
        let open Format in
        Reg.Map.iter
          (fun reg id ->
            fprintf ppf "%a defined by instruction %a@." Printmach.reg reg
              Instruction.Id.print id)
          t
    end

    type t = D.t Instruction.Id.Tbl.t

    let from_block block =
      let t = Block.size block |> Instruction.Id.Tbl.create in
      let f map instruction =
        (* record current value *)
        let instruction = Instruction.basic instruction in
        let id = Instruction.id instruction in
        Instruction.Id.Tbl.add t id map;
        (* transform *)
        let remove map reg_array =
          Array.fold_left (fun m r -> Reg.Map.remove r m) map reg_array
        in
        let add map reg_array id =
          Array.fold_left (fun m r -> Reg.Map.add r id m) map reg_array
        in
        let map = remove map (Instruction.destroyed instruction) in
        let map = add map (Instruction.results instruction) id in
        map
      in
      let init =
        (* ignore definitions outside the block *)
        Reg.Map.empty
      in
      let map = DLL.fold_left (Block.body block) ~init ~f in
      let terminator_id = Block.terminator block |> Instruction.id in
      Instruction.Id.Tbl.add t terminator_id map;
      t

    let get t id reg =
      let d = Instruction.Id.Tbl.find t id in
      D.get d reg

    let dump ppf ~(block : Block.t) (t : t) =
      let open Format in
      DLL.iter (Block.body block) ~f:(fun instruction ->
          let instruction = Instruction.basic instruction in
          let id = Instruction.id instruction in
          fprintf ppf "Reaching definitions after instruction %a:@.%a@."
            Instruction.Id.print id D.dump
            (Instruction.Id.Tbl.find t id))
  end

  (* [Reaching_definitions] wouldn't be needed here if we had SSA
     representation. Coverting to SSA even at a basic block level would require
     some change in emit because instruction selection (a) relies on sharing of
     registers between arguments and results to emit shorter encodings of
     instructions such as Add and Shift, (b) forces the use of certain hardware
     registers, for example Div and Bswap. *)
  module Reg_defined_at_instruction : sig
    type t

    val create : Reg.t -> Instruction.t -> Reaching_definitions.t -> t

    val equal : t -> t -> bool

    val is_val : t -> bool

    val get_offset : t -> t -> Block.t -> Reaching_definitions.t -> int option

    val print : Format.formatter -> t -> unit
  end = struct
    type t =
      { reg : Reg.t;
        def : Instruction.Id.t option
      }

    let print ppf t =
      let pp ppf def =
        match def with
        | None -> Format.fprintf ppf "(unknown)"
        | Some id -> Instruction.Id.print ppf id
      in
      Format.fprintf ppf "%a at %a" Printmach.reg t.reg pp t.def

    let init reg id reaching_definitions =
      let def = Reaching_definitions.get reaching_definitions id reg in
      { reg; def }

    let create reg instruction reaching_definitions =
      let id = Instruction.id instruction in
      init reg id reaching_definitions

    let equal { reg = r1; def = d1 } { reg = r2; def = d2 } =
      Reg.same r1 r2 && Option.equal Instruction.Id.equal d1 d2

    let is_val t = Cmm.is_val t.reg.typ

    let get_offset t1 t2 block reaching_definitions =
      (* Heuristic to identify some very simple relations of the form [t2 = t1 +
         N]. This heuristic is relatively cheap, because it only follows a
         single use-def chain, backwards from [t2] until it finds [t1], and
         returns the accumulated offset N such that [t2 = t1 + N]. *)
      let add n acc =
        (* [acc = None] indicates that the accumulator is not initialized. *)
        match acc with None -> Some n | Some n' -> Some (n + n')
      in
      let rec loop ~cur ~dst acc =
        (* If [cur] does not depend on [dst], or an operation is encountered
           that cannot be expressed as a constant offset, return None *)
        if equal cur dst
        then add 0 acc
        else
          match cur.def with
          | None ->
            (* [cur] defined outside the block, so it is not defined as an
               offset of [dst]. *)
            None
          | Some id -> (
            let next =
              let instruction = Block.find block id in
              match Instruction.arguments instruction with
              | [| reg |] -> (
                match Instruction.op instruction with
                | None -> None
                | Some op -> (
                  match[@warning "-fragile-match"] op with
                  | Move | Spill | Reload -> Some (reg, 0)
                  | Intop_imm (Iadd, n) -> Some (reg, n)
                  | Intop_imm (Isub, n) -> Some (reg, -n)
                  | _ -> None))
              | _ -> None
            in
            match next with
            | None -> None
            | Some (reg, n) ->
              let acc = add n acc in
              let cur = init reg id reaching_definitions in
              loop ~cur ~dst acc)
      in
      (* CR-someday gyorsh: add the symmetric loop r with a negative offset? *)
      (* The backward traversal starts from [t2]. *)
      loop ~cur:t2 ~dst:t1 None
  end

  (* CR-someday gyorsh: [Memory] can be merged with [Reaching_definitions] but
     let's do it separately first for simplicity. *)
  module Memory : sig
    type t

    module Operation : sig
      type t

      val first_memory_arg_index : t -> int
    end

    module Dependencies : sig
      type t

      val get : t -> Instruction.Id.t -> Instruction.Id.Set.t

      val for_all :
        t -> f:(Instruction.Id.t -> Instruction.Id.t -> bool) -> bool

      val dump : Format.formatter -> block:Block.t -> t -> unit
    end

    val from_block : Block.t -> Reaching_definitions.t -> t * Dependencies.t

    (* val is_memory_operation : t -> Instruction.t -> bool *)

    val get_memory_operation : t -> Instruction.t -> Operation.t option

    val is_adjacent :
      t ->
      Instruction.t ->
      Instruction.t ->
      Block.t ->
      Reaching_definitions.t ->
      bool

    val dump : Format.formatter -> block:Block.t -> t -> unit
  end = struct
    module Partition : sig
      include Identifiable.S

      (** Abstraction of all previously allocated blocks of memory.  *)
      val unknown : t

      (** Represents a block of memory that was freshly allocated at the
          [allocation_site]. *)
      val create : allocation_site:Instruction.Id.t -> t

      (* (** [init] is the singleton set of [unknown] partition.  *)
       * val init : Set.t *)
      module Set : sig
        include module type of Set

        val unknown : t
      end
    end = struct
      module S = struct
        type t = Instruction.Id.t option

        let compare t1 t2 = Option.compare Instruction.Id.compare t1 t2

        let equal t1 t2 = compare t1 t2 = 0

        let hash t =
          match t with None -> 0 | Some id -> Instruction.Id.hash id

        let print ppf t =
          match t with
          | None -> Format.fprintf ppf "unknown"
          | Some id ->
            Format.fprintf ppf "alloc_site=%a" Instruction.Id.print id

        let output oc t = Printf.fprintf oc "%s" (Format.asprintf "%a" print t)
      end

      include S
      include Identifiable.Make (S)

      let unknown = None

      let create ~allocation_site = Some allocation_site

      module Set = struct
        include Set

        let unknown = singleton unknown
      end
    end

    module Memory_access = Arch.Memory_access

    module Operation : sig
      type t

      (* Reaching_definitions is used here to disambiguate register names. It
         wouldn't be needed here if we had SSA representation. *)
      val create : Instruction.t -> Reaching_definitions.t -> t option

      val desc : t -> Memory_access.desc

      val address_args : t -> Reg.t array

      val non_address_args : t -> Reg.t array

      val first_memory_arg_index : t -> int

      val get_instruction_id : t -> Instruction.Id.t

      (** [is_adjacent t1 t2] assumes that [t1] and [t2] have isomorphic operations,
          and conservatively returns [false] unless it can prove that [t1] and [t2]
          are adjacent (i.e., the accesses are disjoint but the intervals of addresses
          have no gap between them). *)
      val is_adjacent : t -> t -> Block.t -> Reaching_definitions.t -> bool

      (** [is_disjoint t1 t2] assumes that [t1] and [t2] have isomorphic operations,
          and conservatively returns [false] unless it can prove that [t1] and [t2]
          access disjoint memory addresses.   *)
      val is_disjoint : t -> t -> Block.t -> Reaching_definitions.t -> bool

      val dump : Format.formatter -> t -> unit
    end = struct
      type t =
        { memory_access : Memory_access.t;
          instruction : Instruction.t;
          (* CR-soon gyorsh: the instruction is only for validation and
             debugging. We don't actually need to keep the little subarrays of
             args around, we can define iterators on the original arguments
             array instead, at least for all current uses. Not worth optimizing
             these little allocations at this point. [address_args] and
             [address_args_defined_at] also have redundant information. *)
          address_args_defined_at : Reg_defined_at_instruction.t array;
          address_args : Reg.t array;
          non_address_args : Reg.t array
        }

      let desc t = Memory_access.desc t.memory_access

      let non_address_args t = t.non_address_args

      let address_args t = t.address_args

      let first_memory_arg_index t =
        Memory_access.first_memory_arg_index t.memory_access

      let get_instruction_id t = Instruction.id t.instruction

      let memory_access (instruction : Instruction.t) : Memory_access.t option =
        match Instruction.op instruction with
        | None ->
          (* conservative *)
          Memory_access.create Arbitrary
        | Some op -> (
          match op with
          | Load { memory_chunk; addressing_mode; mutability; is_atomic } ->
            let desc =
              Memory_access.Read
                { width_in_bits = Cmm.width_in_bits memory_chunk;
                  addressing_mode;
                  is_mutable =
                    (match mutability with
                    | Mutable -> true
                    | Immutable -> false);
                  is_atomic
                }
            in
            Memory_access.create ~first_memory_arg_index:0 desc
          | Store (memory_chunk, addressing_mode, is_assign) ->
            let desc =
              Memory_access.Write
                { width_in_bits = Cmm.width_in_bits memory_chunk;
                  addressing_mode;
                  init_or_assign =
                    (if is_assign
                    then Memory_access.Init_or_assign.Assignment
                    else Memory_access.Init_or_assign.Initialization)
                }
            in
            Memory_access.create desc ~first_memory_arg_index:1
          | Intop_atomic { op; size; addr } ->
            let desc =
              Memory_access.Read_and_write
                { width_in_bits = Cmm.atomic_bitwidth_to_bits size;
                  addressing_mode = addr;
                  is_atomic = true
                }
            in
            let first_memory_arg_index =
              match op with Compare_and_swap -> 2 | Fetch_and_add -> 1
            in
            Memory_access.create ~first_memory_arg_index desc
          | Specific s -> Memory_access.of_specific_operation s
          | Begin_region | End_region ->
            (* conservative, don't reorder around region begin/end. *)
            Memory_access.create Arbitrary
          | Name_for_debugger _ | Dls_get | Poll | Opaque | Probe_is_enabled _
            ->
            (* conservative, don't reorder around this instruction. *)
            Memory_access.create Arbitrary
          | Spill | Reload ->
            Misc.fatal_error
              "Unexpected instruction Spill or Reload during vectorize"
          | Move | Reinterpret_cast _ | Static_cast _ | Const_int _
          | Const_float32 _ | Const_float _ | Const_symbol _ | Const_vec128 _
          | Stackoffset _ | Intop _ | Intop_imm _ | Floatop _ | Csel _ | Alloc _
            ->
            None)

      let create (instruction : Instruction.t) reaching_definitions : t option =
        match memory_access instruction with
        | None -> None
        | Some memory_access ->
          let arguments = Instruction.arguments instruction in
          let address_args, non_address_args =
            match Memory_access.first_memory_arg_index memory_access with
            | 0 ->
              (* avoid copy *)
              arguments, [||]
            | i ->
              ( Array.sub arguments i (Array.length arguments - i),
                Array.sub arguments 0 i )
          in
          let address_args_defined_at =
            Array.map
              (fun reg ->
                Reg_defined_at_instruction.create reg instruction
                  reaching_definitions)
              address_args
          in
          Some
            { memory_access;
              instruction;
              address_args;
              address_args_defined_at;
              non_address_args
            }

      let to_bits bytes = bytes * 8

      let get_addressing_mode t =
        match Memory_access.desc t.memory_access with
        | Read { addressing_mode; _ }
        | Write { addressing_mode; _ }
        | Read_and_write { addressing_mode; _ } ->
          Some addressing_mode
        | Arbitrary | Alloc -> None

      let get_width_in_bits t =
        match Memory_access.desc t.memory_access with
        | Read { width_in_bits; _ }
        | Write { width_in_bits; _ }
        | Read_and_write { width_in_bits; _ } ->
          Some width_in_bits
        | Arbitrary | Alloc -> None

      let offset_in_bytes ~arg_offset_in_bytes (t1 : t) (t2 : t) =
        match get_addressing_mode t1, get_addressing_mode t2 with
        | Some addressing_mode_1, Some addressing_mode_2 ->
          Arch.addressing_offset_in_bytes addressing_mode_1 addressing_mode_2
            t1.address_args_defined_at t2.address_args_defined_at
            ~arg_offset_in_bytes
        | (Some _ | None), _ -> None

      let actual_arg_offset_in_bytes block reaching_definitions r1 r2 =
        (* Heuristic for handling array accesses where the scale register is not
           the same, but a simple equation. *)
        Reg_defined_at_instruction.get_offset r1 r2 block reaching_definitions

      (* CR-someday gyorsh: this can be extended in the future to handle
         "scatter-gather" accesses. *)
      let is_adjacent (t1 : t) (t2 : t) block reaching_definitions =
        let arg_offset_in_bytes =
          actual_arg_offset_in_bytes block reaching_definitions
        in
        assert (Instruction.have_isomorphic_op t1.instruction t2.instruction);
        match offset_in_bytes t1 t2 ~arg_offset_in_bytes with
        | None -> false
        | Some offset_in_bytes ->
          to_bits offset_in_bytes = (get_width_in_bits t1 |> Option.get)

      (** [is_before t1 t2] returns true if we can prove that t1 and t2 are disjoint
          intervals within the same block, and t1 is before t2. *)
      let is_before ~arg_offset_in_bytes t1 t2 block =
        let res =
          match offset_in_bytes ~arg_offset_in_bytes t1 t2 with
          | None -> false
          | Some offset_in_bytes ->
            State.dump_debug (Block.state block)
              "offset_in_bytes = %d, get_width_in_bits t1 = %d \n"
              (to_bits offset_in_bytes)
              (get_width_in_bits t1 |> Option.get);
            to_bits offset_in_bytes >= (get_width_in_bits t1 |> Option.get)
        in
        State.dump_debug (Block.state block) "is_before (%a) (%a) = %b \n"
          Instruction.print t1.instruction Instruction.print t2.instruction res;
        res

      (* [is_adjacent] implies [is_disjoint
         ~arg_offset_in_bytes:actual_arg_offset_in_bytes] *)
      let is_disjoint ~arg_offset_in_bytes t1 t2 block =
        let res =
          is_before ~arg_offset_in_bytes t1 t2 block
          || is_before ~arg_offset_in_bytes t2 t1 block
        in
        State.dump_debug (Block.state block) "is_disjoint (%a) (%a) = %b \n"
          Instruction.print t1.instruction Instruction.print t2.instruction res;
        res

      let maybe_closure_block t =
        (* Closure blocks are not mutated after initialization. *)
        match Memory_access.desc t.memory_access with
        | Read { is_mutable = true; _ } ->
          (* CR gyorsh: is it sound to return [false] here? Are reads from
             closure emitted as immutable? Can immutability of closure blocks be
             lost through optimizations such as CSE in the middle end? *)
          false
        | Read { is_mutable = false; _ } -> true
        | Read_and_write _ -> false
        | Write { init_or_assign = Assignment; _ } -> false
        | Write { init_or_assign = Initialization; _ } -> true
        | Arbitrary -> true
        | Alloc -> true

      let points_to_ocaml_block t block =
        let val_args =
          t.address_args_defined_at |> Array.to_seq
          |> Seq.filter Reg_defined_at_instruction.is_val
          |> List.of_seq
        in
        State.dump_debug (Block.state block)
          "points_to_ocaml_block: %a val_args = %a\n" Instruction.print
          t.instruction
          (Format.pp_print_list Reg_defined_at_instruction.print)
          val_args;
        match val_args with [base] -> Some base | [] | _ -> None

      let points_to_ocaml_block_not_closure_block t block =
        if maybe_closure_block t then None else points_to_ocaml_block t block

      let is_disjoint t1 t2 block reaching_definitions =
        let equiv () =
          (* heuristic to detect that either (a) r1 and r2 point to the same
             ocaml block, or (b) r1 and r2 point to disjoint ocaml blocks, but
             not to partially overlapping. *)
          match
            ( points_to_ocaml_block_not_closure_block t1 block,
              points_to_ocaml_block_not_closure_block t2 block )
          with
          | Some base1, Some base2 ->
            State.dump_debug (Block.state block)
              "Found equiv: base 1 = %a, base2 = %a\n"
              Reg_defined_at_instruction.print base1
              Reg_defined_at_instruction.print base2;
            Some (base1, base2)
          | (Some _ | None), _ ->
            State.dump_debug (Block.state block)
              "Not found equiv for (%a) and (%a)\n" Instruction.print_id
              t1.instruction Instruction.print_id t2.instruction;
            None
        in
        let actual_arg_offset_in_bytes r1 r2 =
          actual_arg_offset_in_bytes block reaching_definitions r1 r2
        in
        let arg_offset_in_bytes r1 r2 =
          State.dump_debug (Block.state block) "arg_offset_in_bytes (%a) (%a)\n"
            Reg_defined_at_instruction.print r1 Reg_defined_at_instruction.print
            r2;
          match actual_arg_offset_in_bytes r1 r2 with
          | Some _ as res -> res
          | None -> (
            match equiv () with
            | None -> None
            | Some (base1, base2) ->
              if Reg_defined_at_instruction.equal base1 r1
                 && Reg_defined_at_instruction.equal base2 r2
                 || Reg_defined_at_instruction.equal base1 r2
                    && Reg_defined_at_instruction.equal base2 r1
              then (
                (* pretend that the registers r1 and r2 are the same. *)
                State.dump_debug (Block.state block)
                  "arg_offset_in_bytes (%a) (%a) = Some 0\n"
                  Reg_defined_at_instruction.print r1
                  Reg_defined_at_instruction.print r2;
                Some 0)
              else None)
        in
        is_disjoint ~arg_offset_in_bytes t1 t2 block

      let dump_desc ppf (t : t) =
        let open Format in
        let print_bool msg ppf b =
          if b then fprintf ppf " (%s=true)" msg else ()
        in
        let print_addr ppf addr =
          Arch.print_addressing Printmach.reg addr ppf t.address_args
        in
        let pr (init_or_assign : Memory_access.Init_or_assign.t) =
          match init_or_assign with
          | Initialization -> "init"
          | Assignment -> "assign"
        in
        match desc t with
        | Alloc -> fprintf ppf "Alloc"
        | Arbitrary -> fprintf ppf "Arbitrary"
        | Read { width_in_bits; addressing_mode; is_mutable; is_atomic } ->
          fprintf ppf "Read%d [%a]%a%a" width_in_bits print_addr addressing_mode
            (print_bool "is_mutable") is_mutable (print_bool "is_atomic")
            is_atomic
        | Write { width_in_bits; addressing_mode; init_or_assign } ->
          fprintf ppf "Write%d [%a] %s" width_in_bits print_addr addressing_mode
            (pr init_or_assign)
        | Read_and_write { width_in_bits; addressing_mode; is_atomic } ->
          fprintf ppf "Read_and_write%d [%a]%a" width_in_bits print_addr
            addressing_mode (print_bool "is_atomic") is_atomic

      let dump ppf t =
        let dump_index =
          let ind = first_memory_arg_index t in
          if Int.equal ind 0
          then ""
          else ", first_memory_arg_index=" ^ string_of_int ind
        in
        Format.fprintf ppf "%a%s\n" dump_desc t dump_index
    end

    module Partitions : sig
      (** Graph where nodes represent disjoint memory areas (partitions) and an edge from
          partition A to B means that A may point to B (i.e., a memory location in A may
          contain the address of a memory location in B). *)
      type t

      val init : t

      val add_node : t -> Partition.t -> t

      val add_edges : t -> src:Partition.Set.t -> dst:Partition.Set.t -> t

      val get_successors : t -> Partition.Set.t -> Partition.Set.t

      val all : t -> Partition.Set.t

      (* val fold : f:(Partition.t -> 'a -> 'a) -> init:'a -> t -> 'a *)
    end = struct
      type t = Partition.Set.t Partition.Map.t

      let init =
        (* [unknown] partition may point to itself *)
        Partition.Map.singleton Partition.unknown Partition.Set.unknown

      let add_node t p = Partition.Map.add p Partition.Set.empty t

      let add_edges t ~src ~dst =
        Partition.Set.fold
          (fun src_p acc ->
            let old = Partition.Map.find src_p acc in
            Partition.Map.add src_p (Partition.Set.union dst old) acc)
          src t

      let get_successors t set =
        let init = Partition.Set.empty in
        Partition.Set.fold
          (fun src acc ->
            let dst = Partition.Map.find src t in
            Partition.Set.union dst acc)
          set init

      let all t =
        t |> Partition.Map.to_seq |> Seq.map fst |> Partition.Set.of_seq

      (* let fold ~f ~init t = Partition.Map.fold (fun p _ acc -> f p acc) t
         init *)
    end

    module Accesses : sig
      type t

      val empty : t

      val add : t -> Partition.t -> Operation.t -> t

      val add_all : t -> Partition.Set.t -> Operation.t -> t

      val fold :
        f:(Partition.t -> Operation.t list -> 'a -> 'a) -> init:'a -> t -> 'a
    end = struct
      (** The order of operations in the list is the reverse of the order they appear in the
          block (relative to each other). *)
      type t = Operation.t list Partition.Map.t

      let empty = Partition.Map.empty

      let get t p =
        match Partition.Map.find_opt p t with None -> [] | Some s -> s

      let add t p op = Partition.Map.add p (op :: get t p) t

      let add_all t set op =
        Partition.Set.fold (fun p acc -> add acc p op) set t

      let fold ~f ~init t = Partition.Map.fold f t init
    end

    module Aliases : sig
      type t

      val empty : t

      val replace_regs : Reg.t array -> Partition.Set.t -> t -> t

      val remove_regs : Reg.t array -> t -> t

      val get_regs : Reg.t array -> t -> Partition.Set.t
    end = struct
      (** for each p, a set partions that p may point to, including p itself explicitly. *)
      type t = Partition.Set.t Reg.Map.t

      let empty = Reg.Map.empty

      let find t reg =
        match Reg.Map.find_opt reg t with
        | None ->
          (* reg is implicitly associated with [Partition.unknown] *)
          Partition.Set.unknown
        | Some old_partitions -> old_partitions

      let get_regs regs t =
        Array.fold_left
          (fun set reg ->
            let p = find t reg in
            Partition.Set.union set p)
          Partition.Set.empty regs

      let replace reg partitions t = Reg.Map.add reg partitions t

      let replace_regs regs partitions t =
        Array.fold_left (fun acc reg -> replace reg partitions acc) t regs

      let remove_regs regs t = replace_regs regs Partition.Set.empty t
    end

    module Points_to : sig
      type t

      val from_block : Block.t -> Reaching_definitions.t -> t

      (* val transform : t -> Instruction.t -> t *)
      (* val partitions : t -> Partitions.t *)
      val operations : t -> Operation.t Instruction.Id.Map.t

      val accesses : t -> Accesses.t
    end = struct
      (* CR gyorsh: not sure if the extra layer of abstraction (Aliases and
         Partitions modules) is useful. *)
      type t =
        { partitions : Partitions.t;
              (** For each partition, the set of partitions it may point. *)
          aliases : Aliases.t;
              (** For each register, the set of partitions it may point to. *)
          accesses : Accesses.t;
              (** For each partition, the list of instructions that may access it. *)
          operations : Operation.t Instruction.Id.Map.t
              (** Mapping from instruction id to the corresponding memory operations. Instructions
              that do no access memory are not in the map. *)
        }

      let init =
        { partitions = Partitions.init;
          aliases = Aliases.empty;
          accesses = Accesses.empty;
          operations = Instruction.Id.Map.empty
        }

      (* let partitions t = t.partitions *)

      let accesses t = t.accesses

      let operations t = t.operations

      let update_aliases t instruction result_may_point_to_partitions =
        let aliases =
          (* Kill: set the points-to of clobbered registers to empty because
             using these registers is illegal. Gen: set the points-to of
             results. *)
          t.aliases
          |> Aliases.remove_regs (Instruction.destroyed instruction)
          |> Aliases.replace_regs
               (Instruction.results instruction)
               result_may_point_to_partitions
        in
        { t with aliases }

      let update t may_access_partitions ~is_atomic instruction op =
        let may_access_partitions =
          if is_atomic
          then
            (* Should not be reordered with respect to any other
               instructions. *)
            Partitions.all t.partitions
          else may_access_partitions
        in
        let accesses = Accesses.add_all t.accesses may_access_partitions op in
        let result_may_point_to_partitions =
          Partitions.get_successors t.partitions may_access_partitions
        in
        let t = { t with accesses } in
        update_aliases t instruction result_may_point_to_partitions

      let transform_write t ~is_atomic ~may_access_any_partition instruction op
          =
        let addr_args = Operation.address_args op in
        let may_access_partitions =
          if may_access_any_partition
          then Partitions.all t.partitions
          else Aliases.get_regs addr_args t.aliases
        in
        let value_to_store = Operation.non_address_args op in
        let may_point_to = Aliases.get_regs value_to_store t.aliases in
        let partitions =
          Partitions.add_edges t.partitions ~src:may_access_partitions
            ~dst:may_point_to
        in
        let t = { t with partitions } in
        update t may_access_partitions ~is_atomic instruction op

      let transform t instruction ~reaching_definitions =
        match Operation.create instruction reaching_definitions with
        | None ->
          (* propagates from args to res *)
          let args_may_point_to_partitions =
            Aliases.get_regs (Instruction.arguments instruction) t.aliases
          in
          update_aliases t instruction args_may_point_to_partitions
        | Some op -> (
          let id = Instruction.id instruction in
          let t =
            { t with operations = Instruction.Id.Map.add id op t.operations }
          in
          match Operation.desc op with
          | Alloc ->
            let fresh_partition = Partition.create ~allocation_site:id in
            let t =
              { t with
                partitions = Partitions.add_node t.partitions fresh_partition;
                accesses = Accesses.add t.accesses fresh_partition op
              }
            in
            update_aliases t instruction
              (Partition.Set.singleton fresh_partition)
          | Read { is_atomic; _ } ->
            assert (Array.length (Operation.non_address_args op) = 0);
            let addr_args = Operation.address_args op in
            let may_access_partitions = Aliases.get_regs addr_args t.aliases in
            update t may_access_partitions ~is_atomic instruction op
          | Write _ ->
            assert (Array.length (Instruction.results instruction) = 0);
            transform_write t ~is_atomic:false ~may_access_any_partition:false
              instruction op
          | Read_and_write { is_atomic; _ } ->
            transform_write t ~is_atomic ~may_access_any_partition:false
              instruction op
          | Arbitrary ->
            transform_write t ~is_atomic:true ~may_access_any_partition:true
              instruction op)

      let from_block block reaching_definitions =
        DLL.fold_left (Block.body block) ~init ~f:(fun acc i ->
            transform acc (Instruction.basic i) ~reaching_definitions)
    end

    (* CR-someday gyorsh: Merge with the dependency graph for regs. Both can be
       computed alongside reaching definitions and points_to. *)
    module Dependencies : sig
      type t

      val from_block : Accesses.t -> Block.t -> Reaching_definitions.t -> t

      val get : t -> Instruction.Id.t -> Instruction.Id.Set.t

      val for_all :
        t -> f:(Instruction.Id.t -> Instruction.Id.t -> bool) -> bool

      val dump : Format.formatter -> block:Block.t -> t -> unit
    end = struct
      (** [i] is mapped to [j] if [i] may directly depend on [j] via memory, i.e., [i] may
          read and [j] may write the same memory location. *)
      type t = Instruction.Id.Set.t Instruction.Id.Map.t

      let get t id =
        let default = Instruction.Id.Set.empty in
        Instruction.Id.Map.find_opt id t |> Option.value ~default

      let for_all t ~f =
        Instruction.Id.Map.for_all
          (fun id dsts -> Instruction.Id.Set.for_all (f id) dsts)
          t

      type dependency_kind =
        | No_direct_dependency
        | Data_dependency
        | Order_constraint

      let dep_kind_to_string d =
        match d with
        | No_direct_dependency -> "No direct dependency"
        | Data_dependency -> "Data dependency"
        | Order_constraint -> "Order constraint"

      let get_dependency_kind ~src ~dst block reaching_definitions =
        (* [get_dependency_kind ~src ~dst] conservatively answers the question:
           does [src] directly depend on [dst]? *)
        let if_not_disjoint m1 m2 dep_kind =
          if Operation.is_disjoint m1 m2 block reaching_definitions
          then No_direct_dependency
          else (
            State.dump_debug (Block.state block) "%s: %a->%a\n"
              (dep_kind_to_string dep_kind)
              Instruction.Id.print
              (Operation.get_instruction_id m1)
              Instruction.Id.print
              (Operation.get_instruction_id m2);
            dep_kind)
        in
        (* CR-someday gyorsh: this big match can be simplified/split by checking
           the following simple conditions: (a) if one of the operations is
           atomic or one of the operations does not have an addressing_mode,
           then no disjointness check needed. (b) if [src] may read and [dst]
           may write, then this is a [Data_dependencies], otherwise it is an
           [Order_constraint]. *)
        match Operation.desc src, Operation.desc dst with
        | Alloc, _ ->
          (* Currently, Alloc always starts a new partition, so it is the first
             operation in the list of accesses of its partition. *)
          Misc.fatal_error "Unexpected Alloc"
        | (Read _ | Write _ | Read_and_write _ | Arbitrary), Alloc ->
          Order_constraint
        | Read { is_atomic = true; _ }, (Write _ | Read_and_write _ | Arbitrary)
          ->
          (* Treat atomic instructions as memory barriers, don't reorder
             anything around an atomic instruction, and conservatively assume
             that an atomic read may access anywhere in memory. *)
          Data_dependency
        | Read { is_atomic = true; _ }, Read _
        | Read { is_atomic = false; _ }, Read { is_atomic = true; _ }
        | (Write _ | Read_and_write _ | Arbitrary), Read { is_atomic = true; _ }
          ->
          Order_constraint
        | Read { is_atomic = false; _ }, Read { is_atomic = false; _ } ->
          No_direct_dependency
        | Write _, Write _ | Write _, Read { is_atomic = false; _ } ->
          if_not_disjoint src dst Order_constraint
        | Read { is_atomic = false; _ }, Write _ ->
          if_not_disjoint src dst Data_dependency
        | Arbitrary, Read { is_atomic = false; _ } -> Order_constraint
        | ( Arbitrary,
            (Write _ | Read_and_write { is_atomic = false; _ } | Arbitrary) )
        | (Read _ | Read_and_write _), Arbitrary ->
          Data_dependency
        | Arbitrary, Read_and_write { is_atomic = true; _ } -> Data_dependency
        | Write _, Arbitrary -> Order_constraint
        | ( Read_and_write { is_atomic = false; _ },
            Read_and_write { is_atomic = false; _ } )
        | Read_and_write { is_atomic = false; _ }, Write _
        | Read { is_atomic = false; _ }, Read_and_write { is_atomic = false; _ }
          ->
          if_not_disjoint src dst Data_dependency
        | Read_and_write { is_atomic = false; _ }, Read { is_atomic = false; _ }
          ->
          if_not_disjoint src dst Order_constraint
        | ( ( Read { is_atomic = false; _ }
            | Read_and_write { is_atomic = false; _ } ),
            Read_and_write { is_atomic = true; _ } ) ->
          Data_dependency
        | Write _, Read_and_write { is_atomic = true; _ } -> Order_constraint
        | Read_and_write { is_atomic = true; _ }, Read _ -> Order_constraint
        | Read_and_write { is_atomic = true; _ }, Write _ -> Data_dependency
        | Read_and_write { is_atomic = true; _ }, Read_and_write _ ->
          Data_dependency
        | Write _, Read_and_write { is_atomic = false; _ } ->
          if_not_disjoint src dst Order_constraint

      let from_block accesses block reaching_definitions =
        (* [create] is quadratic in the number of memory operations in the
           block. *)
        let add t ~src ~dst =
          let src_id = Operation.get_instruction_id src in
          let dst_id = Operation.get_instruction_id dst in
          let f set =
            match set with
            | None -> Some (Instruction.Id.Set.singleton dst_id)
            | Some set -> Some (Instruction.Id.Set.add dst_id set)
          in
          Instruction.Id.Map.update src_id f t
        in
        let add_dep t ~src ~dst =
          match get_dependency_kind ~src ~dst block reaching_definitions with
          | No_direct_dependency -> t
          | Data_dependency | Order_constraint ->
            (* CR-someday gyorsh: we don't currently use the distinction between
               data dependencies and order constraints, but it can be useful for
               future optimizations. *)
            add t ~src ~dst
        in
        let rec add_deps t ~src operations =
          match operations with
          | [] -> t
          | hd :: tl ->
            let t = add_dep t ~src ~dst:hd in
            add_deps t ~src tl
        in
        let rec add_operations t operations =
          match operations with
          | [] -> t
          | hd :: tl ->
            let t = add_deps t ~src:hd tl in
            add_operations t tl
        in
        Accesses.fold accesses ~init:Instruction.Id.Map.empty
          ~f:(fun _p operations acc -> add_operations acc operations)

      let dump ppf ~block t =
        (* print dependencies in the order they appear in the block. *)
        DLL.iter (Block.body block) ~f:(fun instruction ->
            let instruction = Instruction.basic instruction in
            let src = Instruction.id instruction in
            let dsts = get t src in
            Instruction.Id.Set.iter
              (fun dst ->
                Format.fprintf ppf "memory dep: %a->%a\n" Instruction.Id.print
                  src Instruction.Id.print dst)
              dsts)
    end

    type t = Operation.t Instruction.Id.Map.t

    let from_block block reaching_definitions =
      let pts = Points_to.from_block block reaching_definitions in
      let operations = Points_to.operations pts in
      let deps =
        Dependencies.from_block (Points_to.accesses pts) block
          reaching_definitions
      in
      operations, deps

    let get_memory_operation t instruction =
      Instruction.Id.Map.find_opt (Instruction.id instruction) t

    let get_memory_operation_exn t instruction =
      Instruction.Id.Map.find (Instruction.id instruction) t

    let is_adjacent (t : t) i1 i2 block reaching_definitions : bool =
      Operation.is_adjacent
        (get_memory_operation_exn t i1)
        (get_memory_operation_exn t i2)
        block reaching_definitions

    let dump ppf ~block t =
      (* print dependencies in the order they appear in the block. *)
      DLL.iter (Block.body block) ~f:(fun instruction ->
          let instruction = Instruction.basic instruction in
          let id = Instruction.id instruction in
          match Instruction.Id.Map.find_opt id t with
          | None -> ()
          | Some op ->
            Format.fprintf ppf "memory op for %a: %a\n" Instruction.Id.print id
              Operation.dump op)
  end

  module Dependency_graph : sig
    type t

    val from_block :
      Block.t -> Reaching_definitions.t -> Memory.Dependencies.t -> t

    val independent : t -> Instruction.Id.t -> Instruction.Id.t -> bool

    val get_direct_dependency_of_arg :
      t -> Instruction.Id.t -> arg_i:int -> Instruction.Id.t option

    val get_direct_dependencies : t -> Instruction.Id.t -> Instruction.Id.Set.t

    val dump : Format.formatter -> block:Block.t -> t -> unit
  end = struct
    module Node = struct
      type t =
        { direct_dependencies_of_args : Instruction.Id.t option array;
              (** instruction that defines the argument *)
          direct_dependencies : Instruction.Id.Set.t;
              (** direct dependencies of all register arguments of this instruction (does
                  not include memory dependencies). *)
          all_dependencies : Instruction.Id.Set.t
              (** transitive reflexive dependencies of this instruction, covers register and
              memory dependencies (but not order constraints). *)
        }

      let init instruction reaching_definitions : t =
        let arguments = Instruction.arguments instruction in
        let id = Instruction.id instruction in
        let direct_dependencies_of_args =
          Array.map (Reaching_definitions.get reaching_definitions id) arguments
        in
        let direct_dependencies =
          Array.fold_left
            (fun acc dep ->
              match dep with
              | None -> acc
              | Some dep -> Instruction.Id.Set.add dep acc)
            Instruction.Id.Set.empty direct_dependencies_of_args
        in
        { direct_dependencies_of_args;
          direct_dependencies;
          (* initialized later: *)
          all_dependencies = Instruction.Id.Set.empty
        }
    end

    type t = Node.t Instruction.Id.Tbl.t

    let get_direct_dependency_of_arg dependency_graph id ~arg_i =
      let (node : Node.t) = Instruction.Id.Tbl.find dependency_graph id in
      node.direct_dependencies_of_args.(arg_i)

    let get_all_dependencies dependency_graph id =
      let (node : Node.t) = Instruction.Id.Tbl.find dependency_graph id in
      node.all_dependencies

    let get_direct_dependencies dependency_graph id =
      let (node : Node.t) = Instruction.Id.Tbl.find dependency_graph id in
      node.direct_dependencies

    let independent t i1 i2 =
      let all_deps_of_i1 = get_all_dependencies t i1 in
      let all_deps_of_i2 = get_all_dependencies t i2 in
      not
        (Instruction.Id.Set.mem i1 all_deps_of_i2
        || Instruction.Id.Set.mem i2 all_deps_of_i1)

    (* CR-soon gyorsh: we don't need to keep [Points_to] and table of
       [Reaching_definitions.t] per instruction after we computed direct
       dependencies between instructions. For now it's easier to keep them for
       debugging and probably not too expensive as there is a lot of sharing in
       the representation of the values in these tables, because they are
       constructed incrementally. *)
    let from_block (block : Block.t) reaching_definitions mem_deps =
      let body = Block.body block in
      let terminator = Block.terminator block in
      let t = Block.size block |> Instruction.Id.Tbl.create in
      let add_dependencies (instruction : Instruction.t) =
        (* Add direct dependencies *)
        let node = Node.init instruction reaching_definitions in
        (* Add transitive dependencies *)
        let id = Instruction.id instruction in
        let all_dependencies =
          let init =
            (* data dependencies via registers and memory, and order
               constraints. *)
            Instruction.Id.Set.union node.direct_dependencies
              (Memory.Dependencies.get mem_deps id)
          in
          Instruction.Id.Set.fold
            (fun new_id acc ->
              let (new_node : Node.t) = Instruction.Id.Tbl.find t new_id in
              Instruction.Id.Set.union new_node.all_dependencies acc)
            init init
        in
        let node = { node with all_dependencies } in
        Instruction.Id.Tbl.add t id node
      in
      DLL.iter body ~f:(add_dependencies << Instruction.basic);
      (* CR gyorsh: not sure we need dependencies before terminator. *)
      add_dependencies terminator;
      t

    let dump ppf ~(block : Block.t) (t : t) =
      let open Format in
      let print_node (instruction : Instruction.t) =
        let args = Instruction.arguments instruction in
        let print_reg arg_i dep =
          let pp ppf o =
            match o with
            | None -> fprintf ppf "none"
            | Some id -> fprintf ppf "instruction %a" Instruction.Id.print id
          in
          let reg = args.(arg_i) in
          fprintf ppf "argument %d, %a depends on %a\n" arg_i Printmach.reg reg
            pp dep
        in
        let id = Instruction.id instruction in
        let node = Instruction.Id.Tbl.find t id in
        fprintf ppf "\n%a\n" Instruction.print instruction;
        fprintf ppf "\ndirect dependencies:\n";
        Instruction.Id.Set.iter
          (fprintf ppf "%a " Instruction.Id.print)
          node.direct_dependencies;
        fprintf ppf "\nall dependencies:\n";
        Instruction.Id.Set.iter
          (fprintf ppf "%a " Instruction.Id.print)
          node.all_dependencies;
        fprintf ppf "\nis direct dependency of:\n";
        fprintf ppf "\narg dependencies:\n";
        Array.iteri print_reg node.direct_dependencies_of_args;
        fprintf ppf "\n"
      in
      fprintf ppf "\ndependency graph:\n";
      DLL.iter (Block.body block) ~f:(fun instruction ->
          print_node (Instruction.basic instruction));
      print_node (Block.terminator block);
      fprintf ppf "\n"
  end

  (* CR-soon gyorsh: make [t] lazy, and only construct when needed, for
     performance reasons. For now, for testing purposes, it's better to always
     construct to increase coverage. *)
  type t =
    { reaching_definitions : Reaching_definitions.t;
      dependency_graph : Dependency_graph.t;
      memory_operations : Memory.t;
      mem_deps : Memory.Dependencies.t;
      block : Block.t
    }

  let state t = Block.state t.block

  let independent t instruction_1 instruction_2 =
    (* This covers register and memory dependencies and order constraints,
       because of the way we constructed the graph. *)
    let id1 = Instruction.id instruction_1 in
    let id2 = Instruction.id instruction_2 in
    Dependency_graph.independent t.dependency_graph id1 id2

  let all_independent t instructions =
    (* All pairs of instruction in the group are independent. *)
    let rec check t instructions =
      match instructions with
      | [] -> true
      | hd :: tl ->
        let b = List.for_all (independent t hd) tl in
        if b
        then (
          State.dump_debug (state t) "Group.all_independent: %a\n"
            Instruction.print_id hd;
          check t tl)
        else false
    in
    check t instructions

  let all_adjacent t instructions =
    (* CR-soon gyorsh: shuffles are not supported yet, but this is the place to
       identify them, for example: i<-[a];j<-[a+8];[b]<-j;[b+8]<-i *)
    let rec check_adjacent hd1 tl1 =
      match tl1 with
      | [] -> true
      | hd2 :: tl2 ->
        if Memory.is_adjacent t.memory_operations hd1 hd2 t.block
             t.reaching_definitions
        then check_adjacent hd2 tl2
        else false
    in
    check_adjacent (List.hd instructions) (List.tl instructions)

  let for_all_memory_dependencies t ~f =
    Memory.Dependencies.for_all ~f t.mem_deps

  (* let is_memory_operation t i = Memory.is_memory_operation
     t.memory_operations i *)

  (* let width_in_bits t i = Memory.width_in_bits t.memory_operation i *)

  let get_direct_dependency_of_arg t =
    Dependency_graph.get_direct_dependency_of_arg t.dependency_graph

  let get_direct_dependencies t id =
    Dependency_graph.get_direct_dependencies t.dependency_graph id

  let get_direct_dependency_of_reg t id reg =
    Reaching_definitions.get t.reaching_definitions id reg

  let get_memory_operation t instructon =
    Memory.get_memory_operation t.memory_operations instructon

  let from_block block =
    let state = Block.state block in
    let reaching_definitions = Reaching_definitions.from_block block in
    State.dump_debug state "%a@."
      (Reaching_definitions.dump ~block)
      reaching_definitions;
    let memory_operations, mem_deps =
      Memory.from_block block reaching_definitions
    in
    State.dump_debug state "%a@." (Memory.dump ~block) memory_operations;
    State.dump_debug state "%a@." (Memory.Dependencies.dump ~block) mem_deps;
    let dependency_graph =
      Dependency_graph.from_block block reaching_definitions mem_deps
    in
    State.dump_debug state "%a@."
      (Dependency_graph.dump ~block)
      dependency_graph;
    { reaching_definitions;
      dependency_graph;
      memory_operations;
      mem_deps;
      block
    }
end

module Computation : sig
  type t

  module Group : sig
    type t

    val scalar_instructions : t -> Instruction.t list

    val vector_instructions : t -> Simd_selection.vectorized_instruction list

    val iter_vectorizable_args : t -> f:(arg_i:int -> unit) -> unit
  end

  module Seed : sig
    type t

    val from_block : Block.t -> Dependencies.t -> t list

    val dump : Format.formatter -> t list -> unit
  end

  val from_seed : Block.t -> Dependencies.t -> Seed.t -> t option

  val dump : Format.formatter -> block:Block.t -> t -> unit

  val dump_one_line_stat : Format.formatter -> t -> unit

  val dump_all : Format.formatter -> block:Block.t -> t list -> unit

  (** [contains t i] returns true iff instruction [i] belongs to some
      group in [t]. [i] need not be the key instruction of the group. *)
  val contains : t -> Instruction.t -> bool

  (** [find_group t key] returns the group of [key] instruction, or
      None if [key] instruction is not the key of any group in [t].  *)
  val find_group : t -> key:Instruction.t -> Group.t option

  (** Selects disjoint computations from the input list of computations
      and returns their union. *)
  val select_and_join : t list -> Block.t -> Dependencies.t -> t option
end = struct
  module Group : sig
    (** Represents scalar instructions and the corresponding
        vector instructions. *)
    type t

    (** guaranteed to return a list with at least 2 instructions. *)
    val scalar_instructions : t -> Instruction.t list

    (** guaranteed to return a non-empty list.  *)
    val vector_instructions : t -> Simd_selection.vectorized_instruction list

    (** maps over the indexes of arguments that need to be considered when vectorizing
        dependencies. Currently skips over arguments that are used for memory address
        calculation. [init] ensures that the memory address arguments have the same values
        for all instructions in the group. The result list is in reverse order. *)
    val map_vectorizable_args : t -> f:(arg_i:int -> 'a) -> 'a list

    val iter_vectorizable_args : t -> f:(arg_i:int -> unit) -> unit

    val for_all_non_vectorizable_args : t -> f:(arg_i:int -> bool) -> bool

    (** [init width_in_bits instructions] checks that [instructions]
        are supported isomorphic scalar instructions that are
        inter-independent and if they have memory accesses,
        the accesses must be adjacent. *)
    val init :
      width_in_bits:int -> Instruction.t list -> Dependencies.t -> t option

    val equal : t -> t -> bool

    val dump : Format.formatter -> t -> unit
  end = struct
    type t =
      { vector_instructions : Simd_selection.vectorized_instruction list;
        instructions : Instruction.t list;
        non_address_arg_count : int;
        arg_count : int
      }

    let scalar_instructions t = t.instructions

    let vector_instructions t = t.vector_instructions

    let equal t1 t2 =
      List.equal Instruction.equal_id t1.instructions t2.instructions

    let get_arg_count i = Instruction.arguments i |> Array.length

    let get_res_count i = Instruction.results i |> Array.length

    let map_vectorizable_args t ~f =
      (* Currently, the code assumes that the (variable number of) address args
         ares always at the end of the array of arguments of an instruction. The
         non-address args therefore start from 0, conveniently, and we don't
         need to know if there are any address args or not, so we don't need to
         know if it's a memory operation or not. *)
      List.init t.non_address_arg_count (fun arg_i -> f ~arg_i)

    let iter_vectorizable_args t ~f =
      (* see [map_vectorizable_args] *)
      for arg_i = 0 to t.non_address_arg_count - 1 do
        f ~arg_i
      done

    let for_all_non_vectorizable_args t ~f =
      (* see [map_vectorizable_args] *)
      let rec loop arg_i =
        if arg_i = t.arg_count
        then true
        else if f ~arg_i
        then loop (arg_i + 1)
        else false
      in
      loop t.non_address_arg_count

    let same_stack_offset instructions =
      match instructions with
      | [] -> true
      | hd :: tl ->
        let stack_offset = Instruction.stack_offset hd in
        List.for_all (Int.equal stack_offset << Instruction.stack_offset) tl

    let have_isomorphic_op instructions =
      match instructions with
      | [] -> true
      | hd :: tl ->
        let is_isomorphic i =
          Instruction.have_isomorphic_op hd i
          && Int.equal (get_arg_count hd) (get_arg_count i)
          && Int.equal (get_res_count hd) (get_res_count i)
        in
        List.for_all is_isomorphic tl

    let independent instructions deps =
      let res = Dependencies.all_independent deps instructions in
      State.dump_debug (Dependencies.state deps) "Group.independent: res=%b\n"
        res;
      res

    (** Returns true if all memory accesses performed by the [instructions] are
        vectorizable.  Current implementation assumes that all instructions have
        isomorphic operations. It returns true if the operations do not access
        memory, or if all memory accesses are adjacent; otherwise returns false.  In
        the future, this can be extended to support shuffles. *)
    let can_vectorize_memory_accesses mem_op instructions deps =
      match mem_op with
      | None -> true
      | Some _ ->
        let res = Dependencies.all_adjacent deps instructions in
        State.dump_debug (Dependencies.state deps)
          "Group.all_adjacent: res=%b\n" res;
        res

    let init ~width_in_bits instructions deps =
      assert (List.length instructions > 1);
      assert (
        width_in_bits * List.length instructions
        = Simd_selection.vector_width_in_bits);
      Format.(
        State.dump_debug (Dependencies.state deps) "Group.init\n%a\n"
          (pp_print_list ~pp_sep:pp_print_newline Instruction.print_id)
          instructions);
      match instructions with
      | [] -> assert false
      | instruction :: _ -> (
        let arg_count = get_arg_count instruction in
        let res_count = get_res_count instruction in
        let mem_op = Dependencies.get_memory_operation deps instruction in
        if not
             (same_stack_offset instructions
             && have_isomorphic_op instructions
             && independent instructions deps
             && can_vectorize_memory_accesses mem_op instructions deps)
        then None
        else
          let cfg_ops = List.map (Option.get << Instruction.op) instructions in
          let vector_instructions =
            Simd_selection.vectorize_operation ~width_in_bits ~arg_count
              ~res_count cfg_ops
          in
          match vector_instructions with
          | None -> None
          | Some vector_instructions ->
            let non_address_arg_count =
              match mem_op with
              | None -> arg_count
              | Some mem_op ->
                Dependencies.Memory.Operation.first_memory_arg_index mem_op
            in
            assert (List.length vector_instructions > 0);
            Some
              { vector_instructions;
                instructions;
                non_address_arg_count;
                arg_count
              })

    (* Load: At the moment, we do not vectorize dependencies of load
       instructions, and don't support vectorizing load instructons that have
       interesting vectorizable address dependencies, such as scatter-gather. *)
    (* Store: It must be the seed group, because store instruction has no
       "result", so it can't be a dependency of another group in the tree. We
       have already checked that seed consists of adjacent memory accesses that
       are independent. The only remaining dependency to check is the new value
       stored. It's always the argument at index 0. *)

    let dump ppf t =
      let open Format in
      let spp ppf l =
        pp_print_list ~pp_sep:pp_print_newline Instruction.print ppf l
      in
      let vpp ppf l =
        let pp ppf (simd_instruction : Simd_selection.vectorized_instruction) =
          fprintf ppf "%a " Cfg.dump_basic (Cfg.Op simd_instruction.operation)
        in
        pp_print_list ~pp_sep:pp_print_newline pp ppf l
      in
      fprintf ppf "Group:\nScalar:\n%a\nVector:\n%a\n" spp t.instructions vpp
        t.vector_instructions
  end

  module Seed : sig
    (** A seed is a group of inter-independent store instructions that access adjacent
        memory addresses. *)
    type t

    val lane_width_in_bits : t -> int

    val group : t -> Group.t

    val from_block : Block.t -> Dependencies.t -> t list

    val dump : Format.formatter -> t list -> unit

    val exists_address_dependency :
      t -> f:(Instruction.Id.t -> Reg.t -> bool) -> bool
  end = struct
    type t =
      { group : Group.t;
        width_in_bits : int
      }

    let init ~width_in_bits instructions deps =
      match Group.init ~width_in_bits instructions deps with
      | None -> None
      | Some group -> Some { group; width_in_bits }

    (* [take ~n list] returns the first [n] items of [list], or None if list has
       fewer than [n] items. *)
    let take ~n ~width_in_bits l =
      let rec loop n l acc =
        if n <= 0
        then Some (List.rev acc)
        else
          match l with
          | [] -> None
          | (w, hd) :: tl ->
            if Int.equal w width_in_bits
            then loop (n - 1) tl (hd :: acc)
            else None
      in
      loop n l []

    let lane_width_in_bits t = t.width_in_bits

    let group t = t.group

    (* [is_store i] if [i] is a Store, return the width in bits, otherwise
       return None. *)
    let is_store instruction =
      match Instruction.op instruction with
      | None -> None
      | Some op -> (
        match op with
        | Store (chunk, _, _) -> Some (Cmm.width_in_bits chunk)
        | Alloc _ | Load _ | Move | Reinterpret_cast _ | Static_cast _ | Spill
        | Reload | Const_int _ | Const_float32 _ | Const_float _
        | Const_symbol _ | Const_vec128 _ | Stackoffset _ | Intop _
        | Intop_imm _ | Intop_atomic _ | Floatop _ | Csel _ | Probe_is_enabled _
        | Opaque | Begin_region | End_region | Specific _ | Name_for_debugger _
        | Dls_get | Poll ->
          None)

    let from_block (block : Block.t) deps : t list =
      (* For each store instruction, tries to form a seed with the closest
         stores after it, skipping other instructions. This is a heuristic that
         considers only [Store] instructions, not other intructions that write
         to memory. The goal is to quickly filter out blocks without candidate
         seed, and only use [deps] when needed. *)
      (* CR-someday gyorsh: try to reorder independent stores to the same base
         address to create adjacent accesses that can be vectorized, for
         example: [b+8] <- x; [b]<-y *)
      (* CR-someday gyorsh: find seeds with any store, not necessarily the
         closest stores. This requires proving that stores that are skipped are
         independent, but at the moment we can only do it if they are to the
         same address which limits the effectiveness of looking further ahead.
         It would only add cases of the form: [b] <- x0; [b+16] <-x2 [b+8] <-x1;
         [b+24] <- x3 *)
      let body = Block.body block in
      let all_stores =
        DLL.fold_right body ~init:[] ~f:(fun i acc ->
            let i = Instruction.basic i in
            match is_store i with
            | Some width_in_bits -> (width_in_bits, i) :: acc
            | None -> acc)
      in
      Format.(
        let pp_pair ppf (x, y) = fprintf ppf "(%d, %a)" x Instruction.print y in
        State.dump_debug (Block.state block)
          "Seeds.from_block: all_stores=\n(%a)\n"
          (pp_print_list ~pp_sep:pp_print_newline pp_pair)
          all_stores);
      let rec loop stores acc =
        match stores with
        | [] -> List.rev acc
        | (width_in_bits, _store) :: tl -> (
          let n = Simd_selection.vector_width_in_bits / width_in_bits in
          if n <= 1
          then
            (* nothing to vectorize, the store's access is at least
               vector-length. *)
            loop tl acc
          else
            match take ~n ~width_in_bits stores with
            | None ->
              (* not enough instructions in [stores] to make a group of [n]
                 instructions. *)
              loop tl acc
            | Some instructions -> (
              Format.(
                State.dump_debug (Block.state block)
                  "Seeds.from_block: instructions=\n(%a)\n"
                  (pp_print_list Instruction.print_id)
                  instructions);
              match init ~width_in_bits instructions deps with
              | None -> loop tl acc
              | Some t -> loop tl (t :: acc)))
      in
      loop all_stores [] |> List.rev

    let exists_address_dependency (t : t) ~f =
      (* Seed construction guarantees that all instruction in the [seed] have
         the same address arguments and isomorphic operations. *)
      let instruction = Group.scalar_instructions t.group |> List.hd in
      let args = Instruction.arguments instruction in
      let len = Array.length args in
      let id = Instruction.id instruction in
      let rec exists i =
        (* The first argument of a store is the new value, the rest are address
           arguments. *)
        if i < 1 then false else f id args.(i) || exists (i - 1)
      in
      exists (len - 1)

    let dump ppf (seeds : t list) =
      let open Format in
      fprintf ppf "\nSeeds:\n";
      List.iter (fun t -> fprintf ppf "(%a)\n" Group.dump t.group) seeds
  end

  type t =
    { groups : Group.t Instruction.Id.Map.t;
      (* [all_instructions] is all the scalar instructions in the computations.
         It is an optimization to cache this value here. It is used for ruling
         out computuations that are invalid or not implementable, and to
         estimate cost/benefit of vectorized computations. *)
      all_scalar_instructions : Instruction.Id.Set.t
    }

  let num_groups t = Instruction.Id.Map.cardinal t.groups

  let num_vector_instructions t =
    Instruction.Id.Map.fold
      (fun _k g acc -> acc + List.length (Group.vector_instructions g))
      t.groups 0

  let num_scalar_instructions t =
    Instruction.Id.Set.cardinal t.all_scalar_instructions

  (** [cost t] returns an integer [n] describing the cost of
      vectorized computation [t] instead of the original code:
      negative [n] means vectorized computation is better than
      the original code. The goal is to find [t] that minimizes cost(t).

      Currently, [cost] uses a naive measure of number of instructions,
      i.e., the difference between the number of vector instructions instructions
      and the number of scalar instructions. *)
  let cost t = num_vector_instructions t - num_scalar_instructions t

  let is_cost_effective t = cost t < 0

  let dump_one_line_stat ppf t =
    Format.fprintf ppf
      "%d groups, %d scalar instructions, %d vector instructions, cost = %d"
      (num_groups t)
      (num_scalar_instructions t)
      (num_vector_instructions t)
      (cost t)

  let dump ppf ~(block : Block.t) t =
    let open Format in
    let print_group id group_option =
      match group_option with
      | None -> ()
      | Some (group : Group.t) ->
        fprintf ppf "\nGroup key: %a\n%a\n" Instruction.Id.print id Group.dump
          group
    in
    Format.fprintf ppf "Vectorized computation has %a:\n" dump_one_line_stat t;
    DLL.iter (Block.body block) ~f:(fun instruction ->
        let instruction = Instruction.basic instruction in
        let id = Instruction.id instruction in
        Instruction.Id.Map.find_opt id t.groups |> print_group id)

  let dump_all ppf ~(block : Block.t) (trees : t list) =
    let open Format in
    let print_trees ppf trees =
      List.iter (fun tree -> fprintf ppf "(%a)\n" (dump ~block) tree) trees
    in
    fprintf ppf "Vectorized computations:\n(%a)\n" print_trees trees
  (* The key is the id of the instruction where the group will be inserted,
     which is the last instruction in the group for now, we can change that
     later *)

  let find_group t ~key =
    let key_id = Instruction.id key in
    Instruction.Id.Map.find_opt key_id t.groups

  let contains_id t id = Instruction.Id.Set.mem id t.all_scalar_instructions

  let contains t instruction =
    let id = Instruction.id instruction in
    contains_id t id

  let is_dependency_of_the_rest_of_body t block deps =
    (* is there an instruction outside the computation that directly depends on
       a register defined in the computation? *)
    let res =
      DLL.exists (Block.body block) ~f:(fun instruction ->
          let instruction = Instruction.basic instruction in
          if contains t instruction
          then false
          else
            let id = Instruction.id instruction in
            let direct_deps = Dependencies.get_direct_dependencies deps id in
            let res =
              not
                (Instruction.Id.Set.disjoint direct_deps
                   t.all_scalar_instructions)
            in
            State.dump_debug (Block.state block)
              "Computation.is_dependency_of_the_rest_of_body = %b %a\n" res
              Instruction.print instruction;
            res)
    in
    State.dump_debug (Block.state block)
      "Computation.is_dependency_of_the_rest_of_body result = %b\n" res;
    res

  (** [depends_on_computation t deps id reg] does the value of [reg] at [id]
      depend on any instruction in [t]? *)
  let depends_on_computation t deps id reg =
    match Dependencies.get_direct_dependency_of_reg deps id reg with
    | None -> false
    | Some reaching_definition_id ->
      Instruction.Id.Set.mem reaching_definition_id t.all_scalar_instructions

  let is_dependency_of_outside_body t block deps =
    (* live registers before terminator represent dependencies outside of
       body. *)
    let terminator_id = Instruction.id (Block.terminator block) in
    let live_before_terminator = Block.get_live_regs_before_terminator block in
    let res =
      (* live register before terminator are not defined by the computation. *)
      Reg.Set.exists
        (depends_on_computation t deps terminator_id)
        live_before_terminator
    in
    State.dump_debug (Block.state block)
      "Computation.is_valid: is_dependency_of_outside_body = %b\n" res;
    res

  let seed_address_does_not_depend_on_tree t block deps seed =
    let res =
      (* the only way seed address can depend on the tree is if there is a
         constant assignment *)
      not
        (Seed.exists_address_dependency seed ~f:(depends_on_computation t deps))
    in
    State.dump_debug (Block.state block)
      "Computation.is_valid seed_address_does_not_depend_on_tree = %b\n" res;
    res

  let respects_memory_dependencies t block deps =
    (* conservative: for each memory dependency (data or order), check that the
       new order of instructions satisfies the dependency. This is needed to
       ensure that memory deps between different groups are respected, and
       memory deps between instructions in and out of the vectorized computation
       are respected. The construction of the groups themselves guarantees that
       there are no deps of any kind (reg, mem, or order) between instructions
       in the same group. *)

    (* CR-someday gyorsh: improve instruction scheduling for vectorized
       instructions to allow more code to be vectorized. Order constraints can
       be used to choose the position of vectorized instructions within a block,
       instead of the predefined position used now for [Group.key]. *)
    let old_pos id = Block.pos block id in
    let new_positions =
      Instruction.Id.Map.fold
        (fun key group acc ->
          let key_pos = old_pos key in
          List.fold_left
            (fun acc i ->
              let id = Instruction.id i in
              Instruction.Id.Map.add id key_pos acc)
            acc
            (Group.scalar_instructions group))
        t.groups Instruction.Id.Map.empty
    in
    let new_pos id =
      match Instruction.Id.Map.find_opt id new_positions with
      | None -> old_pos id
      | Some pos -> pos
    in
    let appears get_pos src ~after:dst = get_pos src > get_pos dst in
    let is_respected src dst =
      (* [src] depends on [dst] implies that [src] appears after [dst] in the
         block, i.e., [old_pos_src] > [old_pos_dst]. Check that [new_pos_src] >
         [new_pos_dst]. *)
      if not (appears old_pos src ~after:dst)
      then
        Misc.fatal_errorf
          "Unexpected old position: %a (old pos %d) depends on %a (old pos %d)"
          Instruction.Id.print src (old_pos src) Instruction.Id.print dst
          (old_pos dst);
      let res = appears new_pos src ~after:dst in
      State.dump_debug (Block.state block)
        "Computation.respects_memory_dependencies = %b: %a (old pos %d, new \
         pos %d) depends on %a (old pos %d, new pos %d)\n"
        res Instruction.Id.print src (old_pos src) (new_pos src)
        Instruction.Id.print dst (old_pos dst) (new_pos dst);
      res
    in
    let res = Dependencies.for_all_memory_dependencies ~f:is_respected deps in
    State.dump_debug (Block.state block)
      "Computation.respects_memory_dependencies result = %b\n" res;
    res

  let respects_register_order_constraints t deps =
    (* Check that read or write of register [r] is not reordered past another
       write to [r] (similarly to respecting order constraints between memory
       operations in [respects_memory_dependencies]). This check would not be
       needed if we had basic-block-level SSA. *)
    let is_valid_definition instruction ~arg_i ~new_pos =
      let id = Instruction.id instruction in
      let args = Instruction.arguments instruction in
      let reg = args.(arg_i) in
      let old_def = Dependencies.get_direct_dependency_of_reg deps id reg in
      let new_def =
        Dependencies.get_direct_dependency_of_reg deps new_pos reg
      in
      match old_def, new_def with
      | None, None ->
        (* The register is defined before the block, and not redefined within
           the block. *)
        true
      | None, Some _ ->
        (* The instruction uses register defined before the block, but the
           register is redefined before the new position. *)
        false
      | Some _, None ->
        Misc.fatal_errorf
          "Use of clobbered register %a at %a, previously defined at %a"
          Printmach.reg reg Instruction.Id.print new_pos Instruction.print
          instruction
      | Some old_def, Some new_def ->
        if Instruction.Id.equal old_def new_def
        then true
        else (* cannot move past another definition point *)
          false
    in
    Instruction.Id.Map.for_all
      (fun key group ->
        let scalar_instructions = Group.scalar_instructions group in
        Group.for_all_non_vectorizable_args group ~f:(fun ~arg_i ->
            List.for_all
              (is_valid_definition ~arg_i ~new_pos:key)
              scalar_instructions))
      t.groups

  (* CR gyorsh: [is_dependency_of_outside_body] condition can be weakened if we
     propagate register substitution to instructions that depend on them outside
     the tree (in the same block and other blocks), but may require additional
     instructions to extract scalar values from vector registers. Same weakening
     can be applied to [is_dependency_of_the_rest_of_body]. We check
     [is_dependency_of_the_rest_of_body] later, after [select_and_merge],
     because it allows us to vectorize computations that share some nodes. *)
  let is_valid t block deps =
    respects_memory_dependencies t block deps
    && respects_register_order_constraints t deps
    && not (is_dependency_of_outside_body t block deps)

  (** The key is the last instruction id, for now. This is the place
      where the vectorized intructions will be inserted. *)
  let get_key block instruction_ids =
    let last_instruction = Block.find_last_instruction block instruction_ids in
    Instruction.id last_instruction

  (** Returns the dependencies of arguments at position [arg_i]
      of each instruction in [instruction_ids]. Returns None if
      one of the instruction's dependencies is None for [arg_i]. *)
  let get_deps deps ~arg_i instruction_ids =
    Misc.Stdlib.List.map_option
      (Dependencies.get_direct_dependency_of_arg deps ~arg_i)
      instruction_ids

  let all_instructions map =
    Instruction.Id.Map.fold
      (fun _key (group : Group.t) acc ->
        let instructions = Group.scalar_instructions group in
        let seq = List.to_seq instructions |> Seq.map Instruction.id in
        Instruction.Id.Set.add_seq seq acc)
      map Instruction.Id.Set.empty

  let empty =
    { groups = Instruction.Id.Map.empty;
      all_scalar_instructions = Instruction.Id.Set.empty
    }

  (* CR gyorsh: if same instruction belongs to two groups, is it handled
     correctly? no, only same key is handled correctly. *)
  (* CR gyorsh: handle same instruction multiple times in the same group
     correctly, don't allow duplicating. Currently, this can only lead to a
     valid tree if the duplicated instruction is [Const_*] *)
  let rec build group map ~block ~deps ~width_in_bits =
    (* Recursively builds the vectorized computation and returns the key of the
       root, otherwise None. It consists of groups do not depend on instructions
       outside the computation except for loads. *)
    match group with
    | None -> None
    | Some (group : Group.t) -> (
      let instruction_ids =
        Group.scalar_instructions group |> List.map Instruction.id
      in
      let key = get_key block instruction_ids in
      (* Is there another group with the same key already in the tree? If the
         key instruction of the group is already in another group, and the other
         group is different from this group, we won't vectorize this for
         simplicity's sake. *)
      match Instruction.Id.Map.find_opt key map with
      | Some (old_group : Group.t) ->
        if Group.equal group old_group then Some map else None
      | None ->
        (* add to the map *)
        let map = Instruction.Id.Map.add key group map in
        (* try to create groups for all dependencies *)
        let dep_groups =
          Group.map_vectorizable_args group ~f:(fun ~arg_i ->
              (* [arg_i] ranges over indexes of arguments that need to be
                 considered when vectorizing dependencies. Currently skips over
                 arguments that are used for memory address calculation.
                 [Group.init] ensures that the memory address arguments have the
                 same values for all instructions in the group. *)
              (* CR-someday gyorsh: refer directly to [Reg.t] instead of
                 positional [arg_i]. Currently, the code assumes that address
                 args are always at the end. *)
              match get_deps deps ~arg_i instruction_ids with
              | None ->
                (* At least one of the arguments has a dependency outside the
                   block. Currently, not supported. *)
                None
              | Some dep_ids ->
                State.dump_debug (Block.state block)
                  "Computation.from_seed build deps arg_i=%d\n" arg_i;
                let instructions = List.map (Block.find block) dep_ids in
                Group.init ~width_in_bits instructions deps)
        in
        let missing_deps = List.exists Option.is_none dep_groups in
        if missing_deps
        then None
        else
          (* recurse to vectorize dependencies. *)
          List.fold_left
            (fun acc g -> Option.bind acc (build g ~block ~deps ~width_in_bits))
            (Some map) dep_groups)

  let from_seed (block : Block.t) deps seed =
    let width_in_bits = Seed.lane_width_in_bits seed in
    let root = Seed.group seed in
    State.dump_debug (Block.state block) "Computation.from_seed root=\n%a\n"
      Group.dump root;
    let map = Instruction.Id.Map.empty in
    match build (Some root) map ~block ~deps ~width_in_bits with
    | None -> None
    | Some map ->
      let t =
        { groups = map; all_scalar_instructions = all_instructions map }
      in
      State.dump_debug (Block.state block)
        "Computation.from_seed build finished\n%a\n" (dump ~block) t;
      assert (seed_address_does_not_depend_on_tree t block deps seed);
      if is_valid t block deps then Some t else None

  let join t1 t2 =
    { groups =
        Instruction.Id.Map.union
          (fun _k g1 g2 ->
            assert (Group.equal g1 g2);
            Some g1)
          t1.groups t2.groups;
      all_scalar_instructions =
        Instruction.Id.Set.union t1.all_scalar_instructions
          t2.all_scalar_instructions
    }

  (** [compatible t t'] returns true if for every group [g] in [t],
      and [g'] in [t'],  [g] and [g'] are equal or have disjoint sets
      of scalar instructions. *)
  let compatible t t' =
    if Instruction.Id.Set.disjoint t.all_scalar_instructions
         t'.all_scalar_instructions
    then true
    else
      let sub t1 t2 =
        Instruction.Id.Map.for_all
          (fun key g1 ->
            match Instruction.Id.Map.find_opt key t2.groups with
            | Some g2 ->
              (* equal groups: if the key is in t2, then the corresponding
                 groups are equal. *)
              (* CR gyorsh: don't need to repeat this check in the symmetric
                 case, but it's easier to understand the code this way. *)
              Group.equal g1 g2
            | None ->
              (* disjoint groups: if the key is not in t2, then all insts are
                 not in t2. *)
              List.for_all
                (fun i ->
                  not
                    (Instruction.Id.Set.mem (Instruction.id i)
                       t2.all_scalar_instructions))
                (Group.scalar_instructions g1))
          t1.groups
      in
      sub t t' && sub t' t

  let select_and_join trees block deps =
    match trees with
    | [] -> None
    | trees ->
      (* sort by cost, ascending *)
      let compare_cost t1 t2 = Int.compare (cost t1) (cost t2) in
      let trees = List.sort compare_cost trees in
      let rec loop trees acc =
        match trees with
        | [] -> acc
        | hd :: tl ->
          if compatible hd acc
          then
            let new_acc = join hd acc in
            if compare_cost new_acc acc < 0
            then loop tl new_acc
            else
              (* CR gyorsh: this case is not reachable with the current cost function. *)
              (* skip [hd], try to add the rest of the tail *)
              loop tl acc
          else (* skip [hd], add the rest of the tail *)
            loop tl acc
      in
      let res = loop trees empty in
      (* CR gyorsh: does join respect memory order? *)
      assert (is_valid res block deps);
      State.dump_debug (Block.state block) "Computation.select_and_join %a\n"
        (dump ~block) res;
      if is_dependency_of_the_rest_of_body res block deps
         || not (is_cost_effective res)
      then None
      else Some res
end

let augment_reg_map reg_map group =
  (* Make sure that [reg_map] contains all scalar registers of the [group] that
     will be replaced by vector registers. It is not enough to map only scalar
     registers that appear in [key_instruction] because another group's key
     instruction may refer to different scalar registers in the same packed
     register (i.e., the order of scalar instructions need not be the same as
     block order, and the key is currently the last instruction in the block
     order.

     For example, group [10;7] and group [13;14]: *)

  (* (id:5) V/63 := val [b:V/62 + 8]
   * (id:6) V/64 := val [a:V/61 + 8]
   * (id:7) Paddint:I/65 := V/64 + V/63 + -1
   * (id:8) V/66 := val [b:V/62]
   * (id:9) V/67 := val [a:V/61]
   * (id:10) Paddint:I/68 := V/67 + V/66 + -1
   * (id:13) val[V/69] := Paddint:I/68 (init)
   * (id:14) val[V/69 + 8] := Paddint:I/65 (init) *)

  (* The key of [10;7] has result register [I/68] but the corresponding argument
     register in the key of [13;14] is [I/65]. *)
  let scalar_instructions = Computation.Group.scalar_instructions group in
  let arg = List.map Instruction.arguments scalar_instructions in
  let res = List.map Instruction.results scalar_instructions in
  let augment index reg_arrays =
    let pack = List.map (fun reg_array -> reg_array.(index)) reg_arrays in
    match pack with
    | [] -> ()
    | hd :: tl -> (
      match Substitution.get_reg_opt reg_map hd with
      | None -> Substitution.fresh_reg_for_pack reg_map pack Vec128
      | Some old_reg_for_hd ->
        (* other registers in the pack must be mapped in the same way as
           [hd]. *)
        List.iter
          (fun reg ->
            match Substitution.get_reg_opt reg_map reg with
            | None ->
              Misc.fatal_errorf
                "augment_reg_map: %a is mapped to %a but %a is not mapped"
                Printmach.reg hd Printmach.reg old_reg_for_hd Printmach.reg reg
            | Some old_reg ->
              if not (Reg.same old_reg_for_hd old_reg)
              then
                Misc.fatal_errorf
                  "augment_reg_map: %a is mapped to %a but %a is mapped to %a"
                  Printmach.reg hd Printmach.reg old_reg_for_hd Printmach.reg
                  reg Printmach.reg old_reg)
          tl)
  in
  (* only some of the args are vectorizable, but all results are vectorizable. *)
  (* CR gyorsh: get rid of positional interface, use registers directly. *)
  Computation.Group.iter_vectorizable_args group ~f:(fun ~arg_i ->
      augment arg_i arg);
  Array.iteri (fun i _r -> augment i res) (List.hd res)

let add_vector_instructions_for_group reg_map state group ~before:cell
    old_instruction =
  let vector_instructions = Computation.Group.vector_instructions group in
  let key_instruction = Instruction.basic old_instruction in
  let new_regs : Reg.t Numbers.Int.Tbl.t = Numbers.Int.Tbl.create 2 in
  let get_new_reg n =
    match Numbers.Int.Tbl.find_opt new_regs n with
    | Some reg -> reg
    | None ->
      let new_reg = Reg.create Vec128 in
      Numbers.Int.Tbl.add new_regs n new_reg;
      new_reg
  in
  let create_instruction
      (simd_instruction : Simd_selection.vectorized_instruction) =
    let get_register (simd_reg : Simd_selection.register) =
      match simd_reg with
      | New n -> get_new_reg n
      | Argument n ->
        let original_reg = (Instruction.arguments key_instruction).(n) in
        Substitution.get_reg_exn reg_map original_reg
      | Result n ->
        let original_reg = (Instruction.results key_instruction).(n) in
        Substitution.get_reg_exn reg_map original_reg
      | Original n ->
        let original_reg = (Instruction.arguments key_instruction).(n) in
        original_reg
    in
    let desc = Cfg.Op simd_instruction.operation in
    let arg = Array.map get_register simd_instruction.arguments in
    let res = Array.map get_register simd_instruction.results in
    let id = State.next_available_instruction state in
    Instruction.copy old_instruction ~desc ~arg ~res ~id
  in
  augment_reg_map reg_map group;
  (* actually insert the vector instructions into the body of the block *)
  List.iter
    (fun simd_instruction ->
      create_instruction simd_instruction |> DLL.insert_before cell)
    vector_instructions

let vectorize (block : Block.t) tree =
  let reg_map = Block.reg_map block in
  let state = Block.state block in
  (* Add vector instructions. *)
  let rec add_vector_instructions cell_option =
    match cell_option with
    | None -> ()
    | Some cell ->
      (let old_instruction = DLL.value cell in
       let instruction = Instruction.basic old_instruction in
       match Computation.find_group tree ~key:instruction with
       | None -> ()
       | Some group ->
         add_vector_instructions_for_group reg_map state group ~before:cell
           old_instruction);
      DLL.next cell |> add_vector_instructions
  in
  let body = Block.body block in
  DLL.hd_cell body |> add_vector_instructions;
  (* Remove all instructions that were replaced by vector instructions. *)
  DLL.filter_left body ~f:(fun instruction ->
      let instruction = Instruction.basic instruction in
      not (Computation.contains tree instruction))

exception Cannot_reorder of Instruction.t * Instruction.t

let can_reorder tree body deps =
  (* Checks nodes can be grouped together. Modifies a copy of the block's body
     to move the scalar instructions together to where they would be replaced by
     the corresponding vector instructions. Does not actually emit vectorized
     instructions. There is a faster/simpler/safer way to emit vectorizerd code.

     Used for validation only. Intended to work even after the vectorization
     heuristics evolve. Can be expensive. *)
  let reorder_instruction body instruction position =
    match position with
    | None -> assert false
    | Some position -> (
      (* find the cell that holds the scalar [instruction] *)
      let same_position i =
        let p = DLL.value position in
        Instruction.equal_id i p
      in
      if same_position instruction
      then
        (* the [instruction] is already at [position], no need to reorder. *)
        DLL.prev position
      else
        let cell =
          DLL.find_cell_opt body ~f:(Instruction.equal_id instruction)
          |> Option.get
        in
        (* Traverse from [cell] to [position], making sure the [instruction] can
           be moved across every [other_instruction] along the way. *)
        let rec can_cross cur =
          match DLL.next cur with
          | None -> assert false
          | Some next_cell ->
            let other_instruction = DLL.value next_cell in
            if not (Dependencies.independent deps instruction other_instruction)
            then raise (Cannot_reorder (instruction, other_instruction))
            else if same_position other_instruction
            then true
            else can_cross next_cell
        in
        match can_cross cell with
        | false -> None
        | true ->
          (* Insert the scalar [instruction] after [position] and return the new
             cell. This duplicates the scalar instruction, including the key. *)
          DLL.insert_after position instruction;
          (* delete old scalar instructions *)
          DLL.delete_curr cell;
          Some position)
  in
  let reorder_group position : Instruction.t DLL.cell option =
    let key = DLL.value position in
    let group = Computation.find_group tree ~key in
    match group with
    | None -> DLL.prev position
    | Some (group : Computation.Group.t) ->
      let instructions = Computation.Group.scalar_instructions group in
      (* traverse [instructions] backwards, moving each to [position], and
         returns the cell before the group. *)
      List.fold_right (reorder_instruction body) instructions (Some position)
  in
  let rec reorder cell_option =
    match cell_option with
    | None -> true
    | Some cell -> reorder_group cell |> reorder
  in
  (* traverse the block backwards *)
  DLL.last_cell body |> reorder

let can_reorder tree block deps =
  let state = Block.state block in
  let body = Block.body block in
  (* copy body *)
  let body = DLL.map ~f:Instruction.basic body in
  let res =
    try can_reorder tree body deps
    with Cannot_reorder (i1, i2) ->
      State.dump_debug state "Cannot reorder %a %a\n\n" Instruction.print_id i1
        Instruction.print_id i2;
      false
  in
  State.dump_debug state "State.can_reorder res=%b\nReordered block:\n" res;
  DLL.iter body ~f:(fun i -> State.dump_debug state "%a\n" Instruction.print i);
  res

let maybe_vectorize block =
  let state = Block.state block in
  let instruction_count = Block.size block in
  let label = Block.start block in
  State.dump state "\nBlock %d:\n" label;
  if instruction_count > State.max_block_size_to_vectorize
  then
    State.dump state
      "Skipping block %d with %d instructions (> %d = \
       max_block_size_to_vectorize).\n"
      label instruction_count State.max_block_size_to_vectorize
  else
    let deps = Dependencies.from_block block in
    let seeds = Computation.Seed.from_block block deps in
    State.dump_debug state "%a@." Computation.Seed.dump seeds;
    let computations =
      List.filter_map (Computation.from_seed block deps) seeds
    in
    State.dump_debug state "%a@." (Computation.dump_all ~block) computations;
    match Computation.select_and_join computations block deps with
    | None -> ()
    | Some computation ->
      let scoped_name =
        State.fun_dbg state |> Debuginfo.get_dbg |> Debuginfo.Dbg.to_list
        |> List.map (fun dbg ->
               Debuginfo.(Scoped_location.string_of_scopes dbg.dinfo_scopes))
        |> String.concat ","
      in
      State.dump state "**** Vectorize selected computation: %a (%s)\n"
        Computation.dump_one_line_stat computation scoped_name;
      State.dump_debug state "%a\n" (Computation.dump ~block) computation;
      if State.extra_debug && not (can_reorder computation block deps)
      then
        Misc.fatal_errorf "Vectorized computation is not valid:\n%a\n"
          (Computation.dump ~block) computation ();
      let dump_block msg block =
        let size = DLL.length (Block.body block) in
        State.dump state "Block %a in %s: %s body instruction count=%d\n"
          Label.print (Block.start block) (State.fun_name state) msg size;
        DLL.iter (Block.body block) ~f:(fun i ->
            State.dump_debug state "%a\n" Instruction.print
              (Instruction.basic i))
      in
      dump_block "before vectorize" block;
      (* This is the only function that changes the [block]. *)
      vectorize block computation;
      dump_block "after vectorize" block;
      ()

let cfg ppf_dump cl =
  let state = State.create ppf_dump cl in
  State.dump state "*** Vectorize@.";
  let cfg = Cfg_with_layout.cfg cl in
  (* Iterate in layout order instead of default block order to make debugging
     easier. *)
  let layout = Cfg_with_layout.layout cl in
  DLL.iter layout ~f:(fun label ->
      let block = Block.create (Cfg.get_block_exn cfg label) state in
      maybe_vectorize block);
  cl
