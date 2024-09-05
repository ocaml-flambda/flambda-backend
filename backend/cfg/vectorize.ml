[@@@ocaml.warning "+a-40-41-42"]

(* Finds independent scalar operations within the same basic block and tries to
   use vector operations if possible *)
(* CR-soon tip: add documentation *)

module DLL = Flambda_backend_utils.Doubly_linked_list

let debug = false

let ( << ) f g x = f (g x)

let all_option_list list =
  List.fold_right
    (fun item result ->
      match item, result with
      | Some item, Some result -> Some (item :: result)
      | _ -> None)
    list (Some [])

let vector_width_in_bits = Simd_selection.vector_width_in_bits

module Instruction : sig
  (* CR-someday tip: consider moving this to cfg or at least have something
     similar there *)
  module Id : sig
    type t

    include Identifiable.S with type t := t

    val to_int : t -> int

    val of_int : int -> t

    val reset_max_id : unit -> unit

    val update_max_id : t -> unit

    val next_available_id : unit -> t
  end

  type t =
    | Basic of Cfg.basic Cfg.instruction
    | Terminator of Cfg.terminator Cfg.instruction

  val id : t -> Id.t

  val arguments : t -> Reg.t Array.t

  val results : t -> Reg.t Array.t

  val destroyed : t -> Reg.t Array.t

  val op : t -> Cfg.operation option

  val have_isomorphic_op : t -> t -> bool

  val to_vector_instructions :
    int -> t list -> Simd_selection.vectorized_instruction list option

  val is_store : t -> bool

  val is_alloc : t -> bool

  val can_cross_loads_or_stores : t -> bool

  val preserves_alloc_freshness : t -> bool

  val body_of : Cfg.basic_block -> t DLL.t

  val tbl_of : Cfg.basic_block -> t Id.Tbl.t

  val find_last_instruction : t DLL.t -> Id.t list -> t

  val print : Format.formatter -> t -> unit
end = struct
  module Id = struct
    include Numbers.Int

    let to_int t = t

    let of_int t = t

    let max_id = ref 0

    let reset_max_id () = max_id := 0

    let update_max_id id = max_id := Int.max id !max_id

    let next_available_id () =
      max_id := !max_id + 1;
      !max_id
  end

  type t =
    | Basic of Cfg.basic Cfg.instruction
    | Terminator of Cfg.terminator Cfg.instruction

  let id (instruction : t) : Id.t =
    match instruction with
    | Basic instruction -> instruction.id
    | Terminator instruction -> instruction.id

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

  let same_stack_offset instructions =
    match List.map stack_offset instructions with
    | [] -> true
    | hd :: tl -> List.for_all (Int.equal hd) tl

  let are_isomorphic cfg_ops =
    match cfg_ops with
    | [] -> true
    | hd :: tl -> List.for_all (op_isomorphic hd) tl

  let to_vector_instructions width_in_bits instructions =
    assert (width_in_bits * List.length instructions = vector_width_in_bits);
    match List.map op instructions |> all_option_list with
    | None -> None
    | Some cfg_ops ->
      if same_stack_offset instructions && are_isomorphic cfg_ops
      then
        let instruction = List.hd instructions in
        let arg_count = arguments instruction |> Array.length in
        let res_count = results instruction |> Array.length in
        Simd_selection.vectorize_operation ~width_in_bits ~arg_count ~res_count
          cfg_ops
      else None

  let is_store (instruction : t) =
    match instruction with
    | Basic basic_instruction -> (
      let desc = basic_instruction.desc in
      match desc with
      | Op op -> (
        match op with
        | Store _ -> true
        | Load _ | Alloc _ | Move | Reinterpret_cast _ | Static_cast _ | Spill
        | Reload | Const_int _ | Const_float32 _ | Const_float _
        | Const_symbol _ | Const_vec128 _ | Stackoffset _ | Intop _
        | Intop_imm _ | Intop_atomic _ | Floatop _ | Csel _ | Probe_is_enabled _
        | Opaque | Begin_region | End_region | Specific _ | Name_for_debugger _
        | Dls_get | Poll ->
          false)
      | Reloadretaddr | Pushtrap _ | Poptrap | Prologue | Stack_check _ -> false
      )
    | Terminator _ -> false

  let is_alloc (instruction : t) =
    match instruction with
    | Basic basic_instruction -> (
      let desc = basic_instruction.desc in
      match desc with
      | Op op -> (
        match op with
        | Alloc _ -> true
        | Load _ | Store _ | Move | Reinterpret_cast _ | Static_cast _ | Spill
        | Reload | Const_int _ | Const_float32 _ | Const_float _
        | Const_symbol _ | Const_vec128 _ | Stackoffset _ | Intop _
        | Intop_imm _ | Intop_atomic _ | Floatop _ | Csel _ | Probe_is_enabled _
        | Opaque | Begin_region | End_region | Specific _ | Name_for_debugger _
        | Dls_get | Poll ->
          false)
      | Reloadretaddr | Pushtrap _ | Poptrap | Prologue | Stack_check _ -> false
      )
    | Terminator _ -> false

  let can_cross_loads_or_stores (instruction : t) =
    (* CR-someday tip: some instructions may or may not cause issues for going
       across a load or a store, for simplicity's sake, let's just return false
       and not let them go across for now, but better handling can be added in
       the future. Also, loads from an immuntable block has no coeffects and may
       have less restrictions*)
    match instruction with
    | Basic basic_instruction -> (
      let desc = basic_instruction.desc in
      match desc with
      | Op op -> (
        match op with
        | Load _ | Store _ | Intop_atomic _ | Alloc _ | Poll | Opaque
        | Begin_region | End_region ->
          false
        | Specific specific_operation ->
          Arch.can_cross_loads_or_stores specific_operation
        | Move | Reinterpret_cast _ | Static_cast _ | Spill | Reload
        | Const_int _ | Const_float32 _ | Const_float _ | Const_symbol _
        | Const_vec128 _ | Stackoffset _ | Intop _ | Intop_imm _ | Floatop _
        | Csel _ | Probe_is_enabled _ | Name_for_debugger _ | Dls_get ->
          true)
      | Reloadretaddr | Pushtrap _ | Poptrap | Prologue | Stack_check _ -> true)
    | Terminator _ -> false

  let preserves_alloc_freshness (instruction : t) =
    match instruction with
    | Basic basic_instruction -> (
      let desc = basic_instruction.desc in
      match desc with
      | Op op -> (
        match op with
        | Load _ | Store _ -> false
        | Specific specific_operation ->
          Arch.preserves_alloc_freshness specific_operation
        | Alloc _ | Move | Reinterpret_cast _ | Static_cast _ | Spill | Reload
        | Const_int _ | Const_float32 _ | Const_float _ | Const_symbol _
        | Const_vec128 _ | Stackoffset _ | Intop _ | Intop_imm _
        | Intop_atomic _ | Floatop _ | Csel _ | Probe_is_enabled _ | Opaque
        | Begin_region | End_region | Name_for_debugger _ | Dls_get | Poll ->
          true)
      | Reloadretaddr | Pushtrap _ | Poptrap | Prologue | Stack_check _ -> true)
    | Terminator _ -> false

  let body_of (block : Cfg.basic_block) =
    DLL.to_list block.body
    |> List.map (fun basic_instruction -> Basic basic_instruction)
    |> DLL.of_list

  let tbl_of block =
    body_of block |> DLL.to_list
    |> List.map (fun instruction -> id instruction, instruction)
    |> Id.Tbl.of_list

  let find_last_instruction body instructions =
    let starting_cell = DLL.last_cell body in
    let instruction_set = Id.Set.of_list instructions in
    let rec find_last cell_option =
      match cell_option with
      | None -> assert false
      | Some cell ->
        let current_instruction = DLL.value cell in
        let current_instruction_id = id current_instruction in
        if Id.Set.exists (Id.equal current_instruction_id) instruction_set
        then current_instruction
        else find_last (DLL.prev cell)
    in
    find_last starting_cell

  let print ppf (instruction : t) : unit =
    match instruction with
    | Basic i -> Cfg.print_basic ppf i
    | Terminator i -> Cfg.print_terminator ppf i
end

module Dependency_graph : sig
  (* The dependency graph shows dependencies between instructions within the
     same basic block *)
  type t

  val latest_change :
    current:Instruction.Id.t -> Reg.t -> Cfg.basic_block -> Instruction.t option

  val from_block : Cfg.basic_block -> t

  val get_arg_dependency :
    t -> Instruction.Id.t -> arg_i:int -> Instruction.Id.t option

  val get_all_dependencies_of_arg :
    t -> Instruction.Id.t -> arg_i:int -> Instruction.Id.Set.t

  val get_is_dependency_of : t -> Instruction.Id.t -> Instruction.Id.Set.t

  val dump : Format.formatter -> t -> Cfg.basic_block -> unit
end = struct
  module Node = struct
    module Reg_node = struct
      type t =
        { reg : Reg.t;
          direct_dependency : Instruction.Id.t option
              (* the most recent instruction in this basic block that may change
                 the value of the argument *)
        }

      let init reg : t = { reg; direct_dependency = None }
    end

    type t =
      { instruction : Instruction.t;
        reg_nodes : Reg_node.t array;
        direct_dependencies : Instruction.Id.Set.t;
            (* direct dependencies of all arguments of this instruction *)
        all_dependencies : Instruction.Id.Set.t;
            (* direct dependencies of this instruction and all dependencies of
               each direct dependency of this instruction *)
        is_direct_dependency_of : Instruction.Id.Set.t
            (* all instructions that have this instruction as a direct
               dependency *)
      }

    let init instruction : t =
      let arguments = Instruction.arguments instruction in
      { instruction;
        reg_nodes =
          Array.init (Array.length arguments) (fun i ->
              Reg_node.init arguments.(i));
        direct_dependencies = Instruction.Id.Set.empty;
        all_dependencies = Instruction.Id.Set.empty;
        is_direct_dependency_of = Instruction.Id.Set.empty
      }
  end

  type t = Node.t Instruction.Id.Tbl.t

  let add = Instruction.Id.Tbl.add

  let find = Instruction.Id.Tbl.find

  let replace = Instruction.Id.Tbl.replace

  let init () : t = Instruction.Id.Tbl.create 100

  let get_arg_dependency dependency_graph id ~arg_i =
    let (node : Node.t) = Instruction.Id.Tbl.find dependency_graph id in
    node.reg_nodes.(arg_i).direct_dependency

  let get_all_dependencies dependency_graph id =
    let (node : Node.t) = Instruction.Id.Tbl.find dependency_graph id in
    node.all_dependencies

  let get_all_dependencies_of_arg dependency_graph id ~arg_i =
    let arg_dependency = get_arg_dependency dependency_graph id ~arg_i in
    match arg_dependency with
    | None -> Instruction.Id.Set.empty
    | Some direct_dependency ->
      get_all_dependencies dependency_graph direct_dependency
      |> Instruction.Id.Set.add direct_dependency

  let get_is_dependency_of dependency_graph instruction =
    let (node : Node.t) =
      Instruction.Id.Tbl.find dependency_graph instruction
    in
    node.is_direct_dependency_of

  let latest_change ~(current : Instruction.Id.t) (reg : Reg.t) block =
    let is_changed_in instruction reg =
      Array.exists (Reg.same reg) (Instruction.results instruction)
      || Array.exists (Reg.same reg) (Instruction.destroyed instruction)
    in
    let body = Instruction.body_of block in
    let starting_cell =
      match
        DLL.find_cell_opt body ~f:(fun instruction ->
            Instruction.id instruction |> Instruction.Id.equal current)
      with
      | None -> DLL.last_cell body
      | Some current_cell -> DLL.prev current_cell
    in
    let rec find_latest_change cell_option =
      match cell_option with
      | None -> None
      | Some cell ->
        let instruction = DLL.value cell in
        if is_changed_in instruction reg
        then Some instruction
        else find_latest_change (DLL.prev cell)
    in
    find_latest_change starting_cell

  let from_block (block : Cfg.basic_block) =
    let dependency_graph = init () in
    let add_arg_dependency instruction arg_i arg =
      let id = Instruction.id instruction in
      let dependency = latest_change ~current:id arg block in
      let node = find dependency_graph id in
      let reg_node = node.reg_nodes.(arg_i) in
      node.reg_nodes.(arg_i)
        <- { reg_node with
             direct_dependency =
               Option.fold ~none:None
                 ~some:(Option.some << Instruction.id)
                 dependency
           }
    in
    let add_arg_dependencies (instruction : Instruction.t) =
      Array.iteri
        (add_arg_dependency instruction)
        (Instruction.arguments instruction)
    in
    let add_dependencies (instruction : Instruction.t) =
      let id = Instruction.id instruction in
      add dependency_graph id (Node.init instruction);
      add_arg_dependencies instruction;
      let arg_indices =
        Instruction.arguments instruction |> Array.mapi (fun arg_i _ -> arg_i)
      in
      let direct_dependencies =
        Array.fold_left
          (fun dependencies arg_i ->
            Option.fold ~none:dependencies
              ~some:(fun dependency ->
                Instruction.Id.Set.add dependency dependencies)
              (find dependency_graph id).reg_nodes.(arg_i).direct_dependency)
          Instruction.Id.Set.empty arg_indices
      in
      let all_dependencies =
        Instruction.Id.Set.fold
          (fun new_id old_indirect_dependencies ->
            let node = Instruction.Id.Tbl.find dependency_graph new_id in
            Instruction.Id.Set.union node.direct_dependencies
              old_indirect_dependencies)
          direct_dependencies direct_dependencies
      in
      let node = find dependency_graph id in
      replace dependency_graph id
        { node with direct_dependencies; all_dependencies }
    in
    let add_all_dependencies () =
      DLL.iter block.body ~f:(fun instruction ->
          add_dependencies (Basic instruction));
      add_dependencies (Terminator block.terminator)
    in
    let set_is_dependency_of instruction_id dependency_id =
      let dependency = find dependency_graph dependency_id in
      replace dependency_graph dependency_id
        { dependency with
          is_direct_dependency_of =
            Instruction.Id.Set.add instruction_id
              dependency.is_direct_dependency_of
        }
    in
    let set_is_dependency_of_plural (instruction : Instruction.t) =
      let id = Instruction.id instruction in
      let node = find dependency_graph id in
      Instruction.Id.Set.iter (set_is_dependency_of id) node.direct_dependencies
    in
    let set_all_is_dependency_of () =
      DLL.iter block.body ~f:(fun instruction ->
          set_is_dependency_of_plural (Basic instruction));
      set_is_dependency_of_plural (Terminator block.terminator)
    in
    add_all_dependencies ();
    set_all_is_dependency_of ();
    dependency_graph

  let dump ppf (t : t) (block : Cfg.basic_block) =
    let open Format in
    let print_reg_node arg_i (reg_node : Node.Reg_node.t) =
      let dependency =
        Option.fold ~none:"none"
          ~some:(sprintf "instruction %d" << Instruction.Id.to_int)
          reg_node.direct_dependency
      in
      fprintf ppf "argument %d, %a depends on %s\n" arg_i Printmach.reg
        reg_node.reg dependency
    in
    let print_node (instruction : Instruction.t) =
      let id = Instruction.id instruction in
      let node = find t id in
      fprintf ppf "\n%d:\n"
        (Instruction.id node.instruction |> Instruction.Id.to_int);
      Instruction.print ppf instruction;
      fprintf ppf "\ndirect dependencies:\n";
      Instruction.Id.Set.iter
        (fprintf ppf "%d " << Instruction.Id.to_int)
        node.direct_dependencies;
      fprintf ppf "\nall dependencies:\n";
      Instruction.Id.Set.iter
        (fprintf ppf "%d " << Instruction.Id.to_int)
        node.all_dependencies;
      fprintf ppf "\nis direct dependency of:\n";
      Instruction.Id.Set.iter
        (fprintf ppf "%d " << Instruction.Id.to_int)
        node.is_direct_dependency_of;
      fprintf ppf "\narg dependencies:\n";
      Array.iteri print_reg_node node.reg_nodes;
      fprintf ppf "\n"
    in
    fprintf ppf "\ndependency graph:\n";
    DLL.iter block.body ~f:(fun instruction -> print_node (Basic instruction));
    print_node (Terminator block.terminator);
    fprintf ppf "\n"
end

module Memory_accesses : sig
  module Memory_operation : sig
    type t

    val instruction : t -> Instruction.t

    val width_in_bits : t -> int

    val dump : Format.formatter -> t -> unit
  end

  type t

  val stores : t -> Instruction.Id.t list

  val get_memory_operation_exn : t -> Instruction.Id.t -> Memory_operation.t

  val from_block : Cfg.basic_block -> Dependency_graph.t -> t

  val all_adjacent : t -> Instruction.Id.t list -> bool

  val inter_independent : t -> Instruction.t list -> bool

  val can_group :
    ?remove:bool -> t -> Instruction.t DLL.t -> Instruction.Id.t list -> bool

  val dump : Format.formatter -> t -> unit
end = struct
  module Memory_operation = struct
    type op =
      | Load
      | Store

    type t =
      { op : op;
        memory_chunk : Cmm.memory_chunk;
        addressing_mode : Arch.addressing_mode;
        instruction : Instruction.t;
        dependent_allocs : Instruction.Id.Set.t;
        unsure_allocs : Instruction.Id.Set.t
      }

    let instruction t = t.instruction

    let init (instruction : Instruction.t) : t option =
      match instruction with
      | Basic basic_instruction -> (
        let desc = basic_instruction.desc in
        match desc with
        | Op op -> (
          match op with
          | Load { memory_chunk; addressing_mode; _ } ->
            Some
              { op = Load;
                memory_chunk;
                addressing_mode;
                instruction;
                dependent_allocs = Instruction.Id.Set.empty;
                unsure_allocs = Instruction.Id.Set.empty
              }
          | Store (memory_chunk, addressing_mode, _) ->
            Some
              { op = Store;
                memory_chunk;
                addressing_mode;
                instruction;
                dependent_allocs = Instruction.Id.Set.empty;
                unsure_allocs = Instruction.Id.Set.empty
              }
          | Specific _ ->
            None
            (* CR-someday tip: may need to rewrite a lot of code to handle loads
               and stores inside [Specific] in the future *)
          | Move | Reinterpret_cast _ | Static_cast _ | Spill | Reload
          | Const_int _ | Const_float32 _ | Const_float _ | Const_symbol _
          | Const_vec128 _ | Stackoffset _ | Intop _ | Intop_imm _
          | Intop_atomic _ | Floatop _ | Csel _ | Probe_is_enabled _ | Opaque
          | Begin_region | End_region | Name_for_debugger _ | Dls_get | Poll
          | Alloc _ ->
            None)
        | Reloadretaddr | Pushtrap _ | Poptrap | Prologue | Stack_check _ ->
          None)
      | Terminator _ -> None

    let memory_arguments (t : t) =
      let arguments = Instruction.arguments t.instruction in
      match t.op with
      | Load -> arguments
      | Store -> Array.sub arguments 1 (Array.length arguments - 1)

    let width_in_bits (t : t) = Cmm.width_in_bits t.memory_chunk

    let print_memory_chunk ppf (t : t) =
      Format.fprintf ppf "%s (length %d)"
        (Printcmm.chunk t.memory_chunk)
        (width_in_bits t)

    let dump ppf (t : t) =
      let open Format in
      let instruction = t.instruction in
      let print_set ppf set =
        Instruction.Id.Set.iter
          (fun id -> fprintf ppf "%d " (Instruction.Id.to_int id))
          set
      in
      fprintf ppf
        "\n\
         Instruction %d: %a (%a, %a)\n\
        \ dependent allocs: %a\n\
        \ unsure_allocs: %a"
        (Instruction.id instruction |> Instruction.Id.to_int)
        Instruction.print instruction print_memory_chunk t
        (Arch.print_addressing Printmach.reg t.addressing_mode)
        (memory_arguments t) print_set t.dependent_allocs print_set
        t.unsure_allocs

    let compare_arguments (t1 : t) (t2 : t) =
      let arguments_1 = memory_arguments t1 in
      let arguments_2 = memory_arguments t2 in
      Array.combine arguments_1 arguments_2
      |> Array.fold_left
           (fun result ((arg1, arg2) : Reg.t * Reg.t) ->
             if result = 0 then Reg.compare arg1 arg2 else result)
           0

    let compare_addressing_modes_and_arguments (t1 : t) (t2 : t) =
      let addressing_mode_comparison =
        Arch.compare_addressing_mode_without_displ t1.addressing_mode
          t2.addressing_mode
      in
      if addressing_mode_comparison = 0
      then
        let arguments_comparison = compare_arguments t1 t2 in
        arguments_comparison
      else addressing_mode_comparison

    let offset_of (t1 : t) (t2 : t) =
      let addressing_mode_and_arguments_comparison =
        compare_addressing_modes_and_arguments t1 t2
      in
      if addressing_mode_and_arguments_comparison = 0
      then Arch.addressing_offset t1.addressing_mode t2.addressing_mode
      else None

    let is_adjacent (t1 : t) (t2 : t) =
      let res =
        if Instruction.have_isomorphic_op t1.instruction t2.instruction
        then
          match offset_of t1 t2 with
          | None -> false
          | Some offset -> width_in_bits t1 = offset * 8
        else false
      in
      res
  end

  type t =
    { loads : Instruction.Id.t list;
      stores : Instruction.Id.t list;
      memory_operations : Memory_operation.t Instruction.Id.Tbl.t
    }

  let stores t = t.stores

  let get_memory_operation_exn t id =
    Instruction.Id.Tbl.find t.memory_operations id

  let from_block (block : Cfg.basic_block) dependency_graph : t =
    (* A heuristic to avoid treating the same "fresh" allocation which address
       stored and loaded into a different register as different, has room for
       improvement. Assumption: if x depends on a fresh allocation, and it is
       certain that y does not depend on that fresh allocation, then they point
       to disjoint addresses *)
    (* At each load or store instruction, it keeps track of all allocs up to
       this point in this basic block and puts them in one of 3 categories:
       [fresh_allocs]: nothing that depends on the address of the fresh alloc
       has been saved as a value; [stored_allocs]: something that depends on the
       address of the fresh alloc has been saved as a value, but nothing has
       been loaded till this point; [unsure_allocs]: something that depends on
       the address of the fresh alloc has been saved as a value, and something
       has been loaded till this point. For each memory operation, we will save
       its dependent allocs and unsure allocs *)
    let body = Instruction.body_of block in
    let id_to_instructions = Instruction.tbl_of block in
    let memory_operations = Instruction.Id.Tbl.create 100 in
    let loads, stores, _, _, _ =
      DLL.fold_left body
        ~f:
          (fun (loads, stores, fresh_allocs, stored_allocs, unsure_allocs)
               instruction ->
          let id = Instruction.id instruction in
          if Instruction.is_alloc instruction
          then
            ( loads,
              stores,
              Instruction.Id.Set.add id fresh_allocs,
              stored_allocs,
              unsure_allocs )
          else
            let memory_operation = Memory_operation.init instruction in
            match memory_operation with
            | None ->
              if Instruction.preserves_alloc_freshness instruction
              then loads, stores, fresh_allocs, stored_allocs, unsure_allocs
              else
                ( loads,
                  stores,
                  Instruction.Id.Set.empty,
                  Instruction.Id.Set.empty,
                  Instruction.Id.Set.union fresh_allocs stored_allocs
                  |> Instruction.Id.Set.union unsure_allocs )
            | Some memory_operation -> (
              let get_dependent_allocs_of_arg arg_i =
                Dependency_graph.get_all_dependencies_of_arg dependency_graph id
                  ~arg_i
                |> Instruction.Id.Set.filter
                     (Instruction.is_alloc
                     << Instruction.Id.Tbl.find id_to_instructions)
              in
              let start_index =
                match memory_operation.op with Load -> 0 | Store -> 1
              in
              let dependent_allocs, _ =
                Array.fold_left
                  (fun (dependent_allocs, arg_i) _ ->
                    ( get_dependent_allocs_of_arg arg_i
                      |> Instruction.Id.Set.union dependent_allocs,
                      arg_i + 1 ))
                  (Instruction.Id.Set.empty, start_index)
                  (Memory_operation.memory_arguments memory_operation)
              in
              match memory_operation.op with
              | Load ->
                Instruction.Id.Tbl.add memory_operations id
                  { memory_operation with dependent_allocs; unsure_allocs };
                ( id :: loads,
                  stores,
                  fresh_allocs,
                  Instruction.Id.Set.empty,
                  Instruction.Id.Set.union stored_allocs unsure_allocs )
              | Store ->
                Instruction.Id.Tbl.add memory_operations id
                  { memory_operation with dependent_allocs; unsure_allocs };
                let new_stored_allocs =
                  Instruction.Id.Set.diff
                    (get_dependent_allocs_of_arg 0)
                    unsure_allocs
                in
                ( loads,
                  id :: stores,
                  Instruction.Id.Set.diff fresh_allocs new_stored_allocs,
                  Instruction.Id.Set.union stored_allocs new_stored_allocs,
                  unsure_allocs )))
        ~init:
          ( [],
            [],
            Instruction.Id.Set.empty,
            Instruction.Id.Set.empty,
            Instruction.Id.Set.empty )
    in
    { loads = List.rev loads; stores = List.rev stores; memory_operations }

  let can_cross t instruction_1 instruction_2 =
    let reg_array_to_set = Reg.Set.of_list << Array.to_list in
    let argument_set = reg_array_to_set << Instruction.arguments
    and affected_set instruction =
      Reg.Set.union
        (Instruction.results instruction |> reg_array_to_set)
        (Instruction.destroyed instruction |> reg_array_to_set)
    in
    let arguments_1 = argument_set instruction_1
    and affected_1 = affected_set instruction_1
    and arguments_2 = argument_set instruction_2
    and affected_2 = affected_set instruction_2 in
    if Reg.Set.disjoint affected_1 affected_2
       && Reg.Set.disjoint arguments_1 affected_2
       && Reg.Set.disjoint affected_1 arguments_2
    then
      let get_memory_operation instruction =
        Instruction.Id.Tbl.find_opt t.memory_operations
          (Instruction.id instruction)
      in
      match
        get_memory_operation instruction_1, get_memory_operation instruction_2
      with
      | None, _ | _, None ->
        Instruction.can_cross_loads_or_stores instruction_1
        || Instruction.can_cross_loads_or_stores instruction_2
      | Some memory_operation_1, Some memory_operation_2 -> (
        match memory_operation_1.op, memory_operation_2.op with
        | Load, Load -> true
        | Load, Store | Store, Load | Store, Store ->
          if Memory_operation.compare_addressing_modes_and_arguments
               memory_operation_1 memory_operation_2
             = 0
          then
            let check_direct_separation left_memory_operation
                right_memory_operation =
              match
                Memory_operation.offset_of left_memory_operation
                  right_memory_operation
              with
              | None -> false
              | Some offset ->
                offset
                > Cmm.width_in_bits
                    left_memory_operation.Memory_operation.memory_chunk
            in
            (* Case 1: address 1 is before address 2; case 2: address 2 is
               before address 1 *)
            check_direct_separation memory_operation_1 memory_operation_2
            || check_direct_separation memory_operation_2 memory_operation_1
          else
            let first_depends_on_some_alloc_second_certainly_does_not_depend_on
                (first : Memory_operation.t) (second : Memory_operation.t) =
              Instruction.Id.Set.is_empty
                (Instruction.Id.Set.diff first.dependent_allocs
                   (Instruction.Id.Set.union second.dependent_allocs
                      second.unsure_allocs))
              |> not
            in
            first_depends_on_some_alloc_second_certainly_does_not_depend_on
              memory_operation_1 memory_operation_2
            || first_depends_on_some_alloc_second_certainly_does_not_depend_on
                 memory_operation_2 memory_operation_1)
    else false

  let can_cross_lists t instructions1 instructions2 =
    let can_cross_list instruction1 =
      List.fold_left
        (fun can instruction2 -> can && can_cross t instruction1 instruction2)
        true instructions2
    in
    List.fold_left
      (fun can instruction1 -> can && can_cross_list instruction1)
      true instructions1

  let all_adjacent t instructions =
    let rec check_adjacent hd1 tl1 =
      match tl1 with
      | [] -> true
      | hd2 :: tl2 ->
        if Memory_operation.is_adjacent
             (get_memory_operation_exn t hd1)
             (get_memory_operation_exn t hd2)
        then check_adjacent hd2 tl2
        else false
    in
    check_adjacent (List.hd instructions) (List.tl instructions)

  let inter_independent t instructions =
    let rec check instructions =
      match instructions with
      | [] -> true
      | hd :: tl -> if can_cross_lists t [hd] tl then check tl else false
    in
    check instructions
  (* All pairs of instruction in the group are inter-independent *)

  let can_group ?(remove = false) t body instructions =
    (* Instructions can be made to be adjacent *)
    let starting_cell = DLL.hd_cell body in
    let rec can_be_together instruction_set group cell_option =
      if Instruction.Id.Set.is_empty instruction_set
      then true
      else
        match cell_option with
        | None -> false
        | Some cell ->
          let current_instruction = DLL.value cell in
          let current_instruction_id = Instruction.id current_instruction in
          let next = DLL.next cell in
          if Instruction.Id.Set.exists
               (Instruction.Id.equal current_instruction_id)
               instruction_set
          then (
            if remove then DLL.delete_curr cell;
            can_be_together
              (Instruction.Id.Set.remove current_instruction_id instruction_set)
              (current_instruction :: group)
              next)
          else if can_cross_lists t group [current_instruction]
          then can_be_together instruction_set group next
          else false
    in
    can_be_together (Instruction.Id.Set.of_list instructions) [] starting_cell

  let dump ppf ({ loads; stores; memory_operations } : t) =
    let open Format in
    let print_list list =
      List.iter
        (fun id ->
          let address = Instruction.Id.Tbl.find memory_operations id in
          Memory_operation.dump ppf address)
        list
    in
    fprintf ppf "\nmemory accesses (loads):\n";
    print_list loads;
    fprintf ppf "\nmemory accesses (stores):\n";
    print_list stores;
    fprintf ppf "\n"
end

module Seed : sig
  type t = Memory_accesses.Memory_operation.t list
  (* A seed is a group of stores instructions to adjacent memory addresses that
     can be made to be adjacent in the list of instructions *)

  val from_block : Cfg.basic_block -> Memory_accesses.t -> t list

  val dump : Format.formatter -> t list -> unit
end = struct
  type t = Memory_accesses.Memory_operation.t list

  let from_block (block : Cfg.basic_block) memory_accesses : t list =
    (* For each store instruction, it tries to form a seed with the closest
       stores after it, it will go down the DLL of instructions and tries to
       move the store instructions across the non-store instructions until all
       the store instructions are together *)
    let body = Instruction.body_of block in
    let all_stores = Memory_accesses.stores memory_accesses in
    List.filter_map
      (fun store_id ->
        let starting_cell =
          DLL.find_cell_opt body ~f:(fun instruction ->
              Instruction.id instruction |> Instruction.Id.equal store_id)
        in
        let rec find_stores n stores cell_option =
          if n = 0
          then Some (List.rev stores)
          else
            match cell_option with
            | None -> None
            | Some cell ->
              let instruction = DLL.value cell in
              if Instruction.is_store instruction
              then find_stores (n - 1) (instruction :: stores) (DLL.next cell)
              else find_stores n stores (DLL.next cell)
        in
        let starting_memory_operation =
          Memory_accesses.get_memory_operation_exn memory_accesses store_id
        in
        let width_in_bits =
          Memory_accesses.Memory_operation.width_in_bits
            starting_memory_operation
        in
        if width_in_bits = 128 (* No point "vectorizing" that *)
        then None
        else
          let items_in_vector = vector_width_in_bits / width_in_bits in
          let store_group = find_stores items_in_vector [] starting_cell in
          match store_group with
          | None -> None
          | Some stores ->
            let store_ids = List.map Instruction.id stores in
            if Memory_accesses.all_adjacent memory_accesses store_ids
               && Memory_accesses.can_group memory_accesses body store_ids
            then
              Some
                (List.map
                   (fun id ->
                     Memory_accesses.get_memory_operation_exn memory_accesses id)
                   store_ids)
            else None)
      all_stores

  let dump ppf (seeds : t list) =
    let open Format in
    let print_seed seed =
      List.iter
        (fun (address : Memory_accesses.Memory_operation.t) ->
          Memory_accesses.Memory_operation.dump ppf address)
        seed
    in
    let print_seeds seeds =
      List.iter
        (fun seed ->
          fprintf ppf "(";
          print_seed seed;
          fprintf ppf "\n)\n")
        seeds
    in
    fprintf ppf "\nseeds:\n";
    print_seeds seeds;
    fprintf ppf "\n"
end

module Computation_tree : sig
  module Node : sig
    type t =
      { vector_instructions : Simd_selection.vectorized_instruction list;
        instructions : Instruction.Id.t list;
        dependencies : Instruction.Id.t array;
        is_dependency_of : Instruction.Id.Set.t
      }
  end

  type t

  val all_instructions : t -> Instruction.Id.Set.t

  val all_nodes : t -> Node.t Instruction.Id.Tbl.t

  val from_block :
    Cfg.basic_block ->
    Dependency_graph.t ->
    Memory_accesses.t ->
    Seed.t list ->
    Cfg_with_infos.t ->
    t list

  val dump : Format.formatter -> t list -> Cfg.basic_block -> unit
end = struct
  module Node = struct
    type t =
      { vector_instructions : Simd_selection.vectorized_instruction list;
        instructions : Instruction.Id.t list;
        dependencies : Instruction.Id.t array;
            (* Only counts dependencies within the same computation tree *)
        is_dependency_of : Instruction.Id.Set.t
      }

    let init width instructions =
      match Instruction.to_vector_instructions width instructions with
      | None -> None
      | Some vector_instructions ->
        Some
          { vector_instructions;
            instructions = List.map Instruction.id instructions;
            dependencies = [||];
            is_dependency_of = Instruction.Id.Set.empty
          }
  end

  type t = Node.t Instruction.Id.Tbl.t
  (* The key is the id of the instruction where the node will be inserted, which
     is the last instruction in the node for now, we can change that later *)

  let init () : t = Instruction.Id.Tbl.create 100

  let all_instructions t =
    Instruction.Id.Tbl.fold
      (fun _ (node : Node.t) instructions ->
        Instruction.Id.Set.of_list node.instructions
        |> Instruction.Id.Set.union instructions)
      t Instruction.Id.Set.empty

  let all_nodes t = t

  let from_seed (block : Cfg.basic_block) dependency_graph memory_accesses
      cfg_with_infos seed =
    let body = Instruction.body_of block in
    let id_to_instructions = Instruction.tbl_of block in
    let find_instruction = Instruction.Id.Tbl.find id_to_instructions in
    let root =
      List.map
        (Instruction.id << Memory_accesses.Memory_operation.instruction)
        seed
    in
    let computation_tree = init () in
    let seed_width_in_bits =
      List.hd seed |> Memory_accesses.Memory_operation.width_in_bits
    in
    let rec build instruction_ids : Instruction.Id.t option =
      (* Recursively builds the computation tree and returns Some id of the root
         if possible, otherwise None *)
      (* Nodes must contain supported isomorphic instructions that are
         interindependent and do not depend on instructions outside the tree
         except for loads, which must be adjacent) *)
      let instructions = List.map find_instruction instruction_ids in
      let last_instruction =
        Instruction.find_last_instruction body instruction_ids
      in
      match Node.init seed_width_in_bits instructions with
      | None -> None
      | Some node -> (
        let key = Instruction.id last_instruction in
        match Instruction.Id.Tbl.find_opt computation_tree key with
        (* Is there another node with the same key already in the tree? *)
        | Some (old_node : Node.t) ->
          if List.equal Instruction.Id.equal node.instructions
               old_node.instructions
          then Some key
          else
            None
            (* The last instruction of the node is already in another node, if
               the other node is different from this node, we won't vectorize
               this for simplicity's sake *)
        | None -> (
          let all_option_array array =
            Array.to_list array |> all_option_list |> Option.map Array.of_list
          in
          let build_arg arg_i =
            match
              List.map
                (Dependency_graph.get_arg_dependency dependency_graph ~arg_i)
                instruction_ids
              |> all_option_list
            with
            | None -> None
            | Some new_instructions -> build new_instructions
          in
          let update_node_dependencies built_dependencies =
            match all_option_array built_dependencies with
            | None -> None
            | Some dependencies ->
              Instruction.Id.Tbl.replace computation_tree key
                { node with dependencies };
              Some key
          in
          match Instruction.op last_instruction with
          | None -> None
          | Some op -> (
            match op with
            | Load _ ->
              let load_width_in_bits =
                Instruction.id last_instruction
                |> Memory_accesses.get_memory_operation_exn memory_accesses
                |> Memory_accesses.Memory_operation.width_in_bits
              in
              if Memory_accesses.all_adjacent memory_accesses instruction_ids
                 && Memory_accesses.inter_independent memory_accesses
                      instructions
                 && load_width_in_bits = seed_width_in_bits
              then (
                Instruction.Id.Tbl.add computation_tree key node;
                Some key)
              else None
            | Store _ ->
              if Memory_accesses.all_adjacent memory_accesses instruction_ids
              then (
                Instruction.Id.Tbl.add computation_tree key node;
                let built_dependencies = [| build_arg 0 |] in
                update_node_dependencies built_dependencies)
              else None
            | Move | Const_int _ | Intop _ | Intop_imm _ | Specific _ ->
              if Memory_accesses.inter_independent memory_accesses instructions
              then (
                Instruction.Id.Tbl.add computation_tree key node;
                let built_dependencies =
                  Array.mapi
                    (fun arg_i _ -> build_arg arg_i)
                    (Instruction.arguments last_instruction)
                in
                update_node_dependencies built_dependencies)
              else None
            | Alloc _ | Reinterpret_cast _ | Static_cast _ | Spill | Reload
            | Const_float32 _ | Const_float _ | Const_symbol _ | Const_vec128 _
            | Stackoffset _ | Intop_atomic _ | Floatop _ | Csel _
            | Probe_is_enabled _ | Opaque | Begin_region | End_region
            | Name_for_debugger _ | Dls_get | Poll ->
              None)))
    in
    let is_valid computation_tree =
      (* Checks nodes can be grouped together, and instructions outside the tree
         do not depend on the tree *)
      let check cell =
        match
          DLL.value cell |> Instruction.id
          |> Instruction.Id.Tbl.find_opt computation_tree
        with
        | None -> true
        | Some (node : Node.t) ->
          Memory_accesses.can_group ~remove:true memory_accesses body
            node.instructions
      in
      let rec validate cell_option =
        match cell_option with
        | None -> true
        | Some cell -> if check cell then validate (DLL.prev cell) else false
      in
      if DLL.last_cell body |> validate
      then
        let tree_is_not_dependency_of_the_rest_of_body =
          let node_is_dependency_of (node : Node.t) =
            List.fold_right
              (fun instruction is_dependency_of ->
                Dependency_graph.get_is_dependency_of dependency_graph
                  instruction
                |> Instruction.Id.Set.union is_dependency_of)
              node.instructions Instruction.Id.Set.empty
          in
          let tree_is_dependency_of =
            Instruction.Id.Tbl.fold
              (fun _ node is_dependency_of ->
                node_is_dependency_of node
                |> Instruction.Id.Set.union is_dependency_of)
              computation_tree Instruction.Id.Set.empty
          in
          DLL.to_list body |> List.map Instruction.id
          |> Instruction.Id.Set.of_list
          |> Instruction.Id.Set.inter tree_is_dependency_of
          |> Instruction.Id.Set.is_empty
        in
        let tree_is_not_dependency_of_outside_body =
          let terminator_id = block.terminator.id in
          let live_before_terminator =
            (Cfg_with_infos.liveness_find cfg_with_infos terminator_id).before
          in
          let latest_changes_of_live =
            Reg.Set.fold
              (fun reg latest_changes ->
                match
                  Dependency_graph.latest_change
                    ~current:(Instruction.Id.of_int terminator_id)
                    reg block
                with
                | None -> latest_changes
                | Some instruction ->
                  Instruction.Id.Set.add
                    (Instruction.id instruction)
                    latest_changes)
              live_before_terminator Instruction.Id.Set.empty
          in
          let tree_instructions =
            Instruction.Id.Tbl.fold
              (fun _ (node : Node.t) instructions ->
                Instruction.Id.Set.of_list node.instructions
                |> Instruction.Id.Set.union instructions)
              computation_tree Instruction.Id.Set.empty
          in
          Instruction.Id.Set.inter tree_instructions latest_changes_of_live
          |> Instruction.Id.Set.is_empty
        in
        let seed_address_does_not_depend_on_tree =
          let instruction =
            List.hd seed |> Memory_accesses.Memory_operation.instruction
          in
          let rec find_address_dependencies n =
            if n < 1
            then Instruction.Id.Set.empty
            else
              let address_dependencies = find_address_dependencies (n - 1) in
              match
                Dependency_graph.get_arg_dependency dependency_graph
                  (Instruction.id instruction)
                  ~arg_i:n
              with
              | None -> address_dependencies
              | Some id -> Instruction.Id.Set.add id address_dependencies
          in
          let address_dependencies =
            find_address_dependencies
              (Array.length (Instruction.arguments instruction) - 1)
          in
          Instruction.Id.Set.inter
            (all_instructions computation_tree)
            address_dependencies
          |> Instruction.Id.Set.is_empty
        in
        tree_is_not_dependency_of_the_rest_of_body
        && tree_is_not_dependency_of_outside_body
        && seed_address_does_not_depend_on_tree
      else false
    in
    let set_is_dependency_of key dependency_id =
      let dependency = Instruction.Id.Tbl.find computation_tree dependency_id in
      Instruction.Id.Tbl.replace computation_tree dependency_id
        { dependency with
          is_dependency_of =
            Instruction.Id.Set.add key dependency.is_dependency_of
        }
    in
    let set_is_dependency_of_plural (instruction : Instruction.t) =
      let key = Instruction.id instruction in
      match Instruction.Id.Tbl.find_opt computation_tree key with
      | None -> ()
      | Some node -> Array.iter (set_is_dependency_of key) node.dependencies
    in
    let set_all_is_dependency_of () =
      DLL.iter block.body ~f:(fun instruction ->
          set_is_dependency_of_plural (Basic instruction))
    in
    match build root with
    | None -> None
    | Some _ ->
      if is_valid computation_tree
      then (
        set_all_is_dependency_of ();
        Some computation_tree)
      else None

  let from_block block dependency_graph memory_accesses seeds cfg_with_infos :
      t list =
    List.filter_map
      (from_seed block dependency_graph memory_accesses cfg_with_infos)
      seeds

  let dump ppf (trees : t list) (block : Cfg.basic_block) =
    let open Format in
    let print_node id node_option =
      match node_option with
      | None -> ()
      | Some (node : Node.t) ->
        fprintf ppf "\nNode key: %d\n" (Instruction.Id.to_int id);
        fprintf ppf "\nOperations:\n";
        List.iter
          (fun (simd_instruction : Simd_selection.vectorized_instruction) ->
            fprintf ppf "%a " Cfg.dump_basic (Cfg.Op simd_instruction.operation))
          node.vector_instructions;
        fprintf ppf "\ninstructions:\n";
        List.iter (fprintf ppf "%d " << Instruction.Id.to_int) node.instructions;
        fprintf ppf "\ndependencies:\n";
        Array.iter
          (fprintf ppf "%d " << Instruction.Id.to_int)
          node.dependencies;
        fprintf ppf "\nis dependency of:\n";
        Instruction.Id.Set.iter
          (fprintf ppf "%d " << Instruction.Id.to_int)
          node.is_dependency_of;
        fprintf ppf "\n"
    in
    let print_tree tree =
      DLL.iter block.body ~f:(fun instruction ->
          let id = Basic instruction |> Instruction.id in
          Instruction.Id.Tbl.find_opt tree id |> print_node id)
    in
    let print_trees trees =
      List.iter
        (fun tree ->
          fprintf ppf "(";
          print_tree tree;
          fprintf ppf ")\n\n")
        trees
    in
    fprintf ppf "\ncomputation trees:\n";
    print_trees trees;
    fprintf ppf "\n"
end

let reg_map = Numbers.Int.Tbl.create 100

let vectorize (block : Cfg.basic_block) all_trees =
  let rec find_independent_trees trees =
    (* CR-someday tip: This is fine for now because trees are independent with
       anything outside the tree except loads, so trees with no instructions in
       common are independent with each other. Will have to re-implement if
       trees have other external dependencies *)
    match trees with
    | [] -> [], Instruction.Id.Set.empty
    | hd :: tl ->
      let tree_instructions = Computation_tree.all_instructions hd in
      let good_trees, good_instructions = find_independent_trees tl in
      if Instruction.Id.Set.inter tree_instructions good_instructions
         |> Instruction.Id.Set.is_empty
      then
        ( hd :: good_trees,
          Instruction.Id.Set.union tree_instructions good_instructions )
      else good_trees, good_instructions
  in
  let good_trees, good_instructions = find_independent_trees all_trees in
  let good_nodes = Instruction.Id.Tbl.create 100 in
  List.iter
    (fun tree ->
      Computation_tree.all_nodes tree
      |> Instruction.Id.Tbl.to_seq
      |> Instruction.Id.Tbl.add_seq good_nodes)
    good_trees;
  let id_to_instructions = Instruction.tbl_of block in
  let add_vector_instructions_for_node cell (node : Computation_tree.Node.t) =
    let max_new_reg =
      Array.fold_left
        (fun current_max (simd_reg : Simd_selection.register) ->
          match simd_reg with
          | New n -> Int.max current_max n
          | Argument _ | Result _ | Original _ -> current_max)
        0
    in
    let all_max_new_reg =
      List.fold_left
        (fun current_max
             (vectrorized_instruction : Simd_selection.vectorized_instruction) ->
          Int.max current_max
            (Int.max
               (max_new_reg vectrorized_instruction.arguments)
               (max_new_reg vectrorized_instruction.results)))
        0
    in
    let new_regs =
      Array.init
        (all_max_new_reg node.vector_instructions + 1)
        (fun _ -> Reg.create Vec128)
    in
    let old_instruction =
      List.hd node.instructions |> Instruction.Id.Tbl.find id_to_instructions
    in
    let get_reg (reg : Reg.t) =
      match Numbers.Int.Tbl.find_opt reg_map reg.stamp with
      | Some reg -> reg
      | None ->
        let new_reg = Reg.create Vec128 in
        Numbers.Int.Tbl.add reg_map reg.stamp new_reg;
        new_reg
    in
    let create_instruction
        (simd_instruction : Simd_selection.vectorized_instruction) =
      let get_register (simd_reg : Simd_selection.register) =
        match simd_reg with
        | New n -> new_regs.(n)
        | Argument n ->
          let original_reg = (Instruction.arguments old_instruction).(n) in
          get_reg original_reg
        | Result n ->
          let original_reg = (Instruction.results old_instruction).(n) in
          get_reg original_reg
        | Original n ->
          let original_reg = (Instruction.arguments old_instruction).(n) in
          original_reg
      in
      match old_instruction with
      | Terminator _ -> assert false
      | Basic instruction ->
        { instruction with
          desc = Cfg.Op simd_instruction.operation;
          arg = Array.map get_register simd_instruction.arguments;
          res = Array.map get_register simd_instruction.results;
          id = Instruction.Id.next_available_id () |> Instruction.Id.to_int
        }
    in
    List.iter
      (fun simd_instruction ->
        create_instruction simd_instruction |> DLL.insert_before cell)
      node.vector_instructions
  in
  let rec add_vector_instructions cell_option =
    match cell_option with
    | None -> ()
    | Some cell ->
      (let instructon = Instruction.Basic (DLL.value cell) in
       match
         Instruction.id instructon |> Instruction.Id.Tbl.find_opt good_nodes
       with
       | None -> ()
       | Some node -> add_vector_instructions_for_node cell node);
      DLL.next cell |> add_vector_instructions
  in
  DLL.hd_cell block.body |> add_vector_instructions;
  DLL.filter_left block.body ~f:(fun instruction ->
      Instruction.Id.Set.exists
        (Instruction.Id.equal (Instruction.id (Instruction.Basic instruction)))
        good_instructions
      |> not)

let dump ppf (block : Cfg.basic_block) ~msg =
  let open Format in
  fprintf ppf "\nextra information %s\n" msg;
  fprintf ppf "body instruction count=%d\n" (DLL.length block.body);
  fprintf ppf "@."

let cfg ppf_dump cl =
  if !Flambda_backend_flags.dump_vectorize
  then Format.fprintf ppf_dump "*** Vectorization@.";
  Instruction.Id.reset_max_id ();
  Cfg_with_layout.iter_instructions cl
    ~instruction:(fun basic_instruction ->
      Basic basic_instruction |> Instruction.id |> Instruction.Id.update_max_id)
    ~terminator:(fun terminator_instruction ->
      Terminator terminator_instruction |> Instruction.id
      |> Instruction.Id.update_max_id);
  Numbers.Int.Tbl.clear reg_map;
  let layout = Cfg_with_layout.layout cl in
  let cfg_with_infos = Cfg_with_infos.make cl in
  DLL.iter layout ~f:(fun label ->
      let block = Cfg.get_block_exn (Cfg_with_layout.cfg cl) label in
      let instruction_count = DLL.length block.body in
      Format.fprintf ppf_dump "\nBlock %d (%d basic instructions):\n" label
        instruction_count;
      if instruction_count > 1000
      then
        Format.fprintf ppf_dump
          "more than 1000 instructions in basic block, cannot vectorize\n"
      else
        let dependency_graph = Dependency_graph.from_block block in
        if debug && !Flambda_backend_flags.dump_vectorize
        then Dependency_graph.dump ppf_dump dependency_graph block;
        let memory_accesses =
          Memory_accesses.from_block block dependency_graph
        in
        if debug && !Flambda_backend_flags.dump_vectorize
        then Memory_accesses.dump ppf_dump memory_accesses;
        let seeds = Seed.from_block block memory_accesses in
        if debug && !Flambda_backend_flags.dump_vectorize
        then Seed.dump ppf_dump seeds;
        let trees =
          Computation_tree.from_block block dependency_graph memory_accesses
            seeds cfg_with_infos
        in
        if debug && !Flambda_backend_flags.dump_vectorize
        then Computation_tree.dump ppf_dump trees block;
        if !Flambda_backend_flags.dump_vectorize
        then dump ppf_dump ~msg:"before vectorize" block;
        vectorize block trees;
        if !Flambda_backend_flags.dump_vectorize
        then dump ppf_dump ~msg:"after vectorize" block);
  cl
