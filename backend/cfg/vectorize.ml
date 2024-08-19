[@@@ocaml.warning "+a-40-41-42"]

(* Finds independent scalar operations within the same basic block and tries to
   use vector operations if possible *)
(* CR-soon tip: add documentation *)

module DLL = Flambda_backend_utils.Doubly_linked_list

let ( << ) f g x = f (g x)

module Instruction : sig
  (* CR-someday tip: consider moving this to cfg or at least have something
     similar there *)
  module Id : sig
    type t

    include Identifiable.S with type t := t

    val to_int : t -> int
  end

  type t =
    | Basic of Cfg.basic Cfg.instruction
    | Terminator of Cfg.terminator Cfg.instruction

  val id : t -> Id.t

  val arguments : t -> Reg.t Array.t

  val results : t -> Reg.t Array.t

  val destroyed : t -> Reg.t Array.t

  val print : Format.formatter -> t -> unit

  val print_reg : Format.formatter -> Reg.t -> unit
end = struct
  module Id = struct
    include Numbers.Int

    let to_int t = t
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

  let print ppf (instruction : t) : unit =
    match instruction with
    | Basic i -> Cfg.print_basic ppf i
    | Terminator i -> Cfg.print_terminator ppf i

  let print_reg ppf (reg : Reg.t) =
    Format.fprintf ppf "reg (%d,\"%s\")" reg.stamp (Reg.name reg)
end

module Adjacent_memory_accesses : sig
  module Memory_operation : sig
    type op =
      | Load
      | Store
    type t=
    { op : op;
      memory_chunk : Cmm.memory_chunk;
      addressing_mode : Arch.addressing_mode;
      instruction : Instruction.t
    }

    val create : Instruction.t -> t option

    val dump : Format.formatter -> t -> unit
  end

  type t

  val from_cfg : Cfg.t -> t Label.Tbl.t

  val dump : Format.formatter -> t Label.Tbl.t -> Cfg_with_layout.t -> unit
end = struct
  module Memory_operation = struct
    type op =
      | Load
      | Store

    type t =
      { op : op;
        memory_chunk : Cmm.memory_chunk;
        addressing_mode : Arch.addressing_mode;
        instruction : Instruction.t
      }

    let create_load (basic_instruction : Cfg.basic Cfg.instruction) : t option =
      let desc = basic_instruction.desc in
      let instruction = Instruction.Basic basic_instruction in
      match desc with
      | Op op -> (
        match op with
        | Load { memory_chunk; addressing_mode; _ } ->
          Some { op = Load; memory_chunk; addressing_mode; instruction }
        | Store _ -> None
        | Move | Reinterpret_cast _ | Static_cast _ | Spill | Reload
        | Const_int _ | Const_float32 _ | Const_float _ | Const_symbol _
        | Const_vec128 _ | Stackoffset _ | Intop _ | Intop_imm _
        | Intop_atomic _ | Floatop _ | Csel _ | Probe_is_enabled _ | Opaque
        | Begin_region | End_region | Specific _ | Name_for_debugger _ | Dls_get
        | Poll | Alloc _ ->
          None)
      | Reloadretaddr | Pushtrap _ | Poptrap | Prologue | Stack_check _ -> None

    let create_store (basic_instruction : Cfg.basic Cfg.instruction) : t option
        =
      let desc = basic_instruction.desc in
      let instruction = Instruction.Basic basic_instruction in
      match desc with
      | Op op -> (
        match op with
        | Load _ -> None
        | Store (memory_chunk, addressing_mode, _) ->
          Some { op = Store; memory_chunk; addressing_mode; instruction }
        | Move | Reinterpret_cast _ | Static_cast _ | Spill | Reload
        | Const_int _ | Const_float32 _ | Const_float _ | Const_symbol _
        | Const_vec128 _ | Stackoffset _ | Intop _ | Intop_imm _
        | Intop_atomic _ | Floatop _ | Csel _ | Probe_is_enabled _ | Opaque
        | Begin_region | End_region | Specific _ | Name_for_debugger _ | Dls_get
        | Poll | Alloc _ ->
          None)
      | Reloadretaddr | Pushtrap _ | Poptrap | Prologue | Stack_check _ -> None

    let create (instruction : Instruction.t) : t option =
      match instruction with
      | Basic basic_instruction -> (
        let desc = basic_instruction.desc in
        match desc with
        | Op op -> (
          match op with
          | Load { memory_chunk; addressing_mode; _ } ->
            Some { op = Load; memory_chunk; addressing_mode; instruction }
          | Store (memory_chunk, addressing_mode, _) ->
            Some { op = Store; memory_chunk; addressing_mode; instruction }
          | Move | Reinterpret_cast _ | Static_cast _ | Spill | Reload
          | Const_int _ | Const_float32 _ | Const_float _ | Const_symbol _
          | Const_vec128 _ | Stackoffset _ | Intop _ | Intop_imm _
          | Intop_atomic _ | Floatop _ | Csel _ | Probe_is_enabled _ | Opaque
          | Begin_region | End_region | Specific _ | Name_for_debugger _
          | Dls_get | Poll | Alloc _ ->
            None)
        | Reloadretaddr | Pushtrap _ | Poptrap | Prologue | Stack_check _ ->
          None)
      | Terminator _ -> None

    let arguments (t : t) =
      let arguments = Instruction.arguments t.instruction in
      match t.op with
      | Load -> arguments
      | Store -> Array.sub arguments 1 (Array.length arguments - 1)

    let width_of (memory_chunk : Cmm.memory_chunk) =
      match memory_chunk with
      | Byte_unsigned | Byte_signed -> 1
      | Sixteen_unsigned | Sixteen_signed -> 2
      | Thirtytwo_unsigned | Thirtytwo_signed | Single _ -> 4
      | Word_int | Word_val | Double -> 8
      | Onetwentyeight_unaligned | Onetwentyeight_aligned -> 16

    let print_memory_chunk ppf (t : t) =
      let open Format in
      let str =
        match t.memory_chunk with
        | Byte_unsigned -> "Byte_unsigned"
        | Byte_signed -> "Byte_signed"
        | Sixteen_unsigned -> "Sixteen_unsigned"
        | Sixteen_signed -> "Sixteen_signed"
        | Thirtytwo_unsigned -> "Thirtytwo_unsigned"
        | Thirtytwo_signed -> "Thirtytwo_signed"
        | Single _ -> "Single"
        | Word_int -> "Word_int"
        | Word_val -> "Word_val"
        | Double -> "Double"
        | Onetwentyeight_unaligned -> "Onetwentyeight_unaligned"
        | Onetwentyeight_aligned -> "Onetwentyeight_aligned"
      in
      fprintf ppf "%s (length %d)" str (width_of t.memory_chunk)

    let dump ppf (t : t) =
      let open Format in
      let instruction = t.instruction in
      fprintf ppf "\nInstruction %d: %a (%a, %a)"
        (Instruction.id instruction |> Instruction.Id.to_int)
        Instruction.print instruction print_memory_chunk t
        (Arch.print_addressing Instruction.print_reg t.addressing_mode)
        (arguments t)

        let compare_arguments (t1 : t) (t2 : t) =
          let arguments_1 = arguments t1 in
              let arguments_2 = arguments t2 in
              Array.combine arguments_1 arguments_2
              |> Array.fold_left
                   (fun result ((arg1, arg2) : Reg.t * Reg.t) ->
                     if result = 0 then Reg.compare arg1 arg2 else result)
                   0
    let compare_addressing_modes_and_arguments (t1 : t) (t2 : t) =
      let addressing_mode_comparison =
        Arch.addressing_compare t1.addressing_mode t2.addressing_mode
      in
      if addressing_mode_comparison = 0
      then
        let arguments_comparison =
          compare_arguments t1 t2
        in
        arguments_comparison
      else addressing_mode_comparison

    let compare (t1 : t) (t2 : t) =
      let addressing_mode_and_arguments_comparison =
        compare_addressing_modes_and_arguments t1 t2
      in
      if addressing_mode_and_arguments_comparison = 0
      then
          let displ_comparison =
            match
              Arch.addressing_displ_compare t1.addressing_mode
                t2.addressing_mode
            with
            | Some offset -> offset
            | None -> assert false
          in
          if displ_comparison = 0
          then
            (Instruction.id t1.instruction |> Instruction.Id.to_int)
            - (Instruction.id t2.instruction |> Instruction.Id.to_int)
          else displ_comparison
      else addressing_mode_and_arguments_comparison

    let offset_of (t1 : t) (t2 : t) =
      let addressing_mode_and_arguments_comparison =
        compare_addressing_modes_and_arguments t1 t2
      in
      if addressing_mode_and_arguments_comparison = 0
      then Arch.addressing_offset t1.addressing_mode t2.addressing_mode
      else None

    let is_adjacent (t1 : t) (t2 : t) =
      let res =
        if Cmm.equal_memory_chunk t1.memory_chunk t2.memory_chunk
        then
          let width = width_of t1.memory_chunk in
          let offset_option = offset_of t1 t2 in
          match offset_option with
          | None -> false
          | Some offset -> width = offset
        else false
      in
      res
  end

  type t =
    { load_runs : Memory_operation.t list list;
      store_runs : Memory_operation.t list list
    }

  let get_sorted_addresses (block : Cfg.basic_block)
      (address_from_instruction :
        Cfg.basic Cfg.instruction -> Memory_operation.t option) =
    let body = DLL.to_list block.body in
    let addresses = List.filter_map address_from_instruction body in
    List.sort Memory_operation.compare addresses

  let from_block (block : Cfg.basic_block) : t =
    let find_runs address_from_instruction =
      let sorted_addresses =
        get_sorted_addresses block address_from_instruction
      in
      let address_runs =
        List.fold_right
          (fun (address : Memory_operation.t) runs ->
            match runs with
            | runs_hd :: runs_tl -> (
              match runs_hd with
              | run_hd :: _ ->
                if Memory_operation.is_adjacent address run_hd
                then (address :: runs_hd) :: runs_tl
                else [address] :: runs
              | [] -> assert false)
            | [] -> [[address]])
          sorted_addresses []
      in
      let address_runs_longer_than_1 =
        List.filter (fun run -> List.length run > 1) address_runs
      in
      address_runs_longer_than_1
    in
    let load_runs = find_runs Memory_operation.create_load in
    let store_runs = find_runs Memory_operation.create_store in
    { load_runs; store_runs }

  let from_cfg (cfg : Cfg.t) : t Label.Tbl.t =
    Label.Tbl.map cfg.blocks from_block

  let dump ppf (block_to_runs : t Label.Tbl.t)
      (cfg_with_layout : Cfg_with_layout.t) =
    let open Format in
    let print_run run =
      List.iter
        (fun (address : Memory_operation.t) ->
          Memory_operation.dump ppf address)
        run
    in
    let print_runs message runs =
      fprintf ppf message;
      List.iter
        (fun run ->
          fprintf ppf "\n(";
          print_run run;
          fprintf ppf "\n)\n")
        runs
    in
    let print_block { load_runs; store_runs } =
      print_runs "\nAdjacent memory accesses for load:\n" load_runs;
      print_runs "\nAdjacent memory accesses for store:\n" store_runs
    in
    fprintf ppf "\nadjacent memory accesses in each basic block of %s:\n"
      (Cfg_with_layout.cfg cfg_with_layout |> Cfg.fun_name);
    DLL.iter (Cfg_with_layout.layout cfg_with_layout) ~f:(fun label ->
        fprintf ppf "\nBlock %d:\n" label;
        Label.Tbl.find block_to_runs label |> print_block)
end

module Dependency_graph : sig
  (* The dependency graph shows dependencies between instructions within the
     same basic block *)
  type t

  val from_cfg : Cfg.t -> t

  val dump : Format.formatter -> t -> Cfg_with_layout.t -> unit
end = struct
  module Node = struct
    module Reg_node = struct
      type t =
        { reg : Reg.t;
          depends_on : Instruction.Id.t option
        }

      let init reg : t = { reg; depends_on = None }
    end

    type t =
      { instruction : Instruction.t;
        reg_nodes : Reg_node.t array;
        depends_on : Instruction.Id.Set.t;
        is_dependency_of : Instruction.Id.Set.t
      }

    let init instruction : t =
      let arguments = Instruction.arguments instruction in
      { instruction;
        reg_nodes =
          Array.init (Array.length arguments) (fun i ->
              arguments.(i) |> Reg_node.init);
        depends_on = Instruction.Id.Set.empty;
        is_dependency_of = Instruction.Id.Set.empty
      }
  end

  type t = Node.t Instruction.Id.Tbl.t

  let add = Instruction.Id.Tbl.add

  let find = Instruction.Id.Tbl.find

  let replace = Instruction.Id.Tbl.replace

  let init () : t = Instruction.Id.Tbl.create 100

  let from_basic_block (block : Cfg.basic_block) ~(dependency_graph : t) =
    let is_changed_in instruction reg =
      Array.exists (Reg.same reg) (Instruction.results instruction)
      || Array.exists (Reg.same reg) (Instruction.destroyed instruction)
    in
    (* CR-soon tip: break it into 2 parts to find the instruction we want then
       go up from there. (currently it loops from the end and changes the answer
       back to None when it encounters the same instruction) *)
    let latest_change ~(current : Instruction.Id.t) (reg : Reg.t) =
      DLL.fold_right block.body
        ~f:(fun basic_instruction latest ->
          let instruction = Instruction.Basic basic_instruction in
          if Instruction.Id.equal current (Instruction.id instruction)
          then None
          else if Option.is_none latest && is_changed_in instruction reg
          then Some instruction
          else latest)
        ~init:None
    in
    let add_arg_dependency instruction arg_i arg =
      let id = Instruction.id instruction in
      let dependency = latest_change ~current:id arg in
      let node = find dependency_graph id in
      let reg_node = node.reg_nodes.(arg_i) in
      node.reg_nodes.(arg_i)
        <- { reg_node with
             depends_on =
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
      let instruction_dependencies =
        Array.fold_left
          (fun dependencies arg_i ->
            Option.fold ~none:dependencies
              ~some:(fun dependency ->
                Instruction.Id.Set.add dependency dependencies)
              (find dependency_graph id).reg_nodes.(arg_i).depends_on)
          Instruction.Id.Set.empty arg_indices
      in
      let node = find dependency_graph id in
      replace dependency_graph id
        { node with depends_on = instruction_dependencies }
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
          is_dependency_of =
            Instruction.Id.Set.add instruction_id dependency.is_dependency_of
        }
    in
    let set_is_dependency_of_plural (instruction : Instruction.t) =
      let id = Instruction.id instruction in
      let node = find dependency_graph id in
      Instruction.Id.Set.iter (set_is_dependency_of id) node.depends_on
    in
    let set_all_is_dependency_of () =
      DLL.iter block.body ~f:(fun instruction ->
          set_is_dependency_of_plural (Basic instruction))
    in
    add_all_dependencies ();
    set_all_is_dependency_of ()

  let from_cfg (cfg : Cfg.t) : t =
    let dependency_graph = init () in
    Cfg.iter_blocks cfg ~f:(fun _ block ->
        from_basic_block block ~dependency_graph);
    dependency_graph

  let dump ppf (t : t) cfg_with_layout =
    let open Format in
    let print_reg_node arg_i (reg_node : Node.Reg_node.t) =
      let dependency =
        Option.fold ~none:"none"
          ~some:(sprintf "instruction %d" << Instruction.Id.to_int)
          reg_node.depends_on
      in
      fprintf ppf "\nargument %d, %a depends on %s\n" arg_i
        Instruction.print_reg reg_node.reg dependency
    in
    let print_node (instruction : Instruction.t) =
      let id = Instruction.id instruction in
      let node = find t id in
      fprintf ppf "\n%d:\n"
        (Instruction.id node.instruction |> Instruction.Id.to_int);
      Instruction.print ppf instruction;
      Array.iteri print_reg_node node.reg_nodes;
      fprintf ppf "\ndepends on:\n";
      Instruction.Id.Set.iter
        (fprintf ppf "%d " << Instruction.Id.to_int)
        node.depends_on;
      fprintf ppf "\nis a dependency of:\n";
      Instruction.Id.Set.iter
        (fprintf ppf "%d " << Instruction.Id.to_int)
        node.is_dependency_of;
      fprintf ppf "\narg dependencies:\n";
      fprintf ppf "\n"
    in
    fprintf ppf "\ndependency graph:\n";
    Cfg_with_layout.iter_instructions cfg_with_layout
      ~instruction:(fun instruction -> print_node (Basic instruction))
      ~terminator:(fun instruction -> print_node (Terminator instruction))
end

module Seed = struct
  type t = Adjacent_memory_accesses.Memory_operation.t list

  let can_swap_adjacent_memory_operations
      (memory_operation_1 : Adjacent_memory_accesses.Memory_operation.t)
      (memory_operation_2 : Adjacent_memory_accesses.Memory_operation.t) =
    match memory_operation_1.op, memory_operation_2.op with
    | Load, Load -> true
    |Load, Store |Store,Load|Store,Store -> false (* do something *)

  let can_swap_adjacent first_instruction second_instruction =
    let reg_array_to_set = Reg.Set.of_list << Array.to_list in
    let argument_set = reg_array_to_set << Instruction.arguments
    and affected_set instruction =
      Reg.Set.union
        (Instruction.results instruction |> reg_array_to_set)
        (Instruction.destroyed instruction |> reg_array_to_set)
    in
    let first_arguments = argument_set first_instruction
    and first_affected = affected_set first_instruction
    and second_arguments = argument_set second_instruction
    and second_affected = affected_set second_instruction in
    if Reg.Set.disjoint first_affected second_affected
       && Reg.Set.disjoint first_arguments second_affected
       && Reg.Set.disjoint first_affected second_arguments
    then
      let first_memory_operation =
        Adjacent_memory_accesses.Memory_operation.create first_instruction
      and second_memory_operation =
        Adjacent_memory_accesses.Memory_operation.create second_instruction
      in
      match first_memory_operation, second_memory_operation with
      | Some first_memory_operation, Some second_memory_operation ->
        can_swap_adjacent_memory_operations first_memory_operation
          second_memory_operation
      | None, _ -> true
      | _, None -> true
    else false

  let from_block block =
    ignore block;
    ignore can_swap_adjacent;
    []

  let from_cfg (cfg : Cfg.t) : t list Label.Tbl.t =
    Label.Tbl.map cfg.blocks from_block

  let dump ppf (block_to_seeds : t list Label.Tbl.t)
      (cfg_with_layout : Cfg_with_layout.t) =
    let open Format in
    let print_seed seed =
      List.iter
        (fun (address : Adjacent_memory_accesses.Memory_operation.t) ->
          Adjacent_memory_accesses.Memory_operation.dump ppf address)
        seed
    in
    let print_seed seeds =
      List.iter
        (fun seed ->
          fprintf ppf "\n(";
          print_seed seed;
          fprintf ppf "\n)\n")
        seeds
    in
    fprintf ppf "\nseeds in each basic block of %s:\n"
      (Cfg_with_layout.cfg cfg_with_layout |> Cfg.fun_name);
    DLL.iter (Cfg_with_layout.layout cfg_with_layout) ~f:(fun label ->
        fprintf ppf "\nBlock %d:\n" label;
        Label.Tbl.find block_to_seeds label |> print_seed)
end

let count_body_instructions cfg =
  Cfg.fold_body_instructions cfg ~f:(fun sum _ -> sum + 1) ~init:0

let dump ppf cfg_with_layout ~msg =
  let open Format in
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  fprintf ppf "\nvectorization extra information for %s\n" msg;
  fprintf ppf "%s\n" (Cfg.fun_name cfg);
  let block_count = Label.Tbl.length cfg.blocks in
  fprintf ppf "blocks.length=%d\n" block_count;
  let body_instruction_count = count_body_instructions cfg in
  fprintf ppf "body instruction count=%d\n" body_instruction_count;
  fprintf ppf "terminator instruction count=%d\n" block_count;
  fprintf ppf "body and terminator instruction count=%d\n"
    (body_instruction_count + block_count);
  fprintf ppf "@."

let cfg ppf_dump cl =
  if !Flambda_backend_flags.dump_vectorize
  then Format.fprintf ppf_dump "*** Vectorization@.";
  let cfg = Cfg_with_layout.cfg cl in
  let body_instruction_count = count_body_instructions cfg in
  (if body_instruction_count > 1000
  then
    Format.fprintf ppf_dump
      "more than 1000 instructions in basic block, cannot vectorize"
  else
    let dependency_graph = Dependency_graph.from_cfg cfg in
    if !Flambda_backend_flags.dump_vectorize
    then Dependency_graph.dump ppf_dump dependency_graph cl;
    let adjacent_memory_accesses = Adjacent_memory_accesses.from_cfg cfg in
    if !Flambda_backend_flags.dump_vectorize
    then Adjacent_memory_accesses.dump ppf_dump adjacent_memory_accesses cl;
    let seeds = Seed.from_cfg cfg in
    if !Flambda_backend_flags.dump_vectorize then Seed.dump ppf_dump seeds cl);
  if !Flambda_backend_flags.dump_vectorize then dump ppf_dump ~msg:"" cl;
  cl
