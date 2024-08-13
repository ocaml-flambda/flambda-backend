[@@@ocaml.warning "+a-40-41-42"]

(* Finds adjacent memory chunks and tries to use vector operations if
   possible *)

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
      fprintf ppf "\nargument %d (reg %d) depends on %s\n" arg_i
        reg_node.reg.stamp dependency
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

module Adjacent_memory_accesses = struct
  module Address = struct
    type op =
      | Load
      | Store

    type t =
      { op : op;
        memory_chunk : Cmm.memory_chunk;
        addressing_mode : Arch.addressing_mode;
        instruction : Instruction.t
      }

    let create_loads (basic_instruction : Cfg.basic Cfg.instruction) : t option
        =
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

    let create_stores (basic_instruction : Cfg.basic Cfg.instruction) : t option
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

    let args (t : t) =
      let args = Instruction.arguments t.instruction in
      match t.op with
      | Load -> args
      | Store -> Array.sub args 1 (Array.length args - 1)

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
      fprintf ppf "%s" str

    let print_addressing ppf (t : t) =
      let open Format in
      let addr = t.addressing_mode in
      let arg = args t in
      let printreg ppf (reg : Reg.t) =
        fprintf ppf "%d,\"%s\"" reg.stamp (Reg.name reg)
      in
      match addr with
      | Ibased (s, _glob, n) -> fprintf ppf "ibased \"%s\" + %i" s n
      | Iindexed n ->
        let idx = Printf.sprintf " + %i" n in
        fprintf ppf "iindexed %a%s" printreg arg.(0) idx
      | Iindexed2 n ->
        let idx = Printf.sprintf " + %i" n in
        fprintf ppf "iindexed2 %a + %a%s" printreg arg.(0) printreg arg.(1) idx
      | Iscaled (scale, n) ->
        let idx = Printf.sprintf " + %i" n in
        fprintf ppf "iscaled %a  * %i%s" printreg arg.(0) scale idx
      | Iindexed2scaled (scale, n) ->
        let idx = Printf.sprintf " + %i" n in
        fprintf ppf "iindexed2scaled %a + %a * %i%s" printreg arg.(0) printreg
          arg.(1) scale idx

    let dump ppf (t : t) =
      let open Format in
      let instruction = t.instruction in
      fprintf ppf "\nInstruction %d: %a (%a, %a)"
        (Instruction.id instruction |> Instruction.Id.to_int)
        Instruction.print instruction print_memory_chunk t print_addressing
        t

    let compare (t1 : t) (t2 : t) =
      let addressing_mode_1 = t1.addressing_mode in
      let addressing_mode_2 = t2.addressing_mode in
      let args_1 = args t1 in
      let args_2 = args t2 in
      let compare_addressing_modes =
        match addressing_mode_1, addressing_mode_2 with
        | Ibased (symbol1, global1, n1), Ibased (symbol2, global2, n2) -> (
          match global1, global2 with
          | Global, Global | Local, Local ->
            if symbol1 < symbol2
            then -1
            else if symbol1 > symbol2
            then 1
            else n1 - n2
          | Global, Local -> -1
          | Local, Global -> 1)
        | Ibased _, _ -> -1
        | _, Ibased _ -> 1
        | Iindexed n1, Iindexed n2 ->
          let arg0_1 = Array.get args_1 0 in
          let arg0_2 = Array.get args_2 0 in
          let arg0_compare = arg0_1.stamp - arg0_2.stamp in
          if arg0_compare = 0 then n1 - n2 else arg0_compare
        | Iindexed _, _ -> -1
        | _, Iindexed _ -> 1
        | Iindexed2 n1, Iindexed2 n2 ->
          let arg0_1 = Array.get args_1 0 in
          let arg0_2 = Array.get args_2 0 in
          let arg0_compare = arg0_1.stamp - arg0_2.stamp in
          if arg0_compare = 0
          then
            let arg1_1 = Array.get args_1 1 in
            let arg1_2 = Array.get args_2 1 in
            let arg1_compare = arg1_1.stamp - arg1_2.stamp in
            if arg1_compare = 0 then n1 - n2 else arg1_compare
          else arg0_compare
        | Iindexed2 _, _ -> -1
        | _, Iindexed2 _ -> 1
        | Iscaled (scale1, n1), Iscaled (scale2, n2) ->
          let arg0_1 = Array.get args_1 0 in
          let arg0_2 = Array.get args_2 0 in
          let arg0_compare = arg0_1.stamp - arg0_2.stamp in
          if arg0_compare = 0
          then
            let scale_compare = scale1 - scale2 in
            if scale_compare = 0 then n1 - n2 else scale_compare
          else arg0_compare
        | Iscaled _, _ -> -1
        | _, Iscaled _ -> 1
        | Iindexed2scaled (scale1, n1), Iindexed2scaled (scale2, n2) ->
          let arg0_1 = Array.get args_1 0 in
          let arg0_2 = Array.get args_2 0 in
          let arg0_compare = arg0_1.stamp - arg0_2.stamp in
          if arg0_compare = 0
          then
            let arg1_1 = Array.get args_1 1 in
            let arg1_2 = Array.get args_2 1 in
            let arg1_compare = arg1_1.stamp - arg1_2.stamp in
            if arg1_compare = 0
            then
              let scale_compare = scale1 - scale2 in
              if scale_compare = 0 then n1 - n2 else scale_compare
            else arg1_compare
          else arg0_compare
      in
      if compare_addressing_modes = 0
      then (Instruction.id t1.instruction |> Instruction.Id.to_int) - (Instruction.id t2.instruction |> Instruction.Id.to_int)
      else compare_addressing_modes

    let offset_of (t1 : t) (t2 : t) =
      let addressing_mode_1 = t1.addressing_mode in
      let addressing_mode_2 = t2.addressing_mode in
      let args_1 = args t1 in
      let args_2 = args t2 in
      match addressing_mode_1, addressing_mode_2 with
      | Ibased (symbol1, global1, n1), Ibased (symbol2, global2, n2) -> (
        match global1, global2 with
        | Global, Global | Local, Local ->
          if symbol1 = symbol2 then Some (n2 - n1) else None
        | Global, Local | Local, Global -> None)
      | Iindexed n1, Iindexed n2 ->
        let arg0_1 = Array.get args_1 0 in
        let arg0_2 = Array.get args_2 0 in
        let arg0_compare = arg0_1.stamp - arg0_2.stamp in
        if arg0_compare = 0 then Some (n2 - n1) else None
      | Iindexed2 n1, Iindexed2 n2 ->
        let arg0_1 = Array.get args_1 0 in
        let arg0_2 = Array.get args_2 0 in
        let arg0_compare = arg0_1.stamp - arg0_2.stamp in
        if arg0_compare = 0
        then
          let arg1_1 = Array.get args_1 1 in
          let arg1_2 = Array.get args_2 1 in
          let arg1_compare = arg1_1.stamp - arg1_2.stamp in
          if arg1_compare = 0 then Some (n2 - n1) else None
        else None
      | Iscaled (scale1, n1), Iscaled (scale2, n2) ->
        let arg0_1 = Array.get args_1 0 in
        let arg0_2 = Array.get args_2 0 in
        let arg0_compare = arg0_1.stamp - arg0_2.stamp in
        if arg0_compare = 0
        then
          let scale_compare = scale1 - scale2 in
          if scale_compare = 0 then Some (n2 - n1) else None
        else None
      | Iindexed2scaled (scale1, n1), Iindexed2scaled (scale2, n2) ->
        let arg0_1 = Array.get args_1 0 in
        let arg0_2 = Array.get args_2 0 in
        let arg0_compare = arg0_1.stamp - arg0_2.stamp in
        if arg0_compare = 0
        then
          let arg1_1 = Array.get args_1 1 in
          let arg1_2 = Array.get args_2 1 in
          let arg1_compare = arg1_1.stamp - arg1_2.stamp in
          if arg1_compare = 0
          then
            let scale_compare = scale1 - scale2 in
            if scale_compare = 0 then Some (n2 - n1) else None
          else None
        else None
      | Ibased _, _ -> None
      | Iindexed _, _ -> None
      | Iindexed2 _, _ -> None
      | Iscaled _, _ -> None
      | Iindexed2scaled _, _ -> None

    let width_of (memory_chunk : Cmm.memory_chunk) =
      match memory_chunk with
      | Byte_unsigned | Byte_signed -> 1
      | Sixteen_unsigned | Sixteen_signed -> 2
      | Thirtytwo_unsigned | Thirtytwo_signed | Single _ -> 4
      | Word_int | Word_val | Double -> 8
      | Onetwentyeight_unaligned | Onetwentyeight_aligned -> 16

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
    { load_runs : Address.t list list;
      store_runs : Address.t list list
    }

  let from_block (block : Cfg.basic_block) : t =
    let body = DLL.to_list block.body in
    let loads = List.filter_map Address.create_loads body in
    let stores = List.filter_map Address.create_stores body in
    let find_runs addresses : Address.t list list =
      let sorted_addresses = List.sort Address.compare addresses in
      let address_runs =
        List.fold_right
          (fun (address : Address.t) runs ->
            match runs with
            | runs_hd :: runs_tl -> (
              match runs_hd with
              | run_hd :: _ ->
                if Address.is_adjacent address run_hd
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
    let load_runs = find_runs loads in
    let store_runs = find_runs stores in
    { load_runs; store_runs }

  let from_cfg (cfg : Cfg.t) : t Label.Tbl.t =
    Label.Tbl.map cfg.blocks from_block

  let dump ppf (block_to_runs : t Label.Tbl.t)
      (cfg_with_layout : Cfg_with_layout.t) =
    let open Format in
    let print_run run =
      List.iter (fun (address : Address.t) -> Address.dump ppf address) run
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

let dump ppf cfg_with_layout ~msg =
  let open Format in
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  fprintf ppf "\nvectorization extra information for %s\n" msg;
  fprintf ppf "%s\n" (Cfg.fun_name cfg);
  let block_count = Label.Tbl.length cfg.blocks in
  fprintf ppf "blocks.length=%d\n" block_count;
  let body_instruction_count =
    Cfg.fold_body_instructions cfg ~f:(fun sum _ -> sum + 1) ~init:0
  in
  fprintf ppf "body instruction count=%d\n" body_instruction_count;
  fprintf ppf "terminator instruction count=%d\n" block_count;
  fprintf ppf "body and terminator instruction count=%d\n"
    (body_instruction_count + block_count);
  fprintf ppf "@."

let cfg ppf_dump cl =
  if !Flambda_backend_flags.dump_vectorize
  then Format.fprintf ppf_dump "*** Vectorization@.";
  let cfg = Cfg_with_layout.cfg cl in
  let dependency_graph = Dependency_graph.from_cfg cfg in
  if !Flambda_backend_flags.dump_vectorize
  then Dependency_graph.dump ppf_dump dependency_graph cl;
  let adjacent_memory_accesses = Adjacent_memory_accesses.from_cfg cfg in
  if !Flambda_backend_flags.dump_vectorize
  then Adjacent_memory_accesses.dump ppf_dump adjacent_memory_accesses cl;
  if !Flambda_backend_flags.dump_vectorize then dump ppf_dump ~msg:"" cl;
  cl
