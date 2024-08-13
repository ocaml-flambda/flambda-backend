[@@@ocaml.warning "+a-40-41-42"]

(* Finds adjacent memory chunks and tries to use vector operations if
   possible *)

module DLL = Flambda_backend_utils.Doubly_linked_list

module Instruction = struct
  module Id = struct
    include Numbers.Int
  end

  type t =
    [ `Basic of Cfg.basic Cfg.instruction
    | `Terminator of Cfg.terminator Cfg.instruction ]

  let id (instruction : t) : Id.t =
    match instruction with
    | `Basic instruction -> instruction.id
    | `Terminator instruction -> instruction.id

  let args (instruction : t) : Reg.t Array.t =
    match instruction with
    | `Basic instruction -> instruction.arg
    | `Terminator instruction -> instruction.arg

  let ress (instruction : t) : Reg.t Array.t =
    match instruction with
    | `Basic instruction -> instruction.res
    | `Terminator instruction -> instruction.res

  let destroyed (instruction : t) : Reg.t Array.t =
    match instruction with
    | `Basic instruction -> Proc.destroyed_at_basic instruction.desc
    | `Terminator instruction -> Proc.destroyed_at_terminator instruction.desc
end

module Dependency_graph = struct
  (* The dependency graph shows dependencies between instructions within the
     same basic block *)
  module Node = struct
    type t =
      { id : Instruction.Id.t;
        out_edges : Instruction.Id.Set.t;
        in_edges : Instruction.Id.Set.t
      }

    let init id : t =
      { id;
        out_edges = Instruction.Id.Set.empty;
        in_edges = Instruction.Id.Set.empty
      }
  end

  type t = Node.t Instruction.Id.Tbl.t

  let init () : t = Instruction.Id.Tbl.create 100

  let from_basic_block (block : Cfg.basic_block) =
    let dependency_graph = init () in
    let is_changed_in instruction reg =
      Array.exists (Reg.same reg) (Instruction.ress instruction)
      || Array.exists (Reg.same reg) (Instruction.destroyed instruction)
    in
    let latest_change ~(current : Instruction.Id.t) (reg : Reg.t) =
      DLL.fold_right block.body
        ~f:(fun basic_instruction latest ->
          let instruction = `Basic basic_instruction in
          if Instruction.Id.equal current (Instruction.id instruction)
          then None
          else if is_changed_in instruction reg && Option.is_none latest
          then Some instruction
          else latest)
        ~init:None
    in
    let add_dependency_for_one_arg instruction arg =
      let id = Instruction.id instruction in
      let dependency = latest_change ~current:id arg in
      Option.fold ~none:()
        ~some:(fun instruction ->
          let old_node = Instruction.Id.Tbl.find dependency_graph id in
          Instruction.Id.Tbl.replace dependency_graph id
            { old_node with
              out_edges =
                Instruction.Id.Set.add
                  (Instruction.id instruction)
                  old_node.out_edges
            })
        dependency
    in
    let find_dependencies (instruction : Instruction.t) =
      let id = Instruction.id instruction in
      Instruction.Id.Tbl.add dependency_graph id (Node.init id);
      Array.iter
        (add_dependency_for_one_arg instruction)
        (Instruction.args instruction)
    in
    let body = block.body in
    DLL.iter body ~f:(fun instruction -> find_dependencies (`Basic instruction));
    find_dependencies (`Terminator block.terminator);
    let set_in_edges id (node : Node.t) =
      let set_in_edge from to_ =
        let old_node = Instruction.Id.Tbl.find dependency_graph to_ in
        Instruction.Id.Tbl.replace dependency_graph to_
          { old_node with
            in_edges = Instruction.Id.Set.add from old_node.in_edges
          }
      in
      Instruction.Id.Set.iter (set_in_edge id) node.out_edges
    in
    Instruction.Id.Tbl.iter set_in_edges dependency_graph;
    dependency_graph

  let from_cfg (cfg : Cfg.t) : t =
    let dependency_graph = init () in
    Cfg.iter_blocks cfg ~f:(fun _ block ->
        Instruction.Id.Tbl.add_seq dependency_graph
          (Instruction.Id.Tbl.to_seq (from_basic_block block)));
    dependency_graph

  let dump ppf (t : t) cfg_with_layout =
    let open Format in
    fprintf ppf "\ndependency graph for %s:\n"
      (Cfg_with_layout.cfg cfg_with_layout |> Cfg.fun_name);
    let print_node (instruction : Instruction.t) =
      let id = Instruction.id instruction in
      let node = Instruction.Id.Tbl.find t id in
      fprintf ppf "\n%d:\n" node.id;
      Cfg.print_instruction ppf instruction;
      fprintf ppf "\ndependencies:\n";
      Instruction.Id.Set.iter (fprintf ppf "%d ") node.out_edges;
      fprintf ppf "\nis a dependency of:\n";
      Instruction.Id.Set.iter (fprintf ppf "%d ") node.in_edges;
      fprintf ppf "\n"
    in
    Cfg_with_layout.iter_instructions cfg_with_layout
      ~instruction:(fun instruction -> print_node (`Basic instruction))
      ~terminator:(fun instruction -> print_node (`Terminator instruction))
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
      let instruction = `Basic basic_instruction in
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
      let instruction = `Basic basic_instruction in
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
      let args = Instruction.args t.instruction in
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
        (Instruction.id instruction)
        Cfg.print_instruction instruction print_memory_chunk t print_addressing
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
      then Instruction.id t1.instruction - Instruction.id t2.instruction
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

let dump ppf cfg_with_layout ~(dependency_graph : Dependency_graph.t)
    ~(adjacent_memory_accesses : Adjacent_memory_accesses.t Label.Tbl.t) ~msg =
  let open Format in
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  let layout = Cfg_with_layout.layout cfg_with_layout in
  fprintf ppf "\nvectorization extra information for %s\n" msg;
  fprintf ppf "%s\n" (Cfg.fun_name cfg);
  fprintf ppf "layout.length=%d\n" (layout |> DLL.length);
  let block_count = Label.Tbl.length cfg.blocks in
  fprintf ppf "blocks.length=%d\n" block_count;
  let body_instruction_count =
    Cfg.fold_body_instructions cfg ~f:(fun sum _ -> sum + 1) ~init:0
  in
  fprintf ppf "body instruction count=%d\n" body_instruction_count;
  fprintf ppf "terminator instruction count=%d\n" block_count;
  fprintf ppf "body and terminator instruction count=%d\n"
    (body_instruction_count + block_count);
  Dependency_graph.dump ppf dependency_graph cfg_with_layout;
  Adjacent_memory_accesses.dump ppf adjacent_memory_accesses cfg_with_layout

let cfg ppf_dump cl =
  let cfg = Cfg_with_layout.cfg cl in
  let dependency_graph = Dependency_graph.from_cfg cfg in
  let adjacent_memory_accesses = Adjacent_memory_accesses.from_cfg cfg in
  if !Flambda_backend_flags.dump_cfg
  then
    Format.fprintf ppf_dump "*** Vectorization@.%a@."
      (dump ~dependency_graph ~adjacent_memory_accesses ~msg:"")
      cl;
  cl
