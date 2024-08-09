[@@@ocaml.warning "+a-40-41-42"]

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

  let arg (instruction : t) : Reg.t Array.t =
    match instruction with
    | `Basic instruction -> instruction.arg
    | `Terminator instruction -> instruction.arg

  let res (instruction : t) : Reg.t Array.t =
    match instruction with
    | `Basic instruction -> instruction.res
    | `Terminator instruction -> instruction.res
end

module Dependency_graph = struct
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

  let from_cfg (cfg : Cfg.t) : t =
    let dependency_graph = init () in
    let is_res_of instruction reg =
      Array.exists (Reg.same reg) (Instruction.res instruction)
    in
    let latest_res ~(current : Instruction.Id.t) (reg : Reg.t)
        (block : Cfg.basic_block) =
      let terminator_instruction = `Terminator block.terminator in
      let init =
        if (not
              (Instruction.Id.equal current
                 (Instruction.id terminator_instruction)))
           && is_res_of terminator_instruction reg
        then Some terminator_instruction
        else None
      in
      DLL.fold_right block.body
        ~f:(fun basic_instruction latest ->
          let instruction = `Basic basic_instruction in
          if Instruction.Id.equal current (Instruction.id instruction)
          then None
          else if is_res_of instruction reg && Option.is_none latest
          then Some instruction
          else latest)
        ~init
    in
    let add_dependencies_for_one_arg (block : Cfg.basic_block) instruction arg =
      let visited = ref Label.Set.empty in
      let rec dependencies_in block =
        match latest_res ~current:(Instruction.id instruction) arg block with
        | Some dependency_instruction ->
          Instruction.Id.Set.of_list [Instruction.id dependency_instruction]
        | None ->
          List.fold_left
            (fun old_set label ->
              if Label.Set.exists (Label.equal label) !visited
              then old_set
              else (
                visited := Label.Set.add label !visited;
                Instruction.Id.Set.union old_set
                  (dependencies_in (Label.Tbl.find cfg.blocks label))))
            Instruction.Id.Set.empty
            (Cfg.predecessor_labels block)
      in
      let id = Instruction.id instruction in
      let old_node = Instruction.Id.Tbl.find dependency_graph id in
      Instruction.Id.Tbl.replace dependency_graph id
        { old_node with
          out_edges =
            Instruction.Id.Set.union (dependencies_in block) old_node.out_edges
        }
    in
    let find_dependencies block (instruction : Instruction.t) =
      let id = Instruction.id instruction in
      Instruction.Id.Tbl.add dependency_graph id (Node.init id);
      Array.iter
        (add_dependencies_for_one_arg block instruction)
        (Instruction.arg instruction)
    in
    let handle_block (block : Cfg.basic_block) =
      let body = block.body in
      DLL.iter body ~f:(fun instruction ->
          find_dependencies block (`Basic instruction));
      find_dependencies block (`Terminator block.terminator)
    in
    Cfg.iter_blocks cfg ~f:(fun _ -> handle_block);
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
end

let dump ppf cfg_with_layout ~(dependency_graph : Dependency_graph.t) ~msg =
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
  fprintf ppf "dependency graph:\n";
  let print_node (instruction : Instruction.t) =
    let id = Instruction.id instruction in
    let node = Instruction.Id.Tbl.find dependency_graph id in
    fprintf ppf "\n%d:\n" node.id;
    Cfg.print_instruction ppf instruction;
    fprintf ppf "\nDependencies:\n";
    Instruction.Id.Set.iter (fprintf ppf "%d ") node.out_edges;
    fprintf ppf "\nIs a dependency of:\n";
    Instruction.Id.Set.iter (fprintf ppf "%d ") node.in_edges;
    fprintf ppf "\n"
  in
  Cfg_with_layout.iter_instructions cfg_with_layout
    ~instruction:(fun instruction -> print_node (`Basic instruction))
    ~terminator:(fun instruction -> print_node (`Terminator instruction))

let cfg ppf_dump cl =
  let cfg = Cfg_with_layout.cfg cl in
  let dependency_graph = Dependency_graph.from_cfg cfg in
  if !Flambda_backend_flags.dump_cfg
  then
    Format.fprintf ppf_dump "*** Vectorization@.%a@."
      (dump ~dependency_graph ~msg:"")
      cl;
  cl
