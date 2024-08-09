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
    let is_res_of instruction reg =
      Array.exists (Reg.same reg) (Instruction.res instruction)
    in
    let latest_res ~(current : Instruction.Id.t) (reg : Reg.t) =
      DLL.fold_right block.body
        ~f:(fun basic_instruction latest ->
          let instruction = `Basic basic_instruction in
          if Instruction.Id.equal current (Instruction.id instruction)
          then None
          else if is_res_of instruction reg && Option.is_none latest
          then Some instruction
          else latest)
        ~init:None
    in
    let add_dependency_for_one_arg instruction arg =
      let id = Instruction.id instruction in
      let dependency = latest_res ~current:id arg in
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
        (Instruction.arg instruction)
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
  let dump ppf (t:t) cfg_with_layout =
    let open Format in
    fprintf ppf "\nDependency graph:\n";
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
    type t =
      | Load of Instruction.t list
      | Store of Instruction.t list

    let from_cfg (cfg : Cfg.t) : t list =
      let from_block block =
        ignore block;
        ignore (Load []);
        ignore (Store []);
        []
      in
      Cfg.fold_blocks
        ~f:(fun _ block old_list ->
          from_block block @ old_list)
        ~init:[] cfg
    let dump ppf (t_list:t list) =
      let open Format in
    fprintf ppf "\nAdjacent memory accesses:\n";
    let print_element t =
      let instructions = match t with
      |Load instructions -> instructions
      |Store instructions -> instructions in
    List.iter (fun instruction -> fprintf ppf "\n%d:\n" (Instruction.id instruction);Cfg.print_instruction ppf instruction) instructions in
    List.iter (fun t ->

      fprintf ppf "\n(\n";print_element t;
      fprintf ppf "\n)\n"
      ) t_list


  end
let dump ppf cfg_with_layout ~(dependency_graph : Dependency_graph.t) ~(adjacent_memory_accesses : Adjacent_memory_accesses.t list) ~msg =
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
  Adjacent_memory_accesses.dump ppf adjacent_memory_accesses

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
