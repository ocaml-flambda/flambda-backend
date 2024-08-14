[@@@ocaml.warning "+a-40-41-42"]

module DLL = Flambda_backend_utils.Doubly_linked_list

let (<<) f g x = f (g x)

module Instruction : sig
  (* CR-someday tip: consider moving this to cfg or at least have something similar there *)
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

  val print :Format.formatter -> t ->
    unit
end = struct
  module Id = struct
    include Numbers.Int

    let to_int t = t;
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

  let print  ppf (instruction : t) : unit =
      match instruction with
      | Basic i -> Cfg.print_basic ppf i
      | Terminator i -> Cfg.print_terminator ppf i
end

module Dependency_graph : sig
  (* The dependency graph shows dependencies between instructions within the
     same basic block *)
  type t
  val from_cfg: Cfg.t -> t
  val dump:Format.formatter -> t -> Cfg_with_layout.t -> unit
end = struct
  module Node = struct
    type t =
      { id : Instruction.Id.t;
        depends_on : Instruction.Id.Set.t;
        is_dependency_of : Instruction.Id.Set.t
      }

    let init id : t =
      { id;
        depends_on = Instruction.Id.Set.empty;
        is_dependency_of = Instruction.Id.Set.empty
      }
  end

  type t = Node.t Instruction.Id.Tbl.t

  let init () : t = Instruction.Id.Tbl.create 100

  let from_basic_block (block : Cfg.basic_block) ~(dependency_graph : t) =
    let is_changed_in instruction reg =
      Array.exists (Reg.same reg) (Instruction.results instruction)
      || Array.exists (Reg.same reg) (Instruction.destroyed instruction)
    in
    (* CR-soon tip: break it into 2 parts to find the instruction we want then go up from there. (currently it loops from the end and changes the answer back to None when it encounters the same instruction) *)
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
    let add_dependency_for_one_arg instruction arg =
      let id = Instruction.id instruction in
      let dependency = latest_change ~current:id arg in
      Option.fold ~none:()
        ~some:(fun instruction ->
          let old_node = Instruction.Id.Tbl.find dependency_graph id in
          Instruction.Id.Tbl.replace dependency_graph id
            { old_node with
              depends_on =
                Instruction.Id.Set.add
                  (Instruction.id instruction)
                  old_node.depends_on
            })
        dependency
    in
    let find_dependencies (instruction : Instruction.t) =
      let id = Instruction.id instruction in
      Instruction.Id.Tbl.add dependency_graph id (Node.init id);
      Array.iter
        (add_dependency_for_one_arg instruction)
        (Instruction.arguments instruction)
    in
    let body = block.body in
    DLL.iter body ~f:(fun instruction -> find_dependencies (Basic instruction));
    find_dependencies (Terminator block.terminator);
    let set_is_dependency_of (instruction : Instruction.t) =
      let id = Instruction.id instruction in
      let node = Instruction.Id.Tbl.find dependency_graph id in
      let set_is_dependency_of_one id dependency =
        let old_node = Instruction.Id.Tbl.find dependency_graph dependency in
        Instruction.Id.Tbl.replace dependency_graph dependency
          { old_node with
            is_dependency_of = Instruction.Id.Set.add id old_node.is_dependency_of
          }
      in
      Instruction.Id.Set.iter (set_is_dependency_of_one id) node.depends_on
    in
    DLL.iter body ~f:(fun instruction -> set_is_dependency_of (Basic instruction))

  let from_cfg (cfg : Cfg.t) : t =
    let dependency_graph = init () in
    Cfg.iter_blocks cfg ~f:(fun _ block -> from_basic_block block ~dependency_graph);
    dependency_graph

  let dump ppf (t : t) cfg_with_layout =
    let open Format in
    fprintf ppf "\nDependency graph:\n";
    let print_node (instruction : Instruction.t) =
      let id = Instruction.id instruction in
      let node = Instruction.Id.Tbl.find t id in
      fprintf ppf "\n%d:\n" (Instruction.Id.to_int node.id);
      Instruction.print ppf instruction;
      fprintf ppf "\ndepends on:\n";
      Instruction.Id.Set.iter (  (fprintf ppf "%d ")<<Instruction.Id.to_int) node.depends_on;
      fprintf ppf "\nis a dependency of:\n";
      Instruction.Id.Set.iter (fprintf ppf "%d "<<Instruction.Id.to_int) node.is_dependency_of;
      fprintf ppf "\n"
    in
    Cfg_with_layout.iter_instructions cfg_with_layout
      ~instruction:(fun instruction -> print_node (Basic instruction))
      ~terminator:(fun instruction -> print_node (Terminator instruction))
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
  if !Flambda_backend_flags.dump_vectorize
  then
      dump ppf_dump ~msg:""
      cl;
  cl
