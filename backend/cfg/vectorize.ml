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

  module Node  = struct
    module Reg_node = struct
    type t =
      { reg : Reg.t;
        depends_on : Instruction.Id.t option;
      }

    let init reg : t =
      { reg;
        depends_on = None;
      }
  end
    type t =
      { instruction : Instruction.t;
      reg_nodes : Reg_node.t array;
        depends_on : Instruction.Id.Set.t;
        is_dependency_of : Instruction.Id.Set.t
      }

    let init instruction : t =
      let arguments = Instruction.arguments instruction in
      { instruction; reg_nodes=Array.init (Array.length arguments) (fun i -> arguments.( i) |> Reg_node.init);
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
    let add_arg_dependency instruction arg_i arg =
      let id = Instruction.id instruction in
      let dependency = latest_change ~current:id arg in
      let node = find dependency_graph id in
      let reg_node =  node.reg_nodes. (arg_i) in
      node.reg_nodes. (arg_i) <-{reg_node with depends_on =
      Option.fold ~none:(None) ~some:(Option.some << Instruction.id) dependency}

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
    let arg_indices = (Instruction.arguments instruction|>Array.mapi (fun arg_i _ -> arg_i) ) in
    let instruction_dependencies = Array.fold_left (fun dependencies arg_i ->

      Option.fold ~none:(dependencies) ~some:(fun dependency -> Instruction.Id.Set.add dependency dependencies) ((find dependency_graph id).reg_nodes.(arg_i).depends_on)) (Instruction.Id.Set.empty) arg_indices  in
      let node = find dependency_graph id in
      replace dependency_graph id  {node with depends_on = instruction_dependencies}
    in


    let add_all_dependencies () =
    DLL.iter block.body ~f:(fun instruction -> add_dependencies (Basic instruction));
    add_dependencies (Terminator block.terminator) in
    let set_is_dependency_of instruction_id dependency_id =
        let dependency = find dependency_graph dependency_id in
        replace dependency_graph dependency_id
          { dependency with
            is_dependency_of = Instruction.Id.Set.add instruction_id dependency.is_dependency_of
          }
      in

    let set_is_dependency_of_plural (instruction : Instruction.t) =
      let id = Instruction.id instruction in
      let node = find dependency_graph id in

      Instruction.Id.Set.iter (set_is_dependency_of id) node.depends_on
    in
    let set_all_is_dependency_of () =
      DLL.iter block.body ~f:(fun instruction -> set_is_dependency_of_plural (Basic instruction)) in
      add_all_dependencies (); set_all_is_dependency_of()

  let from_cfg (cfg : Cfg.t) : t =
    let dependency_graph = init () in
    Cfg.iter_blocks cfg ~f:(fun _ block -> from_basic_block block ~dependency_graph);
    dependency_graph

  let dump ppf (t : t) cfg_with_layout =
    let open Format in
    let print_reg_node arg_i (reg_node : Node.Reg_node.t) =
      let dependency = Option.fold ~none:"none" ~some:(sprintf "instruction %d" << Instruction.Id.to_int) reg_node.depends_on in
      fprintf ppf "\nargument %d (reg %d) depends on %s\n" arg_i reg_node.reg.stamp dependency;
    in
    let print_node (instruction : Instruction.t) =
      let id = Instruction.id instruction in
      let node = find t id in
      fprintf ppf "\n%d:\n" (Instruction.id node.instruction |> Instruction.Id.to_int);
      Instruction.print ppf instruction;
      Array.iteri (print_reg_node) node.reg_nodes;
      fprintf ppf "\ndepends on:\n";
      Instruction.Id.Set.iter (  (fprintf ppf "%d ")<<Instruction.Id.to_int) node.depends_on;
      fprintf ppf "\nis a dependency of:\n";
      Instruction.Id.Set.iter (fprintf ppf "%d "<<Instruction.Id.to_int) node.is_dependency_of;
      fprintf ppf "\narg dependencies:\n";
      fprintf ppf "\n"
    in
    fprintf ppf "\ndependency graph:\n";
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
