open! Rev_expr
module Graph = Global_flow_graph
module Dot = Dot_printer

type code_dep = Dot.code_dep =
  { params : Variable.t list;
    my_closure : Variable.t;
    return : Variable.t list; (* Dummy variable representing return value *)
    exn : Variable.t; (* Dummy variable representing exn return value *)
    code_id_for_escape : Code_id.t
  }

type apply_dep =
  { apply_in_func : Code_id.t option;
    apply_code_id : Code_id.t;
    apply_params : Simple.t list;
    apply_closure : Simple.t option;
    apply_return : Variable.t list option;
    apply_exn : Variable.t
  }

type cont_kind = Normal of Variable.t list

type denv =
  { parent : rev_expr_holed;
    conts : cont_kind Continuation.Map.t;
    current_code_id : Code_id.t option
  }

module Acc : sig
  type t

  val create : unit -> t

  val kind : Name.t -> Flambda_kind.t -> t -> unit

  val bound_parameter_kind : Bound_parameter.t -> t -> unit

  val alias_kind : Name.t -> Simple.t -> t -> unit

  val kinds : t -> Flambda_kind.t Name.Map.t

  val record_dep : denv:denv -> Name.t -> Graph.Dep.t -> t -> unit

  val record_dep' : denv:denv -> Code_id_or_name.t -> Graph.Dep.t -> t -> unit

  val record_deps :
    denv:denv -> Code_id_or_name.t -> Graph.Dep.Set.t -> t -> unit

  val cont_dep : denv:denv -> Variable.t -> Simple.t -> t -> unit

  val func_param_dep : denv:denv -> Bound_parameter.t -> Variable.t -> t -> unit

  val root : Variable.t -> t -> unit

  val used : denv:denv -> Simple.t -> t -> unit

  val used_code_id : Code_id.t -> t -> unit

  val called : denv:denv -> Code_id.t -> t -> unit

  val add_apply : apply_dep -> t -> unit

  val add_set_of_closures_dep : Name.t -> Code_id.t -> t -> unit

  val add_code : Code_id.t -> code_dep -> t -> unit

  val find_code : t -> Code_id.t -> code_dep

  val code_deps : t -> code_dep Code_id.Map.t

  val deps : t -> Graph.graph * Graph.fun_graph
end = struct
  type t =
    { mutable code : code_dep Code_id.Map.t;
      mutable apply_deps : apply_dep list;
      mutable set_of_closures_dep : (Name.t * Code_id.t) list;
      deps1 : Graph.graph;
      deps2 : Graph.fun_graph;
      mutable kinds : Flambda_kind.t Name.Map.t
    }

  let code_deps t = t.code

  let create () =
    { code = Code_id.Map.empty;
      apply_deps = [];
      set_of_closures_dep = [];
      deps1 =
        { toplevel_graph = Graph.create ();
          function_graphs = Hashtbl.create 100
        };
      deps2 = Graph.create ();
      kinds = Name.Map.empty
    }

  let kinds t = t.kinds

  let kind name k t = t.kinds <- Name.Map.add name k t.kinds

  let bound_parameter_kind (bp : Bound_parameter.t) t =
    let kind = Flambda_kind.With_subkind.kind (Bound_parameter.kind bp) in
    let name = Name.var (Bound_parameter.var bp) in
    t.kinds <- Name.Map.add name kind t.kinds

  let alias_kind name simple t =
    let kind =
      Simple.pattern_match simple
        ~name:(fun name ~coercion:_ ->
          (* Symbols are always values and might not be in t.kinds *)
          if Name.is_symbol name
          then Flambda_kind.value
          else
            match Name.Map.find_opt name t.kinds with
            | Some k -> k
            | None -> Misc.fatal_errorf "Unbound name %a" Name.print name)
        ~const:(fun const ->
          match Int_ids.Const.descr const with
          | Naked_immediate _ -> Flambda_kind.naked_immediate
          | Tagged_immediate _ -> Flambda_kind.value
          | Naked_float _ -> Flambda_kind.naked_float
          | Naked_float32 _ -> Flambda_kind.naked_float32
          | Naked_int32 _ -> Flambda_kind.naked_int32
          | Naked_int64 _ -> Flambda_kind.naked_int64
          | Naked_nativeint _ -> Flambda_kind.naked_nativeint
          | Naked_vec128 _ -> Flambda_kind.naked_vec128)
    in
    t.kinds <- Name.Map.add name kind t.kinds

  let add_code code_id dep t = t.code <- Code_id.Map.add code_id dep t.code

  let find_code t code_id = Code_id.Map.find code_id t.code

  let cur_deps_from_code_id_deps code_id (deps : Graph.graph) =
    match code_id with
    | None -> deps.toplevel_graph
    | Some code_id -> (
      match Hashtbl.find_opt deps.function_graphs code_id with
      | None ->
        let fdeps = Graph.create () in
        Hashtbl.add deps.function_graphs code_id fdeps;
        fdeps
      | Some deps -> deps)

  let cur_deps_from_code_id code_id t =
    cur_deps_from_code_id_deps code_id t.deps1

  let cur_deps ~denv t = cur_deps_from_code_id denv.current_code_id t

  let record_dep ~denv name dep t =
    let name = Code_id_or_name.name name in
    Graph.add_dep (cur_deps ~denv t) name dep;
    Graph.add_dep t.deps2 name dep

  let record_dep' ~denv code_id_or_name dep t =
    Graph.add_dep (cur_deps ~denv t) code_id_or_name dep;
    Graph.add_dep t.deps2 code_id_or_name dep

  let record_deps ~denv code_id_or_name deps t =
    Graph.add_deps (cur_deps ~denv t) code_id_or_name deps;
    Graph.add_deps t.deps2 code_id_or_name deps

  let cont_dep ~denv pat dep t =
    Simple.pattern_match dep
      ~name:(fun name ~coercion:_ ->
        Graph.add_cont_dep (cur_deps ~denv t) pat name;
        Graph.add_cont_dep t.deps2 pat name)
      ~const:(fun _ -> ())

  let func_param_dep ~denv param arg t =
    Graph.add_func_param (cur_deps ~denv t)
      ~param:(Bound_parameter.var param)
      ~arg:(Name.var arg);
    Graph.add_func_param t.deps2
      ~param:(Bound_parameter.var param)
      ~arg:(Name.var arg)

  let root v t =
    Graph.add_use t.deps1.toplevel_graph (Code_id_or_name.var v);
    Graph.add_use t.deps2 (Code_id_or_name.var v)

  let used ~denv dep t =
    Simple.pattern_match dep
      ~name:(fun name ~coercion:_ ->
        Graph.add_use (cur_deps ~denv t) (Code_id_or_name.name name);
        match denv.current_code_id with
        | None -> Graph.add_use t.deps2 (Code_id_or_name.name name)
        | Some code_id ->
          Graph.add_dep t.deps2
            (Code_id_or_name.code_id code_id)
            (Graph.Dep.Use (Code_id_or_name.name name)))
      ~const:(fun _ -> ())

  let used_code_id code_id t =
    Graph.add_use t.deps1.toplevel_graph (Code_id_or_name.code_id code_id);
    Graph.add_use t.deps2 (Code_id_or_name.code_id code_id)

  let called ~denv code_id t =
    Graph.add_called (cur_deps ~denv t) code_id;
    match denv.current_code_id with
    | None -> Graph.add_called t.deps2 code_id
    | Some code_id2 ->
      Graph.add_dep t.deps2
        (Code_id_or_name.code_id code_id2)
        (Graph.Dep.Use (Code_id_or_name.code_id code_id))

  let add_apply apply t = t.apply_deps <- apply :: t.apply_deps

  let add_set_of_closures_dep name code_id t =
    t.set_of_closures_dep <- (name, code_id) :: t.set_of_closures_dep

  let deps t =
    List.iter
      (fun { apply_in_func;
             apply_code_id;
             apply_params;
             apply_closure;
             apply_return;
             apply_exn
           } ->
        let deps = cur_deps_from_code_id apply_in_func t in
        let code_dep = find_code t apply_code_id in
        let add_cond_dep param name =
          let param = Name.var param in
          match apply_in_func with
          | None ->
            Graph.add_dep t.deps2
              (Code_id_or_name.name param)
              (Graph.Dep.Alias name)
          | Some code_id ->
            Graph.add_dep t.deps2
              (Code_id_or_name.name param)
              (Graph.Dep.Alias_if_def (name, code_id));
            Graph.add_dep t.deps2
              (Code_id_or_name.code_id code_id)
              (Graph.Dep.Propagate (name, param))
        in
        List.iter2
          (fun param arg ->
            Simple.pattern_match arg
              ~name:(fun name ~coercion:_ ->
                Graph.add_cont_dep deps param name;
                add_cond_dep param name)
              ~const:(fun _ -> ()))
          code_dep.params apply_params;
        (match apply_closure with
        | None -> ()
        | Some apply_closure ->
          Simple.pattern_match apply_closure
            ~name:(fun name ~coercion:_ ->
              Graph.add_cont_dep deps code_dep.my_closure name;
              add_cond_dep code_dep.my_closure name)
            ~const:(fun _ -> ()));
        (match apply_return with
        | None -> ()
        | Some apply_return ->
          List.iter2
            (fun arg param ->
              Graph.add_cont_dep deps param (Name.var arg);
              add_cond_dep param (Name.var arg))
            code_dep.return apply_return);
        Graph.add_cont_dep deps apply_exn (Name.var code_dep.exn);
        add_cond_dep apply_exn (Name.var code_dep.exn))
      t.apply_deps;
    List.iter
      (fun (name, code_id) ->
        let code_id =
          try
            let code_dep = find_code t code_id in
            code_dep.code_id_for_escape
          with Not_found -> code_id
        in
        Graph.add_dep t.deps1.toplevel_graph
          (Code_id_or_name.name name)
          (Graph.Dep.Contains (Code_id_or_name.code_id code_id));
        Graph.add_dep t.deps2
          (Code_id_or_name.name name)
          (Graph.Dep.Block (Code_of_closure, (Code_id_or_name.code_id code_id))))
      t.set_of_closures_dep;
    t.deps1, t.deps2
end

type acc = Acc.t

let apply_cont_deps denv acc apply_cont =
  let cont = Apply_cont_expr.continuation apply_cont in
  let args = Apply_cont_expr.args apply_cont in
  let params = Continuation.Map.find cont denv.conts in
  let (Normal params) = params in
  List.iter2 (fun param dep -> Acc.cont_dep ~denv param dep acc) params args

let prepare_code ~denv acc (code_id : Code_id.t) (code : Code.t) =
  let return = [Variable.create "function_return"] in
  let exn = Variable.create "function_exn" in
  let my_closure = Variable.create "my_closure" in
  let params =
    let arity = Code.params_arity code in
    List.init (Flambda_arity.cardinal_unarized arity) (fun i ->
        Variable.create (Printf.sprintf "function_param_%i" i))
  in
  let has_unsafe_result_type =
    match Code.result_types code with
    | Unknown -> false
    | Bottom -> false
    | Ok _ -> true
  in
  let code_id_for_escape = Code_id.rename code_id in
  let code_dep = { return; my_closure; exn; params; code_id_for_escape } in
  let () =
    if has_unsafe_result_type
    then
      List.iter
        (fun var -> Acc.used ~denv (Simple.var var) acc)
        ((my_closure :: params) @ (exn :: return))
    else
      let deps =
        Graph.Dep.Use (Code_id_or_name.var exn)
        :: List.map
             (fun var -> Graph.Dep.Use (Code_id_or_name.var var))
             return
      in
      List.iter
        (fun dep ->
          Acc.record_dep' ~denv
            (Code_id_or_name.code_id code_id_for_escape)
            dep acc)
        deps
  in
  let () =
    Acc.record_dep' ~denv
      (Code_id_or_name.code_id code_id_for_escape)
      (Graph.Dep.Use (Code_id_or_name.code_id code_id))
      acc
  in
  Acc.add_code code_id code_dep acc

let record_set_of_closures_deps ~denv names_and_function_slots set_of_closures
    acc : unit =
  let funs =
    Function_declarations.funs (Set_of_closures.function_decls set_of_closures)
  in
  let () =
    Function_slot.Lmap.iter
      (fun function_slot name ->
        Acc.kind name Flambda_kind.value acc;
        let code_id =
          (Function_slot.Map.find function_slot funs
            : Function_declarations.code_id_in_function_declaration)
        in
        match code_id with
        | Deleted -> ()
        | Code_id code_id ->
          if Compilation_unit.is_current (Code_id.get_compilation_unit code_id)
          then Acc.add_set_of_closures_dep name code_id acc
        (* let code_id = Code_id_or_name.code_id code_id in Acc.record_dep ~denv
           name (Graph.Dep.Contains code_id) acc *))
      names_and_function_slots
  in
  let deps =
    Value_slot.Map.fold
      (fun value_slot simple set ->
        Simple.pattern_match
          ~const:(fun _ -> set)
          ~name:(fun name ~coercion:_ ->
            Graph.Dep.Set.add
              (Block (Value_slot value_slot, Code_id_or_name.name name))
              set)
          simple)
      (Set_of_closures.value_slots set_of_closures)
      Graph.Dep.Set.empty
  in
  let deps =
    Function_slot.Lmap.fold
      (fun function_slot name set ->
        Graph.Dep.Set.add
          (Block (Function_slot function_slot, Code_id_or_name.name name))
          set)
      names_and_function_slots deps
  in
  Function_slot.Lmap.iter
    (fun _function_slot name ->
      Acc.record_deps ~denv (Code_id_or_name.name name) deps acc)
    names_and_function_slots

let rec traverse (denv : denv) (acc : acc) (expr : Flambda.Expr.t) : rev_expr =
  match Flambda.Expr.descr expr with
  | Invalid { message } ->
    let expr = Invalid { message } in
    { expr; holed_expr = denv.parent }
  | Switch switch ->
    let expr = Switch switch in
    let () =
      Acc.used ~denv (Switch_expr.scrutinee switch) acc;
      Targetint_31_63.Map.iter
        (fun _ apply_cont -> apply_cont_deps denv acc apply_cont)
        (Switch_expr.arms switch)
    in
    { expr; holed_expr = denv.parent }
  | Apply_cont apply_cont ->
    let expr = Apply_cont apply_cont in
    apply_cont_deps denv acc apply_cont;
    { expr; holed_expr = denv.parent }
  | Apply apply -> begin
    let default_acc acc =
      (* TODO regions? *)
      let () =
        List.iter (fun arg -> Acc.used ~denv arg acc) (Apply_expr.args apply)
      in
      let () =
        match Apply_expr.callee apply with
        | None -> ()
        | Some callee -> Acc.used ~denv callee acc
      in
      let () =
        List.iter
          (fun (arg, _) -> Acc.used ~denv arg acc)
          (Exn_continuation.extra_args (Apply_expr.exn_continuation apply))
      in
      let () =
        match Apply_expr.call_kind apply with
        | Function _ -> ()
        | Method { obj; kind = _; alloc_mode = _ } -> Acc.used ~denv obj acc
        | C_call _ -> ()
      in
      ()
    in
    let () =
      match Apply_expr.call_kind apply with
      | Function { function_call = Direct code_id; _ } ->
        (* TODO think about wether we should propagate that cross module.
           Probably not *)
        if Compilation_unit.is_current (Code_id.get_compilation_unit code_id)
        then (
          let return_args =
            match Apply_expr.continuation apply with
            | Never_returns -> None
            | Return cont -> (
              match Continuation.Map.find cont denv.conts with
              | Normal params -> Some params)
          in
          let exn_arg =
            let exn = Apply_expr.exn_continuation apply in
            let extra_args = Exn_continuation.extra_args exn in
            let (Normal exn_params) =
              Continuation.Map.find
                (Exn_continuation.exn_handler exn)
                denv.conts
            in
            match exn_params with
            | [] -> assert false
            | exn_param :: extra_params ->
              let () =
                List.iter2
                  (fun param (arg, _kind) -> Acc.cont_dep ~denv param arg acc)
                  extra_params extra_args
              in
              exn_param
          in
          let apply_dep =
            { apply_in_func = denv.current_code_id;
              apply_code_id = code_id;
              apply_params = Apply_expr.args apply;
              apply_closure = Apply_expr.callee apply;
              apply_return = return_args;
              apply_exn = exn_arg
            }
          in
          Acc.add_apply apply_dep acc;
          (* TODO regions? *)
          (* TODO record function use *)
          Acc.called ~denv code_id acc)
        else default_acc acc
      | Function
          { function_call = Indirect_unknown_arity | Indirect_known_arity; _ }
      | Method _ | C_call _ ->
        default_acc acc
    in
    let expr = Apply apply in
    { expr; holed_expr = denv.parent }
  end
  | Let_cont let_cont -> begin
    match let_cont with
    | Non_recursive
        { handler; num_free_occurrences = _; is_applied_with_traps = _ } ->
      Flambda.Non_recursive_let_cont_handler.pattern_match handler
        ~f:(fun cont ~body ->
          let cont_handler =
            Flambda.Non_recursive_let_cont_handler.handler handler
          in
          traverse_cont_handler
            { parent = Up;
              conts = denv.conts;
              current_code_id = denv.current_code_id
            } acc cont_handler (fun handler acc ->
              let conts =
                Continuation.Map.add cont
                  (Normal (Bound_parameters.vars handler.bound_parameters))
                  denv.conts
              in
              let denv =
                { parent = Let_cont { cont; handler; parent = denv.parent };
                  conts;
                  current_code_id = denv.current_code_id
                }
              in
              traverse denv acc body))
    | Recursive handlers -> begin
      (* Warning non tail rec on traverse_cont_handler, probably OK *)
      Flambda.Recursive_let_cont_handlers.pattern_match handlers
        ~f:(fun ~invariant_params ~body handlers ->
          let invariant_params_vars = Bound_parameters.vars invariant_params in
          let handlers =
            Continuation.Map.map
              (fun cont_handler ->
                Flambda.Continuation_handler.pattern_match cont_handler
                  ~f:(fun bound_parameters ~handler ->
                    cont_handler, bound_parameters, handler))
              (Flambda.Continuation_handlers.to_map handlers)
          in
          let conts =
            Continuation.Map.fold
              (fun cont (_, bp, _) conts ->
                Continuation.Map.add cont
                  (Normal (invariant_params_vars @ Bound_parameters.vars bp))
                  conts)
              handlers denv.conts
          in
          (* Record kinds of bound parameters *)
          let () =
            List.iter
              (fun bp -> Acc.bound_parameter_kind bp acc)
              (Bound_parameters.to_list invariant_params)
          in
          let () =
            Continuation.Map.iter
              (fun _ (_, bp, _) ->
                List.iter
                  (fun bp -> Acc.bound_parameter_kind bp acc)
                  (Bound_parameters.to_list bp))
              handlers
          in
          let handlers =
            Continuation.Map.fold
              (fun cont (cont_handler, bound_parameters, handler) handlers ->
                let is_exn_handler =
                  Flambda.Continuation_handler.is_exn_handler cont_handler
                in
                let is_cold =
                  Flambda.Continuation_handler.is_cold cont_handler
                in
                let expr =
                  traverse
                    { parent = Up;
                      conts;
                      current_code_id = denv.current_code_id
                    }
                    acc handler
                in
                let handler =
                  { bound_parameters; expr; is_exn_handler; is_cold }
                in
                Continuation.Map.add cont handler handlers)
              handlers Continuation.Map.empty
          in
          let denv =
            { parent =
                Let_cont_rec
                  { invariant_params; handlers; parent = denv.parent };
              conts;
              current_code_id = denv.current_code_id
            }
          in
          traverse denv acc body)
    end
  end
  | Let let_expr ->
    let bound_pattern, body =
      Flambda.Let_expr.pattern_match let_expr ~f:(fun bound_pattern ~body ->
          bound_pattern, body)
    in
    let defining_expr = Flambda.Let_expr.defining_expr let_expr in
    let known_field_of_block field block =
      Simple.pattern_match field
        ~name:(fun _ ~coercion:_ -> None)
        ~const:(fun cst ->
          Simple.pattern_match block
            ~const:(fun _ -> None)
              (* CR ncourant: it seems this const case can happen with the following code:
               *
               * let[@inline] f b x = if b then Lazy.force x else 0
               * let g b = f b (lazy 0)
               *
               * It is unclear why it has not been transformed by an Invalid by simplify, however.
               *)
            ~name:(fun block ~coercion:_ ->
              match[@ocaml.warning "-4"] Int_ids.Const.descr cst with
              | Tagged_immediate i ->
                let i = Targetint_31_63.to_int_exn i in
                Some (i, block)
              | _ -> assert false))
    in
    let record acc name dep = Acc.record_dep ~denv name dep acc in
    let default_bp acc dep =
      let bound_to = Bound_pattern.free_names bound_pattern in
      Name_occurrences.fold_names bound_to
        ~f:(fun () bound_to -> Acc.record_dep ~denv bound_to dep acc)
        ~init:()
    in
    let default acc =
      Name_occurrences.fold_names
        ~f:(fun () free_name -> default_bp acc (Graph.Dep.Use (Code_id_or_name.name free_name)))
        ~init:()
        (Flambda.Named.free_names defining_expr)
    in
    let () =
      match defining_expr with
      | Set_of_closures set_of_closures ->
        (* TODO kind *)
        let names_and_function_slots =
          let bound_vars =
            match bound_pattern with
            | Set_of_closures set -> set
            | Static _ | Singleton _ -> assert false
          in
          let funs =
            Function_declarations.funs_in_order
              (Set_of_closures.function_decls set_of_closures)
          in
          Function_slot.Lmap.of_list
          @@ List.map2
               (fun function_slot bound_var ->
                 function_slot, Name.var (Bound_var.var bound_var))
               (Function_slot.Lmap.keys funs)
               bound_vars
        in
        record_set_of_closures_deps ~denv names_and_function_slots
          set_of_closures acc
      | Static_consts group ->
        (* TODO kind *)
        let bound_static =
          match bound_pattern with
          | Static b -> b
          | Singleton _ | Set_of_closures _ -> assert false
        in
        let () =
          Flambda.Static_const_group.match_against_bound_static group
            bound_static ~init:()
            ~code:(fun () -> prepare_code ~denv acc)
            ~deleted_code:(fun _ _ -> ())
            ~set_of_closures:(fun _ ~closure_symbols:_ _ -> ())
            ~block_like:(fun _ _ _ -> ())
        in
        Flambda.Static_const_group.match_against_bound_static group bound_static
          ~init:()
          ~code:(fun () _code_id _code ->
            (* TODO: (is there anything to do here ? *)
            ())
          ~deleted_code:(fun () _ -> ())
          ~set_of_closures:(fun () ~closure_symbols set_of_closures ->
            let names_and_function_slots =
              Function_slot.Lmap.map Name.symbol closure_symbols
            in
            record_set_of_closures_deps ~denv names_and_function_slots
              set_of_closures acc)
          ~block_like:(fun () symbol static_const ->
            let name = Name.symbol symbol in
            match[@ocaml.warning "-4"] static_const with
            | Block (_, _, fields) | Immutable_value_array fields ->
              List.iteri
                (fun i (field : Field_of_static_block.t) ->
                  match field with
                  | Symbol s ->
                    record acc name
                      (Graph.Dep.Block
                         (Graph.Field.Block i, Code_id_or_name.symbol s))
                  | Tagged_immediate _ -> ()
                  | Dynamically_computed (v, _) ->
                    record acc name
                      (Graph.Dep.Block
                         (Graph.Field.Block i, Code_id_or_name.var v)))
                fields
            | Set_of_closures _ -> assert false
            | _ -> ())
      | Prim (prim, _dbg) -> begin
        let () =
          let kind = Flambda_primitive.result_kind' prim in
          let name =
            Name.var @@ Bound_var.var
            @@ Bound_pattern.must_be_singleton bound_pattern
          in
          Acc.kind name kind acc
        in
        match[@ocaml.warning "-4"] prim with
        | Variadic (Make_block (_, _mutability, _), fields) ->
          List.iteri
            (fun i field ->
              Simple.pattern_match field
                ~name:(fun name ~coercion:_ ->
                  default_bp acc
                    (Graph.Dep.Block
                       (Graph.Field.Block i, Code_id_or_name.name name)))
                ~const:(fun _ -> ()))
            fields
        | Unary (Project_function_slot { move_from = _; move_to }, block) ->
          let block =
            Simple.pattern_match block
              ~name:(fun name ~coercion:_ -> name)
              ~const:(fun _ -> assert false)
          in
          let dep = Graph.Dep.Field (Function_slot move_to, block) in
          default_bp acc dep
        | Unary (Project_value_slot { project_from = _; value_slot }, block) ->
          let block =
            Simple.pattern_match block
              ~name:(fun name ~coercion:_ -> name)
              ~const:(fun _ -> assert false)
          in
          let dep = Graph.Dep.Field (Value_slot value_slot, block) in
          default_bp acc dep
        | Binary (Block_load (_access_kind, _mutability), block, field) -> begin
          (* Loads from mutable blocks are tracked here. This is ok as long as
             store are properly tracked also. This is a flow insensitive
             dependency analysis: this might produce surprising results
             sometimes *)
          match known_field_of_block field block with
          | None -> default acc
          | Some (field, block) ->
            default_bp acc (Graph.Dep.Field (Block field, block))
        end
        | prim ->
          let () =
            match Flambda_primitive.effects_and_coeffects prim with
            | Arbitrary_effects, _, _ ->
              let bound_to = Bound_pattern.free_names bound_pattern in
              Name_occurrences.fold_names bound_to
                ~f:(fun () bound_to ->
                  Acc.used ~denv (Simple.name bound_to) acc)
                ~init:()
            | _ -> ()
          in
          default acc
      end
      | Simple s ->
        (* TODO kind *)
        let () =
          let name =
            Name.var @@ Bound_var.var
            @@ Bound_pattern.must_be_singleton bound_pattern
          in
          Acc.alias_kind name s acc
        in
        Simple.pattern_match s
          ~name:(fun name ~coercion:_ -> default_bp acc (Graph.Dep.Alias name))
          ~const:(fun _ -> default acc)
      | Rec_info _ ->
        (* TODO kind *)
        default acc
    in
    let named : rev_named =
      match defining_expr with
      | Set_of_closures set_of_closures ->
        let function_decls = Set_of_closures.function_decls set_of_closures in
        let value_slots = Set_of_closures.value_slots set_of_closures in
        let alloc_mode = Set_of_closures.alloc_mode set_of_closures in
        let set_of_closures = { function_decls; value_slots; alloc_mode } in
        Set_of_closures set_of_closures
      | Static_consts group ->
        let bound_static =
          match bound_pattern with
          | Static b -> b
          | Singleton _ | Set_of_closures _ -> assert false
        in
        let rev_group =
          Flambda.Static_const_group.match_against_bound_static group
            bound_static ~init:[]
            ~code:(fun rev_group code_id code ->
              let code = traverse_code acc code_id code in
              Code code :: rev_group)
            ~deleted_code:(fun rev_group _ -> Deleted_code :: rev_group)
            ~set_of_closures:
              (fun rev_group ~closure_symbols:_ set_of_closures ->
              let static_const = Static_const.set_of_closures set_of_closures in
              Static_const static_const :: rev_group)
            ~block_like:(fun rev_group _symbol static_const ->
              (* TODO: register make block deps *)
              Static_const static_const :: rev_group)
        in
        let group = List.rev rev_group in
        Static_consts group
      (* TODO set_of_closures in Static_consts *)
      | Prim _ -> Named defining_expr
      | Simple _ -> Named defining_expr
      | Rec_info _ as defining_expr -> Named defining_expr
    in
    let let_acc =
      Let { bound_pattern; defining_expr = named; parent = denv.parent }
    in
    traverse
      { parent = let_acc;
        conts = denv.conts;
        current_code_id = denv.current_code_id
      }
      acc body

and traverse_code (acc : acc) (code_id : Code_id.t) (code : Code.t) : rev_code =
  let params_and_body = Code.params_and_body code in
  let code_metadata = Code.code_metadata code in
  let free_names_of_params_and_body = Code0.free_names code in
  (* Note: this significately degrades the analysis on zero_alloc code. However,
     it is highly unclear what should be done for zero_alloc code, so we simply
     mark the code as escaping. *)
  let never_delete =
    match Code_metadata.check code_metadata with
    | Default_check -> !Clflags.zero_alloc_check_assert_all
    | Assume { property = Zero_alloc; _ } -> false
    | Check { property = Zero_alloc; _ } -> true
  in
  let is_opaque = Code_metadata.is_opaque code_metadata in
  Flambda.Function_params_and_body.pattern_match params_and_body
    ~f:(fun
         ~return_continuation
         ~exn_continuation
         params
         ~body
         ~my_closure
         ~is_my_closure_used:_
         ~my_region
         ~my_depth
         ~free_names_of_body:_
       ->
      let code_dep = Acc.find_code acc code_id in
      if never_delete then Acc.used_code_id code_dep.code_id_for_escape acc;
      let maybe_opaque var = if is_opaque then Variable.rename var else var in
      let return = List.map maybe_opaque code_dep.return in
      let exn = maybe_opaque code_dep.exn in
      let conts =
        Continuation.Map.of_list
          [return_continuation, Normal return; exn_continuation, Normal [exn]]
      in
      let denv = { parent = Up; conts; current_code_id = Some code_id } in
      if is_opaque
      then List.iter (fun v -> Acc.used ~denv (Simple.var v) acc) (exn :: return);
      let () =
        List.iter
          (fun bp -> Acc.bound_parameter_kind bp acc)
          (Bound_parameters.to_list params)
      in
      Acc.kind (Name.var my_closure) Flambda_kind.value acc;
      Acc.kind (Name.var my_region) Flambda_kind.region acc;
      Acc.kind (Name.var my_depth) Flambda_kind.rec_info acc;
      let () =
        if is_opaque
        then
          List.iter
            (fun arg -> Acc.used ~denv (Simple.var arg) acc)
            code_dep.params
        else
          List.iter2
            (fun param arg -> Acc.func_param_dep ~denv param arg acc)
            (Bound_parameters.to_list params)
            code_dep.params
      in
      let () =
        if is_opaque
        then Acc.used ~denv (Simple.var code_dep.my_closure) acc
        else
          Acc.record_dep ~denv (Name.var my_closure)
            (Alias (Name.var code_dep.my_closure))
            acc
      in
      let body = traverse denv acc body in
      let params_and_body =
        { return_continuation;
          exn_continuation;
          params;
          body;
          my_closure;
          my_region;
          my_depth
        }
      in
      { params_and_body; code_metadata; free_names_of_params_and_body })

and traverse_cont_handler :
    type a.
    denv ->
    acc ->
    Flambda.Continuation_handler.t ->
    (cont_handler -> acc -> a) ->
    a =
 fun denv acc cont_handler k ->
  let is_exn_handler =
    Flambda.Continuation_handler.is_exn_handler cont_handler
  in
  let is_cold = Flambda.Continuation_handler.is_cold cont_handler in
  Flambda.Continuation_handler.pattern_match cont_handler
    ~f:(fun bound_parameters ~handler ->
      let () =
        List.iter
          (fun bp -> Acc.bound_parameter_kind bp acc)
          (Bound_parameters.to_list bound_parameters)
      in
      let expr = traverse denv acc handler in
      let handler = { bound_parameters; expr; is_exn_handler; is_cold } in
      k handler acc)

let debug_print = Global_flow_graph.debug_print

let run (unit : Flambda_unit.t) =
  let acc = Acc.create () in
  let holed =
    Profile.record_call ~accumulate:false "down" (fun () ->
        let dummy_toplevel_return = Variable.create "dummy_toplevel_return" in
        let dummy_toplevel_exn = Variable.create "dummy_toplevel_exn" in
        Acc.root dummy_toplevel_return acc;
        Acc.root dummy_toplevel_exn acc;
        let conts =
          Continuation.Map.of_list
            [ ( Flambda_unit.return_continuation unit,
                Normal [dummy_toplevel_return] );
              Flambda_unit.exn_continuation unit, Normal [dummy_toplevel_exn] ]
        in
        traverse
          { parent = Up; conts; current_code_id = None }
          acc (Flambda_unit.body unit))
  in
  let deps = Acc.deps acc in
  let kinds = Acc.kinds acc in
  let graph = { Global_flow_graph.toplevel_graph = snd deps; function_graphs = Hashtbl.create 0 } in
  let () = if debug_print then Dot.print_dep (Acc.code_deps acc, graph) in
  holed, deps, kinds
