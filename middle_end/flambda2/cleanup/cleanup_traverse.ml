type tail_expr =
  | Invalid of { message : string }
  | Apply_cont of Apply_cont_expr.t
  | Switch of Switch_expr.t
  | Apply of Apply_expr.t

type rev_expr_holed =
  | Up
  | Let of
      { bound_pattern : Bound_pattern.t;
        defining_expr : rev_named;
        parent : rev_expr_holed
      }
  | Let_cont of
      { cont : Continuation.t;
        handler : cont_handler;
        parent : rev_expr_holed
      }
  | Let_cont_rec of
      { invariant_params : Bound_parameters.t;
        handlers : cont_handler Continuation.Map.t;
        parent : rev_expr_holed
      }

and rev_named =
  | Named of Flambda.named
  | Set_of_closures of rev_set_of_closures
  | Static_consts of rev_static_const_or_code list

and rev_static_const_or_code =
  | Code of rev_code
  | Deleted_code
  | Static_const of Static_const.t

and rev_code =
  { params_and_body : rev_params_and_body;
    free_names_of_params_and_body : Name_occurrences.t;
    code_metadata : Code_metadata.t
  }

and rev_params_and_body =
  { return_continuation : Continuation.t;
    exn_continuation : Continuation.t;
    params : Bound_parameters.t;
    body : rev_expr;
    my_closure : Variable.t;
    my_region : Variable.t;
    my_depth : Variable.t
  }

and rev_set_of_closures =
  { value_slots : Simple.t Value_slot.Map.t;
    function_decls : Function_declarations.t;
    alloc_mode : Alloc_mode.For_allocations.t
  }

and cont_handler =
  { bound_parameters : Bound_parameters.t;
    is_exn_handler : bool;
    is_cold : bool;
    expr : rev_expr
  }

and rev_expr =
  { expr : tail_expr;
    holed_expr : rev_expr_holed
  }

module Deps = Cleanup_deps
module Dot = Cleanup_dot

type code_dep = Cleanup_dot.code_dep =
  { params : Variable.t list;
    my_closure : Variable.t;
    return : Variable.t list; (* Dummy variable representing return value *)
    exn : Variable.t (* Dummy variable representing exn return value *)
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

module Dacc : sig
  type t

  val create : unit -> t

  val kind : Name.t -> Flambda_kind.t -> t -> unit

  val bound_parameter_kind : Bound_parameter.t -> t -> unit

  val alias_kind : Name.t -> Simple.t -> t -> unit

  val kinds : t -> Flambda_kind.t Name.Map.t

  val record_dep : denv:denv -> Name.t -> Deps.Dep.t -> t -> unit

  val record_dep' : denv:denv -> Code_id_or_name.t -> Deps.Dep.t -> t -> unit

  val record_deps : denv:denv -> Code_id_or_name.t -> Deps.DepSet.t -> t -> unit

  val cont_dep : denv:denv -> Variable.t -> Simple.t -> t -> unit

  val func_param_dep : denv:denv -> Bound_parameter.t -> Variable.t -> t -> unit

  val root : Variable.t -> t -> unit

  val used : denv:denv -> Simple.t -> t -> unit

  val used_code_id : Code_id.t -> t -> unit

  val called : denv:denv -> Code_id.t -> t -> unit

  val add_apply : apply_dep -> t -> unit

  val add_code : Code_id.t -> code_dep -> t -> unit

  val find_code : t -> Code_id.t -> code_dep

  val code_deps : t -> code_dep Code_id.Map.t

  val deps : t -> Deps.graph
end = struct
  type t =
    { mutable code : code_dep Code_id.Map.t;
      mutable apply_deps : apply_dep list;
      deps : Deps.graph;
      mutable kinds : Flambda_kind.t Name.Map.t
    }

  let code_deps t = t.code

  let create () =
    { code = Code_id.Map.empty;
      apply_deps = [];
      deps =
        { toplevel_graph = Deps.create ();
          function_graphs = Hashtbl.create 100
        };
      kinds = Name.Map.empty
    }

  let kinds t = t.kinds

  let kind name k t =
    t.kinds <- Name.Map.add name k t.kinds

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
          | Naked_int32 _ -> Flambda_kind.naked_int32
          | Naked_int64 _ -> Flambda_kind.naked_int64
          | Naked_nativeint _ -> Flambda_kind.naked_nativeint
          | Naked_vec128 _ -> Flambda_kind.naked_vec128)
    in
    t.kinds <- Name.Map.add name kind t.kinds

  let add_code code_id dep t =
    t.code <- Code_id.Map.add code_id dep t.code

  let find_code t code_id = Code_id.Map.find code_id t.code

  let cur_deps_from_code_id code_id t =
    match code_id with
    | None -> t.deps.toplevel_graph
    | Some code_id -> (
      match Hashtbl.find_opt t.deps.function_graphs code_id with
      | None ->
        let deps = Deps.create () in
        Hashtbl.add t.deps.function_graphs code_id deps;
        deps
      | Some deps -> deps)

  let cur_deps ~denv t = cur_deps_from_code_id denv.current_code_id t

  let record_dep ~denv name dep t =
    let name = Code_id_or_name.name name in
    Deps.add_dep (cur_deps ~denv t) name dep

  let record_dep' ~denv code_id_or_name dep t =
    Deps.add_dep (cur_deps ~denv t) code_id_or_name dep

  let record_deps ~denv code_id_or_name deps t =
    Deps.add_deps (cur_deps ~denv t) code_id_or_name deps

  let cont_dep ~denv pat dep t =
    Simple.pattern_match dep
      ~name:(fun name ~coercion:_ ->
        Deps.add_cont_dep (cur_deps ~denv t) pat name)
      ~const:(fun _ -> ())

  let func_param_dep ~denv param arg t =
    Deps.add_func_param (cur_deps ~denv t)
      ~param:(Bound_parameter.var param)
      ~arg:(Name.var arg)

  let root v t =
    Deps.add_use t.deps.toplevel_graph (Code_id_or_name.var v)

  let used ~denv dep t =
    Simple.pattern_match dep
      ~name:(fun name ~coercion:_ ->
        Deps.add_use (cur_deps ~denv t) (Code_id_or_name.name name))
      ~const:(fun _ -> ())

  let used_code_id code_id t =
    Deps.add_use t.deps.toplevel_graph (Code_id_or_name.code_id code_id)

  let called ~denv code_id t =
    Deps.add_called (cur_deps ~denv t) code_id

  let add_apply apply t =
    t.apply_deps <- apply :: t.apply_deps

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
        List.iter2
          (fun param arg ->
            Simple.pattern_match arg
              ~name:(fun name ~coercion:_ -> Deps.add_cont_dep deps param name)
              ~const:(fun _ -> ()))
          code_dep.params apply_params;
        (match apply_closure with
        | None -> ()
        | Some apply_closure ->
          Simple.pattern_match apply_closure
            ~name:(fun name ~coercion:_ ->
              Deps.add_cont_dep deps code_dep.my_closure name)
            ~const:(fun _ -> ()));
        (match apply_return with
        | None -> ()
        | Some apply_return ->
          List.iter2
            (fun arg param -> Deps.add_cont_dep deps param (Name.var arg))
            code_dep.return apply_return);
        Deps.add_cont_dep deps apply_exn (Name.var code_dep.exn))
      t.apply_deps;
    t.deps
end

type dacc = Dacc.t

let apply_cont_deps denv dacc apply_cont =
  let cont = Apply_cont_expr.continuation apply_cont in
  let args = Apply_cont_expr.args apply_cont in
  let params = Continuation.Map.find cont denv.conts in
  let (Normal params) = params in
  List.iter2
    (fun param dep -> Dacc.cont_dep ~denv param dep dacc)
    params args

let prepare_code ~denv dacc (code_id : Code_id.t) (code : Code.t) =
  let return = [Variable.create "function_return"] in
  let exn = Variable.create "function_exn" in
  let my_closure = Variable.create "my_closure" in
  let params =
    let arity = Code.params_arity code in
    List.init (Flambda_arity.cardinal_unarized arity) (fun i ->
        Variable.create (Printf.sprintf "function_param_%i" i))
  in
  let code_dep = { return; my_closure; exn; params } in
  let () =
    (* TODO finer grain to only leak the full results when the function
       escapes *)
    let deps =
      Deps.Dep.Return_of_that_function (Name.var exn)
      :: List.map
           (fun var -> Deps.Dep.Return_of_that_function (Name.var var))
           return
    in
    List.iter
      (fun dep ->
        Dacc.record_dep' ~denv (Code_id_or_name.code_id code_id) dep dacc)
      deps
  in
  Dacc.add_code code_id code_dep dacc

let record_set_of_closures_deps ~denv names_and_function_slots set_of_closures
    dacc : unit =
  let funs =
    Function_declarations.funs (Set_of_closures.function_decls set_of_closures)
  in
  let dacc =
    Function_slot.Lmap.fold
      (fun function_slot name acc ->
        Dacc.kind name Flambda_kind.value acc;
        let ({ code_id; is_required_at_runtime }) =
          (Function_slot.Map.find function_slot funs
            : Function_declarations.code_id_in_function_declaration)
        in
        (if is_required_at_runtime
         then
           let code_id = Code_id_or_name.code_id code_id in
           Dacc.record_dep ~denv name (Deps.Dep.Contains code_id) acc);
        acc
      )
      names_and_function_slots dacc
  in
  let deps =
    Value_slot.Map.fold
      (fun value_slot simple set ->
        Simple.pattern_match
          ~const:(fun _ -> set)
          ~name:(fun name ~coercion:_ ->
            Deps.DepSet.add
              (Block (Value_slot value_slot, Code_id_or_name.name name))
              set)
          simple)
      (Set_of_closures.value_slots set_of_closures)
      Deps.DepSet.empty
  in
  let deps =
    Function_slot.Lmap.fold
      (fun function_slot name set ->
        Deps.DepSet.add
          (Block (Function_slot function_slot, Code_id_or_name.name name))
          set)
      names_and_function_slots deps
  in
  Function_slot.Lmap.iter
    (fun _function_slot name ->
      Dacc.record_deps ~denv (Code_id_or_name.name name) deps dacc)
    names_and_function_slots
let rec traverse (denv : denv) (dacc : dacc) (expr : Flambda.Expr.t) : rev_expr =
  match Flambda.Expr.descr expr with
  | Invalid { message } ->
    let expr = Invalid { message } in
    { expr; holed_expr = denv.parent }
  | Switch switch ->
    let expr = Switch switch in
    let () =
      Dacc.used ~denv (Switch_expr.scrutinee switch) dacc;
      Targetint_31_63.Map.iter
        (fun _ apply_cont -> apply_cont_deps denv dacc apply_cont)
        (Switch_expr.arms switch)
    in
    { expr; holed_expr = denv.parent }
  | Apply_cont apply_cont ->
    let expr = Apply_cont apply_cont in
    apply_cont_deps denv dacc apply_cont;
    { expr; holed_expr = denv.parent }
  | Apply apply -> begin
    let default_dacc dacc =
      (* TODO regions? *)
      let () =
        List.iter
          (fun arg -> Dacc.used ~denv arg dacc)
          (Apply_expr.args apply)
      in
      let () =
        match Apply_expr.callee apply with
        | None -> ()
        | Some callee -> Dacc.used ~denv callee dacc
      in
      let () =
        List.iter
          (fun (arg, _) -> Dacc.used ~denv arg dacc)
          (Exn_continuation.extra_args (Apply_expr.exn_continuation apply))
      in
      let () =
        match Apply_expr.call_kind apply with
        | Function _ -> ()
        | Method { obj; kind = _; alloc_mode = _ } -> Dacc.used ~denv obj dacc
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
        then
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
                  (fun param (arg, _kind) ->
                    Dacc.cont_dep ~denv param arg dacc)
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
          Dacc.add_apply apply_dep dacc;
          (* TODO regions? *)
          (* TODO record function use *)
          Dacc.called ~denv code_id dacc
        else default_dacc dacc
      | Function
          { function_call = Indirect_unknown_arity | Indirect_known_arity; _ }
      | Method _ | C_call _ ->
        default_dacc dacc
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
            } dacc cont_handler (fun handler dacc ->
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
              traverse denv dacc body))
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
          let dacc =
            (* Record kinds of bound parameters *)
            let () =
              List.iter
                (fun bp -> Dacc.bound_parameter_kind bp dacc)
                (Bound_parameters.to_list invariant_params)
            in
            let () =
              Continuation.Map.iter
                (fun _ (_, bp, _) ->
                  List.iter
                    (fun bp -> Dacc.bound_parameter_kind bp dacc)
                    (Bound_parameters.to_list bp))
                handlers
            in
            dacc
          in
          let dacc, handlers =
            Continuation.Map.fold
              (fun cont (cont_handler, bound_parameters, handler)
                   (dacc, handlers) ->
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
                    dacc handler
                in
                let handler =
                  { bound_parameters; expr; is_exn_handler; is_cold }
                in
                dacc, Continuation.Map.add cont handler handlers)
              handlers
              (dacc, Continuation.Map.empty)
          in
          let denv =
            { parent =
                Let_cont_rec
                  { invariant_params; handlers; parent = denv.parent };
              conts;
              current_code_id = denv.current_code_id
            }
          in
          traverse denv dacc body)
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
    let records dacc deps =
      List.iter
        (fun (name, dep) -> Dacc.record_dep ~denv name dep dacc)
        deps
    in
    let default_bp dacc dep =
      let bound_to = Bound_pattern.free_names bound_pattern in
      Name_occurrences.fold_names bound_to
        ~f:(fun () bound_to -> Dacc.record_dep ~denv bound_to dep dacc)
        ~init:()
    in
    let default_bps dacc dep = List.iter (default_bp dacc) dep in
    let default dacc =
      Name_occurrences.fold_names
        ~f:(fun () free_name -> default_bp dacc (Deps.Dep.Use free_name))
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
          set_of_closures dacc
      | Static_consts group ->
        (* TODO kind *)
        let bound_static =
          match bound_pattern with
          | Static b -> b
          | Singleton _ | Set_of_closures _ -> assert false
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
              set_of_closures dacc)
          ~block_like:(fun () symbol static_const ->
            let name = Name.symbol symbol in
            match[@ocaml.warning "-4"] static_const with
            | Block (_, _, fields) | Immutable_value_array fields ->
              let acc = ref [] in
              List.iteri
                (fun i (field : Field_of_static_block.t) ->
                  match field with
                  | Symbol s ->
                    acc
                      := ( name,
                           Deps.Dep.Block
                             (Deps.Block i, Code_id_or_name.symbol s) )
                         :: !acc
                  | Tagged_immediate _ -> ()
                  | Dynamically_computed (v, _) ->
                    acc
                      := ( name,
                           Deps.Dep.Block (Deps.Block i, Code_id_or_name.var v)
                         )
                         :: !acc)
                fields;
              records dacc !acc
            | Set_of_closures _ -> assert false
            | _ -> ())
      | Prim (prim, _dbg) -> begin
        let () =
          let kind = Flambda_primitive.result_kind' prim in
          let name =
            Name.var @@ Bound_var.var
            @@ Bound_pattern.must_be_singleton bound_pattern
          in
          Dacc.kind name kind dacc
        in
        match[@ocaml.warning "-4"] prim with
        | Variadic (Make_block (_, _mutability, _), fields) ->
          let acc = ref [] in
          List.iteri
            (fun i field ->
              Simple.pattern_match field
                ~name:(fun name ~coercion:_ ->
                  acc
                    := Deps.Dep.Block (Deps.Block i, Code_id_or_name.name name)
                       :: !acc)
                ~const:(fun _ -> ()))
            fields;
          default_bps dacc !acc
        | Unary (Project_function_slot { move_from = _; move_to }, block) ->
          let block =
            Simple.pattern_match block
              ~name:(fun name ~coercion:_ -> name)
              ~const:(fun _ -> assert false)
          in
          let dep = Deps.Dep.Field (Function_slot move_to, block) in
          default_bp dacc dep
        | Unary (Project_value_slot { project_from = _; value_slot }, block) ->
          let block =
            Simple.pattern_match block
              ~name:(fun name ~coercion:_ -> name)
              ~const:(fun _ -> assert false)
          in
          let dep = Deps.Dep.Field (Value_slot value_slot, block) in
          default_bp dacc dep
        | Binary (Block_load (_access_kind, _mutability), block, field) -> begin
          (* Loads from mutable blocks are tracked here. This is ok as long as
             store are properly tracked also. This is a flow insensitive
             dependency analysis: this might produce surprising results
             sometimes *)
          (match known_field_of_block field block with
          | None -> default dacc
          | Some (field, block) ->
            default_bp dacc (Deps.Dep.Field (Block field, block)))
        end
        (*        | Ternary
         *           (Block_set (_access_kind, _init_or_assign), block, field, value) ->
         *         begin
         *         match known_field_of_block field block with
         *         | None ->
         *           (* When the field is unknown, the value (and the field) escapes, but
         *              only if the block is live *)
         *           let block =
         *             Simple.pattern_match block
         *               ~name:(fun name ~coercion:_ -> name)
         *               ~const:(fun _ -> assert false)
         *           in
         *           let field_dep =
         *             Simple.pattern_match field
         *               ~name:(fun name ~coercion:_ -> [block, Deps.Dep.Use name])
         *               ~const:(fun _ -> [])
         *             (* TODO improve ! *)
         *           in
         *           let value_dep =
         *             Simple.pattern_match value
         *               ~name:(fun name ~coercion:_ ->
         *                 [block, Deps.Dep.Contains (Code_id_or_name.name name)])
         *               ~const:(fun _ -> [])
         *           in
         *           let effect_dep =
         *             (* A bit too hackish ? *)
         *             (* If the block is used: effects on it have to happen, otherwise
         *                we can drop it *)
         *             let bound_to = Bound_pattern.free_names bound_pattern in
         *             Name_occurrences.fold_names bound_to
         *               ~f:(fun acc bound_to -> (block, Deps.Dep.Use bound_to) :: acc)
         *               ~init:[]
         *           in
         *           let dep = field_dep @ value_dep @ effect_dep in
         *           records dacc dep
         *         | Some (field, block) ->
         *           let value_dep =
         *             Simple.pattern_match value
         *               ~name:(fun name ~coercion:_ ->
         *                 [ ( block,
         *                     Deps.Dep.Block (Block field, Code_id_or_name.name name) )
         *                 ])
         *               ~const:(fun _ -> [])
         *           in
         *           let effect_dep =
         *             (* A bit too hackish ? *)
         *             (* If the block is used: effects on it have to happen, otherwise
         *                we can drop it *)
         *             let bound_to = Bound_pattern.free_names bound_pattern in
         *             Name_occurrences.fold_names bound_to
         *               ~f:(fun acc bound_to -> (block, Deps.Dep.Block (Block field, Code_id_or_name.name bound_to)) :: acc)
         *               ~init:[]
         *           in
         *           (* TODO: record dependency on the primitive for side effects *)
         *           records dacc (value_dep @ effect_dep)
         *         end *)
        | prim ->
          let () =
            match Flambda_primitive.effects_and_coeffects prim with
            | Arbitrary_effects, _, _ ->
              let bound_to = Bound_pattern.free_names bound_pattern in
              Name_occurrences.fold_names bound_to
                ~f:(fun () bound_to ->
                  Dacc.used ~denv (Simple.name bound_to) dacc)
                ~init:()
            | _ -> ()
          in
          default dacc
      end
      | Simple s ->
        (* TODO kind *)
        let () =
          let name =
            Name.var @@ Bound_var.var
            @@ Bound_pattern.must_be_singleton bound_pattern
          in
          Dacc.alias_kind name s dacc
        in
        Simple.pattern_match s
          ~name:(fun name ~coercion:_ -> default_bp dacc (Deps.Dep.Alias name))
          ~const:(fun _ -> default dacc)
      | Rec_info _ ->
        (* TODO kind *)
        default dacc
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
        let () =
          Flambda.Static_const_group.match_against_bound_static group
            bound_static ~init:() ~code:(fun () -> prepare_code ~denv dacc)
            ~deleted_code:(fun _ _ -> ())
            ~set_of_closures:(fun _ ~closure_symbols:_ _ -> ())
            ~block_like:(fun _ _ _ -> ())
        in
        let rev_group =
          Flambda.Static_const_group.match_against_bound_static group
            bound_static ~init:([])
            ~code:(fun (rev_group) code_id code ->
              let code = traverse_code dacc code_id code in
              Code code :: rev_group)
            ~deleted_code:(fun (rev_group) _ ->
              Deleted_code :: rev_group)
            ~set_of_closures:(fun
                (rev_group) ~closure_symbols:_ set_of_closures ->
              let static_const = Static_const.set_of_closures set_of_closures in
              Static_const static_const :: rev_group)
            ~block_like:(fun (rev_group) _symbol static_const ->
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
      dacc body

and traverse_code (dacc : dacc) (code_id : Code_id.t) (code : Code.t) :
    rev_code =
  let params_and_body = Code.params_and_body code in
  let code_metadata = Code.code_metadata code in
  let free_names_of_params_and_body = Code0.free_names code in
  (* Note: this significately degrades the analysis on zero_alloc code. However,
     it is highly unclear what should be done for zero_alloc code, so we simply
     mark the code as escaping. *)
  let never_delete =
    match Code_metadata.check code_metadata with
    | Default_check -> !Clflags.zero_alloc_check_assert_all
    | Ignore_assert_all Zero_alloc -> false
    | Assume { property = Zero_alloc; _ } -> false
    | Check { property = Zero_alloc; _ } -> true
  in
  if never_delete then Dacc.used_code_id code_id dacc;
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
      let code_dep = Dacc.find_code dacc code_id in
      let conts =
        Continuation.Map.of_list
          [ return_continuation, Normal code_dep.return;
            exn_continuation, Normal [code_dep.exn] ]
      in
      let denv = { parent = Up; conts; current_code_id = Some code_id } in
      let () =
        List.iter
          (fun bp -> Dacc.bound_parameter_kind bp dacc)
          (Bound_parameters.to_list params)
      in
      Dacc.kind (Name.var my_closure) Flambda_kind.value dacc;
      Dacc.kind (Name.var my_region) Flambda_kind.region dacc;
      Dacc.kind (Name.var my_depth) Flambda_kind.rec_info dacc;
      let () =
        List.iter2
          (fun param arg -> Dacc.func_param_dep ~denv param arg dacc)
          (Bound_parameters.to_list params)
          code_dep.params
      in
      let () =
        Dacc.record_dep ~denv (Name.var my_closure)
          (Alias (Name.var code_dep.my_closure))
          dacc
      in
      let body = traverse denv dacc body in
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
    dacc ->
    Flambda.Continuation_handler.t ->
    (cont_handler -> dacc -> a) ->
    a =
 fun denv dacc cont_handler k ->
  let is_exn_handler =
    Flambda.Continuation_handler.is_exn_handler cont_handler
  in
  let is_cold = Flambda.Continuation_handler.is_cold cont_handler in
  Flambda.Continuation_handler.pattern_match cont_handler
    ~f:(fun bound_parameters ~handler ->
      let () =
        List.iter
          (fun bp -> Dacc.bound_parameter_kind bp dacc)
          (Bound_parameters.to_list bound_parameters)
      in
      let expr = traverse denv dacc handler in
      let handler = { bound_parameters; expr; is_exn_handler; is_cold } in
      k handler dacc)

let do_print = Cleanup_deps.do_print

let run (unit : Flambda_unit.t) =
  let dacc = Dacc.create () in
  let holed =
    Profile.record_call ~accumulate:false "down" (fun () ->
        let dummy_toplevel_return = Variable.create "dummy_toplevel_return" in
        let dummy_toplevel_exn = Variable.create "dummy_toplevel_exn" in
        Dacc.root dummy_toplevel_return dacc;
        Dacc.root dummy_toplevel_exn dacc;
        let conts =
          Continuation.Map.of_list
            [ ( Flambda_unit.return_continuation unit,
                Normal [dummy_toplevel_return] );
              Flambda_unit.exn_continuation unit, Normal [dummy_toplevel_exn] ]
        in
        traverse
          { parent = Up; conts; current_code_id = None }
          dacc (Flambda_unit.body unit))
  in
  let deps = Dacc.deps dacc in
  let kinds = Dacc.kinds dacc in
  let () = if do_print then Dot.print_dep (Dacc.code_deps dacc, deps) in
  holed, deps, kinds
