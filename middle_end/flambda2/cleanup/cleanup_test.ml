[@@@ocaml.warning "-32"]

[@@@ocaml.warning "-34"]

module Rebuilt_expr = struct
  type continuation_handler =
    { handler : Flambda.Continuation_handler.t;
      free_names : Name_occurrences.t
    }

  type continuation_handlers =
    { handlers : Flambda.Continuation_handler.t Continuation.Map.t;
      free_names : Name_occurrences.t
    }

  type t =
    { expr : Flambda.Expr.t;
      free_names : Name_occurrences.t
    }

  let create_let bound_pattern defining_expr ~body =
    let free_names =
      Name_occurrences.diff
        (Name_occurrences.union
           (Flambda.Named.free_names defining_expr)
           body.free_names)
        ~without:(Bound_pattern.free_names bound_pattern)
    in
    let let_expr =
      Flambda.Let_expr.create bound_pattern defining_expr ~body:body.expr
        ~free_names_of_body:(Known body.free_names)
    in
    let expr = Flambda.Expr.create_let let_expr in
    { expr; free_names }

  let create_continuation_handler bound_parameters ~handler ~is_exn_handler
      ~is_cold =
    let free_names =
      Name_occurrences.diff handler.free_names
        ~without:(Bound_parameters.free_names bound_parameters)
    in
    let handler =
      Flambda.Continuation_handler.create bound_parameters ~handler:handler.expr
        ~free_names_of_handler:(Known handler.free_names) ~is_exn_handler
        ~is_cold
    in
    { handler; free_names }

  let create_continuation_handlers handlers =
    Continuation.Map.fold
      (fun cont (handler : continuation_handler) { handlers; free_names } ->
        let handlers = Continuation.Map.add cont handler.handler handlers in
        let free_names = Name_occurrences.union free_names handler.free_names in
        { handlers; free_names })
      handlers
      { handlers = Continuation.Map.empty; free_names = Name_occurrences.empty }

  let create_non_recursive_let_cont cont (cont_handler : continuation_handler)
      ~body =
    let expr =
      Flambda.Let_cont_expr.create_non_recursive cont cont_handler.handler
        ~body:body.expr ~free_names_of_body:(Known body.free_names)
    in
    let free_names =
      Name_occurrences.union body.free_names cont_handler.free_names
    in
    { expr; free_names }

  let create_recursive_let_cont ~invariant_params handlers ~body =
    let handlers = create_continuation_handlers handlers in
    let expr =
      Flambda.Let_cont_expr.create_recursive ~invariant_params handlers.handlers
        ~body:body.expr
    in
    let free_names =
      Name_occurrences.union body.free_names handlers.free_names
    in
    { expr; free_names }

  let from_expr ~expr ~free_names = { expr; free_names }
end

module RE = Rebuilt_expr

type apply_cont =
  { cont : Continuation.t;
    trap_action : Trap_action.t option;
    args : Simple.t list;
    dbg : Debuginfo.t
  }

type tail_expr =
  | Raw of Flambda.Expr.t
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
    holed_expr : rev_expr_holed;
    free_names : Name_occurrences.t
  }

module Deps = Cleanup_deps

type code_dep =
  { params : Variable.t list;
    my_closure : Variable.t;
    return : Variable.t list; (* Dummy variable representing return value *)
    exn : Variable.t (* Dummy variable representing exn return value *)
  }

type apply_dep =
  { apply_in_func : Code_id.t option;
    apply_code_id : Code_id.t;
    apply_params : Simple.t list;
    apply_closure : Simple.t;
    apply_return : Variable.t list option;
    apply_exn : Variable.t
  }

module Dot = struct
  let dep_graph_ppf =
    lazy
      (let filename = "dep.dot" in
       let ch = open_out filename in
       let ppf = Format.formatter_of_out_channel ch in
       (* Format.fprintf ppf "digraph g {@\n"; *)
       (* at_exit (fun () -> *)
       (*     Format.fprintf ppf "@\n}@."; *)
       (*     close_out ch); *)
       ppf)

  let dot_count = ref ~-1

  let print_graph ~print ~print_name ~lazy_ppf ~graph =
    match print_name with
    | None -> ()
    | Some print_name ->
      incr dot_count;
      let ppf = Lazy.force lazy_ppf in
      Format.fprintf ppf "digraph g {@\n";
      print ~ctx:!dot_count ~print_name ppf graph;
      Format.fprintf ppf "@\n}@."

  module P = struct
    let node_id ~ctx ppf (variable : Code_id_or_name.t) =
      Format.fprintf ppf "node_%d_%d" ctx (variable :> int)

    let node ~ctx ~root ppf name =
      if root
      then
        Format.fprintf ppf "%a [shape=record label=\"%a\"];@\n" (node_id ~ctx)
          name Code_id_or_name.print name
      else
        Format.fprintf ppf "%a [label=\"%a\"];@\n" (node_id ~ctx) name
          Code_id_or_name.print name

    let dep_names (dep : Deps.Dep.t) =
      match dep with
      | Deps.Dep.Return_of_that_function n
      | Deps.Dep.Alias n
      | Deps.Dep.Use n
      | Deps.Dep.Field (_, n) ->
        [Code_id_or_name.name n]
      | Deps.Dep.Contains n | Deps.Dep.Block (_, n) -> [n]
      | Deps.Dep.Apply (n, c) ->
        [Code_id_or_name.name n; Code_id_or_name.code_id c]

    let all_names t =
      let names = Hashtbl.create 100 in
      Hashtbl.iter
        (fun name dep ->
          let dep_names =
            Deps.DepSet.fold (fun dep acc -> dep_names dep @ acc) dep []
          in
          List.iter
            (fun name -> Hashtbl.replace names name ())
            (name :: dep_names))
        t.Deps.name_to_dep;
      names

    let nodes ~all_cdep ~ctx ppf t =
      Hashtbl.iter
        (fun name _ ->
          if Code_id_or_name.Set.mem name all_cdep
          then ()
          else
            let root =
              Code_id_or_name.pattern_match'
                ~code_id:(fun _ -> false)
                ~name:(fun name ->
                  Hashtbl.mem t.Deps.used (Code_id_or_name.name name))
                name
            in
            node ~ctx ~root ppf name)
        (all_names t)

    let edge ~ctx ppf src (dst : Deps.Dep.t) =
      let color, deps =
        match dst with
        | Return_of_that_function name -> "purple", [Code_id_or_name.name name]
        | Alias name -> "black", [Code_id_or_name.name name]
        | Use name ->
          (* ignore name; *)
          (* "red", [] *)
          "red", [Code_id_or_name.name name]
        | Contains name -> "yellow", [name]
        | Field (_, name) -> "green", [Code_id_or_name.name name]
        | Block (_, name) -> "blue", [name]
        | Apply (name, _code) -> "pink", [Code_id_or_name.name name]
      in
      List.iter
        (fun dst ->
          Format.fprintf ppf "%a -> %a [color=\"%s\"];@\n" (node_id ~ctx) src
            (node_id ~ctx) dst color)
        deps

    let edges ~ctx ppf t =
      Hashtbl.iter
        (fun src dst_set ->
          Deps.DepSet.iter (fun dst -> edge ~ctx ppf src dst) dst_set)
        t.Deps.name_to_dep

    let code_deps ~ctx ~code_id ppf code_dep =
      node ~ctx ~root:false ppf (Code_id_or_name.code_id code_id);
      node ~ctx ~root:false ppf (Code_id_or_name.var code_dep.my_closure);
      List.iter
        (fun v -> node ~ctx ~root:false ppf (Code_id_or_name.var v))
        ((code_dep.exn :: code_dep.return) @ code_dep.params)

    let print_fundep ~all_cdep ~code_dep ~ctx (code_id : Code_id.t) ppf t =
      Format.fprintf ppf
        "subgraph cluster_%d_%d { label=\"%a\"@\n\
        \ subgraph cluster_%d_%d_intf { label=\"interface\"@\n\
        \ %a } @\n\
         %a@\n\
         @\n\
         }@\n"
        ctx
        (code_id :> int)
        Code_id.print code_id ctx
        (code_id :> int)
        (code_deps ~ctx ~code_id) code_dep (nodes ~all_cdep ~ctx) t

    let print_fundeps ~all_cdep ~code_dep ~ctx ppf fundeps =
      Hashtbl.iter
        (fun code_id fungraph ->
          print_fundep ~all_cdep
            ~code_dep:(Code_id.Map.find code_id code_dep)
            ~ctx code_id ppf fungraph)
        fundeps

    let all_edges ~ctx ppf (t : Deps.graph) =
      edges ~ctx ppf t.toplevel_graph;
      Hashtbl.iter (fun _ fungraph -> edges ~ctx ppf fungraph) t.function_graphs

    let print ~ctx ~print_name ppf
        ((code_dep, t) : code_dep Code_id.Map.t * Deps.graph) =
      let all_cdep =
        Code_id.Map.fold
          (fun code_id dep s ->
            List.fold_left
              (fun s x -> Code_id_or_name.Set.add x s)
              s
              (Code_id_or_name.code_id code_id
              :: List.map Code_id_or_name.var
                   ((dep.my_closure :: dep.exn :: dep.return) @ dep.params)))
          code_dep Code_id_or_name.Set.empty
      in
      Flambda_colours.without_colours ~f:(fun () ->
          Format.fprintf ppf
            "subgraph cluster_%d { label=\"%s\"@\n%a@\n%a@\n%a}@." ctx
            print_name (nodes ~all_cdep ~ctx) t.toplevel_graph
            (print_fundeps ~all_cdep ~code_dep ~ctx)
            t.function_graphs (all_edges ~ctx) t)
  end

  let print_dep dep =
    let print_name = Some "dep" in
    print_graph ~print_name ~lazy_ppf:dep_graph_ppf ~graph:dep ~print:P.print
end

type cont_kind = Normal of Variable.t list

type denv =
  { parent : rev_expr_holed;
    conts : cont_kind Continuation.Map.t;
    current_code_id : Code_id.t option
  }

module Dacc : sig
  type t

  val empty : unit -> t

  val kind : Name.t -> Flambda_kind.t -> t -> t

  val bound_parameter_kind : Bound_parameter.t -> t -> t

  val alias_kind : Name.t -> Simple.t -> t -> t

  val kinds : t -> Flambda_kind.t Name.Map.t

  val let_ : t -> t

  val let_cont : t -> t

  val let_rec_cont : t -> t

  val func : t -> t

  val let_dep : denv:denv -> Bound_pattern.t -> Deps.Dep.t -> t -> t

  val record_dep : denv:denv -> Name.t -> Deps.Dep.t -> t -> t

  val record_dep' : denv:denv -> Code_id_or_name.t -> Deps.Dep.t -> t -> t

  val record_deps : denv:denv -> Code_id_or_name.t -> Deps.DepSet.t -> t -> t

  val cont_dep : denv:denv -> Variable.t -> Simple.t -> t -> t

  val func_param_dep : denv:denv -> Bound_parameter.t -> Variable.t -> t -> t

  val root : Variable.t -> t -> t

  val used : denv:denv -> Simple.t -> t -> t

  val used_code_id : denv:denv -> Code_id.t -> t -> t

  val opaque_let_dependency :
    denv:denv -> Bound_pattern.t -> Name_occurrences.t -> t -> t

  val let_field : denv:denv -> Bound_pattern.t -> Deps.field -> Name.t -> t -> t

  val add_apply : apply_dep -> t -> t

  val add_code : Code_id.t -> code_dep -> t -> t

  val find_code : t -> Code_id.t -> code_dep

  val code_deps : t -> code_dep Code_id.Map.t

  val pp : Format.formatter -> t -> unit

  val deps : t -> Deps.graph

  val todo : unit -> t

  val todo' : t -> t
end = struct
  type t =
    { let_ : int;
      let_cont : int;
      let_rec_cont : int;
      func : int;
      code : code_dep Code_id.Map.t;
      apply_deps : apply_dep list;
      deps : Deps.graph;
      kinds : Flambda_kind.t Name.Map.t
    }

  let pp ppf t =
    Format.fprintf ppf "let : %i@ let_cont : %i@ let_rec_cont : %i@ func : %i"
      t.let_ t.let_cont t.let_rec_cont t.func

  let code_deps t = t.code

  let empty () =
    { let_ = 0;
      let_cont = 0;
      let_rec_cont = 0;
      func = 0;
      code = Code_id.Map.empty;
      apply_deps = [];
      deps =
        { toplevel_graph = Deps.create ();
          function_graphs = Hashtbl.create 100
        };
      kinds = Name.Map.empty
    }

  let kinds t = t.kinds

  let kind name k t = { t with kinds = Name.Map.add name k t.kinds }

  let bound_parameter_kind (bp : Bound_parameter.t) t =
    let kind = Flambda_kind.With_subkind.kind (Bound_parameter.kind bp) in
    let name = Name.var (Bound_parameter.var bp) in
    { t with kinds = Name.Map.add name kind t.kinds }

  let alias_kind name simple t =
    let kind =
      Simple.pattern_match simple
        ~name:(fun name ~coercion:_ ->
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
    { t with kinds = Name.Map.add name kind t.kinds }

  let add_code code_id dep t =
    { t with code = Code_id.Map.add code_id dep t.code }

  let find_code t code_id = Code_id.Map.find code_id t.code

  let let_ t = { t with let_ = t.let_ + 1 }

  let let_cont t = { t with let_cont = t.let_cont + 1 }

  let let_rec_cont t = { t with let_rec_cont = t.let_rec_cont + 1 }

  let func t = { t with func = t.func + 1 }

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

  let opaque_let_dependency ~denv pat fv t =
    Deps.add_opaque_let_dependency (cur_deps ~denv t) pat fv;
    t

  let let_field ~denv pat field name t =
    Deps.add_let_field (cur_deps ~denv t) pat field name;
    t

  let let_dep ~denv pat dep t =
    Deps.add_let_dep (cur_deps ~denv t) pat dep;
    t

  let record_dep ~denv name dep t =
    let name = Code_id_or_name.name name in
    Deps.add_dep (cur_deps ~denv t) name dep;
    t

  let record_dep' ~denv code_id_or_name dep t =
    Deps.add_dep (cur_deps ~denv t) code_id_or_name dep;
    t

  let record_deps ~denv code_id_or_name deps t =
    Deps.add_deps (cur_deps ~denv t) code_id_or_name deps;
    t

  let cont_dep ~denv pat dep t =
    Simple.pattern_match dep
      ~name:(fun name ~coercion:_ ->
        Deps.add_cont_dep (cur_deps ~denv t) pat name)
      ~const:(fun _ -> ());
    t

  let func_param_dep ~denv param arg t =
    Deps.add_func_param (cur_deps ~denv t)
      ~param:(Bound_parameter.var param)
      ~arg:(Name.var arg);
    t

  let root v t =
    Deps.add_use t.deps.toplevel_graph (Code_id_or_name.var v);
    t

  let used ~denv dep t =
    Simple.pattern_match dep
      ~name:(fun name ~coercion:_ ->
        Deps.add_use (cur_deps ~denv t) (Code_id_or_name.name name))
      ~const:(fun _ -> ());
    t

  let used_code_id ~denv code_id t =
    Deps.add_use (cur_deps ~denv t) (Code_id_or_name.code_id code_id);
    t

  let add_apply apply t = { t with apply_deps = apply :: t.apply_deps }

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
        Simple.pattern_match apply_closure
          ~name:(fun name ~coercion:_ ->
            Deps.add_cont_dep deps code_dep.my_closure name)
          ~const:(fun _ -> ());
        (match apply_return with
        | None -> ()
        | Some apply_return ->
          List.iter2
            (fun arg param -> Deps.add_cont_dep deps param (Name.var arg))
            code_dep.return apply_return);
        Deps.add_cont_dep deps apply_exn (Name.var code_dep.exn))
      t.apply_deps;
    t.deps

  let todo _ = empty ()

  let todo' t = t
end

type dacc = Dacc.t

type handlers = cont_handler Continuation.Map.t

let apply_cont_deps denv dacc apply_cont =
  let cont = Apply_cont_expr.continuation apply_cont in
  let args = Apply_cont_expr.args apply_cont in
  let params = Continuation.Map.find cont denv.conts in
  let (Normal params) = params in
  List.fold_left2
    (fun dacc param dep -> Dacc.cont_dep ~denv param dep dacc)
    dacc params args

let prepare_code ~denv dacc (code_id : Code_id.t) (code : Code.t) =
  let return = [Variable.create "function_return"] in
  let exn = Variable.create "function_exn" in
  let my_closure = Variable.create "my_closure" in
  let params =
    let arity = Code.params_arity code in
    List.init (Flambda_arity.cardinal arity) (fun i ->
        Variable.create (Printf.sprintf "function_param_%i" i))
  in
  let code_dep = { return; my_closure; exn; params } in
  let dacc =
    (* TODO finer grain to only leak the full results when the function
       escapes *)
    let deps =
      Deps.Dep.Return_of_that_function (Name.var exn)
      :: List.map
           (fun var -> Deps.Dep.Return_of_that_function (Name.var var))
           return
    in
    List.fold_left
      (fun dacc dep ->
        Dacc.record_dep' ~denv (Code_id_or_name.code_id code_id) dep dacc)
      dacc deps
  in
  Dacc.add_code code_id code_dep dacc

let record_set_of_closures_deps ~denv names_and_function_slots set_of_closures
    dacc =
  let funs =
    Function_declarations.funs (Set_of_closures.function_decls set_of_closures)
  in
  let dacc =
    Function_slot.Lmap.fold
      (fun function_slot name acc ->
        let code_id = Function_slot.Map.find function_slot funs in
        let code_id = Code_id_or_name.code_id code_id in
        Dacc.record_dep ~denv name (Deps.Dep.Contains code_id) acc)
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
  Function_slot.Lmap.fold
    (fun _function_slot name dacc ->
      Dacc.record_deps ~denv (Code_id_or_name.name name) deps dacc)
    names_and_function_slots dacc

let rec traverse (denv : denv) (dacc : dacc) (expr : Flambda.Expr.t) =
  match Flambda.Expr.descr expr with
  | Invalid _ ->
    let expr = Raw expr in
    ( { expr; holed_expr = denv.parent; free_names = Name_occurrences.empty },
      dacc )
  | Switch switch ->
    let expr = Switch switch in
    let dacc =
      let dacc = Dacc.used ~denv (Switch_expr.scrutinee switch) dacc in
      Targetint_31_63.Map.fold
        (fun _ apply_cont dacc -> apply_cont_deps denv dacc apply_cont)
        (Switch_expr.arms switch) dacc
    in
    ( { expr;
        holed_expr = denv.parent;
        free_names = Flambda.Switch.free_names switch
      },
      dacc )
  | Apply_cont apply_cont ->
    let expr = Apply_cont apply_cont in
    let dacc = apply_cont_deps denv dacc apply_cont in
    ( { expr;
        holed_expr = denv.parent;
        free_names = Flambda.Apply_cont.free_names apply_cont
      },
      dacc )
  | Apply apply -> begin
    let default_dacc dacc =
      (* TODO regions? *)
      let dacc =
        List.fold_left
          (fun dacc arg -> Dacc.used ~denv arg dacc)
          dacc (Apply_expr.args apply)
      in
      let dacc = Dacc.used ~denv (Apply_expr.callee apply) dacc in
      let dacc =
        List.fold_left
          (fun dacc (arg, _) -> Dacc.used ~denv arg dacc)
          dacc
          (Exn_continuation.extra_args (Apply_expr.exn_continuation apply))
      in
      dacc
    in
    let dacc =
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
          let dacc, exn_arg =
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
              let dacc =
                List.fold_left2
                  (fun dacc param (arg, _kind) ->
                    Dacc.cont_dep ~denv param arg dacc)
                  dacc extra_params extra_args
              in
              dacc, exn_param
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
          let dacc = Dacc.add_apply apply_dep dacc in
          (* TODO regions? *)
          (* TODO record function use *)
          let dacc = Dacc.used_code_id ~denv code_id dacc in
          dacc
        else default_dacc dacc
      | Function
          { function_call = Indirect_unknown_arity | Indirect_known_arity; _ }
      | Method _ | C_call _ ->
        default_dacc dacc
    in
    let expr = Apply apply in
    ( { expr;
        holed_expr = denv.parent;
        free_names = Flambda.Apply.free_names apply
      },
      dacc )
  end
  | Let_cont let_cont -> begin
    match let_cont with
    | Non_recursive
        { handler; num_free_occurrences = _; is_applied_with_traps = _ } ->
      let dacc = Dacc.let_cont dacc in
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
      let dacc = Dacc.let_rec_cont dacc in
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
            let dacc =
              List.fold_left
                (fun dacc bp -> Dacc.bound_parameter_kind bp dacc)
                dacc
                (Bound_parameters.to_list invariant_params)
            in
            let dacc =
              Continuation.Map.fold
                (fun _ (_, bp, _) dacc ->
                  List.fold_left
                    (fun dacc bp -> Dacc.bound_parameter_kind bp dacc)
                    dacc
                    (Bound_parameters.to_list bp))
                handlers dacc
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
                let expr, dacc =
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
    let dacc = Dacc.let_ dacc in
    let defining_expr = Flambda.Let_expr.defining_expr let_expr in
    let known_field_of_block field block =
      Simple.pattern_match field
        ~name:(fun _ ~coercion:_ -> None)
        ~const:(fun cst ->
          let block =
            Simple.pattern_match block
              ~name:(fun name ~coercion:_ -> name)
              ~const:(fun _ -> assert false)
          in
          match[@ocaml.warning "-4"] Int_ids.Const.descr cst with
          | Tagged_immediate i ->
            let i = Targetint_31_63.to_int_exn i in
            Some (i, block)
          | _ -> assert false)
    in
    let records dacc deps =
      List.fold_left
        (fun dacc (name, dep) -> Dacc.record_dep ~denv name dep dacc)
        dacc deps
    in
    let default_bp dacc dep : Dacc.t =
      let bound_to = Bound_pattern.free_names bound_pattern in
      Name_occurrences.fold_names bound_to
        ~f:(fun dacc bound_to -> Dacc.record_dep ~denv bound_to dep dacc)
        ~init:dacc
    in
    let default_bps dacc dep : Dacc.t = List.fold_left default_bp dacc dep in
    let default dacc : Dacc.t =
      Name_occurrences.fold_names
        ~f:(fun dacc free_name -> default_bp dacc (Deps.Dep.Use free_name))
        ~init:dacc
        (Flambda.Named.free_names defining_expr)
    in
    let dacc : Dacc.t =
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
          ~init:dacc
          ~code:(fun dacc _code_id _code ->
            (* TODO: (is there anything to do here ? *)
            dacc)
          ~deleted_code:(fun dacc _ -> dacc)
          ~set_of_closures:(fun dacc ~closure_symbols set_of_closures ->
            let names_and_function_slots =
              Function_slot.Lmap.map Name.symbol closure_symbols
            in
            record_set_of_closures_deps ~denv names_and_function_slots
              set_of_closures dacc)
          ~block_like:(fun dacc symbol static_const ->
            let name = Name.symbol symbol in
            match[@ocaml.warning "-4"] static_const with
            | Block (_tag, _mut, fields) ->
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
            | _ -> dacc)
      | Prim (prim, _dbg) -> begin
        let dacc =
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
        | Unary
            ( Project_value_slot { project_from = _; value_slot; kind = _ },
              block ) ->
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
          match known_field_of_block field block with
          | None -> default dacc
          | Some (field, block) ->
            default_bp dacc (Deps.Dep.Field (Block field, block))
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
          let dacc =
            match Flambda_primitive.effects_and_coeffects prim with
            | Arbitrary_effects, _, _ ->
              let bound_to = Bound_pattern.free_names bound_pattern in
              Name_occurrences.fold_names bound_to
                ~f:(fun dacc bound_to ->
                  Dacc.used ~denv (Simple.name bound_to) dacc)
                ~init:dacc
            | _ -> dacc
          in
          default dacc
      end
      | Simple s ->
        (* TODO kind *)
        let dacc =
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
    let (named, dacc) : rev_named * dacc =
      match defining_expr with
      | Set_of_closures set_of_closures ->
        let function_decls = Set_of_closures.function_decls set_of_closures in
        let value_slots = Set_of_closures.value_slots set_of_closures in
        let alloc_mode = Set_of_closures.alloc_mode set_of_closures in
        let set_of_closures = { function_decls; value_slots; alloc_mode } in
        Set_of_closures set_of_closures, dacc
      | Static_consts group ->
        let bound_static =
          match bound_pattern with
          | Static b -> b
          | Singleton _ | Set_of_closures _ -> assert false
        in
        let dacc =
          Flambda.Static_const_group.match_against_bound_static group
            bound_static ~init:dacc ~code:(prepare_code ~denv)
            ~deleted_code:(fun dacc _ -> dacc)
            ~set_of_closures:(fun dacc ~closure_symbols:_ _ -> dacc)
            ~block_like:(fun dacc _ _ -> dacc)
        in
        let dacc, rev_group =
          Flambda.Static_const_group.match_against_bound_static group
            bound_static ~init:(dacc, [])
            ~code:(fun (dacc, rev_group) code_id code ->
              let code, dacc = traverse_code dacc code_id code in
              dacc, Code code :: rev_group)
            ~deleted_code:(fun (dacc, rev_group) _ ->
              dacc, Deleted_code :: rev_group)
            ~set_of_closures:
              (fun (dacc, rev_group) ~closure_symbols:_ set_of_closures ->
              let static_const = Static_const.set_of_closures set_of_closures in
              dacc, Static_const static_const :: rev_group)
            ~block_like:(fun (dacc, rev_group) _symbol static_const ->
              (* TODO: register make block deps *)
              dacc, Static_const static_const :: rev_group)
        in
        let group = List.rev rev_group in
        Static_consts group, dacc
      (* TODO set_of_closures in Static_consts *)
      | Prim _ -> Named defining_expr, dacc
      | Simple _ -> Named defining_expr, dacc
      | Rec_info _ as defining_expr -> Named defining_expr, dacc
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
    rev_code * dacc =
  let dacc = Dacc.func dacc in
  let params_and_body = Code.params_and_body code in
  let code_metadata = Code.code_metadata code in
  let free_names_of_params_and_body = Code0.free_names code in
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
      let dacc =
        List.fold_left
          (fun dacc bp -> Dacc.bound_parameter_kind bp dacc)
          dacc
          (Bound_parameters.to_list params)
      in
      let dacc = Dacc.kind (Name.var my_closure) Flambda_kind.value dacc in
      let dacc = Dacc.kind (Name.var my_region) Flambda_kind.region dacc in
      let dacc = Dacc.kind (Name.var my_depth) Flambda_kind.rec_info dacc in

      let dacc =
        List.fold_left2
          (fun dacc param arg -> Dacc.func_param_dep ~denv param arg dacc)
          dacc
          (Bound_parameters.to_list params)
          code_dep.params
      in
      let dacc =
        Dacc.record_dep ~denv (Name.var my_closure)
          (Alias (Name.var code_dep.my_closure))
          dacc
      in
      let body, dacc = traverse denv dacc body in
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
      { params_and_body; code_metadata; free_names_of_params_and_body }, dacc)

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
      let dacc =
        List.fold_left
          (fun dacc bp -> Dacc.bound_parameter_kind bp dacc)
          dacc
          (Bound_parameters.to_list bound_parameters)
      in
      let expr, dacc = traverse denv dacc handler in
      let handler = { bound_parameters; expr; is_exn_handler; is_cold } in
      k handler dacc)

type uses = Dep_solver.result

let poison (kind : Flambda_kind.t) =
  match kind with
  | Value -> Simple.const_int (Targetint_31_63.of_int 123456789)
  | Naked_number Naked_float ->
    Simple.const
      (Int_ids.Const.naked_float
         (Numeric_types.Float_by_bit_pattern.create 123456789.))
  | Naked_number Naked_immediate ->
    Simple.const
      (Int_ids.Const.naked_immediate (Targetint_31_63.of_int 123456789))
  | Naked_number Naked_int32 ->
    Simple.const (Int_ids.Const.naked_int32 123456789l)
  | Naked_number Naked_int64 ->
    Simple.const (Int_ids.Const.naked_int64 123456789L)
  | Naked_number Naked_nativeint ->
    Simple.const
      (Int_ids.Const.naked_nativeint (Targetint_32_64.of_int 123456789))
  | Naked_number Naked_vec128 ->
    Simple.const
      (Int_ids.Const.naked_vec128 Vector_types.Vec128.Bit_pattern.zero)
  | Region | Rec_info ->
    Misc.fatal_errorf "No dummy value available for kind %a" Flambda_kind.print
      kind

let rewrite_simple kinds (uses : uses) simple =
  Simple.pattern_match simple
    ~name:(fun name ~coercion:_ ->
        if Hashtbl.mem uses (Code_id_or_name.name name)
        then simple
        else
          let kind =
            match Name.Map.find_opt name kinds with
            | Some k -> k
            | None -> Misc.fatal_errorf "Unbound name %a" Name.print name
          in
          poison kind)
    ~const:(fun _ -> simple)

let rec rebuild_expr (kinds : Flambda_kind.t Name.Map.t) (uses : uses)
    (rev_expr : rev_expr) : RE.t =
  let { expr; holed_expr; free_names } = rev_expr in
  let expr =
    match expr with
    | Raw expr -> expr
    | Apply_cont ac ->
      let ac =
        Apply_cont_expr.with_continuation_and_args ac
          (Apply_cont_expr.continuation ac)
          ~args:(List.map (rewrite_simple kinds uses) (Apply_cont_expr.args ac))
      in
      Flambda.Expr.create_apply_cont ac
    | Switch switch -> Flambda.Expr.create_switch switch
    | Apply apply ->
      (* TODO rewrite other simples *)
      let apply =
        Apply_expr.create
          ~callee:(rewrite_simple kinds uses (Apply_expr.callee apply))
          ~continuation:(Apply_expr.continuation apply)
          (Apply_expr.exn_continuation apply)
          ~args:(List.map (rewrite_simple kinds uses) (Apply_expr.args apply))
          ~args_arity:(Apply_expr.args_arity apply)
          ~return_arity:(Apply_expr.return_arity apply)
          ~call_kind:(Apply_expr.call_kind apply)
          (Apply_expr.dbg apply)
          ~inlined:(Apply_expr.inlined apply)
          ~inlining_state:(Apply_expr.inlining_state apply)
          ~probe:(Apply_expr.probe apply)
          ~position:(Apply_expr.position apply)
          ~relative_history:(Apply_expr.relative_history apply)
          ~region:(Apply_expr.region apply)
      in
      Flambda.Expr.create_apply apply
  in
  rebuild_holed kinds uses holed_expr (RE.from_expr ~expr ~free_names)

and rebuild_function_params_and_body (kinds : Flambda_kind.t Name.Map.t)
    (uses : uses) (params_and_body : rev_params_and_body) :
    Flambda.function_params_and_body =
  let { return_continuation;
        exn_continuation;
        params;
        body;
        my_closure;
        my_region;
        my_depth
      } =
    params_and_body
  in
  let body = rebuild_expr kinds uses body in
  let params_and_body =
    Flambda.Function_params_and_body.create ~return_continuation
      ~exn_continuation params ~body:body.expr
      ~free_names_of_body:(Known body.free_names) ~my_closure ~my_region
      ~my_depth
  in
  params_and_body

and rebuild_holed (kinds : Flambda_kind.t Name.Map.t) (uses : uses)
    (rev_expr : rev_expr_holed) (hole : RE.t) : RE.t =
  match rev_expr with
  | Up -> hole
  | Let let_ -> (
    let[@local] default () =
      let subexpr =
        let defining_expr =
          match let_.defining_expr with
          | Named defining_expr -> defining_expr
          | Static_consts group ->
            let static_const_or_code = function
              | Deleted_code -> Flambda.Static_const_or_code.deleted_code
              | Code
                  { params_and_body;
                    code_metadata;
                    free_names_of_params_and_body
                  } ->
                let params_and_body =
                  rebuild_function_params_and_body kinds uses params_and_body
                in
                let code =
                  Code.create_with_metadata ~params_and_body ~code_metadata
                    ~free_names_of_params_and_body
                in
                Flambda.Static_const_or_code.create_code code
              | Static_const static_const ->
                Flambda.Static_const_or_code.create_static_const static_const
            in
            let group =
              Flambda.Static_const_group.create
                (List.map static_const_or_code group)
            in
            Flambda.Named.create_static_consts group
          | Set_of_closures { value_slots; alloc_mode; function_decls } ->
            let set_of_closures =
              Set_of_closures.create ~value_slots alloc_mode function_decls
            in
            Flambda.Named.create_set_of_closures set_of_closures
        in
        RE.create_let let_.bound_pattern defining_expr ~body:hole
      in
      rebuild_holed kinds uses let_.parent subexpr
    in
    let[@local] erase () =
      Format.eprintf "Removing %a@." Bound_pattern.print let_.bound_pattern;
      rebuild_holed kinds uses let_.parent hole
    in
    match let_.bound_pattern with
    | Set_of_closures _ -> default ()
    | Static l ->
      if List.for_all
           (fun (p : Bound_static.Pattern.t) ->
             match p with
             | Code code_id ->
               not (Hashtbl.mem uses (Code_id_or_name.code_id code_id))
             | Block_like _ | Set_of_closures _ -> false)
           (Bound_static.to_list l)
      then erase ()
      else default ()
    | Singleton v ->
      let v = Bound_var.var v in
      if Hashtbl.mem uses (Code_id_or_name.var v)
      then default ()
      else erase ())
  | Let_cont { cont; parent; handler } ->
    let cont_handler =
      let { bound_parameters; expr; is_exn_handler; is_cold } = handler in
      let handler = rebuild_expr kinds uses expr in
      RE.create_continuation_handler bound_parameters ~handler ~is_exn_handler
        ~is_cold
    in
    let let_cont_expr =
      RE.create_non_recursive_let_cont cont cont_handler ~body:hole
    in
    rebuild_holed kinds uses parent let_cont_expr
  | Let_cont_rec { parent; handlers; invariant_params } ->
    let handlers =
      Continuation.Map.map
        (fun handler ->
          let { bound_parameters; expr; is_exn_handler; is_cold } = handler in
          let handler = rebuild_expr kinds uses expr in
          RE.create_continuation_handler bound_parameters ~handler
            ~is_exn_handler ~is_cold)
        handlers
    in
    let let_cont_expr =
      RE.create_recursive_let_cont ~invariant_params handlers ~body:hole
    in
    rebuild_holed kinds uses parent let_cont_expr

let unit_with_body (unit : Flambda_unit.t) (body : Flambda.Expr.t) =
  Flambda_unit.create
    ~return_continuation:(Flambda_unit.return_continuation unit)
    ~exn_continuation:(Flambda_unit.exn_continuation unit)
    ~toplevel_my_region:(Flambda_unit.toplevel_my_region unit)
    ~body
    ~module_symbol:(Flambda_unit.module_symbol unit)
    ~used_value_slots:(Flambda_unit.used_value_slots unit)

let do_print = true

let run (unit : Flambda_unit.t) =
  (* Format.printf "CLEANUP@."; *)
  let dacc = Dacc.empty () in
  let holed, dacc =
    Profile.record_call ~accumulate:false "down" (fun () ->
        let dummy_toplevel_return = Variable.create "dummy_toplevel_return" in
        let dummy_toplevel_exn = Variable.create "dummy_toplevel_exn" in
        let dacc = Dacc.root dummy_toplevel_return dacc in
        let dacc = Dacc.root dummy_toplevel_exn dacc in
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
  if do_print then Format.printf "DACC %a@." Dacc.pp dacc;
  let deps = Dacc.deps dacc in
  let kinds = Dacc.kinds dacc in
  let () = if do_print then Dot.print_dep (Dacc.code_deps dacc, deps) in
  if do_print then Format.printf "USED %a@." Deps.pp_used deps;
  let solved_dep = Dep_solver.fixpoint deps in
  if do_print then Format.printf "RESULT@ %a@." Dep_solver.pp_result solved_dep;
  let rebuilt_expr =
    Profile.record_call ~accumulate:true "up" (fun () ->
        rebuild_expr kinds solved_dep holed)
  in
  unit_with_body unit rebuilt_expr.expr
