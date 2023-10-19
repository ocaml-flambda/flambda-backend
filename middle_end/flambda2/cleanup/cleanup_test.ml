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

module Deps = struct
  type field =
    | Block of int
    | Value_slot of Value_slot.t
    | Function_slot of Function_slot.t

  module Dep = struct
    type t =
      | Alias of Name.t
      | Use of Name.t
      | Contains of Name.t
      | Field of field * Name.t
      | Block of (field * Name.t) list
      | Apply of Name.t * Code_id.t

    let compare = compare
  end

  module DepSet = Set.Make (Dep)

  type graph =
    { name_to_dep : (Name.t, DepSet.t) Hashtbl.t;
      used : (Name.t, unit) Hashtbl.t (* TODO: Conditionnal on a Code_id *)
    }

  let create () =
    { name_to_dep = Hashtbl.create 100; used = Hashtbl.create 100 }

  let insert t k v =
    match Hashtbl.find_opt t k with
    | None -> Hashtbl.add t k (DepSet.singleton v)
    | Some s -> Hashtbl.replace t k (DepSet.add v s)

  let add_opaque_let_dependency t bp fv =
    let tbl = t.name_to_dep in
    let bound_to = Bound_pattern.free_names bp in
    let f () bound_to =
      Name_occurrences.fold_names fv
        ~f:(fun () dep -> insert tbl bound_to (Dep.Use dep))
        ~init:()
    in
    Name_occurrences.fold_names bound_to ~f ~init:()

  let add_let_field t bp field name =
    let tbl = t.name_to_dep in
    let bound_to = Bound_pattern.free_names bp in
    let f () bound_to = insert tbl bound_to (Dep.Field (field, name)) in
    Name_occurrences.fold_names bound_to ~f ~init:()

  let add_let_dep t bp dep =
    let tbl = t.name_to_dep in
    let bound_to = Bound_pattern.free_names bp in
    let f () bound_to = insert tbl bound_to dep in
    Name_occurrences.fold_names bound_to ~f ~init:()

  let add_cont_dep t bp dep =
    let tbl = t.name_to_dep in
    insert tbl (Name.var bp) (Alias dep)

  let add_func_param t ~param ~arg =
    let tbl = t.name_to_dep in
    insert tbl (Name.var param) (Alias arg)

  let add_use t dep = Hashtbl.replace t.used dep ()
end

module Dot = struct
  let dep_graph_ppf =
    lazy
      (let filename = "dep.dot" in
       let ch = open_out filename in
       let ppf = Format.formatter_of_out_channel ch in
       Format.fprintf ppf "digraph g {@\n";
       at_exit (fun () ->
           Format.fprintf ppf "@\n}@.";
           close_out ch);
       ppf)

  let dot_count = ref ~-1

  let print_graph ~print ~print_name ~lazy_ppf ~graph =
    match print_name with
    | None -> ()
    | Some print_name ->
      incr dot_count;
      let ppf = Lazy.force lazy_ppf in
      print ~ctx:!dot_count ~print_name ppf graph

  module P = struct
    let node_id ~ctx ppf (variable : Name.t) =
      Format.fprintf ppf "node_%d_%d" ctx (variable :> int)

    let node ~ctx ~root ppf name =
      if root
      then
        Format.fprintf ppf "%a [shape=record label=\"%a\"];@\n" (node_id ~ctx)
          name Name.print name
      else
        Format.fprintf ppf "%a [label=\"%a\"];@\n" (node_id ~ctx) name
          Name.print name

    let nodes ~ctx ppf t =
      Hashtbl.iter
        (fun name _ ->
          let root = Hashtbl.mem t.Deps.used name in
          node ~ctx ~root ppf name)
        t.Deps.name_to_dep

    let edge ~ctx ppf src (dst : Deps.Dep.t) =
      let color, deps =
        match dst with
        | Alias name -> "black", [name]
        | Use name ->
          (* ignore name; *)
          (* "red", [] *)
          "red", [name]
        | Contains name -> "yellow", [name]
        | Field (_, name) -> "green", [name]
        | Block fields -> "blue", List.map snd fields
        | Apply (name, _code) -> "pink", [name]
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

    let print ~ctx ~print_name ppf t =
      Flambda_colours.without_colours ~f:(fun () ->
          Format.fprintf ppf
            "subgraph cluster_%d { label=\"%s\"@\n%a@\n%a@\n}@." ctx print_name
            (nodes ~ctx) t (edges ~ctx) t)
  end

  let print_dep dep =
    let print_name = Some "dep" in
    print_graph ~print_name ~lazy_ppf:dep_graph_ppf ~graph:dep ~print:P.print
end

type code_dep =
  { params : Variable.t list;
    return : Variable.t list; (* Dummy variable representing return value *)
    exn : Variable.t list (* Dummy variable representing exn return value *)
  }

module Dacc : sig
  type t

  val empty : unit -> t

  val let_ : t -> t

  val let_cont : t -> t

  val let_rec_cont : t -> t

  val func : t -> t

  val let_dep : Bound_pattern.t -> Deps.Dep.t -> t -> t

  val cont_dep : Variable.t -> Simple.t -> t -> t

  val func_param_dep : Bound_parameter.t -> Variable.t -> t -> t

  val used : Simple.t -> t -> t

  val opaque_let_dependency : Bound_pattern.t -> Name_occurrences.t -> t -> t

  val let_field : Bound_pattern.t -> Deps.field -> Name.t -> t -> t

  val add_code : Code_id.t -> code_dep -> t -> t

  val find_code : t -> Code_id.t -> code_dep

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
      deps : Deps.graph
    }

  let pp ppf t =
    Format.fprintf ppf "let : %i@ let_cont : %i@ let_rec_cont : %i@ func : %i"
      t.let_ t.let_cont t.let_rec_cont t.func

  let deps t = t.deps

  let empty () =
    { let_ = 0;
      let_cont = 0;
      let_rec_cont = 0;
      func = 0;
      code = Code_id.Map.empty;
      deps = Deps.create ()
    }

  let add_code code_id dep t =
    { t with code = Code_id.Map.add code_id dep t.code }

  let find_code t code_id = Code_id.Map.find code_id t.code

  let let_ t = { t with let_ = t.let_ + 1 }

  let let_cont t = { t with let_cont = t.let_cont + 1 }

  let let_rec_cont t = { t with let_rec_cont = t.let_rec_cont + 1 }

  let func t = { t with func = t.func + 1 }

  let opaque_let_dependency pat fv t =
    Deps.add_opaque_let_dependency t.deps pat fv;
    t

  let let_field pat field name t =
    Deps.add_let_field t.deps pat field name;
    t

  let let_dep pat dep t =
    Deps.add_let_dep t.deps pat dep;
    t

  let cont_dep pat dep t =
    Simple.pattern_match dep
      ~name:(fun name ~coercion:_ -> Deps.add_cont_dep t.deps pat name)
      ~const:(fun _ -> ());
    t

  let func_param_dep param arg t =
    Deps.add_func_param t.deps
      ~param:(Bound_parameter.var param)
      ~arg:(Name.var arg);
    t

  let used dep t =
    Simple.pattern_match dep
      ~name:(fun name ~coercion:_ -> Deps.add_use t.deps name)
      ~const:(fun _ -> ());
    t

  let todo _ = empty ()

  let todo' t = t
end

type dacc = Dacc.t

type cont_kind =
  | Normal of Variable.t list
  | Return
  | Exn

type denv =
  { parent : rev_expr_holed;
    conts : cont_kind Continuation.Map.t
  }

type handlers = cont_handler Continuation.Map.t

type tt = handlers -> rev_expr * dacc

let apply_cont_deps denv dacc apply_cont =
  let cont = Apply_cont_expr.continuation apply_cont in
  let args = Apply_cont_expr.args apply_cont in
  let params = Continuation.Map.find cont denv.conts in
  match params with
  | Normal params ->
    List.fold_left2
      (fun dacc param dep -> Dacc.cont_dep param dep dacc)
      dacc params args
  | Return | Exn -> Dacc.todo' dacc

let prepare_code dacc (code_id : Code_id.t) (code : Code.t) =
  let return = [Variable.create "function_return"] in
  let exn = [Variable.create "function_exn"] in
  let params =
    (* TODO: better. We just need the arity *)
    let arity = Code.params_arity code in
    (* let arity = *)
    (*   let params_and_body = Code.params_and_body code in *)
    (*   Flambda.Function_params_and_body.pattern_match params_and_body *)
    (*     ~f:(fun *)
    (*          ~return_continuation:_ *)
    (*          ~exn_continuation:_ *)
    (*          params *)
    (*          ~body:_ *)
    (*          ~my_closure:_ *)
    (*          ~is_my_closure_used:_ *)
    (*          ~my_region:_ *)
    (*          ~my_depth:_ *)
    (*          ~free_names_of_body:_ *)
    (*        -> Bound_parameters.arity params) *)
    (* in *)
    List.init (Flambda_arity.cardinal arity) (fun i ->
        Variable.create (Printf.sprintf "function_param_%i" i))
  in
  let code_dep = { return; exn; params } in
  Dacc.add_code code_id code_dep dacc

let rec traverse (denv : denv) (dacc : dacc) (expr : Flambda.Expr.t) =
  match Flambda.Expr.descr expr with
  | Invalid _ ->
    let expr = Raw expr in
    ( { expr; holed_expr = denv.parent; free_names = Name_occurrences.empty },
      dacc )
  | Switch switch ->
    let expr = Switch switch in
    let dacc =
      let dacc = Dacc.used (Switch_expr.scrutinee switch) dacc in
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
  | Apply apply ->
    (* TODO apply_dep *)
    let expr = Raw expr in
    ( { expr;
        holed_expr = denv.parent;
        free_names = Flambda.Apply.free_names apply
      },
      dacc )
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
          traverse_cont_handler { parent = Up; conts = denv.conts }
            dacc cont_handler (fun handler dacc ->
              let conts =
                Continuation.Map.add cont
                  (Normal (Bound_parameters.vars handler.bound_parameters))
                  denv.conts
              in
              let denv =
                { parent = Let_cont { cont; handler; parent = denv.parent };
                  conts
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
                let expr, dacc = traverse { parent = Up; conts } dacc handler in
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
              conts
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
    let dep : Deps.Dep.t option =
      match defining_expr with
      | Set_of_closures _set_of_closures -> None
      | Static_consts _group -> None
      | Prim (prim, _dbg) -> begin
        match[@ocaml.warning "-4"] prim with
        | Variadic (Make_block (_, Immutable, _), fields) ->
          let acc = ref [] in
          List.iteri
            (fun i field ->
              Simple.pattern_match field
                ~name:(fun name ~coercion:_ ->
                  acc := (Deps.Block i, name) :: !acc)
                ~const:(fun _ -> ()))
            fields;
          Some (Block !acc)
        | Unary (Project_function_slot { move_from = _; move_to }, block) ->
          let block =
            Simple.pattern_match block
              ~name:(fun name ~coercion:_ -> name)
              ~const:(fun _ -> assert false)
          in
          let dep = Some (Deps.Dep.Field (Function_slot move_to, block)) in
          dep
        | Unary
            ( Project_value_slot { project_from = _; value_slot; kind = _ },
              block ) ->
          let block =
            Simple.pattern_match block
              ~name:(fun name ~coercion:_ -> name)
              ~const:(fun _ -> assert false)
          in
          let dep = Some (Deps.Dep.Field (Value_slot value_slot, block)) in
          dep
        | Binary (Block_load (_access_kind, Immutable), block, field) ->
          let dep =
            Simple.pattern_match field
              ~name:(fun _ ~coercion:_ -> None)
              ~const:(fun cst ->
                let block =
                  Simple.pattern_match block
                    ~name:(fun name ~coercion:_ -> name)
                    ~const:(fun _ -> assert false)
                in
                match Int_ids.Const.descr cst with
                | Tagged_immediate i ->
                  let i = Targetint_31_63.to_int_exn i in
                  Some (Deps.Dep.Field (Block i, block))
                | _ -> assert false)
          in
          dep
        | _ -> None
      end
      | Simple s ->
        let dep =
          Simple.pattern_match s
            ~name:(fun name ~coercion:_ -> Some (Deps.Dep.Alias name))
            ~const:(fun _ -> None)
        in
        dep
      | Rec_info _ -> None
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
            bound_static ~init:dacc ~code:prepare_code
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
    let dacc =
      match dep with
      | None ->
        Dacc.opaque_let_dependency bound_pattern
          (Flambda.Named.free_names defining_expr)
          dacc
      | Some dep -> Dacc.let_dep bound_pattern dep dacc
    in
    let let_acc =
      Let { bound_pattern; defining_expr = named; parent = denv.parent }
    in
    traverse { parent = let_acc; conts = denv.conts } dacc body

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
            exn_continuation, Normal code_dep.exn ]
      in
      let dacc =
        List.fold_left2
          (fun dacc param arg -> Dacc.func_param_dep param arg dacc)
          dacc
          (Bound_parameters.to_list params)
          code_dep.params
      in
      let body, dacc = traverse { parent = Up; conts } dacc body in
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
      let expr, dacc = traverse denv dacc handler in
      let handler = { bound_parameters; expr; is_exn_handler; is_cold } in
      k handler dacc)

let rec rebuild_expr (rev_expr : rev_expr) : RE.t =
  let { expr; holed_expr; free_names } = rev_expr in
  let expr =
    match expr with
    | Raw expr -> expr
    | Apply_cont ac -> Flambda.Expr.create_apply_cont ac
    | Switch switch -> Flambda.Expr.create_switch switch
    | Apply apply -> Flambda.Expr.create_apply apply
  in
  rebuild_holed holed_expr (RE.from_expr ~expr ~free_names)

and rebuild_function_params_and_body (params_and_body : rev_params_and_body) :
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
  let body = rebuild_expr body in
  let params_and_body =
    Flambda.Function_params_and_body.create ~return_continuation
      ~exn_continuation params ~body:body.expr
      ~free_names_of_body:(Known body.free_names) ~my_closure ~my_region
      ~my_depth
  in
  params_and_body

and rebuild_holed (rev_expr : rev_expr_holed) (hole : RE.t) : RE.t =
  match rev_expr with
  | Up -> hole
  | Let let_ ->
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
                rebuild_function_params_and_body params_and_body
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
    rebuild_holed let_.parent subexpr
  | Let_cont { cont; parent; handler } ->
    let cont_handler =
      let { bound_parameters; expr; is_exn_handler; is_cold } = handler in
      let handler = rebuild_expr expr in
      RE.create_continuation_handler bound_parameters ~handler ~is_exn_handler
        ~is_cold
    in
    let let_cont_expr =
      RE.create_non_recursive_let_cont cont cont_handler ~body:hole
    in
    rebuild_holed parent let_cont_expr
  | Let_cont_rec { parent; handlers; invariant_params } ->
    let handlers =
      Continuation.Map.map
        (fun handler ->
          let { bound_parameters; expr; is_exn_handler; is_cold } = handler in
          let handler = rebuild_expr expr in
          RE.create_continuation_handler bound_parameters ~handler
            ~is_exn_handler ~is_cold)
        handlers
    in
    let let_cont_expr =
      RE.create_recursive_let_cont ~invariant_params handlers ~body:hole
    in
    rebuild_holed parent let_cont_expr

let unit_with_body (unit : Flambda_unit.t) (body : Flambda.Expr.t) =
  Flambda_unit.create
    ~return_continuation:(Flambda_unit.return_continuation unit)
    ~exn_continuation:(Flambda_unit.exn_continuation unit)
    ~toplevel_my_region:(Flambda_unit.toplevel_my_region unit)
    ~body
    ~module_symbol:(Flambda_unit.module_symbol unit)
    ~used_value_slots:(Flambda_unit.used_value_slots unit)

let run (unit : Flambda_unit.t) =
  (* Format.printf "CLEANUP@."; *)
  let dacc = Dacc.empty () in
  let holed, _dacc =
    Profile.record_call ~accumulate:false "down" (fun () ->
        let conts =
          Continuation.Map.of_list
            [ Flambda_unit.return_continuation unit, Return;
              Flambda_unit.exn_continuation unit, Exn ]
        in
        traverse { parent = Up; conts } dacc (Flambda_unit.body unit))
  in
  Profile.record_call ~accumulate:false "size" (fun () ->
      let size = Obj.reachable_words (Obj.repr holed) in
      Format.printf "CLEANUP %i@." (size / 1000));
  Format.printf "DACC %a@." Dacc.pp _dacc;
  let () = Dot.print_dep (Dacc.deps _dacc) in
  let rebuilt_expr =
    Profile.record_call ~accumulate:true "up" (fun () -> rebuild_expr holed)
  in
  unit_with_body unit rebuilt_expr.expr
