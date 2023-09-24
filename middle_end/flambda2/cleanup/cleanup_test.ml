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
  { expr : Flambda.Expr.t;
    holed_expr : rev_expr_holed;
    free_names : Name_occurrences.t
  }

type dacc = { parent : rev_expr_holed }

type handlers = cont_handler Continuation.Map.t

let rec traverse (dacc : dacc) (expr : Flambda.Expr.t) =
  match Flambda.Expr.descr expr with
  | Invalid _ ->
    { expr; holed_expr = dacc.parent; free_names = Name_occurrences.empty }
  | Switch switch ->
    { expr;
      holed_expr = dacc.parent;
      free_names = Flambda.Switch.free_names switch
    }
  | Apply_cont apply_cont ->
    { expr;
      holed_expr = dacc.parent;
      free_names = Flambda.Apply_cont.free_names apply_cont
    }
  | Apply apply ->
    { expr;
      holed_expr = dacc.parent;
      free_names = Flambda.Apply.free_names apply
    }
  | Let_cont let_cont ->
    (match let_cont with
     | Non_recursive
         { handler; num_free_occurrences = _; is_applied_with_traps = _ } ->
       Flambda.Non_recursive_let_cont_handler.pattern_match handler
         ~f:(fun cont ~body ->
           let cont_handler =
             Flambda.Non_recursive_let_cont_handler.handler handler
           in
           traverse_cont_handler cont_handler (fun handler ->
               let dacc =
                 { parent = Let_cont { cont; handler; parent = dacc.parent } }
               in
               traverse dacc body))
     | Recursive handlers ->
       Flambda.Recursive_let_cont_handlers.pattern_match handlers
         ~f:(fun ~invariant_params ~body handlers ->
           Continuation.Map.fold
             (fun cont handler k ->
               traverse_cont_handler handler
                 (fun (handler : cont_handler) handlers ->
                   k (Continuation.Map.add cont handler handlers)))
             (Flambda.Continuation_handlers.to_map handlers)
             (fun handlers ->
               let dacc =
                 { parent =
                     Let_cont_rec
                       { invariant_params; handlers; parent = dacc.parent }
                 }
               in
               traverse dacc body)
             Continuation.Map.empty)
      : rev_expr)
  | Let let_expr ->
    let named =
      match Flambda.Let_expr.defining_expr let_expr with
      | Set_of_closures set_of_closures ->
        let function_decls = Set_of_closures.function_decls set_of_closures in
        let value_slots = Set_of_closures.value_slots set_of_closures in
        let alloc_mode = Set_of_closures.alloc_mode set_of_closures in
        let set_of_closures = { function_decls; value_slots; alloc_mode } in
        Set_of_closures set_of_closures
      (* TODO set_of_closures in Static_consts *)
      | (Simple _ | Prim _ | Static_consts _ | Rec_info _) as defining_expr ->
        Named defining_expr
    in
    Flambda.Let_expr.pattern_match let_expr ~f:(fun bp ~body ->
        ignore bp;
        let let_acc =
          Let
            { bound_pattern = bp; defining_expr = named; parent = dacc.parent }
        in
        traverse { parent = let_acc } body)

and traverse_cont_handler :
    type a. Flambda.Continuation_handler.t -> (cont_handler -> a) -> a =
 fun cont_handler k ->
  let is_exn_handler =
    Flambda.Continuation_handler.is_exn_handler cont_handler
  in
  let is_cold = Flambda.Continuation_handler.is_cold cont_handler in
  Flambda.Continuation_handler.pattern_match cont_handler
    ~f:(fun bound_parameters ~handler ->
      let expr = traverse { parent = Up } handler in
      let handler = { bound_parameters; expr; is_exn_handler; is_cold } in
      k handler)

let rec rebuild_expr (rev_expr : rev_expr) : RE.t =
  let { expr; holed_expr; free_names } = rev_expr in
  rebuild_holed holed_expr (RE.from_expr ~expr ~free_names)

and rebuild_holed (rev_expr : rev_expr_holed) (hole : RE.t) : RE.t =
  match rev_expr with
  | Up -> hole
  | Let let_ ->
    let subexpr =
      match let_.defining_expr with
      | Named defining_expr ->
        RE.create_let let_.bound_pattern defining_expr ~body:hole
      | Set_of_closures { value_slots; alloc_mode; function_decls } ->
        let set_of_closures =
          Set_of_closures.create ~value_slots alloc_mode function_decls
        in
        let defining_expr =
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
  let holed = traverse { parent = Up } (Flambda_unit.body unit) in
  let rebuilt_expr = rebuild_expr holed in
  unit_with_body unit rebuilt_expr.expr
