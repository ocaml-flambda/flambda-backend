type rev_expr_holed =
  | Up
  | Let of
      { bound_pattern : Bound_pattern.t;
        defining_expr : Flambda.named;
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

and cont_handler =
  { bound_parameters : Bound_parameters.t;
    is_exn_handler : bool;
    is_cold : bool;
    expr : rev_expr
  }

and rev_expr = Flambda.Expr.t * rev_expr_holed

type dacc = { parent : rev_expr_holed }

type handlers = cont_handler Continuation.Map.t

let rec traverse (dacc : dacc) (expr : Flambda.Expr.t) =
  match Flambda.Expr.descr expr with
  | Invalid _ | Switch _ | Apply_cont _ | Apply _ -> expr, dacc.parent
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
    Flambda.Let_expr.pattern_match let_expr ~f:(fun bp ~body ->
        ignore bp;
        let let_acc =
          Let
            { bound_pattern = bp;
              defining_expr = Flambda.Let_expr.defining_expr let_expr;
              parent = dacc.parent
            }
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

let rec rebuild_expr (rev_expr : rev_expr) : Flambda.Expr.t =
  let e, holed = rev_expr in
  rebuild_holed holed e

and rebuild_holed (rev_expr : rev_expr_holed) (hole : Flambda.Expr.t) :
    Flambda.Expr.t =
  match rev_expr with
  | Up -> hole
  | Let let_ ->
    let let_expr =
      Flambda.Let_expr.create let_.bound_pattern let_.defining_expr ~body:hole
        ~free_names_of_body:Unknown (* TODO *)
    in
    let subexpr = Flambda.Expr.create_let let_expr in
    rebuild_holed let_.parent subexpr
  | Let_cont { cont; parent; handler } ->
    let cont_handler =
      let { bound_parameters; expr; is_exn_handler; is_cold } = handler in
      let handler = rebuild_expr expr in
      Flambda.Continuation_handler.create bound_parameters ~handler
        ~free_names_of_handler:Unknown (* TODO *)
        ~is_exn_handler ~is_cold
    in
    let let_cont_expr =
      Flambda.Let_cont_expr.create_non_recursive cont cont_handler ~body:hole
        ~free_names_of_body:Unknown (* TODO *)
    in
    rebuild_holed parent let_cont_expr
  | Let_cont_rec { parent; handlers; invariant_params } ->
    let handlers =
      Continuation.Map.map
        (fun handler ->
          let { bound_parameters; expr; is_exn_handler; is_cold } = handler in
          let handler = rebuild_expr expr in
          Flambda.Continuation_handler.create bound_parameters ~handler
            ~free_names_of_handler:Unknown (* TODO *)
            ~is_exn_handler ~is_cold)
        handlers
    in
    let let_cont_expr =
      Flambda.Let_cont_expr.create_recursive ~invariant_params handlers
        ~body:hole
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
  let expr = rebuild_expr holed in
  unit_with_body unit expr
