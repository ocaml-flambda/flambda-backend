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
open! Cleanup_traverse

type rev_expr = Cleanup_traverse.rev_expr

let all_slot_offsets = ref Slot_offsets.empty

let all_code = ref Code_id.Map.empty

let deleted_code = ref Code_id.Map.empty

type uses = Dep_solver.result

let poison_value = 0 (* 123456789 *)
let poison (kind : Flambda_kind.t) =
  match kind with
  | Value -> Simple.const_int (Targetint_31_63.of_int poison_value)
  | Naked_number Naked_float ->
    Simple.const
      (Int_ids.Const.naked_float
         (Numeric_types.Float_by_bit_pattern.create (float_of_int poison_value)))
  | Naked_number Naked_immediate ->
    Simple.const
      (Int_ids.Const.naked_immediate (Targetint_31_63.of_int poison_value))
  | Naked_number Naked_int32 ->
    Simple.const (Int_ids.Const.naked_int32 (Int32.of_int poison_value))
  | Naked_number Naked_int64 ->
    Simple.const (Int_ids.Const.naked_int64 (Int64.of_int poison_value))
  | Naked_number Naked_nativeint ->
    Simple.const
      (Int_ids.Const.naked_nativeint (Targetint_32_64.of_int poison_value))
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
            | None -> if Name.is_symbol name then Flambda_kind.value else Misc.fatal_errorf "Unbound name %a" Name.print name
          in
          poison kind)
    ~const:(fun _ -> simple)

let rewrite_simple_opt (uses : uses) = function
  | None -> None
  | (Some simple) as simpl ->
  Simple.pattern_match simple
    ~name:(fun name ~coercion:_ ->
        if Hashtbl.mem uses (Code_id_or_name.name name)
        then simpl
        else None)
    ~const:(fun _ -> simpl)

let rewrite_or_variable default uses (or_variable : _ Or_variable.t) =
  match or_variable with
  | Or_variable.Const _ -> or_variable
  | Or_variable.Var (v, _) ->
    if Hashtbl.mem uses (Code_id_or_name.var v) then or_variable else Or_variable.Const default

let rewrite_field_of_static_block _kinds uses (field : Field_of_static_block.t) =
  match field with
  | Tagged_immediate _ -> field
  | Symbol sym -> if Hashtbl.mem uses (Code_id_or_name.symbol sym) then field else Field_of_static_block.Tagged_immediate (Targetint_31_63.of_int poison_value)
  | Dynamically_computed (v, _) ->
    if Hashtbl.mem uses (Code_id_or_name.var v) then field else Field_of_static_block.Tagged_immediate (Targetint_31_63.of_int poison_value)

let rewrite_static_const kinds uses (sc : Static_const.t) =
  match sc with
  | Static_const.Set_of_closures sc ->
    let set_of_closures =
      Set_of_closures.create
        ~value_slots:
          (Value_slot.Map.map
             (rewrite_simple kinds uses)
             (Set_of_closures.value_slots sc))
        (Set_of_closures.alloc_mode sc)
        (Set_of_closures.function_decls sc)
    in
    all_slot_offsets
      := Slot_offsets.add_set_of_closures !all_slot_offsets ~is_phantom:false
           set_of_closures;
    Static_const.set_of_closures set_of_closures
  | Static_const.Block (tag, mut, fields) ->
    let fields = List.map (rewrite_field_of_static_block kinds uses) fields in
    Static_const.block tag mut fields
  | Static_const.Boxed_float f ->
    Static_const.boxed_float (rewrite_or_variable Numeric_types.Float_by_bit_pattern.zero uses f)
  | Static_const.Boxed_int32 n ->
    Static_const.boxed_int32 (rewrite_or_variable Int32.zero uses n)
  | Static_const.Boxed_int64 n ->
    Static_const.boxed_int64 (rewrite_or_variable Int64.zero uses n)
  | Static_const.Boxed_nativeint n ->
    Static_const.boxed_nativeint (rewrite_or_variable Targetint_32_64.zero uses n)
  | Static_const.Boxed_vec128 n ->
    Static_const.boxed_vec128 (rewrite_or_variable Vector_types.Vec128.Bit_pattern.zero uses n)
  | Static_const.Immutable_float_block fields ->
    let fields = List.map (rewrite_or_variable Numeric_types.Float_by_bit_pattern.zero uses) fields in
    Static_const.immutable_float_block fields
  | Static_const.Immutable_float_array fields ->
    let fields = List.map (rewrite_or_variable Numeric_types.Float_by_bit_pattern.zero uses) fields in
    Static_const.immutable_float_array fields
  | Static_const.Immutable_value_array fields ->
    let fields = List.map (rewrite_field_of_static_block kinds uses) fields in
    Static_const.immutable_value_array fields
  | Static_const.Empty_array _ -> sc
  | Static_const.Mutable_string _ -> sc
  | Static_const.Immutable_string _ -> sc
  | Static_const.Immutable_int32_array fields ->
    let fields = List.map (rewrite_or_variable Int32.zero uses) fields in
    Static_const.immutable_int32_array fields
  | Static_const.Immutable_int64_array fields ->
    let fields = List.map (rewrite_or_variable Int64.zero uses) fields in
    Static_const.immutable_int64_array fields
  | Static_const.Immutable_nativeint_array fields ->
    let fields = List.map (rewrite_or_variable Targetint_32_64.zero uses) fields in
    Static_const.immutable_nativeint_array fields




let rewrite_static_const_or_code kinds uses (sc : Flambda.static_const_or_code) =
  match sc with
  | Code _ -> sc
  | Deleted_code -> sc
  | Static_const sc -> Flambda.Static_const_or_code.create_static_const (rewrite_static_const kinds uses sc)

let rewrite_static_const_group kinds uses (group : Flambda.static_const_group) =
  Flambda.Static_const_group.map ~f:(rewrite_static_const_or_code kinds uses) group

let rewrite_set_of_closures bound (uses : uses) value_slots alloc_mode function_decls =
            let slot_is_used slot =
              List.exists (fun bv ->
                  match Hashtbl.find_opt uses (Code_id_or_name.var (Bound_var.var bv)) with
                  | None | Some Bottom -> false
                  | Some Top -> true
                  | Some (Fields (_, m)) -> Dep_solver.Field.Map.mem slot m
                ) bound
            in
            let value_slots = Value_slot.Map.filter (fun slot _ -> slot_is_used (Value_slot slot)) value_slots in
            (* let dummy_code_id = Code_id.create ~name:"dummy_code_id" Compilation_unit.dummy in *)
            let open Function_declarations in
            let function_decls =
              Function_declarations.create
                (Function_slot.Lmap.mapi (fun slot { code_id ; is_required_at_runtime } -> { code_id ; is_required_at_runtime = is_required_at_runtime && slot_is_used (Function_slot slot) })
                (Function_declarations.funs_in_order function_decls))
               in
            (* TODO remove unused function slots as well *)
              Set_of_closures.create ~value_slots alloc_mode function_decls


let rewrite_named kinds uses (named : Flambda.named) =
  match named with
  | Simple simple -> Flambda.Named.create_simple (rewrite_simple kinds uses simple)
  | Prim (prim, dbg) ->
    let prim = Flambda_primitive.map_args (rewrite_simple kinds uses) prim in
    Flambda.Named.create_prim prim dbg
  | Set_of_closures s -> Flambda.Named.create_set_of_closures s (* TODO *)
  | Static_consts sc -> Flambda.Named.create_static_consts (rewrite_static_const_group kinds uses sc)
  | Rec_info r -> Flambda.Named.create_rec_info r

let rewrite_apply_cont_expr kinds uses ac =
  Apply_cont_expr.with_continuation_and_args ac
    (Apply_cont_expr.continuation ac)
    ~args:(List.map (rewrite_simple kinds uses) (Apply_cont_expr.args ac))

let rec rebuild_expr (kinds : Flambda_kind.t Name.Map.t) (uses : uses)
    (rev_expr : rev_expr) : RE.t =
  let { expr; holed_expr } = rev_expr in
  let expr, free_names =
    match expr with
    | Invalid { message } ->
      Flambda.Expr.create_invalid (Message message), Name_occurrences.empty
    | Apply_cont ac ->
      let ac = rewrite_apply_cont_expr kinds uses ac in
      Flambda.Expr.create_apply_cont ac, Apply_cont_expr.free_names ac
    | Switch switch ->
      let switch =
        Switch_expr.create
          ~condition_dbg:(Switch_expr.condition_dbg switch)
          (* Scrutinee should never need rewriting, do it anyway for completeness *)
          ~scrutinee:(rewrite_simple kinds uses (Switch_expr.scrutinee switch))
          ~arms:(Targetint_31_63.Map.map (rewrite_apply_cont_expr kinds uses) (Switch_expr.arms switch))
      in
      Flambda.Expr.create_switch switch, Switch_expr.free_names switch
    | Apply apply ->
      (* TODO rewrite other simples *)
      let call_kind =
        match Apply_expr.call_kind apply with
        | (Function _ as ck) ->
          ck (* todo alloc_mode? *)
        | Method { kind; obj; alloc_mode } ->
          (* todo alloc_mode? *)
          Call_kind.method_call kind ~obj:(rewrite_simple kinds uses obj) alloc_mode
        | (C_call _ as ck) -> ck
      in
      let apply =
        Apply_expr.create
          ~callee:(rewrite_simple_opt uses (Apply_expr.callee apply))
          ~continuation:(Apply_expr.continuation apply)
          (Apply_expr.exn_continuation apply)
          ~args:(List.map (rewrite_simple kinds uses) (Apply_expr.args apply))
          ~args_arity:(Apply_expr.args_arity apply)
          ~return_arity:(Apply_expr.return_arity apply)
          ~call_kind
          (Apply_expr.dbg apply)
          ~inlined:(Apply_expr.inlined apply)
          ~inlining_state:(Apply_expr.inlining_state apply)
          ~probe:(Apply_expr.probe apply)
          ~position:(Apply_expr.position apply)
          ~relative_history:(Apply_expr.relative_history apply)
      in
      Flambda.Expr.create_apply apply, Apply_expr.free_names apply
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
      let[@local] erase () =
        (* Format.eprintf "Removing %a@." Bound_pattern.print let_.bound_pattern; *)
        rebuild_holed kinds uses let_.parent hole
      in
      let[@local] default () =
      let subexpr =
        let bp, defining_expr =
          match let_.defining_expr with
          | Named defining_expr -> let_.bound_pattern, defining_expr
          | Static_consts group ->
            let bound_static = match let_.bound_pattern with Static l -> l | Set_of_closures _ | Singleton _ -> assert false in
            let bound_and_group = List.filter_map (fun (((p, e) as arg) : Bound_static.Pattern.t * _) ->
                match p with
                | Code code_id ->
                  if Hashtbl.mem uses (Code_id_or_name.code_id code_id) then Some arg else ((match e with Code { code_metadata ; _ } -> deleted_code := Code_id.Map.add code_id code_metadata !deleted_code | Deleted_code -> () | Static_const _ -> assert false); Some (p, Deleted_code))
                | Block_like sym -> if Hashtbl.mem uses (Code_id_or_name.symbol sym) then Some arg else None
                | Set_of_closures m -> if Function_slot.Lmap.exists (fun _ sym -> Hashtbl.mem uses (Code_id_or_name.symbol sym)) m then Some arg else None
              ) (List.combine (Bound_static.to_list bound_static) group)
            in
            let bound_static, group = List.split bound_and_group in
            let static_const_or_code = function
              | Deleted_code -> Flambda.Static_const_or_code.deleted_code
              | Code
                  { params_and_body;
                    code_metadata;
                    free_names_of_params_and_body
                  } ->
                let is_my_closure_used = Hashtbl.mem uses (Code_id_or_name.var params_and_body.my_closure) in
                let params_and_body =
                  rebuild_function_params_and_body kinds uses params_and_body
                in
                let code_metadata =
                  if Bool.equal is_my_closure_used (Code_metadata.is_my_closure_used code_metadata) then
                    code_metadata
                  else
                    (assert (not is_my_closure_used); Code_metadata.with_is_my_closure_used is_my_closure_used code_metadata)
                in
                let code =
                  Code.create_with_metadata ~params_and_body ~code_metadata
                    ~free_names_of_params_and_body
                in
                all_code := Code_id.Map.add (Code.code_id code) code !all_code;
                Flambda.Static_const_or_code.create_code code
              | Static_const static_const ->
                Flambda.Static_const_or_code.create_static_const static_const
            in
            let group =
              Flambda.Static_const_group.create
                (List.map static_const_or_code group)
            in
            Bound_pattern.static (Bound_static.create bound_static), Flambda.Named.create_static_consts group
          | Set_of_closures { value_slots; alloc_mode; function_decls } ->
            let bound = match let_.bound_pattern with
                Set_of_closures s -> s | Static _ | Singleton _ -> assert false
            in
            let set_of_closures =
              rewrite_set_of_closures bound uses value_slots alloc_mode
                function_decls
            in
            let is_phantom =
              Name_mode.is_phantom @@ Bound_pattern.name_mode let_.bound_pattern
            in
            all_slot_offsets
              := Slot_offsets.add_set_of_closures !all_slot_offsets ~is_phantom
                   set_of_closures;
            ( let_.bound_pattern,
              Flambda.Named.create_set_of_closures set_of_closures )
        in
        let defining_expr = rewrite_named kinds uses defining_expr in
        RE.create_let bp defining_expr ~body:hole
      in
      rebuild_holed kinds uses let_.parent subexpr
    in
    match let_.bound_pattern with
    | Set_of_closures _ -> default ()
    | Static _ -> default ()
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

let rebuild kinds solved_dep holed =
  all_slot_offsets := Slot_offsets.empty;
  all_code := Code_id.Map.empty;
  (* Not really used: to remove *)
  deleted_code := Code_id.Map.empty;
  let rebuilt_expr =
    Profile.record_call ~accumulate:true "up" (fun () ->
        rebuild_expr kinds solved_dep holed)
  in
  rebuilt_expr.expr, rebuilt_expr.free_names, !all_code, !all_slot_offsets
