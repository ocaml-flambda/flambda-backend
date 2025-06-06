(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Nathanaëlle Courant, Pierre Chambart, OCamlPro               *)
(*                                                                        *)
(*   Copyright 2024 OCamlPro SAS                                          *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Flambda.Import
open! Rev_expr
open! Traverse_acc.Env
module Acc = Traverse_acc
module Graph = Global_flow_graph
module Dot = Dot_printer

type denv = Acc.Env.t

type acc = Acc.t

let apply_cont_deps denv acc apply_cont =
  let cont = Apply_cont_expr.continuation apply_cont in
  let args = Apply_cont_expr.args apply_cont in
  let params = Continuation.Map.find cont denv.conts in
  let (Normal params) = params in
  List.iter2 (fun param dep -> Acc.alias_dep ~denv param dep acc) params args

let reaper_test_opaque = Sys.getenv_opt "REAPEROPAQUE" <> None

let prepare_code ~denv acc (code_id : Code_id.t) (code : Code.t) =
  let return =
    List.init
      (Flambda_arity.cardinal_unarized (Code.result_arity code))
      (fun i ->
        Variable.create
          (Format.asprintf "function_return_%i_%a" i Code_id.print code_id))
  in
  let exn = Variable.create "function_exn" in
  let my_closure = Variable.create "my_closure" in
  let arity = Code.params_arity code in
  let params =
    List.init (Flambda_arity.cardinal_unarized arity) (fun i ->
        Variable.create (Printf.sprintf "function_param_%i" i))
  in
  let has_unsafe_result_type =
    match Code.result_types code with
    | Unknown -> false
    | Bottom -> false
    | Ok _ -> true
  in
  let never_delete =
    match Code.zero_alloc_attribute code with
    | Default_zero_alloc ->
      (* The effect of [Clflags.zero_alloc_assert] has been compiled into
         [Check] earlier. *)
      false
    | Assume _ -> false
    | Check _ -> true
  in
  let call_witnesses =
    List.init
      (if Code.is_tupled code then 1 else Flambda_arity.num_params arity)
      (fun i ->
        Code_id_or_name.var
          (Variable.create
             (Printf.sprintf "witness_%d_for_%s" i (Code_id.name code_id))))
  in
  let code_dep =
    { Traverse_acc.arity;
      return;
      my_closure;
      exn;
      params;
      is_tupled = Code.is_tupled code;
      call_witnesses
    }
  in
  List.iteri
    (fun i witness ->
      Graph.add_constructor_dep (Acc.graph acc) ~base:witness
        (Code_id_of_call_witness i)
        ~from:(Code_id_or_name.code_id code_id))
    call_witnesses;
  Graph.add_alias (Acc.graph acc)
    ~to_:(Code_id_or_name.code_id code_id)
    ~from:(Code_id_or_name.name denv.le_monde_exterieur);
  (* Graph.add_use_dep (Acc.graph acc) ~to_:indirect_call_witness
     ~from:(Code_id_or_name.code_id code_id); *)
  (* let le_monde_exterieur = denv.le_monde_exterieur in List.iter (fun param ->
     let param = Code_id_or_name.var param in Graph.add_propagate_dep (Acc.graph
     acc) ~if_used:indirect_call_witness ~from:le_monde_exterieur ~to_:param)
     params; *)
  if has_unsafe_result_type || never_delete
  then (
    List.iter
      (fun var -> Acc.used ~denv (Simple.var var) acc)
      ((my_closure :: params) @ (exn :: return));
    let le_monde_exterieur = Code_id_or_name.name denv.le_monde_exterieur in
    List.iter
      (fun param ->
        let param = Code_id_or_name.var param in
        Graph.add_alias (Acc.graph acc) ~to_:param ~from:le_monde_exterieur)
      (my_closure :: params));
  if never_delete then Acc.used_code_id code_id acc;
  Acc.add_code code_id code_dep acc

let record_set_of_closures_deps denv names_and_function_slots set_of_closures
    acc : unit =
  (* Here and later in [traverse_call_kind], some dependencies are not
     immediately registered, because the code, which is dominator-scoped, has
     not yet been seen due to the traversal order. *)
  let funs =
    Function_declarations.funs (Set_of_closures.function_decls set_of_closures)
  in
  Function_slot.Lmap.iter
    (fun function_slot name ->
      Acc.kind name Flambda_kind.value acc;
      let code_id =
        (Function_slot.Map.find function_slot funs
          : Function_declarations.code_id_in_function_declaration)
      in
      match code_id with
      | Deleted _ -> ()
      | Code_id { code_id; only_full_applications } ->
        Acc.add_set_of_closures_dep name code_id ~only_full_applications acc)
    names_and_function_slots;
  Function_slot.Lmap.iter
    (fun _function_slot function_slot_name ->
      Value_slot.Map.iter
        (fun value_slot simple ->
          let name = Acc.simple_to_name acc ~denv simple in
          Graph.add_constructor_dep (Acc.graph acc)
            ~base:(Code_id_or_name.name function_slot_name)
            (Value_slot value_slot)
            ~from:(Code_id_or_name.name name))
        (Set_of_closures.value_slots set_of_closures);
      Function_slot.Lmap.iter
        (fun function_slot name ->
          Graph.add_constructor_dep (Acc.graph acc)
            ~base:(Code_id_or_name.name function_slot_name)
            (Function_slot function_slot)
            ~from:(Code_id_or_name.name name))
        names_and_function_slots)
    names_and_function_slots

let rec traverse (denv : denv) (acc : acc) (expr : Expr.t) : rev_expr =
  match Expr.descr expr with
  | Let let_expr -> traverse_let denv acc let_expr
  | Let_cont let_cont -> traverse_let_cont denv acc let_cont
  | Apply apply -> traverse_apply denv acc apply
  | Apply_cont apply_cont -> traverse_apply_cont denv acc apply_cont
  | Switch switch -> traverse_switch denv acc switch
  | Invalid { message } -> traverse_invalid denv acc ~message

and traverse_let denv acc let_expr : rev_expr =
  let bound_pattern, body =
    Let.pattern_match let_expr ~f:(fun bound_pattern ~body ->
        bound_pattern, body)
  in
  let defining_expr = Let.defining_expr let_expr in
  let default_bp addf =
    let bound_to = Bound_pattern.free_names bound_pattern in
    Name_occurrences.fold_names bound_to
      ~f:(fun () bound_to -> addf (Code_id_or_name.name bound_to))
      ~init:()
  in
  let default acc =
    Name_occurrences.fold_names
      ~f:(fun () free_name ->
        default_bp (fun to_ ->
            Graph.add_use_dep (Acc.graph acc) ~to_
              ~from:(Code_id_or_name.name free_name)))
      ~init:()
      (Named.free_names defining_expr)
  in
  (match defining_expr with
  | Set_of_closures set_of_closures ->
    traverse_set_of_closures denv acc ~bound_pattern set_of_closures
  | Static_consts group -> traverse_static_consts denv acc ~bound_pattern group
  | Prim (prim, _dbg) ->
    traverse_prim denv acc ~bound_pattern prim ~default ~default_bp
  | Simple s ->
    Acc.alias_kind
      (Name.var (Bound_var.var (Bound_pattern.must_be_singleton bound_pattern)))
      s acc;
    let name = Code_id_or_name.name (Acc.simple_to_name acc ~denv s) in
    default_bp (fun to_ -> Graph.add_alias (Acc.graph acc) ~to_ ~from:name)
  | Rec_info _ -> default acc);
  let make_set_of_closures set_of_closures =
    let function_decls = Set_of_closures.function_decls set_of_closures in
    let value_slots = Set_of_closures.value_slots set_of_closures in
    let alloc_mode = Set_of_closures.alloc_mode set_of_closures in
    { function_decls; value_slots; alloc_mode }
  in
  let named : rev_named =
    match defining_expr with
    | Set_of_closures set_of_closures ->
      Set_of_closures (make_set_of_closures set_of_closures)
    | Static_consts group ->
      let bound_static =
        match bound_pattern with
        | Static b -> b
        | Singleton _ | Set_of_closures _ -> assert false
      in
      let rev_group =
        Static_const_group.match_against_bound_static group bound_static
          ~init:[]
          ~code:(fun rev_group code_id code ->
            let code =
              traverse_code acc code_id code
                ~le_monde_exterieur:denv.le_monde_exterieur
                ~all_constants:denv.all_constants
            in
            Code code :: rev_group)
          ~deleted_code:(fun rev_group _ -> Deleted_code :: rev_group)
          ~set_of_closures:(fun rev_group ~closure_symbols:_ set_of_closures ->
            Static_const
              (Set_of_closures (make_set_of_closures set_of_closures))
            :: rev_group)
          ~block_like:(fun rev_group _symbol static_const ->
            Static_const (Other static_const) :: rev_group)
      in
      let group = List.rev rev_group in
      Static_consts group
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
      current_code_id = denv.current_code_id;
      le_monde_exterieur = denv.le_monde_exterieur;
      all_constants = denv.all_constants
    }
    acc body

and traverse_prim denv acc ~bound_pattern (prim : Flambda_primitive.t) ~default
    ~(default_bp : (Code_id_or_name.t -> unit) -> unit) =
  let () =
    let kind = Flambda_primitive.result_kind' prim in
    let name =
      Name.var (Bound_var.var (Bound_pattern.must_be_singleton bound_pattern))
    in
    Acc.kind name kind acc
  in
  match[@ocaml.warning "-4"] prim with
  | Variadic (Make_block (block_kind, _mutability, _), fields) ->
    let _tag, block_shape = Flambda_primitive.Block_kind.to_shape block_kind in
    List.iteri
      (fun i field ->
        let kind = Flambda_kind.Block_shape.element_kind block_shape i in
        let name = Acc.simple_to_name acc ~denv field in
        default_bp (fun base ->
            Graph.add_constructor_dep (Acc.graph acc) ~base
              (Block (i, kind))
              ~from:(Code_id_or_name.name name)))
      fields;
    default_bp (fun base ->
        Graph.add_constructor_dep (Acc.graph acc) ~base Is_int
          ~from:(Code_id_or_name.name denv.all_constants);
        Graph.add_constructor_dep (Acc.graph acc) ~base Get_tag
          ~from:(Code_id_or_name.name denv.all_constants))
  | Unary (Opaque_identity { middle_end_only = true; _ }, arg)
    when reaper_test_opaque ->
    (* XXX TO REMOVE !!! *)
    let arg = Code_id_or_name.name (Acc.simple_to_name acc ~denv arg) in
    default_bp (fun to_ -> Graph.add_alias (Acc.graph acc) ~to_ ~from:arg)
  | Unary (Project_function_slot { move_from = _; move_to }, block) ->
    let block = Code_id_or_name.name (Acc.simple_to_name acc ~denv block) in
    default_bp (fun to_ ->
        Graph.add_accessor_dep (Acc.graph acc) ~to_ (Function_slot move_to)
          ~base:block)
  | Unary (Project_value_slot { project_from = _; value_slot }, block) ->
    let block = Code_id_or_name.name (Acc.simple_to_name acc ~denv block) in
    default_bp (fun to_ ->
        Graph.add_accessor_dep (Acc.graph acc) ~to_ (Value_slot value_slot)
          ~base:block)
  | Unary (Block_load { kind; mut; field }, block) -> (
    (* Loads from mutable blocks are also tracked here. This is ok because
       stores automatically escape the block. CR ncourant: think about whether
       we can make stores only escape the corresponding fields of the block
       instead of the whole block. *)
    let kind = Flambda_primitive.Block_access_kind.element_kind_for_load kind in
    let block = Code_id_or_name.name (Acc.simple_to_name acc ~denv block) in
    default_bp (fun to_ ->
        Graph.add_accessor_dep (Acc.graph acc) ~to_
          (Block (Targetint_31_63.to_int field, kind))
          ~base:block);
    match mut with
    | Immutable | Immutable_unique -> ()
    | Mutable ->
      default_bp (fun to_ ->
          Graph.add_alias (Acc.graph acc) ~to_
            ~from:(Code_id_or_name.name denv.le_monde_exterieur)))
  | Unary (Is_int { variant_only = true }, arg) ->
    let name = Code_id_or_name.name (Acc.simple_to_name acc ~denv arg) in
    default_bp (fun to_ ->
        Graph.add_accessor_dep (Acc.graph acc) ~to_ Is_int ~base:name)
  | Unary (Get_tag, arg) ->
    let name = Code_id_or_name.name (Acc.simple_to_name acc ~denv arg) in
    default_bp (fun to_ ->
        Graph.add_accessor_dep (Acc.graph acc) ~to_ Get_tag ~base:name)
  | prim ->
    let () =
      match Flambda_primitive.effects_and_coeffects prim with
      | Arbitrary_effects, _, _ ->
        let bound_to = Bound_pattern.free_names bound_pattern in
        Name_occurrences.fold_names bound_to
          ~f:(fun () bound_to -> Acc.used ~denv (Simple.name bound_to) acc)
          ~init:()
      | _ -> ()
    in
    default_bp (fun to_ ->
        Graph.add_use_dep (Acc.graph acc)
          ~from:(Code_id_or_name.name denv.le_monde_exterieur)
          ~to_);
    default acc

and traverse_set_of_closures denv acc ~(bound_pattern : Bound_pattern.t)
    set_of_closures =
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
      (List.map2
         (fun function_slot bound_var ->
           function_slot, Name.var (Bound_var.var bound_var))
         (Function_slot.Lmap.keys funs)
         bound_vars)
  in
  record_set_of_closures_deps denv names_and_function_slots set_of_closures acc

and traverse_static_consts denv acc ~(bound_pattern : Bound_pattern.t) group =
  let bound_static =
    match bound_pattern with
    | Static b -> b
    | Singleton _ | Set_of_closures _ -> assert false
  in
  Static_const_group.match_against_bound_static group bound_static ~init:()
    ~code:(fun () -> prepare_code ~denv acc)
    ~deleted_code:(fun _ _ -> ())
    ~set_of_closures:(fun _ ~closure_symbols:_ _ -> ())
    ~block_like:(fun _ _ _ -> ());
  Static_const_group.match_against_bound_static group bound_static ~init:()
    ~code:(fun () _code_id _code -> ())
    ~deleted_code:(fun () _ -> ())
    ~set_of_closures:(fun () ~closure_symbols set_of_closures ->
      let names_and_function_slots =
        Function_slot.Lmap.map Name.symbol closure_symbols
      in
      record_set_of_closures_deps denv names_and_function_slots set_of_closures
        acc)
    ~block_like:(fun () symbol static_const ->
      let name = Name.symbol symbol in
      let[@inline always] block_field_kind i =
        match[@ocaml.warning "-4"] static_const with
        | Block (_, _, shape, _) ->
          Flambda_kind.Scannable_block_shape.element_kind shape i
        | Immutable_value_array _ -> Flambda_kind.value
        | _ -> assert false
      in
      match[@ocaml.warning "-4"] static_const with
      | Block (_, _, _, fields) | Immutable_value_array fields ->
        List.iteri
          (fun i (field : Simple.With_debuginfo.t) ->
            let kind = block_field_kind i in
            let field_name =
              Acc.simple_to_name acc ~denv (Simple.With_debuginfo.simple field)
            in
            Graph.add_constructor_dep (Acc.graph acc)
              ~base:(Code_id_or_name.name name)
              (Block (i, kind))
              ~from:(Code_id_or_name.name field_name))
          fields;
        Graph.add_constructor_dep (Acc.graph acc)
          ~base:(Code_id_or_name.name name)
          Is_int
          ~from:(Code_id_or_name.name denv.all_constants);
        Graph.add_constructor_dep (Acc.graph acc)
          ~base:(Code_id_or_name.name name)
          Get_tag
          ~from:(Code_id_or_name.name denv.all_constants)
      | Set_of_closures _ -> assert false
      | _ ->
        Graph.add_alias (Acc.graph acc)
          ~to_:(Code_id_or_name.name name)
          ~from:(Code_id_or_name.name denv.all_constants))

and traverse_let_cont denv acc (let_cont : Let_cont.t) : rev_expr =
  match let_cont with
  | Non_recursive
      { handler; num_free_occurrences = _; is_applied_with_traps = _ } ->
    Non_recursive_let_cont_handler.pattern_match handler ~f:(fun cont ~body ->
        traverse_let_cont_non_recursive denv acc cont ~body handler)
  | Recursive handlers ->
    Recursive_let_cont_handlers.pattern_match handlers
      ~f:(fun ~invariant_params ~body handlers ->
        traverse_let_cont_recursive denv acc ~invariant_params ~body handlers)

and traverse_let_cont_non_recursive denv acc cont ~body handler =
  let cont_handler = Non_recursive_let_cont_handler.handler handler in
  let traverse handler acc =
    Acc.continuation_info acc cont
      { params = Bound_parameters.vars handler.bound_parameters;
        arity =
          Flambda_arity.unarize
            (Bound_parameters.arity handler.bound_parameters);
        is_exn_handler = Continuation_handler.is_exn_handler cont_handler
      };
    let conts =
      Continuation.Map.add cont
        (Normal (Bound_parameters.vars handler.bound_parameters))
        denv.conts
    in
    let denv =
      { parent = Let_cont { cont; handler; parent = denv.parent };
        conts;
        current_code_id = denv.current_code_id;
        le_monde_exterieur = denv.le_monde_exterieur;
        all_constants = denv.all_constants
      }
    in
    traverse denv acc body
  in
  traverse_cont_handler
    { parent = Hole;
      conts = denv.conts;
      current_code_id = denv.current_code_id;
      le_monde_exterieur = denv.le_monde_exterieur;
      all_constants = denv.all_constants
    }
    acc cont_handler traverse

and traverse_let_cont_recursive denv acc ~invariant_params ~body handlers =
  let invariant_params_vars = Bound_parameters.vars invariant_params in
  let invariant_params_arity =
    Flambda_arity.unarize (Bound_parameters.arity invariant_params)
  in
  let handlers =
    Continuation.Lmap.map
      (fun cont_handler ->
        Continuation_handler.pattern_match cont_handler
          ~f:(fun bound_parameters ~handler ->
            cont_handler, bound_parameters, handler))
      (Continuation_handlers.to_map handlers)
  in
  let conts =
    Continuation.Lmap.fold
      (fun cont (_, bp, _) conts ->
        let params = invariant_params_vars @ Bound_parameters.vars bp in
        let arity =
          invariant_params_arity
          @ Flambda_arity.unarize (Bound_parameters.arity bp)
        in
        Acc.continuation_info acc cont { params; is_exn_handler = false; arity };
        Continuation.Map.add cont (Normal params) conts)
      handlers denv.conts
  in
  (* Record kinds of bound parameters *)
  Bound_parameters.iter
    (fun bp -> Acc.bound_parameter_kind bp acc)
    invariant_params;
  Continuation.Lmap.iter
    (fun _ (_, bp, _) ->
      Bound_parameters.iter (fun bp -> Acc.bound_parameter_kind bp acc) bp)
    handlers;
  let handlers =
    Continuation.Lmap.map
      (fun (cont_handler, bound_parameters, handler) ->
        let is_exn_handler = Continuation_handler.is_exn_handler cont_handler in
        let is_cold = Continuation_handler.is_cold cont_handler in
        let expr =
          traverse
            { parent = Hole;
              conts;
              current_code_id = denv.current_code_id;
              le_monde_exterieur = denv.le_monde_exterieur;
              all_constants = denv.all_constants
            }
            acc handler
        in
        let handler = { bound_parameters; expr; is_exn_handler; is_cold } in
        handler)
      handlers
  in
  let denv =
    { parent = Let_cont_rec { invariant_params; handlers; parent = denv.parent };
      conts;
      current_code_id = denv.current_code_id;
      le_monde_exterieur = denv.le_monde_exterieur;
      all_constants = denv.all_constants
    }
  in
  traverse denv acc body

and traverse_cont_handler :
    type a.
    denv -> acc -> Continuation_handler.t -> (cont_handler -> acc -> a) -> a =
 fun denv acc cont_handler k ->
  let is_exn_handler = Continuation_handler.is_exn_handler cont_handler in
  let is_cold = Continuation_handler.is_cold cont_handler in
  Continuation_handler.pattern_match cont_handler
    ~f:(fun bound_parameters ~handler ->
      Bound_parameters.iter
        (fun bp -> Acc.bound_parameter_kind bp acc)
        bound_parameters;
      let expr = traverse denv acc handler in
      let handler = { bound_parameters; expr; is_exn_handler; is_cold } in
      k handler acc)

and traverse_apply denv acc apply : rev_expr =
  let return_args =
    match Apply.continuation apply with
    | Never_returns -> None
    | Return cont -> (
      match Continuation.Map.find cont denv.conts with
      | Normal params -> Some params)
  in
  let exn_arg =
    let exn = Apply.exn_continuation apply in
    let extra_args = Exn_continuation.extra_args exn in
    let (Normal exn_params) =
      Continuation.Map.find (Exn_continuation.exn_handler exn) denv.conts
    in
    match exn_params with
    | [] -> assert false
    | exn_param :: extra_params ->
      List.iter2
        (fun param (arg, _kind) -> Acc.alias_dep ~denv param arg acc)
        extra_params extra_args;
      exn_param
  in
  let default_acc acc =
    (* CR ncourant: track regions properly *)
    List.iter (fun arg -> Acc.used ~denv arg acc) (Apply.args apply);
    (match Apply.callee apply with
    | None -> ()
    | Some callee -> Acc.used ~denv callee acc);
    Acc.alias_dep ~denv exn_arg (Simple.name denv.le_monde_exterieur) acc;
    List.iter
      (fun param ->
        Acc.alias_dep ~denv param (Simple.name denv.le_monde_exterieur) acc)
      (match return_args with None -> [] | Some l -> l);
    match Apply.call_kind apply with
    | Function _ -> ()
    | Method { obj; kind = _; alloc_mode = _ } -> Acc.used ~denv obj acc
    | C_call _ -> ()
    | Effect (Perform { eff }) -> Acc.used ~denv eff acc
    | Effect (Reperform { eff; cont; last_fiber }) ->
      Acc.used ~denv eff acc;
      Acc.used ~denv cont acc;
      Acc.used ~denv last_fiber acc
    | Effect (Run_stack { stack; f; arg }) ->
      Acc.used ~denv stack acc;
      Acc.used ~denv f acc;
      Acc.used ~denv arg acc
    | Effect (Resume { stack; f; arg; last_fiber }) ->
      Acc.used ~denv stack acc;
      Acc.used ~denv f acc;
      Acc.used ~denv arg acc;
      Acc.used ~denv last_fiber acc
  in
  traverse_call_kind denv acc apply ~exn_arg ~return_args ~default_acc;
  let expr = Apply apply in
  { expr; holed_expr = denv.parent }

and traverse_call_kind denv acc apply ~exn_arg ~return_args ~default_acc =
  let calls_are_not_pure = Variable.create "not_pure" in
  Acc.used ~denv (Simple.var calls_are_not_pure) acc;
  let add_call_widget (function_call : Call_kind.Function_call.t) =
    let args, closure_entry_point =
      match function_call with
      | Indirect_unknown_arity ->
        ( Flambda_arity.group_by_parameter (Apply.args_arity apply)
            (Apply.args apply),
          Global_flow_graph.Indirect_code_pointer )
      | Indirect_known_arity | Direct _ ->
        [Apply.args apply], Global_flow_graph.Direct_code_pointer
    in
    (* List.iter (fun arg -> Acc.used ~denv arg acc) (Apply.args apply); *)
    let callee =
      match Apply.callee apply with
      | None -> assert false
      | Some callee ->
        Code_id_or_name.name (Acc.simple_to_name acc ~denv callee)
    in
    let rec add_deps callee args calls_are_not_pure =
      match args with
      | [] -> Misc.fatal_error "add_deps: no args"
      | first :: rest -> (
        List.iteri
          (fun i arg ->
            Graph.add_coaccessor_dep (Acc.graph acc)
              ~to_:(Code_id_or_name.name (Acc.simple_to_name acc ~denv arg))
              (Param (closure_entry_point, i))
              ~base:callee)
          first;
        Graph.add_accessor_dep (Acc.graph acc)
          ~to_:(Code_id_or_name.var calls_are_not_pure)
          Code_of_closure ~base:callee;
        Graph.add_accessor_dep (Acc.graph acc)
          ~to_:(Code_id_or_name.var exn_arg)
          (Apply (closure_entry_point, Exn))
          ~base:callee;
        match rest with
        | [] -> (
          match return_args with
          | None -> ()
          | Some return_args ->
            List.iteri
              (fun i return_arg ->
                Graph.add_accessor_dep (Acc.graph acc)
                  ~to_:(Code_id_or_name.var return_arg)
                  (Apply (closure_entry_point, Normal i))
                  ~base:callee)
              return_args)
        | _ :: _ ->
          let v = Variable.create "partial_apply" in
          Graph.add_accessor_dep (Acc.graph acc) ~to_:(Code_id_or_name.var v)
            (Apply (closure_entry_point, Normal 0))
            ~base:callee;
          let calls_are_not_pure = Variable.create "not_pure" in
          Acc.used ~denv (Simple.var calls_are_not_pure) acc;
          add_deps (Code_id_or_name.var v) rest calls_are_not_pure)
    in
    add_deps callee args calls_are_not_pure
  in
  match Apply.call_kind apply with
  | Function { function_call = Direct code_id as function_call; _ } ->
    (* CR ncourant: think about cross-module propagation *)
    (* if Compilation_unit.is_current (Code_id.get_compilation_unit code_id)
       then ( let apply_dep = { Traverse_acc.function_containing_apply_expr =
       denv.current_code_id; apply_code_id = code_id; apply_args = Apply.args
       apply; apply_closure = Apply.callee apply; params_of_apply_return_cont =
       return_args; param_of_apply_exn_cont = exn_arg; not_pure_call_witness =
       calls_are_not_pure } in Acc.add_apply apply_dep acc; if Option.is_some
       (Apply.callee apply) then add_call_widget function_call) else default_acc
       acc *)
    if Option.is_some (Apply.callee apply)
    then add_call_widget function_call
    else if Compilation_unit.is_current (Code_id.get_compilation_unit code_id)
    then
      let apply_dep =
        { Traverse_acc.function_containing_apply_expr = denv.current_code_id;
          apply_code_id = code_id;
          apply_args = Apply.args apply;
          apply_closure = Apply.callee apply;
          params_of_apply_return_cont = return_args;
          param_of_apply_exn_cont = exn_arg;
          not_pure_call_witness = calls_are_not_pure
        }
      in
      Acc.add_apply apply_dep acc
    else default_acc acc
  | Function
      { function_call =
          (Indirect_unknown_arity | Indirect_known_arity) as function_call;
        _
      } ->
    add_call_widget function_call
  | Method _ | C_call _ | Effect _ -> default_acc acc

and traverse_apply_cont denv acc apply_cont : rev_expr =
  let expr = Apply_cont apply_cont in
  apply_cont_deps denv acc apply_cont;
  { expr; holed_expr = denv.parent }

and traverse_switch denv acc switch : rev_expr =
  let expr = Switch switch in
  Acc.used ~denv (Switch_expr.scrutinee switch) acc;
  Targetint_31_63.Map.iter
    (fun _ apply_cont -> apply_cont_deps denv acc apply_cont)
    (Switch_expr.arms switch);
  { expr; holed_expr = denv.parent }

and traverse_invalid denv _acc ~message =
  let expr = Invalid { message } in
  { expr; holed_expr = denv.parent }

and traverse_code (acc : acc) (code_id : Code_id.t) (code : Code.t)
    ~le_monde_exterieur ~all_constants : rev_code =
  let params_and_body = Code.params_and_body code in
  Function_params_and_body.pattern_match params_and_body
    ~f:(fun
         ~return_continuation
         ~exn_continuation
         params
         ~body
         ~my_closure
         ~is_my_closure_used:_
         ~my_region
         ~my_ghost_region
         ~my_depth
         ~free_names_of_body:_
       ->
      traverse_function_params_and_body acc code_id code ~return_continuation
        ~exn_continuation params ~body ~my_closure ~my_region ~my_ghost_region
        ~my_depth ~le_monde_exterieur ~all_constants)

and traverse_function_params_and_body acc code_id code ~return_continuation
    ~exn_continuation params ~body ~my_closure ~my_region ~my_ghost_region
    ~le_monde_exterieur ~all_constants ~my_depth : rev_code =
  let code_metadata = Code.code_metadata code in
  let free_names_of_params_and_body = Code0.free_names code in
  (* Note: this significately degrades the analysis on zero_alloc code. However,
     it is highly unclear what should be done for zero_alloc code, so we simply
     mark the code as escaping. *)
  let is_opaque = Code_metadata.is_opaque code_metadata in
  let code_dep = Acc.find_code acc code_id in
  let maybe_opaque var = if is_opaque then Variable.rename var else var in
  let return = List.map maybe_opaque code_dep.return in
  let exn = maybe_opaque code_dep.exn in
  let conts =
    Continuation.Map.of_list
      [return_continuation, Normal return; exn_continuation, Normal [exn]]
  in
  Acc.continuation_info acc return_continuation
    { is_exn_handler = false;
      params = return;
      arity =
        Flambda_arity.unarized_components
          (Code_metadata.result_arity code_metadata)
    };
  Acc.continuation_info acc exn_continuation
    { is_exn_handler = true;
      params = [exn];
      arity = [Flambda_kind.With_subkind.any_value]
    };
  Acc.fixed_arity_continuation acc return_continuation;
  Acc.fixed_arity_continuation acc exn_continuation;
  let denv =
    { parent = Hole;
      conts;
      current_code_id = Some code_id;
      le_monde_exterieur;
      all_constants
    }
  in
  Bound_parameters.iter (fun bp -> Acc.bound_parameter_kind bp acc) params;
  Acc.kind (Name.var my_closure) Flambda_kind.value acc;
  Option.iter
    (fun region -> Acc.kind (Name.var region) Flambda_kind.region acc)
    my_region;
  Option.iter
    (fun region -> Acc.kind (Name.var region) Flambda_kind.region acc)
    my_ghost_region;
  Acc.kind (Name.var my_depth) Flambda_kind.rec_info acc;
  if is_opaque
  then (
    List.iter (fun arg -> Acc.used ~denv (Simple.var arg) acc) code_dep.params;
    List.iter (fun v -> Acc.used ~denv (Simple.var v) acc) (exn :: return);
    let[@inline] any_source v =
      Graph.add_any_source (Acc.graph acc) (Code_id_or_name.var v)
    in
    List.iter
      (fun param -> any_source (Bound_parameter.var param))
      (Bound_parameters.to_list params);
    any_source my_closure;
    any_source my_depth;
    Option.iter any_source my_region;
    Option.iter any_source my_ghost_region;
    List.iter any_source (code_dep.exn :: code_dep.return))
  else
    List.iter2
      (fun param arg ->
        Graph.add_alias (Acc.graph acc)
          ~to_:(Code_id_or_name.var (Bound_parameter.var param))
          ~from:(Code_id_or_name.var arg))
      (Bound_parameters.to_list params)
      code_dep.params;
  if is_opaque
  then Acc.used ~denv (Simple.var code_dep.my_closure) acc
  else
    Graph.add_alias (Acc.graph acc)
      ~to_:(Code_id_or_name.var my_closure)
      ~from:(Code_id_or_name.var code_dep.my_closure);
  let body = traverse denv acc body in
  let params_and_body =
    { return_continuation;
      exn_continuation;
      params;
      body;
      my_closure;
      my_region;
      my_ghost_region;
      my_depth
    }
  in
  { params_and_body; code_metadata; free_names_of_params_and_body }

type result =
  { holed : Rev_expr.t;
    deps : Global_flow_graph.graph;
    kinds : Flambda_kind.t Name.Map.t;
    fixed_arity_continuations : Continuation.Set.t;
    continuation_info : Acc.continuation_info Continuation.Map.t;
    code_deps : Traverse_acc.code_dep Code_id.Map.t
  }

let run ~get_code_metadata (unit : Flambda_unit.t) =
  let acc = Acc.create () in
  let le_monde_exterieur =
    Symbol.create
      (Compilation_unit.get_current_exn ())
      (Linkage_name.of_string "le_monde_extérieur")
  in
  (* Graph.add_use_dep (Acc.graph acc) ~to_:(Code_id_or_name.symbol
     le_monde_exterieur) ~from:(Code_id_or_name.symbol le_monde_exterieur); *)
  Graph.add_any_source (Acc.graph acc)
    (Code_id_or_name.symbol le_monde_exterieur);
  let all_constants =
    Symbol.create
      (Compilation_unit.get_current_exn ())
      (Linkage_name.of_string "all_constants")
  in
  (* Graph.add_use_dep (Acc.graph acc) ~to_:(Code_id_or_name.symbol
     all_constants) ~from:(Code_id_or_name.symbol all_constants); *)
  Graph.add_any_source (Acc.graph acc) (Code_id_or_name.symbol all_constants);
  let create_holed () =
    let dummy_toplevel_return = Variable.create "dummy_toplevel_return" in
    let dummy_toplevel_exn = Variable.create "dummy_toplevel_exn" in
    Acc.root dummy_toplevel_return acc;
    Acc.root dummy_toplevel_exn acc;
    let return_continuation = Flambda_unit.return_continuation unit in
    let exn_continuation = Flambda_unit.exn_continuation unit in
    let conts =
      Continuation.Map.of_list
        [ return_continuation, Normal [dummy_toplevel_return];
          exn_continuation, Normal [dummy_toplevel_exn] ]
    in
    Acc.continuation_info acc return_continuation
      { is_exn_handler = false;
        params = [dummy_toplevel_return];
        arity = [Flambda_kind.With_subkind.any_value]
      };
    Acc.continuation_info acc exn_continuation
      { is_exn_handler = true;
        params = [dummy_toplevel_exn];
        arity = [Flambda_kind.With_subkind.any_value]
      };
    Acc.fixed_arity_continuation acc return_continuation;
    Acc.fixed_arity_continuation acc exn_continuation;
    traverse
      { parent = Hole;
        conts;
        current_code_id = None;
        le_monde_exterieur = Name.symbol le_monde_exterieur;
        all_constants = Name.symbol all_constants
      }
      acc (Flambda_unit.body unit)
  in
  let holed = Profile.record_call ~accumulate:false "down" create_holed in
  let deps =
    Acc.deps ~get_code_metadata
      ~le_monde_exterieur:(Name.symbol le_monde_exterieur)
      ~all_constants:(Name.symbol all_constants)
      acc
  in
  let kinds = Acc.kinds acc in
  let fixed_arity_continuations = Acc.fixed_arity_continuations acc in
  let continuation_info = Acc.get_continuation_info acc in
  let code_deps = Acc.code_deps acc in
  let () =
    let debug_print = Flambda_features.dump_reaper () in
    if false && debug_print then Dot.print_dep deps
  in
  { holed;
    deps;
    kinds;
    fixed_arity_continuations;
    continuation_info;
    code_deps
  }
