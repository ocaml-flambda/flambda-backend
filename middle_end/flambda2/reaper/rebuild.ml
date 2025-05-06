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
module Float = Numeric_types.Float_by_bit_pattern
module Float32 = Numeric_types.Float32_by_bit_pattern
module RE = Rebuilt_expr
module Field = Global_flow_graph.Field

type rev_expr = Rev_expr.t

let all_slot_offsets = ref Slot_offsets.empty

let all_code = ref Code_id.Map.empty

type param_decision =
  | Keep of Variable.t * Flambda_kind.With_subkind.t
  | Delete
  | Unbox of Variable.t Dep_solver.unboxed_fields Field.Map.t

let print_param_decision ppf param_decision =
  match param_decision with
  | Keep (v, kind) ->
    Format.fprintf ppf "Keep (%a, %a)" Variable.print v
      Flambda_kind.With_subkind.print kind
  | Delete -> Format.fprintf ppf "Delete"
  | Unbox fields ->
    Format.fprintf ppf "Unbox %a"
      (Field.Map.print (Dep_solver.print_unboxed_fields Variable.print))
      fields

type env =
  { uses : Dep_solver.result;
    code_deps : Traverse_acc.code_dep Code_id.Map.t;
    get_code_metadata : Code_id.t -> Code_metadata.t;
    (* TODO change names *)
    cont_params_to_keep : param_decision list Continuation.Map.t;
    should_keep_param :
      Continuation.t ->
      Variable.t ->
      Flambda_kind.With_subkind.t ->
      param_decision;
    (* TODO same here *)
    function_params_to_keep : param_decision list Code_id.Map.t;
    should_keep_function_param :
      Code_id.t -> Variable.t -> Flambda_kind.With_subkind.t -> param_decision;
    function_return_decision : param_decision list Code_id.Map.t
  }

let freshen_decisions = function
  | Delete -> Delete
  | Keep (v, kind) -> Keep (Variable.rename v, kind)
  | Unbox fields ->
    Unbox
      (Field.Map.map
         (Dep_solver.map_unboxed_fields (fun v -> Variable.rename v))
         fields)

let is_used (env : env) cn = Dep_solver.has_use env.uses cn

let is_code_id_used (env : env) code_id =
  is_used env (Code_id_or_name.code_id code_id)
  || not (Compilation_unit.is_current (Code_id.get_compilation_unit code_id))

let is_symbol_used (env : env) symbol =
  is_used env (Code_id_or_name.symbol symbol)
  || not (Compilation_unit.is_current (Symbol.compilation_unit symbol))

let is_var_used (env : env) kinds var =
  let kind = Name.Map.find (Name.var var) kinds in
  match[@ocaml.warning "-4"] kind with
  | Flambda_kind.(Region | Rec_info) -> true
  | _ -> is_used env (Code_id_or_name.var var)

let is_name_used (env : env) kinds name =
  Name.pattern_match name ~symbol:(is_symbol_used env)
    ~var:(is_var_used env kinds)

let poison_value = 0 (* 123456789 *)

let poison kind = Simple.const_int_of_kind kind poison_value

let rec fold_unboxed_with_kind (f : Flambda_kind.t -> 'a -> 'b -> 'b)
    (fields : 'a Dep_solver.unboxed_fields Field.Map.t) acc =
  Field.Map.fold
    (fun field elt acc ->
      match (elt : _ Dep_solver.unboxed_fields) with
      | Not_unboxed elt -> f (Field.kind field) elt acc
      | Unboxed fields -> fold_unboxed_with_kind f fields acc)
    fields acc

(* This is not symmetrical!! [fields1] must define a subset of [fields2], but
   does not have to define all of them. *)
let rec fold2_unboxed_subset (f : 'a -> 'b -> 'c -> 'c)
    (fields1 : 'a Dep_solver.unboxed_fields)
    (fields2 : 'b Dep_solver.unboxed_fields) acc =
  match fields1, fields2 with
  | Not_unboxed x1, Not_unboxed x2 -> f x1 x2 acc
  | Not_unboxed _, Unboxed _ | Unboxed _, Not_unboxed _ ->
    Misc.fatal_errorf "[fold2_unboxed_subset]"
  | Unboxed fields1, Unboxed fields2 ->
    Field.Map.fold
      (fun field f1 acc ->
        let f2 = Field.Map.find field fields2 in
        fold2_unboxed_subset f f1 f2 acc)
      fields1 acc

let rec fold2_unboxed_subset_with_kind
    (f : Flambda_kind.t -> 'a -> 'b -> 'c -> 'c)
    (fields1 : 'a Dep_solver.unboxed_fields Field.Map.t)
    (fields2 : 'b Dep_solver.unboxed_fields Field.Map.t) acc =
  Field.Map.fold
    (fun field f1 acc ->
      let f2 = Field.Map.find field fields2 in
      match
        (f1, f2 : _ Dep_solver.unboxed_fields * _ Dep_solver.unboxed_fields)
      with
      | Not_unboxed x1, Not_unboxed x2 -> f (Field.kind field) x1 x2 acc
      | Not_unboxed _, Unboxed _ | Unboxed _, Not_unboxed _ ->
        Misc.fatal_errorf "[fold2_unboxed_subset]"
      | Unboxed fields1, Unboxed fields2 ->
        fold2_unboxed_subset_with_kind f fields1 fields2 acc)
    fields1 acc

let simple_is_unboxable env simple =
  Simple.pattern_match
    ~const:(fun _ -> false)
    ~name:(fun name ~coercion:_ ->
      Option.is_some
        (Dep_solver.get_unboxed_fields env.uses (Code_id_or_name.name name)))
    simple

let get_simple_unboxable env simple =
  Simple.pattern_match
    ~const:(fun _ -> assert false)
    ~name:(fun name ~coercion:_ ->
      Option.get
        (Dep_solver.get_unboxed_fields env.uses (Code_id_or_name.name name)))
    simple

let rewrite_simple kinds (env : env) simple =
  Simple.pattern_match simple
    ~name:(fun name ~coercion:_ ->
      if not
           (Option.is_none
              (Dep_solver.get_unboxed_fields env.uses
                 (Code_id_or_name.name name)))
      then simple (* XXX Misc.fatal_errorf "UNBOXED?? %a@." Name.print name; *)
      else if is_name_used env kinds name
      then simple
      else
        let kind =
          match Name.Map.find_opt name kinds with
          | Some k -> k
          | None ->
            if Name.is_symbol name
            then Flambda_kind.value
            else Misc.fatal_errorf "Unbound name %a" Name.print name
        in
        poison kind)
    ~const:(fun _ -> simple)

let rewrite_simple_opt (env : env) kinds = function
  | None -> None
  | Some simple as simpl ->
    Simple.pattern_match simple
      ~name:(fun name ~coercion:_ ->
        if is_name_used env kinds name then simpl else None)
      ~const:(fun _ -> simpl)

let rewrite_or_variable default env kinds (or_variable : _ Or_variable.t) =
  (* CR ncourant: rewrite unboxed variables *)
  match or_variable with
  | Const _ -> or_variable
  | Var (v, _) ->
    if is_var_used env kinds v then or_variable else Or_variable.Const default

let rewrite_simple_with_debuginfo kinds env (simple : Simple.With_debuginfo.t) =
  Simple.With_debuginfo.create
    (rewrite_simple kinds env (Simple.With_debuginfo.simple simple))
    (Simple.With_debuginfo.dbg simple)

let rewrite_set_of_closures env kinds ~(bound : Name.t list)
    ({ function_decls; value_slots; alloc_mode } : rev_set_of_closures) =
  let slot_is_used slot =
    List.exists
      (fun bound_name ->
        Dep_solver.field_used env.uses (Code_id_or_name.name bound_name) slot)
      bound
  in
  let code_is_used bound_name =
    Dep_solver.field_used env.uses
      (Code_id_or_name.name bound_name)
      Code_of_closure
  in
  let new_repr =
    match bound with
    | bound :: _ ->
      Dep_solver.get_changed_representation env.uses
        (Code_id_or_name.name bound)
    | [] -> Misc.fatal_error "Empty set of closures"
  in
  let value_slots, function_slot_rewrites =
    match new_repr with
    | None ->
      let value_slots =
        Value_slot.Map.filter_map
          (fun slot simple ->
            if not (slot_is_used (Value_slot slot))
            then None
            else Some (rewrite_simple kinds env simple))
          value_slots
      in
      value_slots, None
    | Some repr ->
      let fields, function_slots =
        match repr with
        | Block_representation _ -> assert false
        | Closure_representation (fields, function_slots, _) ->
          fields, Some function_slots
      in
      let existing_value_slots = value_slots in
      let value_slots =
        Field.Map.fold
          (fun field uf value_slots ->
            match (field : Field.t) with
            | Is_int | Get_tag | Block _ -> assert false
            | Code_of_closure | Apply _ | Code_id_of_call_witness _ ->
              assert false
            | Function_slot _ -> assert false
            | Value_slot value_slot -> (
              let arg = Value_slot.Map.find value_slot existing_value_slots in
              if simple_is_unboxable env arg
              then
                fold2_unboxed_subset
                  (fun ff var value_slots ->
                    Value_slot.Map.add ff (Simple.var var) value_slots)
                  uf
                  (Dep_solver.Unboxed (get_simple_unboxable env arg))
                  value_slots
              else
                match uf with
                | Dep_solver.Not_unboxed ff ->
                  Value_slot.Map.add ff
                    (rewrite_simple kinds env arg)
                    value_slots
                | Dep_solver.Unboxed _ ->
                  Misc.fatal_errorf "trying to unbox simple"))
          fields Value_slot.Map.empty
      in
      value_slots, function_slots
  in
  let open Function_declarations in
  let function_decls =
    List.map2
      (fun bound_name (function_slot, code_id) ->
        let code_id =
          match code_id with
          | Deleted _ -> code_id
          | Code_id code_id ->
            if code_is_used bound_name
            then Code_id code_id
            else
              let code_metadata = env.get_code_metadata code_id in
              Deleted
                { function_slot_size =
                    Code_metadata.function_slot_size code_metadata;
                  dbg = Code_metadata.dbg code_metadata
                }
        in
        let function_slot =
          match function_slot_rewrites with
          | None -> function_slot
          | Some function_slot_rewrites -> (
            match
              Function_slot.Map.find function_slot function_slot_rewrites
            with
            | function_slot -> function_slot
            | exception Not_found ->
              Misc.fatal_errorf "Could not find rewritten function slot for %a"
                Function_slot.print function_slot)
        in
        function_slot, code_id)
      bound
      (Function_slot.Lmap.bindings
         (Function_declarations.funs_in_order function_decls))
  in
  let function_decls =
    Function_declarations.create (Function_slot.Lmap.of_list function_decls)
  in
  let set_of_closures =
    Set_of_closures.create ~value_slots alloc_mode function_decls
  in
  all_slot_offsets
    := Slot_offsets.add_set_of_closures !all_slot_offsets ~is_phantom:false
         set_of_closures;
  set_of_closures

let rewrite_static_const kinds (env : env) (sc : Static_const.t) =
  match sc with
  | Set_of_closures _ ->
    (* Already rewritten *)
    sc
  | Block (tag, mut, shape, fields) ->
    let fields = List.map (rewrite_simple_with_debuginfo kinds env) fields in
    Static_const.block tag mut shape fields
  | Boxed_float f ->
    Static_const.boxed_float (rewrite_or_variable Float.zero env kinds f)
  | Boxed_float32 f ->
    Static_const.boxed_float32 (rewrite_or_variable Float32.zero env kinds f)
  | Boxed_int32 n ->
    Static_const.boxed_int32 (rewrite_or_variable Int32.zero env kinds n)
  | Boxed_int64 n ->
    Static_const.boxed_int64 (rewrite_or_variable Int64.zero env kinds n)
  | Boxed_nativeint n ->
    Static_const.boxed_nativeint
      (rewrite_or_variable Targetint_32_64.zero env kinds n)
  | Boxed_vec128 n ->
    Static_const.boxed_vec128
      (rewrite_or_variable Vector_types.Vec128.Bit_pattern.zero env kinds n)
  | Immutable_float_block fields ->
    let fields = List.map (rewrite_or_variable Float.zero env kinds) fields in
    Static_const.immutable_float_block fields
  | Immutable_float_array fields ->
    let fields = List.map (rewrite_or_variable Float.zero env kinds) fields in
    Static_const.immutable_float_array fields
  | Immutable_float32_array fields ->
    let fields = List.map (rewrite_or_variable Float32.zero env kinds) fields in
    Static_const.immutable_float32_array fields
  | Immutable_value_array fields ->
    let fields = List.map (rewrite_simple_with_debuginfo kinds env) fields in
    Static_const.immutable_value_array fields
  | Immutable_int32_array fields ->
    let fields = List.map (rewrite_or_variable Int32.zero env kinds) fields in
    Static_const.immutable_int32_array fields
  | Immutable_int64_array fields ->
    let fields = List.map (rewrite_or_variable Int64.zero env kinds) fields in
    Static_const.immutable_int64_array fields
  | Immutable_nativeint_array fields ->
    let fields =
      List.map (rewrite_or_variable Targetint_32_64.zero env kinds) fields
    in
    Static_const.immutable_nativeint_array fields
  | Immutable_vec128_array fields ->
    let fields =
      List.map
        (rewrite_or_variable Vector_types.Vec128.Bit_pattern.zero env kinds)
        fields
    in
    Static_const.immutable_vec128_array fields
  | Empty_array _ | Mutable_string _ | Immutable_string _ -> sc

let rewrite_static_const_or_code kinds env (sc : Static_const_or_code.t) =
  match sc with
  | Code _ -> sc
  | Deleted_code -> sc
  | Static_const sc ->
    Static_const_or_code.create_static_const (rewrite_static_const kinds env sc)

let rewrite_static_const_group kinds env (group : Static_const_group.t) =
  Static_const_group.map ~f:(rewrite_static_const_or_code kinds env) group

let simple_changed_repr env simple =
  Simple.pattern_match
    ~const:(fun _ -> false)
    ~name:(fun name ~coercion:_ ->
      Option.is_some
        (Dep_solver.get_changed_representation env.uses
           (Code_id_or_name.name name)))
    simple

let get_simple_changed_repr env simple =
  Simple.pattern_match
    ~const:(fun _ -> assert false)
    ~name:(fun name ~coercion:_ ->
      Option.get
        (Dep_solver.get_changed_representation env.uses
           (Code_id_or_name.name name)))
    simple

let get_parameters params_decisions =
  List.fold_left
    (fun acc param_decision ->
      match param_decision with
      | Delete -> acc
      | Keep (var, kind) -> Bound_parameter.create var kind :: acc
      | Unbox fields ->
        fold_unboxed_with_kind
          (fun kind v acc ->
            Bound_parameter.create v (Flambda_kind.With_subkind.anything kind)
            :: acc)
          fields acc)
    [] params_decisions
  |> List.rev

let get_args kinds env params_decisions args =
  List.fold_left2
    (fun acc arg param_decision ->
      match param_decision with
      | Delete -> acc
      | Keep _ -> rewrite_simple kinds env arg :: acc
      | Unbox fields ->
        let arg_fields = get_simple_unboxable env arg in
        fold2_unboxed_subset_with_kind
          (fun _kind _param arg_field acc -> Simple.var arg_field :: acc)
          fields arg_fields acc)
    [] args params_decisions
  |> List.rev

let get_args_with_kinds kinds env params_decisions args =
  List.fold_left2
    (fun acc arg param_decision ->
      match param_decision with
      | Delete -> acc
      | Keep (_, kind) -> (rewrite_simple kinds env arg, kind) :: acc
      | Unbox fields ->
        let arg_fields = get_simple_unboxable env arg in
        fold2_unboxed_subset_with_kind
          (fun kind _param arg_field acc ->
            (Simple.var arg_field, Flambda_kind.With_subkind.anything kind)
            :: acc)
          fields arg_fields acc)
    [] args params_decisions
  |> List.rev

let get_arity params_decisions =
  let arity =
    List.fold_left
      (fun acc param_decision ->
        match param_decision with
        | Delete -> acc
        | Keep (_, kind) -> kind :: acc
        | Unbox fields ->
          fold_unboxed_with_kind
            (fun kind _ acc -> Flambda_kind.With_subkind.anything kind :: acc)
            fields acc)
      [] params_decisions
    |> List.rev
  in
  Flambda_arity.(
    create
      [ Unboxed_product
          (List.map (fun k -> Component_for_creation.Singleton k) arity) ])

let get_simple_kind kinds simple =
  Simple.pattern_match'
    ~const:(fun const -> Reg_width_const.kind const)
    ~symbol:(fun _ ~coercion:_ -> Flambda_kind.value)
    ~var:(fun var ~coercion:_ -> Name.Map.find (Name.var var) kinds)
    simple

let rewrite_named kinds env (named : Named.t) =
  let[@local] rewrite_field_access arg field =
    let arg = get_simple_unboxable env arg in
    let var = Field.Map.find field arg in
    let var =
      match var with
      | Dep_solver.Not_unboxed var -> var
      | Dep_solver.Unboxed _ ->
        Misc.fatal_errorf "Trying to bind non-unboxed to unboxed"
    in
    Named.create_simple (Simple.var var)
  in
  let[@local] rewrite_field_access_chg_repr arg field dbg =
    let arg_repr = get_simple_changed_repr env arg in
    match arg_repr with
    | Block_representation (arg_fields, _size) -> (
      let f = Field.Map.find field arg_fields in
      match f with
      | Dep_solver.Unboxed _ ->
        Misc.fatal_errorf "Trying to bind non-unboxed to unboxed"
      | Dep_solver.Not_unboxed (field, kind) ->
        Named.create_prim
          (Flambda_primitive.Unary
             ( Block_load
                 { field = Targetint_31_63.of_int field; kind; mut = Immutable },
               arg ))
          dbg)
    | Closure_representation (arg_fields, function_slots, current_function_slot)
      -> (
      let f = Field.Map.find field arg_fields in
      match f with
      | Dep_solver.Unboxed _ ->
        Misc.fatal_errorf "Trying to bind non-unboxed to unboxed"
      | Dep_solver.Not_unboxed value_slot ->
        Named.create_prim
          (Flambda_primitive.Unary
             ( Project_value_slot
                 { value_slot;
                   project_from =
                     Function_slot.Map.find current_function_slot function_slots
                 },
               arg ))
          dbg)
  in
  match[@ocaml.warning "-4"] named with
  | Simple simple -> Named.create_simple (rewrite_simple kinds env simple)
  | Prim (Unary (Block_load { kind; field; _ }, arg), _dbg)
    when simple_is_unboxable env arg ->
    let kind = Flambda_primitive.Block_access_kind.element_kind_for_load kind in
    let field =
      Global_flow_graph.Field.Block (Targetint_31_63.to_int field, kind)
    in
    rewrite_field_access arg field
  | Prim (Unary (Project_value_slot { value_slot; _ }, arg), _dbg)
    when simple_is_unboxable env arg ->
    rewrite_field_access arg (Global_flow_graph.Field.Value_slot value_slot)
  | Prim (Unary (Is_int { variant_only = true }, arg), _dbg)
    when simple_is_unboxable env arg ->
    rewrite_field_access arg Global_flow_graph.Field.Is_int
  | Prim (Unary (Get_tag, arg), _dbg) when simple_is_unboxable env arg ->
    rewrite_field_access arg Global_flow_graph.Field.Get_tag
  | Prim (Unary (Block_load { kind; field; _ }, arg), dbg)
    when simple_changed_repr env arg ->
    let kind = Flambda_primitive.Block_access_kind.element_kind_for_load kind in
    let field =
      Global_flow_graph.Field.Block (Targetint_31_63.to_int field, kind)
    in
    rewrite_field_access_chg_repr arg field dbg
  | Prim (Unary (Project_value_slot { value_slot; _ }, arg), dbg)
    when simple_changed_repr env arg ->
    rewrite_field_access_chg_repr arg
      (Global_flow_graph.Field.Value_slot value_slot) dbg
  | Prim (Unary (Is_int { variant_only = true }, arg), dbg)
    when simple_changed_repr env arg ->
    rewrite_field_access_chg_repr arg Global_flow_graph.Field.Is_int dbg
  | Prim (Unary (Get_tag, arg), dbg) when simple_changed_repr env arg ->
    rewrite_field_access_chg_repr arg Global_flow_graph.Field.Get_tag dbg
  | Prim (prim, dbg) ->
    let prim = Flambda_primitive.map_args (rewrite_simple kinds env) prim in
    Named.create_prim prim dbg
  | Set_of_closures s -> Named.create_set_of_closures s (* Already rewritten *)
  | Static_consts sc ->
    Named.create_static_consts (rewrite_static_const_group kinds env sc)
  | Rec_info r -> Named.create_rec_info r

let is_dead_var env kinds v =
  let kind = Name.Map.find (Name.var v) kinds in
  match[@ocaml.warning "-4"] kind with
  | Flambda_kind.(Region | Rec_info) -> false
  | _ -> not (Dep_solver.has_source env.uses (Code_id_or_name.var v))

let rewrite_apply_cont_expr kinds env ac =
  let cont = Apply_cont_expr.continuation ac in
  let args = Apply_cont_expr.args ac in
  if List.exists
       (fun arg ->
         Simple.pattern_match arg
           ~name:(fun name ~coercion:_ ->
             (not (Dep_solver.has_source env.uses (Code_id_or_name.name name)))
             && Name.pattern_match name
                  ~symbol:(fun s_ ->
                    Compilation_unit.is_current (Symbol.compilation_unit s_))
                  ~var:(fun _ -> true))
           ~const:(fun _ -> false))
       args
  then None
  else
    let args =
      let args_to_keep = Continuation.Map.find cont env.cont_params_to_keep in
      get_args kinds env args_to_keep args
    in
    Some (Apply_cont_expr.with_continuation_and_args ac cont ~args)

type change_calling_convention =
  | Not_changing_calling_convention
  | Changing_calling_convention of Code_id.t

let function_params_and_body_free_names fpb =
  Function_params_and_body.pattern_match fpb
    ~f:(fun
         ~return_continuation
         ~exn_continuation
         params
         ~body:_
         ~my_closure
         ~is_my_closure_used:_
         ~my_region
         ~my_ghost_region
         ~my_depth
         ~free_names_of_body
       ->
      let f =
        match free_names_of_body with Unknown -> assert false | Known f -> f
      in
      let f =
        Name_occurrences.remove_continuation f ~continuation:return_continuation
      in
      let f =
        Name_occurrences.remove_continuation f ~continuation:exn_continuation
      in
      let o2l = function None -> [] | Some x -> [x] in
      List.fold_left
        (fun f var -> Name_occurrences.remove_var f ~var)
        f
        (o2l my_region @ o2l my_ghost_region
        @ (my_closure :: my_depth :: Bound_parameters.vars params)))

let rec rebuild_expr (kinds : Flambda_kind.t Name.Map.t) (env : env)
    (rev_expr : rev_expr) : RE.t =
  let { expr; holed_expr } = rev_expr in
  let expr =
    match expr with
    | Invalid { message } ->
      RE.from_expr
        ~expr:(Expr.create_invalid (Message message))
        ~free_names:Name_occurrences.empty
    | Apply_cont ac -> (
      match rewrite_apply_cont_expr kinds env ac with
      | None ->
        RE.from_expr
          ~expr:
            (Expr.create_invalid
               (Message
                  (Format.asprintf "Dead variable in apply cont: %a"
                     Apply_cont_expr.print ac)))
          ~free_names:Name_occurrences.empty
      | Some ac ->
        let expr = Expr.create_apply_cont ac in
        let free_names = Apply_cont_expr.free_names ac in
        RE.from_expr ~expr ~free_names)
    | Switch switch ->
      let arms =
        Targetint_31_63.Map.filter_map
          (fun _ -> rewrite_apply_cont_expr kinds env)
          (Switch_expr.arms switch)
      in
      if Targetint_31_63.Map.is_empty arms
      then
        RE.from_expr
          ~expr:(Expr.create_invalid Zero_switch_arms)
          ~free_names:Name_occurrences.empty
      else
        let switch =
          Switch_expr.create
            ~condition_dbg:(Switch_expr.condition_dbg switch)
              (* Scrutinee should never need rewriting, do it anyway for
                 completeness *)
            ~scrutinee:(rewrite_simple kinds env (Switch_expr.scrutinee switch))
            ~arms
        in
        let expr = Expr.create_switch switch in
        let free_names = Switch_expr.free_names switch in
        RE.from_expr ~expr ~free_names
    | Apply apply -> (
      (* CR ncourant: we never rewrite alloc_mode. This is currently ok because
         we never remove begin- or end-region primitives, but might be needed
         later if we chose to handle them. *)
      let call_kind =
        let rewrite_simple = rewrite_simple kinds env in
        match Apply.call_kind apply with
        | Function _ as ck -> ck
        | Method { kind; obj; alloc_mode } ->
          Call_kind.method_call kind ~obj:(rewrite_simple obj) alloc_mode
        | C_call _ as ck -> ck
        | Effect (Perform { eff }) ->
          Call_kind.effect (Call_kind.Effect.perform ~eff:(rewrite_simple eff))
        | Effect (Reperform { eff; cont; last_fiber }) ->
          Call_kind.effect
            (Call_kind.Effect.reperform ~eff:(rewrite_simple eff)
               ~cont:(rewrite_simple cont)
               ~last_fiber:(rewrite_simple last_fiber))
        | Effect (Run_stack { stack; f; arg }) ->
          Call_kind.effect
            (Call_kind.Effect.run_stack ~stack:(rewrite_simple stack)
               ~f:(rewrite_simple f) ~arg:(rewrite_simple arg))
        | Effect (Resume { stack; f; arg; last_fiber }) ->
          Call_kind.effect
            (Call_kind.Effect.resume ~stack:(rewrite_simple stack)
               ~f:(rewrite_simple f) ~arg:(rewrite_simple arg)
               ~last_fiber:(rewrite_simple last_fiber))
      in
      let code_id_actually_called, new_call_kind, _should_break_call =
        let called c alloc_mode call_kind was_indirect_unknown_arity =
          let code_id =
            Simple.pattern_match c
              ~const:(fun _ -> None)
              ~name:(fun name ~coercion:_ ->
                Dep_solver.code_id_actually_called env.uses name)
          in
          match code_id with
          | None -> None, call_kind, false
          | Some (code_id, num_already_applied_params) ->
            if num_already_applied_params <> 0 then failwith "todo";
            let new_call_kind =
              Call_kind.direct_function_call code_id alloc_mode
            in
            Some code_id, new_call_kind, was_indirect_unknown_arity
        in
        match[@ocaml.warning "-4"] call_kind with
        | Function { function_call = Direct code_id; alloc_mode } -> (
          match Apply.callee apply with
          | Some c
            when Simple.pattern_match'
                   ~var:(fun _ ~coercion:_ -> true)
                   ~const:(fun _ -> true)
                   ~symbol:(fun s ~coercion:_ ->
                     Compilation_unit.is_current (Symbol.compilation_unit s))
                   c ->
            let call_kind =
              if Dep_solver.has_use env.uses (Code_id_or_name.code_id code_id)
              then call_kind
              else Call_kind.indirect_function_call_known_arity alloc_mode
            in
            called c alloc_mode call_kind false
          | None | Some _ -> Some code_id, call_kind, false)
        | Function { function_call = Indirect_unknown_arity; alloc_mode = _ } ->
          (* called (Option.get (Apply.callee apply)) alloc_mode call_kind
             true *)
          None, call_kind, false
        | Function { function_call = Indirect_known_arity; alloc_mode } ->
          called (Option.get (Apply.callee apply)) alloc_mode call_kind false
        | _ -> None, call_kind, false
      in
      let updating_calling_convention =
        match code_id_actually_called with
        | None -> Not_changing_calling_convention
        | Some code_id -> (
          (* Format.eprintf "CODE ID %a: %a@." (Format.pp_print_option
             Simple.print) (Apply.callee apply) Code_id.print code_id; *)
          match Code_id.Map.find_opt code_id env.code_deps with
          | None -> Not_changing_calling_convention
          | Some _ ->
            let cannot_change_calling_convention =
              Dep_solver.cannot_change_calling_convention env.uses code_id
            in
            if cannot_change_calling_convention
            then Not_changing_calling_convention
            else Changing_calling_convention code_id)
      in
      let exn_continuation = Apply.exn_continuation apply in
      let exn_continuation =
        let exn_handler = Exn_continuation.exn_handler exn_continuation in
        let extra_args =
          let selected_extra_args =
            let extra_args = Exn_continuation.extra_args exn_continuation in
            (* try *)
            let args_to_keep =
              Continuation.Map.find exn_handler env.cont_params_to_keep
              |> List.tl
              (* This contains the exn argument that is not part of the extra
                 args *)
            in
            get_args_with_kinds kinds env args_to_keep (List.map fst extra_args)
            (* with Not_found -> (* Not defined in cont_params_to_keep *)
               extra_args *)
          in
          (* List.map (fun (simple, kind) -> rewrite_simple kinds env simple,
             kind) *)
          selected_extra_args
        in
        Exn_continuation.create ~exn_handler ~extra_args
      in
      let make_apply_wrapper
          (make_apply :
            continuation:Apply_expr.Result_continuation.t -> Apply_expr.t)
          apply_continuation return_decisions =
        match (apply_continuation : Apply_expr.Result_continuation.t) with
        | Never_returns ->
          let apply = make_apply ~continuation:Never_returns in
          RE.from_expr ~expr:(Expr.create_apply apply)
            ~free_names:(Apply.free_names apply)
        | Return return_cont ->
          let return_decisions = List.map freshen_decisions return_decisions in
          let apply_decisions =
            Continuation.Map.find return_cont env.cont_params_to_keep
          in
          let return_cont_wrapper = Continuation.rename return_cont in
          let apply = make_apply ~continuation:(Return return_cont_wrapper) in
          let apply_expr = Expr.create_apply apply in
          let cont_handler =
            let return_parameters = get_parameters return_decisions in
            let handler =
              try
                let _, rev_args =
                  (* TODO if the decisions are equal, don't introduce the
                     wrapper. Not really important but this will be simpler for
                     debugging *)
                  List.fold_left2
                    (fun (i, rev_args) apply_decision func_decision ->
                      match apply_decision, func_decision with
                      | Unbox _, (Keep _ | Delete) | (Keep _ | Delete), Unbox _
                        ->
                        let[@inline] error () =
                          Misc.fatal_errorf
                            "Inconsistent apply (%a) and func (%a) decisions:@ \
                             %a@."
                            print_param_decision apply_decision
                            print_param_decision func_decision Apply.print apply
                        in
                        let direct_or_indirect =
                          match[@ocaml.warning "-4"] Apply.call_kind apply with
                          | Function { function_call = Direct _; _ } -> error ()
                          | Function { function_call = Indirect_known_arity; _ }
                            ->
                            Global_flow_graph.Direct_code_pointer
                          | _ -> Global_flow_graph.Indirect_code_pointer
                        in
                        let field =
                          Global_flow_graph.Field.Apply
                            ( direct_or_indirect,
                              Global_flow_graph.Field.Normal i )
                        in
                        let has_any_source =
                          Dep_solver.not_local_field_has_source env.uses
                            (Simple.pattern_match
                               (Option.get (Apply.callee apply))
                               ~name:(fun name ~coercion:_ ->
                                 Code_id_or_name.name name)
                               ~const:(fun _ -> assert false))
                            field
                        in
                        if has_any_source then error () else raise Exit
                      | Delete, _ -> i + 1, rev_args
                      | Keep (_, _), Keep (v, _) ->
                        i + 1, Simple.var v :: rev_args
                      | Keep (_, kind), Delete ->
                        ( i + 1,
                          poison (Flambda_kind.With_subkind.kind kind)
                          :: rev_args )
                      | Unbox fields_apply, Unbox fields_func ->
                        ( i + 1,
                          fold2_unboxed_subset_with_kind
                            (fun _kind _var_apply var_func rev_args ->
                              Simple.var var_func :: rev_args)
                            fields_apply fields_func rev_args ))
                    (0, []) apply_decisions return_decisions
                in
                let args = List.rev rev_args in
                let apply_cont =
                  Apply_cont_expr.create return_cont ~args ~dbg:Debuginfo.none
                in
                RE.from_expr
                  ~expr:(Expr.create_apply_cont apply_cont)
                  ~free_names:(Apply_cont_expr.free_names apply_cont)
              with Exit ->
                RE.from_expr
                  ~expr:
                    (Expr.create_invalid
                       (Message
                          (Format.asprintf "Function call to %a never returns"
                             Simple.print
                             (Option.get (Apply.callee apply)))))
                  ~free_names:Name_occurrences.empty
            in
            RE.create_continuation_handler
              (Bound_parameters.create return_parameters)
              ~handler ~is_exn_handler:false ~is_cold:false
            (* TODO: take the one from the original return cont *)
          in
          let body =
            RE.from_expr ~expr:apply_expr ~free_names:(Apply.free_names apply)
          in
          RE.create_non_recursive_let_cont return_cont_wrapper cont_handler
            ~body
      in
      (* TODO rewrite arities *)
      match updating_calling_convention with
      | Not_changing_calling_convention ->
        (* Format.eprintf "NOT CHANGING CALLING CONVENTION %a@." Apply.print
           apply; *)
        let args = List.map (rewrite_simple kinds env) (Apply.args apply) in
        let args_arity = Apply.args_arity apply in
        let return_arity = Apply.return_arity apply in
        let make_apply =
          Apply.create
          (* Note here that callee is rewritten with [rewrite_simple_opt], which
             will put [None] as the callee instead of a dummy value, as a dummy
             value would then be further used in a later simplify pass to refine
             the call kind and produce an invalid. *)
            ~callee:(rewrite_simple_opt env kinds (Apply.callee apply))
            exn_continuation ~args ~args_arity ~return_arity
            ~call_kind:new_call_kind (Apply.dbg apply)
            ~inlined:(Apply.inlined apply)
            ~inlining_state:(Apply.inlining_state apply)
            ~probe:(Apply.probe apply) ~position:(Apply.position apply)
            ~relative_history:(Apply.relative_history apply)
        in
        let func_decisions =
          List.map
            (fun kind -> Keep (Variable.create "function_return", kind))
            (Flambda_arity.unarized_components return_arity)
        in
        make_apply_wrapper make_apply (Apply.continuation apply) func_decisions
      | Changing_calling_convention code_id ->
        (* Format.eprintf "CHANGING CALLING CONVENTION %a %a@." Code_id.print
           code_id Apply.print apply; *)
        let args_from_unboxed_callee, callee =
          match Apply.callee apply with
          | Some callee when simple_is_unboxable env callee ->
            let fields = get_simple_unboxable env callee in
            let new_args =
              fold_unboxed_with_kind
                (fun kind v acc ->
                  (Simple.var v, Flambda_kind.With_subkind.anything kind) :: acc)
                fields []
            in
            new_args, None
          | (None | Some _) as callee ->
            ( [],
              (* Note here that callee is rewritten with [rewrite_simple_opt],
                 which will put [None] as the callee instead of a dummy value,
                 as a dummy value would then be further used in a later simplify
                 pass to refine the call kind and produce an invalid. *)
              rewrite_simple_opt env kinds callee )
        in
        let params_decisions =
          match Code_id.Map.find_opt code_id env.function_params_to_keep with
          | None -> assert false
          | Some p -> p
        in
        let params_decisions =
          Flambda_arity.group_by_parameter (Apply.args_arity apply)
            params_decisions
        in
        let args =
          Flambda_arity.group_by_parameter (Apply.args_arity apply)
            (Apply.args apply)
        in
        let args =
          List.map2 (get_args_with_kinds kinds env) params_decisions args
        in
        let args =
          match args with
          | [] -> assert false
          | first :: rest -> (args_from_unboxed_callee @ first) :: rest
        in
        let args_arity =
          let components_for args =
            Flambda_arity.Component_for_creation.Unboxed_product
              (List.map
                 (fun (_, k) ->
                   Flambda_arity.Component_for_creation.Singleton k)
                 args)
          in
          Flambda_arity.create (List.map components_for args)
        in
        let return_decisions =
          Code_id.Map.find code_id env.function_return_decision
        in
        let return_arity =
          Flambda_arity.unarize_t (get_arity return_decisions)
        in
        let args = List.map fst (List.flatten args) in
        let make_apply ~continuation =
          Apply.create ~callee ~continuation exn_continuation ~args ~args_arity
            ~return_arity ~call_kind:new_call_kind (Apply.dbg apply)
            ~inlined:(Apply.inlined apply)
            ~inlining_state:(Apply.inlining_state apply)
            ~probe:(Apply.probe apply) ~position:(Apply.position apply)
            ~relative_history:(Apply.relative_history apply)
        in
        make_apply_wrapper make_apply (Apply.continuation apply)
          return_decisions)
  in
  rebuild_holed kinds env holed_expr expr

and rebuild_function_params_and_body (kinds : Flambda_kind.t Name.Map.t)
    (env : env) code_metadata (params_and_body : rev_params_and_body) =
  let { return_continuation;
        exn_continuation;
        params;
        body;
        my_closure;
        my_region;
        my_ghost_region;
        my_depth
      } =
    params_and_body
  in
  let code_id = Code_metadata.code_id code_metadata in
  let updating_calling_convention =
    match Code_id.Map.find_opt code_id env.code_deps with
    | None -> assert false
    | Some _ ->
      let cannot_change_calling_convention =
        Dep_solver.cannot_change_calling_convention env.uses code_id
      in
      if cannot_change_calling_convention
      then Not_changing_calling_convention
      else Changing_calling_convention code_id
  in
  let rebuild_body () =
    let all_vars =
      Option.to_list my_region
      @ Option.to_list my_ghost_region
      @ (my_closure :: Bound_parameters.vars params)
    in
    match List.filter (is_dead_var env kinds) all_vars with
    | [] -> rebuild_expr kinds env body
    | _ :: _ as dead_vars ->
      let msg =
        Format.asprintf
          "Function is never called because of dead parameters: [%a]."
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
             Variable.print)
          dead_vars
      in
      RE.from_expr
        ~expr:(Expr.create_invalid (Message msg))
        ~free_names:Name_occurrences.empty
  in
  match updating_calling_convention with
  | Not_changing_calling_convention ->
    let body = rebuild_body () in
    (* Format.eprintf "REBUILD %a FREE %a@." Code_id.print code_id
       Name_occurrences.print body.free_names; *)
    ( Function_params_and_body.create ~return_continuation ~exn_continuation
        params ~body:body.expr ~free_names_of_body:(Known body.free_names)
        ~my_closure ~my_region ~my_ghost_region ~my_depth,
      code_metadata )
  | Changing_calling_convention code_id ->
    let return_decisions =
      Code_id.Map.find code_id env.function_return_decision
    in
    let params_decision =
      Code_id.Map.find code_id env.function_params_to_keep
    in
    let result_arity = Flambda_arity.unarize_t (get_arity return_decisions) in
    let code_metadata =
      Code_metadata.with_result_arity result_arity code_metadata
    in
    let params_decision =
      List.map2
        (fun decision param ->
          match decision with
          | Delete -> Delete
          | Unbox _ ->
            Unbox
              (Option.get
                 (Dep_solver.get_unboxed_fields env.uses
                    (Code_id_or_name.var (Bound_parameter.var param))))
          | Keep (_, kind) -> Keep (Bound_parameter.var param, kind))
        params_decision
        (Bound_parameters.to_list params)
    in
    let params_decision =
      Flambda_arity.group_by_parameter
        (Code_metadata.params_arity code_metadata)
        params_decision
    in
    let params = List.map (fun p -> get_parameters p) params_decision in
    let params_from_closure, code_metadata =
      match
        (* TODO move that in the decisions There should be a single record field
           with all the decisions for return params and closure *)
        Dep_solver.get_unboxed_fields env.uses (Code_id_or_name.var my_closure)
      with
      | None -> [], code_metadata
      | Some fields ->
        ( fold_unboxed_with_kind
            (fun kind v acc ->
              Bound_parameter.create v (Flambda_kind.With_subkind.anything kind)
              :: acc)
            fields [],
          Code_metadata.with_is_my_closure_used false code_metadata )
    in
    let params =
      match params with
      | [] -> assert false
      | first :: rest -> (params_from_closure @ first) :: rest
    in
    let params_arity =
      let components_for params =
        Flambda_arity.Component_for_creation.Unboxed_product
          (List.map
             (fun bp ->
               Flambda_arity.Component_for_creation.Singleton
                 (Bound_parameter.kind bp))
             params)
      in
      Flambda_arity.create (List.map components_for params)
    in
    let code_metadata =
      Code_metadata.with_is_tupled false
        (Code_metadata.with_params_arity params_arity code_metadata)
    in
    (* XXX never called indirectly *)
    let body = rebuild_body () in
    (* Format.eprintf "REBUILD %a FREE %a@." Code_id.print code_id
       Name_occurrences.print body.free_names; *)
    (* assert (List.exists Fun.id (Continuation.Map.find return_continuation
       env.cont_params_to_keep)); *)
    ( Function_params_and_body.create ~return_continuation ~exn_continuation
        (Bound_parameters.create (List.flatten params))
        ~body:body.expr ~free_names_of_body:(Known body.free_names) ~my_closure
        ~my_region ~my_ghost_region ~my_depth,
      code_metadata )

and bind_fields fields arg_fields hole =
  fold2_unboxed_subset
    (fun var arg hole ->
      let bp =
        Bound_pattern.singleton (Bound_var.create var Name_mode.normal)
      in
      RE.create_let bp (Named.create_simple (Simple.var arg)) ~body:hole)
    fields arg_fields hole

and rebuild_holed (kinds : Flambda_kind.t Name.Map.t) (env : env)
    (rev_expr : rev_expr_holed) (hole : RE.t) : RE.t =
  match rev_expr with
  | Up -> hole
  | Let let_ -> (
    let[@local] erase () = rebuild_holed kinds env let_.parent hole in
    let[@local] default () =
      let subexpr =
        let bp, defining_expr =
          match let_.defining_expr with
          | Named defining_expr -> let_.bound_pattern, defining_expr
          | Static_consts group ->
            let bound_static =
              match let_.bound_pattern with
              | Static l -> l
              | Set_of_closures _ | Singleton _ ->
                (* Bound pattern is static consts, so can't bind something
                   else *)
                assert false
            in
            let bound_and_group =
              List.filter_map
                (fun ((p, e) as arg : Bound_static.Pattern.t * _) ->
                  match p with
                  | Code code_id ->
                    if is_code_id_used env code_id
                    then Some arg
                    else (
                      (match e with
                      | Code _ -> ()
                      | Deleted_code -> ()
                      | Static_const _ ->
                        (* Pattern is [Code _], so can't bind static const *)
                        assert false);
                      Some (p, Deleted_code))
                  | Block_like sym ->
                    if is_symbol_used env sym then Some arg else None
                  | Set_of_closures m ->
                    if Function_slot.Lmap.exists
                         (fun _ sym ->
                           assert (
                             not
                               (Option.is_some
                                  (Dep_solver.get_changed_representation
                                     env.uses
                                     (Code_id_or_name.symbol sym))));
                           is_symbol_used env sym)
                         m
                    then Some arg
                    else None)
                (List.combine (Bound_static.to_list bound_static) group)
            in
            let bound_static, _group = List.split bound_and_group in
            let static_const_or_code
                ((bound_to : Bound_static.Pattern.t), static_const_or_code) =
              match static_const_or_code with
              | Deleted_code -> Static_const_or_code.deleted_code
              | Code
                  { params_and_body;
                    code_metadata;
                    free_names_of_params_and_body = _
                  } ->
                let is_my_closure_used =
                  is_var_used env kinds params_and_body.my_closure
                in
                let code_metadata =
                  if Bool.equal is_my_closure_used
                       (Code_metadata.is_my_closure_used code_metadata)
                  then code_metadata
                  else (
                    assert (not is_my_closure_used);
                    Code_metadata.with_is_my_closure_used is_my_closure_used
                      code_metadata)
                in
                let params_and_body, code_metadata =
                  rebuild_function_params_and_body kinds env code_metadata
                    params_and_body
                in
                let code =
                  Code.create_with_metadata ~params_and_body ~code_metadata
                    ~free_names_of_params_and_body:
                      (function_params_and_body_free_names params_and_body)
                in
                assert (
                  Compilation_unit.is_current
                    (Code_id.get_compilation_unit (Code.code_id code)));
                all_code := Code_id.Map.add (Code.code_id code) code !all_code;
                Static_const_or_code.create_code code
              | Static_const (Set_of_closures set_of_closures) ->
                let bound_to =
                  match bound_to with
                  | Set_of_closures function_slots ->
                    Function_slot.Lmap.data function_slots
                  | Code _ | Block_like _ ->
                    Misc.fatal_error "Expected Set_of_closures"
                in
                let bound_to = List.map Name.symbol bound_to in
                let set_of_closures =
                  rewrite_set_of_closures env kinds ~bound:bound_to
                    set_of_closures
                in
                Static_const.set_of_closures set_of_closures
                |> Static_const_or_code.create_static_const
              | Static_const (Other static_const) -> (
                match static_const with
                | Block _ | Boxed_float32 _ | Boxed_float _ | Boxed_int32 _
                | Boxed_int64 _ | Boxed_nativeint _ | Boxed_vec128 _
                | Immutable_float_block _ | Immutable_float_array _
                | Immutable_float32_array _ | Immutable_int32_array _
                | Immutable_int64_array _ | Immutable_nativeint_array _
                | Immutable_vec128_array _ | Immutable_value_array _
                | Empty_array _ | Mutable_string _ | Immutable_string _ ->
                  Static_const_or_code.create_static_const static_const
                | Set_of_closures _ ->
                  Misc.fatal_errorf
                    "Set_of_closures is not permitted in conjunction with \
                     Other in the Static_const case:@ %a"
                    Static_const.print static_const)
            in
            let group =
              Static_const_group.create
                (List.map static_const_or_code bound_and_group)
            in
            ( Bound_pattern.static (Bound_static.create bound_static),
              Named.create_static_consts group )
          | Set_of_closures set_of_closures ->
            let bound =
              match let_.bound_pattern with
              | Set_of_closures bound_vars ->
                List.map Name.var (List.map Bound_var.var bound_vars)
              | Static _ | Singleton _ ->
                (* Pattern is a set of closures *)
                assert false
            in
            let set_of_closures =
              rewrite_set_of_closures env kinds ~bound set_of_closures
            in
            let is_phantom =
              Name_mode.is_phantom @@ Bound_pattern.name_mode let_.bound_pattern
            in
            all_slot_offsets
              := Slot_offsets.add_set_of_closures !all_slot_offsets ~is_phantom
                   set_of_closures;
            let_.bound_pattern, Named.create_set_of_closures set_of_closures
        in
        begin
          match let_.bound_pattern with
          | Bound_pattern.Set_of_closures bvs
            when List.exists
                   (fun bv ->
                     Option.is_some
                       (Dep_solver.get_unboxed_fields env.uses
                          (Code_id_or_name.var (Bound_var.var bv))))
                   bvs ->
            assert (
              List.for_all
                (fun bv ->
                  (not
                     (Dep_solver.has_use env.uses
                        (Code_id_or_name.var (Bound_var.var bv))))
                  || Option.is_some
                       (Dep_solver.get_unboxed_fields env.uses
                          (Code_id_or_name.var (Bound_var.var bv))))
                bvs);
            List.fold_left
              (fun hole bv ->
                if not
                     (Dep_solver.has_use env.uses
                        (Code_id_or_name.var (Bound_var.var bv)))
                then hole
                else
                  let to_bind =
                    Option.get
                      (Dep_solver.get_unboxed_fields env.uses
                         (Code_id_or_name.var (Bound_var.var bv)))
                  in
                  let value_slots =
                    match let_.defining_expr with
                    | Named (Set_of_closures _set) ->
                      (* Possible ? *)
                      assert false
                      (* Set_of_closures.value_slots set *)
                    | Set_of_closures set -> set.value_slots
                    | _ -> assert false
                  in
                  Field.Map.fold
                    (fun (field : Global_flow_graph.Field.t) var hole ->
                      match field with
                      | Value_slot value_slot ->
                        let arg = Value_slot.Map.find value_slot value_slots in
                        if simple_is_unboxable env arg
                        then
                          bind_fields var
                            (Dep_solver.Unboxed (get_simple_unboxable env arg))
                            hole
                        else
                          let var =
                            match var with
                            | Dep_solver.Not_unboxed var -> var
                            | Dep_solver.Unboxed _ ->
                              Misc.fatal_errorf "Trying to unbox non-unboxable"
                          in
                          let bp =
                            Bound_pattern.singleton
                              (Bound_var.create var Name_mode.normal)
                          in
                          RE.create_let bp (Named.create_simple arg) ~body:hole
                      | Block _ | Is_int | Get_tag | Function_slot _
                      | Code_of_closure | Apply _ | Code_id_of_call_witness _ ->
                        assert false)
                    to_bind hole)
              hole bvs
          | Bound_pattern.Singleton bv
            when Option.is_some
                   (Dep_solver.get_unboxed_fields env.uses
                      (Code_id_or_name.var (Bound_var.var bv))) -> (
            let to_bind =
              Option.get
                (Dep_solver.get_unboxed_fields env.uses
                   (Code_id_or_name.var (Bound_var.var bv)))
            in
            let load_field field arg dbg =
              let oarg = arg in
              let arg =
                Simple.pattern_match arg
                  ~const:(fun _ ->
                    Misc.fatal_error "Loading unboxed from constant")
                  ~name:(fun name ~coercion:_ -> name)
              in
              let arg = Code_id_or_name.name arg in
              match Dep_solver.get_unboxed_fields env.uses arg with
              | Some arg ->
                bind_fields (Dep_solver.Unboxed to_bind)
                  (Field.Map.find field arg) hole
              | None -> (
                assert (
                  Option.is_some
                    (Dep_solver.get_changed_representation env.uses arg));
                let arg =
                  Option.get
                    (Dep_solver.get_changed_representation env.uses arg)
                in
                match arg with
                | Block_representation (arg_fields, _size) ->
                  let arg = Field.Map.find field arg_fields in
                  fold2_unboxed_subset
                    (fun var (field, kind) hole ->
                      let bp =
                        Bound_pattern.singleton
                          (Bound_var.create var Name_mode.normal)
                      in
                      let named =
                        Named.create_prim
                          (Flambda_primitive.Unary
                             ( Block_load
                                 { field = Targetint_31_63.of_int field;
                                   kind;
                                   mut = Immutable
                                 },
                               oarg ))
                          dbg
                      in
                      RE.create_let bp named ~body:hole)
                    (Dep_solver.Unboxed to_bind) arg hole
                | Closure_representation
                    (arg_fields, function_slots, current_function_slot) ->
                  let arg = Field.Map.find field arg_fields in
                  fold2_unboxed_subset
                    (fun var value_slot hole ->
                      let bp =
                        Bound_pattern.singleton
                          (Bound_var.create var Name_mode.normal)
                      in
                      let named =
                        Named.create_prim
                          (Flambda_primitive.Unary
                             ( Project_value_slot
                                 { value_slot;
                                   project_from =
                                     Function_slot.Map.find
                                       current_function_slot function_slots
                                 },
                               oarg ))
                          dbg
                      in
                      RE.create_let bp named ~body:hole)
                    (Dep_solver.Unboxed to_bind) arg hole)
            in
            match let_.defining_expr with
            | Named named -> (
              match named with
              | Prim (Variadic (Make_block (kind, _, _), args), _dbg) ->
                Field.Map.fold
                  (fun (field : Global_flow_graph.Field.t) var hole ->
                    let arg =
                      match field with
                      | Block (nth, field_kind) ->
                        let arg =
                          if nth < List.length args
                          then
                            let arg = List.nth args nth in
                            if Flambda_kind.equal field_kind
                                 (get_simple_kind kinds arg)
                            then arg
                            else poison field_kind
                          else poison field_kind
                        in
                        if simple_is_unboxable env arg
                        then Either.Right (get_simple_unboxable env arg)
                        else Either.Left arg
                      | Is_int -> Either.Left Simple.untagged_const_false
                      | Get_tag ->
                        let tag, _ =
                          Flambda_primitive.Block_kind.to_shape kind
                        in
                        Either.Left
                          (Simple.untagged_const_int
                             (Tag.to_targetint_31_63 tag))
                      | Value_slot _ | Function_slot _ | Code_of_closure
                      | Apply _ | Code_id_of_call_witness _ ->
                        assert false
                    in
                    match arg with
                    | Either.Left simple ->
                      let var =
                        match var with
                        | Dep_solver.Not_unboxed var -> var
                        | Dep_solver.Unboxed _ ->
                          Misc.fatal_errorf "Trying to unbox non-unboxable"
                      in
                      let bp =
                        Bound_pattern.singleton
                          (Bound_var.create var Name_mode.normal)
                      in
                      RE.create_let bp (Named.create_simple simple) ~body:hole
                    | Either.Right arg_fields ->
                      bind_fields var (Dep_solver.Unboxed arg_fields) hole)
                  to_bind hole
              (* | Prim ( Unary (Opaque_identity { middle_end_only = true; _ },
                 arg), _dbg ) -> (* XXX TO REMOVE *) bind_fields
                 (Dep_solver.Unboxed to_bind) (Dep_solver.Unboxed
                 (get_simple_unboxable env arg)) hole *)
              | Prim (Unary (Block_load { field; kind; _ }, arg), dbg) ->
                let field =
                  Field.Block
                    ( Targetint_31_63.to_int field,
                      Flambda_primitive.Block_access_kind.element_kind_for_load
                        kind )
                in
                load_field field arg dbg
              | Prim
                  ( Unary
                      (Project_value_slot { value_slot; project_from = _ }, arg),
                    dbg ) ->
                let field = Field.Value_slot value_slot in
                load_field field arg dbg
              | Prim (Unary (Project_function_slot _, arg), _) | Simple arg ->
                bind_fields (Dep_solver.Unboxed to_bind)
                  (Dep_solver.Unboxed (get_simple_unboxable env arg))
                  hole
              | named ->
                Format.printf "BOUM ? %a@." Named.print named;
                assert false)
            | _ -> assert false)
          | Bound_pattern.Singleton bv
            when Option.is_some
                   (Dep_solver.get_changed_representation env.uses
                      (Code_id_or_name.var (Bound_var.var bv))) -> (
            (* TODO when this block is stored anywhere else, the subkind is no
               longer correct... we need to fix that somehow *)
            match let_.defining_expr with
            | Named
                (Prim
                  ( Unary (Project_function_slot { move_from; move_to }, arg),
                    dbg )) ->
              let fields =
                Option.get
                  (Dep_solver.get_changed_representation env.uses
                     (Code_id_or_name.var (Bound_var.var bv)))
              in
              let fss =
                match fields with
                | Block_representation _ -> assert false
                | Closure_representation (_, fss, _) -> fss
              in
              let named =
                Named.create_prim
                  (Unary
                     ( Project_function_slot
                         { move_to = Function_slot.Map.find move_to fss;
                           move_from = Function_slot.Map.find move_from fss
                         },
                       arg ))
                  dbg
              in
              RE.create_let bp named ~body:hole
            | Named
                (Prim
                  (Variadic (Make_block (kind, _mut, alloc_mode), args), dbg))
              ->
              let fields =
                Option.get
                  (Dep_solver.get_changed_representation env.uses
                     (Code_id_or_name.var (Bound_var.var bv)))
              in
              let fields, size =
                match fields with
                | Block_representation (fields, size) -> fields, size
                | Closure_representation _ -> assert false
              in
              let mp =
                Field.Map.fold
                  (fun f uf mp ->
                    match (f : Field.t) with
                    | Block (i, _kind) -> (
                      let arg = List.nth args i in
                      if simple_is_unboxable env arg
                      then
                        fold2_unboxed_subset
                          (fun (ff, _) var mp ->
                            Numeric_types.Int.Map.add ff (Simple.var var) mp)
                          uf
                          (Dep_solver.Unboxed (get_simple_unboxable env arg))
                          mp
                      else
                        match uf with
                        | Dep_solver.Not_unboxed (ff, _) ->
                          Numeric_types.Int.Map.add ff
                            (rewrite_simple kinds env arg)
                            mp
                        | Dep_solver.Unboxed _ ->
                          Misc.fatal_errorf "trying to unbox simple")
                    | Get_tag -> (
                      let tag =
                        match kind with
                        | Values (tag, _) | Mixed (tag, _) ->
                          Tag.to_targetint_31_63 (Tag.Scannable.to_tag tag)
                        | Naked_floats ->
                          Tag.to_targetint_31_63 Tag.double_array_tag
                      in
                      match uf with
                      | Dep_solver.Not_unboxed (ff, _) ->
                        Numeric_types.Int.Map.add ff
                          (rewrite_simple kinds env (Simple.const_int tag))
                          mp
                      | Dep_solver.Unboxed _ ->
                        Misc.fatal_errorf "trying to unbox simple")
                    | Is_int -> (
                      match uf with
                      | Dep_solver.Not_unboxed (ff, _) ->
                        Numeric_types.Int.Map.add ff
                          (rewrite_simple kinds env Simple.const_one)
                          mp
                      | Dep_solver.Unboxed _ ->
                        Misc.fatal_errorf "trying to unbox simple")
                    | Value_slot _ | Function_slot _ | Code_of_closure | Apply _
                    | Code_id_of_call_witness _ ->
                      assert false)
                  fields Numeric_types.Int.Map.empty
              in
              let args =
                List.init size (fun i ->
                    match Numeric_types.Int.Map.find_opt i mp with
                    | None -> Simple.const_zero
                    | Some x -> x)
              in
              let named =
                Named.create_prim
                  (Flambda_primitive.Variadic
                     ( Make_block
                         ( Flambda_primitive.Block_kind.Values
                             ( Tag.Scannable.zero,
                               List.map
                                 (fun _ -> Flambda_kind.With_subkind.any_value)
                                 args ),
                           Immutable,
                           alloc_mode ),
                       args ))
                  dbg
              in
              RE.create_let bp named ~body:hole
            | _ ->
              let defining_expr = rewrite_named kinds env defining_expr in
              RE.create_let bp defining_expr ~body:hole)
          | Bound_pattern.Set_of_closures bvs
            when List.exists
                   (fun bv ->
                     Option.is_some
                       (Dep_solver.get_changed_representation env.uses
                          (Code_id_or_name.var (Bound_var.var bv))))
                   bvs ->
            let bound = List.map (fun bv -> Name.var (Bound_var.var bv)) bvs in
            let set =
              match let_.defining_expr with
              | Named (Set_of_closures _set) ->
                (* Possible ? *)
                assert false
                (* Set_of_closures.value_slots set *)
              | Set_of_closures set -> set
              | _ -> assert false
            in
            let set_of_closures =
              rewrite_set_of_closures env kinds ~bound set
            in
            RE.create_let bp
              (Named.create_set_of_closures set_of_closures)
              ~body:hole
          | _ -> (
            match[@ocaml.warning "-4"] defining_expr with
            | Prim
                ( Variadic
                    (Make_block (block_kind, mutability, alloc_mode), fields),
                  dbg ) ->
              let bound_name =
                match bp with
                | Singleton v -> Name.var (Bound_var.var v)
                | Set_of_closures _ | Static _ -> assert false
              in
              let _tag, block_shape =
                Flambda_primitive.Block_kind.to_shape block_kind
              in
              let block_kind =
                match block_kind with
                | Mixed _ | Naked_floats -> block_kind
                | Values (tag, subkinds) -> (
                  let ks =
                    Flambda_kind.With_subkind.create Flambda_kind.value
                      (Flambda_kind.With_subkind.Non_null_value_subkind.Variant
                         { consts = Targetint_31_63.Set.empty;
                           non_consts =
                             Tag.Scannable.Map.singleton tag
                               (block_shape, subkinds)
                         })
                      Flambda_kind.With_subkind.Nullable.Non_nullable
                  in
                  let ks =
                    Dep_solver.rewrite_kind_with_subkind env.uses bound_name ks
                  in
                  let[@local] with_subkinds subkinds =
                    Flambda_primitive.Block_kind.Values (tag, subkinds)
                  in
                  let[@local] default () =
                    with_subkinds
                      (List.map
                         (fun ks -> Flambda_kind.With_subkind.erase_subkind ks)
                         subkinds)
                  in
                  match Flambda_kind.With_subkind.non_null_value_subkind ks with
                  | Variant { consts = _; non_consts } -> (
                    match Tag.Scannable.Map.get_singleton non_consts with
                    | Some (_, (_, subkinds)) -> with_subkinds subkinds
                    | None -> default ())
                  | _ -> default ())
              in
              let bound_name = Code_id_or_name.name bound_name in
              let fields =
                List.mapi
                  (fun i field ->
                    let kind =
                      Flambda_kind.Block_shape.element_kind block_shape i
                    in
                    let f = Global_flow_graph.Field.Block (i, kind) in
                    if Dep_solver.field_used env.uses bound_name f
                    then rewrite_simple kinds env field
                    else poison kind)
                  fields
              in
              RE.create_let bp
                (Named.create_prim
                   (Variadic
                      (Make_block (block_kind, mutability, alloc_mode), fields))
                   dbg)
                ~body:hole
            | _ ->
              let defining_expr = rewrite_named kinds env defining_expr in
              RE.create_let bp defining_expr ~body:hole)
        end
        [@ocaml.warning "-4"]
      in
      rebuild_holed kinds env let_.parent subexpr
    in
    match let_.bound_pattern with
    | Set_of_closures _ -> default ()
    | Static _ -> default ()
    | Singleton v ->
      let v = Bound_var.var v in
      (* CR ncourant: we should probably properly track regions *)
      let is_begin_region =
        match[@ocaml.warning "-4"] let_.defining_expr with
        | Named (Prim (Variadic (Begin_region _, _), _)) -> true
        | _ -> false
      in
      if is_begin_region || is_var_used env kinds v
      then default ()
      else erase ())
  | Let_cont { cont; parent; handler } ->
    if not (Name_occurrences.mem_continuation hole.free_names cont)
    then rebuild_holed kinds env parent hole
    else
      let { bound_parameters; expr; is_exn_handler; is_cold } = handler in
      let parameters_to_keep =
        Continuation.Map.find cont env.cont_params_to_keep
      in
      let cont_handler =
        let handler =
          match
            List.filter (is_dead_var env kinds)
              (Bound_parameters.vars bound_parameters)
          with
          | [] -> rebuild_expr kinds env expr
          | _ :: _ as dead_vars ->
            let msg =
              Format.asprintf
                "Continuation is never called because of dead parameters: [%a]."
                (Format.pp_print_list
                   ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
                   Variable.print)
                dead_vars
            in
            RE.from_expr
              ~expr:(Expr.create_invalid (Message msg))
              ~free_names:Name_occurrences.empty
        in
        let l = get_parameters parameters_to_keep in
        let l =
          List.concat_map
            (fun param ->
              let v = Bound_parameter.var param in
              match
                Dep_solver.get_unboxed_fields env.uses (Code_id_or_name.var v)
              with
              | None -> [param]
              | Some fields ->
                fold_unboxed_with_kind
                  (fun kind v acc ->
                    Bound_parameter.create v
                      (Flambda_kind.With_subkind.anything kind)
                    :: acc)
                  fields [])
            l
        in
        RE.create_continuation_handler
          (Bound_parameters.create l)
          ~handler ~is_exn_handler ~is_cold
      in
      let let_cont_expr =
        RE.create_non_recursive_let_cont cont cont_handler ~body:hole
      in
      rebuild_holed kinds env parent let_cont_expr
  | Let_cont_rec { parent; handlers; invariant_params } ->
    (* TODO unboxed parameters *)
    let filter_params cont params =
      let params = Bound_parameters.to_list params in
      let params =
        get_parameters
          (List.map
             (fun param ->
               env.should_keep_param cont
                 (Bound_parameter.var param)
                 (Bound_parameter.kind param))
             params)
      in
      Bound_parameters.create params
    in
    let handlers =
      Continuation.Map.mapi
        (fun cont handler ->
          let { bound_parameters; expr; is_exn_handler; is_cold } = handler in
          let bound_parameters = filter_params cont bound_parameters in
          let handler = rebuild_expr kinds env expr in
          RE.create_continuation_handler bound_parameters ~handler
            ~is_exn_handler ~is_cold)
        handlers
    in
    let invariant_params =
      filter_params
        (fst (Continuation.Map.min_binding handlers))
        invariant_params
    in
    let let_cont_expr =
      RE.create_recursive_let_cont ~invariant_params handlers ~body:hole
    in
    rebuild_holed kinds env parent let_cont_expr

type result =
  { body : Expr.t;
    free_names : Name_occurrences.t;
    all_code : Code.t Code_id.Map.t;
    slot_offsets : Slot_offsets.t
  }

let rebuild ~(code_deps : Traverse_acc.code_dep Code_id.Map.t)
    ~(continuation_info : Traverse_acc.continuation_info Continuation.Map.t)
    ~fixed_arity_continuations kinds (solved_dep : Dep_solver.result)
    get_code_metadata holed =
  all_slot_offsets := Slot_offsets.empty;
  all_code := Code_id.Map.empty;
  let should_keep_function_param code_id =
    let cannot_change_calling_convention =
      Dep_solver.cannot_change_calling_convention solved_dep code_id
    in
    if cannot_change_calling_convention
    then (fun var kind ->
      assert (
        Option.is_none
          (Dep_solver.get_unboxed_fields solved_dep (Code_id_or_name.var var)));
      Keep (var, kind))
    else
      fun param kind ->
      match
        Dep_solver.get_unboxed_fields solved_dep (Code_id_or_name.var param)
      with
      | None ->
        let is_var_used =
          Dep_solver.has_use solved_dep (Code_id_or_name.var param)
        in
        if true || is_var_used then Keep (param, kind) else Delete
      | Some fields -> Unbox fields
  in
  let function_params_to_keep =
    Code_id.Map.mapi
      (fun code_id (code_dep : Traverse_acc.code_dep) ->
        let kinds = Flambda_arity.unarize code_dep.arity in
        List.map2 (should_keep_function_param code_id) code_dep.params kinds)
      code_deps
  in
  let should_keep_function_param code_id =
    match Code_id.Map.find_opt code_id code_deps with
    | None -> fun var kind -> Keep (var, kind)
    | Some _ -> should_keep_function_param code_id
  in
  let function_return_decision =
    Code_id.Map.mapi
      (fun code_id (code_dep : Traverse_acc.code_dep) ->
        let cannot_change_calling_convention =
          Dep_solver.cannot_change_calling_convention solved_dep code_id
        in
        let metadata = get_code_metadata code_id in
        let kinds =
          Flambda_arity.unarized_components
            (Code_metadata.result_arity metadata)
        in
        if cannot_change_calling_convention
        then List.map2 (fun v kind -> Keep (v, kind)) code_dep.return kinds
        else
          (* Format.eprintf "DIRECT: %a@." Code_id.print code_id; *)
          List.map2
            (fun v kind ->
              match
                Dep_solver.get_unboxed_fields solved_dep (Code_id_or_name.var v)
              with
              | None ->
                let is_var_used =
                  Dep_solver.has_use solved_dep (Code_id_or_name.var v)
                in
                let kind =
                  Dep_solver.rewrite_kind_with_subkind solved_dep (Name.var v)
                    kind
                in
                (* TODO: fix this, needs the mapping between code ids of
                   functions and their return continuations *)
                if true || is_var_used then Keep (v, kind) else Delete
              | Some fields -> Unbox fields)
            code_dep.return kinds)
      code_deps
  in
  let should_keep_param cont param kind =
    let keep_all_parameters =
      Continuation.Set.mem cont fixed_arity_continuations
    in
    match
      Dep_solver.get_unboxed_fields solved_dep (Code_id_or_name.var param)
    with
    | None ->
      if keep_all_parameters
         ||
         let is_var_used =
           Dep_solver.has_use solved_dep (Code_id_or_name.var param)
         in
         is_var_used
         ||
         let info = Continuation.Map.find cont continuation_info in
         info.is_exn_handler && Variable.equal param (List.hd info.params)
      then
        Keep
          ( param,
            Dep_solver.rewrite_kind_with_subkind solved_dep (Name.var param)
              kind )
      else Delete
    | Some fields -> Unbox fields
  in
  let cont_params_to_keep =
    Continuation.Map.mapi
      (fun cont (info : Traverse_acc.continuation_info) ->
        List.map2 (should_keep_param cont) info.params info.arity)
      continuation_info
  in
  let env =
    { uses = solved_dep;
      code_deps;
      get_code_metadata;
      cont_params_to_keep;
      should_keep_param;
      function_params_to_keep;
      should_keep_function_param;
      function_return_decision
    }
  in
  let rebuilt_expr =
    Profile.record_call ~accumulate:true "up" (fun () ->
        rebuild_expr kinds env holed)
  in
  { body = rebuilt_expr.expr;
    free_names = rebuilt_expr.free_names;
    all_code = !all_code;
    slot_offsets = !all_slot_offsets
  }
