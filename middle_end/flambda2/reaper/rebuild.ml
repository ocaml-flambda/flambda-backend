(******************************************************************************
 *                             flambda-backend                                *
 *                                                                            *
 *             Nathanaëlle Courant, Pierre Chambart, OCamlPro                 *
 *                        Mark Shinwell, Jane Street                          *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2024--2025 OCamlPro SAS                                      *
 * Copyright (c) 2025 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

open! Flambda.Import
module DS = Dep_solver
module Float = Numeric_types.Float_by_bit_pattern
module Float32 = Numeric_types.Float32_by_bit_pattern
module GFG = Global_flow_graph
module K = Flambda_kind
module KS = K.With_subkind
module Int = Numeric_types.Int
module P = Flambda_primitive
module RE = Rebuilt_expr
module SC = Static_const
module Field = GFG.Field

type param_decision =
  | Keep of Variable.t * KS.t
  | Delete
  | Unbox of Variable.t DS.unboxed_fields Field.Map.t

let print_param_decision ppf param_decision =
  match param_decision with
  | Keep (v, kind) ->
    Format.fprintf ppf "Keep (%a, %a)" Variable.print v KS.print kind
  | Delete -> Format.fprintf ppf "Delete"
  | Unbox fields ->
    Format.fprintf ppf "Unbox %a"
      (Field.Map.print (DS.print_unboxed_fields Variable.print))
      fields

type env =
  { uses : DS.result;
    code_deps : Traverse_acc.code_dep Code_id.Map.t;
    get_code_metadata : Code_id.t -> Code_metadata.t;
    (* TODO change names *)
    cont_params_to_keep : param_decision list Continuation.Map.t;
    should_keep_param : Continuation.t -> Variable.t -> KS.t -> param_decision;
    (* TODO same here *)
    function_params_to_keep : param_decision list Code_id.Map.t;
    should_keep_function_param :
      Code_id.t -> Variable.t -> KS.t -> param_decision;
    function_return_decision : param_decision list Code_id.Map.t;
    kinds : K.t Name.Map.t
  }

type rebuild_result =
  { all_slot_offsets : Slot_offsets.t;
    all_code : Code.t Code_id.Map.t
  }

let freshen_decisions = function
  | Delete -> Delete
  | Keep (v, kind) -> Keep (Variable.rename v, kind)
  | Unbox fields ->
    Unbox
      (Field.Map.map
         (DS.map_unboxed_fields (fun v -> Variable.rename v))
         fields)

let is_used (env : env) cn = DS.has_use env.uses cn

let is_code_id_used (env : env) code_id =
  is_used env (Code_id_or_name.code_id code_id)
  || not (Compilation_unit.is_current (Code_id.get_compilation_unit code_id))

let is_symbol_used (env : env) symbol =
  is_used env (Code_id_or_name.symbol symbol)
  || not (Compilation_unit.is_current (Symbol.compilation_unit symbol))

let raw_is_var_used uses var kind =
  match (kind : K.t) with
  | Region | Rec_info -> true
  | Value | Naked_number _ -> DS.has_use uses (Code_id_or_name.var var)

let is_var_used (env : env) var =
  raw_is_var_used env.uses var (Name.Map.find (Name.var var) env.kinds)

let is_name_used (env : env) name =
  Name.pattern_match name ~symbol:(is_symbol_used env) ~var:(is_var_used env)

(* XXX so which is it? *)
let poison_value = 0 (* 123456789 *)

let poison kind = Simple.const_int_of_kind kind poison_value

let rec fold_unboxed_with_kind (f : K.t -> 'a -> 'b -> 'b)
    (fields : 'a DS.unboxed_fields Field.Map.t) acc =
  Field.Map.fold
    (fun field elt acc ->
      match (elt : _ DS.unboxed_fields) with
      | Not_unboxed elt -> f (Field.kind field) elt acc
      | Unboxed fields -> fold_unboxed_with_kind f fields acc)
    fields acc

(* This is not symmetrical!! [fields1] must define a subset of [fields2], but
   does not have to define all of them. *)
let rec fold2_unboxed_subset (f : 'a -> 'b -> 'c -> 'c)
    (fields1 : 'a DS.unboxed_fields) (fields2 : 'b DS.unboxed_fields) acc =
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

let rec fold2_unboxed_subset_with_kind (f : K.t -> 'a -> 'b -> 'c -> 'c)
    (fields1 : 'a DS.unboxed_fields Field.Map.t)
    (fields2 : 'b DS.unboxed_fields Field.Map.t) acc =
  Field.Map.fold
    (fun field f1 acc ->
      let f2 = Field.Map.find field fields2 in
      match (f1, f2 : _ DS.unboxed_fields * _ DS.unboxed_fields) with
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
        (DS.get_unboxed_fields env.uses (Code_id_or_name.name name)))
    simple

let get_simple_unboxable env simple =
  Simple.pattern_match
    ~const:(fun _ -> assert false)
    ~name:(fun name ~coercion:_ ->
      Option.get (DS.get_unboxed_fields env.uses (Code_id_or_name.name name)))
    simple

let simple_changed_repr env simple =
  Simple.pattern_match
    ~const:(fun _ -> false)
    ~name:(fun name ~coercion:_ ->
      Option.is_some
        (DS.get_changed_representation env.uses (Code_id_or_name.name name)))
    simple

let get_simple_changed_repr env simple =
  Simple.pattern_match
    ~const:(fun _ -> assert false)
    ~name:(fun name ~coercion:_ ->
      Option.get
        (DS.get_changed_representation env.uses (Code_id_or_name.name name)))
    simple

let get_parameters params_decisions =
  List.fold_left
    (fun acc param_decision ->
      match param_decision with
      | Delete -> acc
      | Keep (var, kind) -> Bound_parameter.create var kind :: acc
      | Unbox fields ->
        fold_unboxed_with_kind
          (fun kind v acc -> Bound_parameter.create v (KS.anything kind) :: acc)
          fields acc)
    [] params_decisions
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
            (fun kind _ acc -> KS.anything kind :: acc)
            fields acc)
      [] params_decisions
    |> List.rev
  in
  Flambda_arity.(
    create
      [ Unboxed_product
          (List.map (fun k -> Component_for_creation.Singleton k) arity) ])

let is_dead_var env v =
  let (kind : K.t) = Name.Map.find (Name.var v) env.kinds in
  match kind with
  | Region | Rec_info -> false
  | Value | Naked_number _ ->
    not (DS.has_source env.uses (Code_id_or_name.var v))

type change_calling_convention =
  | Not_changing_calling_convention
  | Changing_calling_convention of Code_id.t

let bind_fields fields arg_fields hole =
  fold2_unboxed_subset
    (fun var arg hole ->
      let bp =
        Bound_pattern.singleton (Bound_var.create var Name_mode.normal)
      in
      RE.create_let bp (Named.create_simple (Simple.var arg)) ~body:hole)
    fields arg_fields hole

let bound_vars_will_be_unboxed env bvs =
  List.exists
    (fun bv ->
      Option.is_some
        (DS.get_unboxed_fields env.uses
           (Code_id_or_name.var (Bound_var.var bv))))
    bvs

let bound_vars_will_have_their_representation_changed env bvs =
  List.exists
    (fun bv ->
      Option.is_some
        (DS.get_changed_representation env.uses
           (Code_id_or_name.var (Bound_var.var bv))))
    bvs

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

let get_simple_kind env simple =
  Simple.pattern_match'
    ~const:(fun const -> Reg_width_const.kind const)
    ~symbol:(fun _ ~coercion:_ -> K.value)
    ~var:(fun var ~coercion:_ -> Name.Map.find (Name.var var) env.kinds)
    simple

let rewrite_simple (env : env) simple =
  Simple.pattern_match simple
    ~name:(fun name ~coercion:_ ->
      if not
           (Option.is_none
              (DS.get_unboxed_fields env.uses (Code_id_or_name.name name)))
      then simple (* XXX Misc.fatal_errorf "UNBOXED?? %a@." Name.print name; *)
      else if is_name_used env name
      then simple
      else
        let kind =
          match Name.Map.find_opt name env.kinds with
          | Some k -> k
          | None ->
            if Name.is_symbol name
            then K.value
            else Misc.fatal_errorf "Unbound name %a" Name.print name
        in
        poison kind)
    ~const:(fun _ -> simple)

let rewrite_simple_opt (env : env) = function
  | None -> None
  | Some simple as simpl ->
    Simple.pattern_match simple
      ~name:(fun name ~coercion:_ ->
        if is_name_used env name then simpl else None)
      ~const:(fun _ -> simpl)

let get_args env params_decisions args =
  List.fold_left2
    (fun acc arg param_decision ->
      match param_decision with
      | Delete -> acc
      | Keep _ -> rewrite_simple env arg :: acc
      | Unbox fields ->
        let arg_fields = get_simple_unboxable env arg in
        fold2_unboxed_subset_with_kind
          (fun _kind _param arg_field acc -> Simple.var arg_field :: acc)
          fields arg_fields acc)
    [] args params_decisions
  |> List.rev

let get_args_with_kinds env params_decisions args =
  List.fold_left2
    (fun acc arg param_decision ->
      match param_decision with
      | Delete -> acc
      | Keep (_, kind) -> (rewrite_simple env arg, kind) :: acc
      | Unbox fields ->
        let arg_fields = get_simple_unboxable env arg in
        fold2_unboxed_subset_with_kind
          (fun kind _param arg_field acc ->
            (Simple.var arg_field, KS.anything kind) :: acc)
          fields arg_fields acc)
    [] args params_decisions
  |> List.rev

let rewrite_or_variable default env (or_variable : _ Or_variable.t) =
  (* CR ncourant: rewrite unboxed variables *)
  match or_variable with
  | Const _ -> or_variable
  | Var (v, _) ->
    if is_var_used env v then or_variable else Or_variable.Const default

let rewrite_or_variables default env or_variables =
  List.map (rewrite_or_variable default env) or_variables

let rewrite_simple_with_debuginfo env (simple : Simple.With_debuginfo.t) =
  Simple.With_debuginfo.create
    (rewrite_simple env (Simple.With_debuginfo.simple simple))
    (Simple.With_debuginfo.dbg simple)

let rewrite_simples_with_debuginfo env simples =
  List.map (rewrite_simple_with_debuginfo env) simples

let rewrite_set_of_closures env res ~(bound : Name.t list)
    ({ Rev_expr.function_decls; value_slots; alloc_mode } :
      Rev_expr.rev_set_of_closures) =
  let slot_is_used slot =
    List.exists
      (fun bound_name ->
        DS.field_used env.uses (Code_id_or_name.name bound_name) slot)
      bound
  in
  let code_is_used bound_name =
    DS.field_used env.uses (Code_id_or_name.name bound_name) Code_of_closure
  in
  let new_repr =
    match bound with
    | bound :: _ ->
      DS.get_changed_representation env.uses (Code_id_or_name.name bound)
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
            else Some (rewrite_simple env simple))
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
          (fun field (uf : _ DS.unboxed_fields) value_slots ->
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
                  (Unboxed (get_simple_unboxable env arg))
                  value_slots
              else
                match uf with
                | Not_unboxed ff ->
                  Value_slot.Map.add ff (rewrite_simple env arg) value_slots
                | Unboxed _ -> Misc.fatal_errorf "trying to unbox simple"))
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
          | Code_id { code_id; only_full_applications } ->
            if code_is_used bound_name
            then Code_id { code_id; only_full_applications }
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
  let res =
    { res with
      all_slot_offsets =
        Slot_offsets.add_set_of_closures res.all_slot_offsets ~is_phantom:false
          set_of_closures
    }
  in
  set_of_closures, res

let rewrite_static_const (env : env) (sc : SC.t) =
  match sc with
  | Set_of_closures _ ->
    (* Already rewritten *)
    sc
  | Block (tag, mut, shape, fields) ->
    SC.block tag mut shape (rewrite_simples_with_debuginfo env fields)
  | Boxed_float f -> SC.boxed_float (rewrite_or_variable Float.zero env f)
  | Boxed_float32 f -> SC.boxed_float32 (rewrite_or_variable Float32.zero env f)
  | Boxed_int32 n -> SC.boxed_int32 (rewrite_or_variable Int32.zero env n)
  | Boxed_int64 n -> SC.boxed_int64 (rewrite_or_variable Int64.zero env n)
  | Boxed_nativeint n ->
    SC.boxed_nativeint (rewrite_or_variable Targetint_32_64.zero env n)
  | Boxed_vec128 n ->
    SC.boxed_vec128
      (rewrite_or_variable Vector_types.Vec128.Bit_pattern.zero env n)
  | Immutable_float_block fields ->
    SC.immutable_float_block (rewrite_or_variables Float.zero env fields)
  | Immutable_float_array fields ->
    SC.immutable_float_array (rewrite_or_variables Float.zero env fields)
  | Immutable_float32_array fields ->
    SC.immutable_float32_array (rewrite_or_variables Float32.zero env fields)
  | Immutable_value_array fields ->
    SC.immutable_value_array (rewrite_simples_with_debuginfo env fields)
  | Immutable_int32_array fields ->
    SC.immutable_int32_array (rewrite_or_variables Int32.zero env fields)
  | Immutable_int64_array fields ->
    SC.immutable_int64_array (rewrite_or_variables Int64.zero env fields)
  | Immutable_nativeint_array fields ->
    SC.immutable_nativeint_array
      (rewrite_or_variables Targetint_32_64.zero env fields)
  | Immutable_vec128_array fields ->
    SC.immutable_vec128_array
      (rewrite_or_variables Vector_types.Vec128.Bit_pattern.zero env fields)
  | Empty_array _ | Mutable_string _ | Immutable_string _ -> sc

let rewrite_static_const_or_code env (sc : Static_const_or_code.t) =
  match sc with
  | Code _ -> sc
  | Deleted_code -> sc
  | Static_const sc ->
    Static_const_or_code.create_static_const (rewrite_static_const env sc)

let rewrite_static_const_group env (group : Static_const_group.t) =
  Static_const_group.map ~f:(rewrite_static_const_or_code env) group

let rebuild_named_default_case env (named : Named.t) =
  let[@local] rewrite_field_access arg field =
    let arg = get_simple_unboxable env arg in
    let var = Field.Map.find field arg in
    let var =
      match var with
      | Not_unboxed var -> var
      | Unboxed _ -> Misc.fatal_errorf "Trying to bind non-unboxed to unboxed"
    in
    Named.create_simple (Simple.var var)
  in
  let[@local] rewrite_field_access_chg_repr arg field dbg =
    let arg_repr = get_simple_changed_repr env arg in
    match arg_repr with
    | Block_representation (arg_fields, _size) -> (
      let f = Field.Map.find field arg_fields in
      match f with
      | Unboxed _ -> Misc.fatal_errorf "Trying to bind non-unboxed to unboxed"
      | Not_unboxed (field, kind) ->
        Named.create_prim
          (P.Unary
             ( Block_load
                 { field = Targetint_31_63.of_int field; kind; mut = Immutable },
               arg ))
          dbg)
    | Closure_representation (arg_fields, function_slots, current_function_slot)
      -> (
      let f = Field.Map.find field arg_fields in
      match f with
      | Unboxed _ -> Misc.fatal_errorf "Trying to bind non-unboxed to unboxed"
      | Not_unboxed value_slot ->
        Named.create_prim
          (P.Unary
             ( Project_value_slot
                 { value_slot;
                   project_from =
                     Function_slot.Map.find current_function_slot function_slots
                 },
               arg ))
          dbg)
  in
  match[@ocaml.warning "-fragile-match"] named with
  | Simple simple -> Named.create_simple (rewrite_simple env simple)
  | Prim (Unary (Block_load { kind; field; _ }, arg), _dbg)
    when simple_is_unboxable env arg ->
    let kind = P.Block_access_kind.element_kind_for_load kind in
    let field = GFG.Field.Block (Targetint_31_63.to_int field, kind) in
    rewrite_field_access arg field
  | Prim (Unary (Project_value_slot { value_slot; _ }, arg), _dbg)
    when simple_is_unboxable env arg ->
    rewrite_field_access arg (GFG.Field.Value_slot value_slot)
  | Prim (Unary (Is_int { variant_only = true }, arg), _dbg)
    when simple_is_unboxable env arg ->
    rewrite_field_access arg GFG.Field.Is_int
  | Prim (Unary (Get_tag, arg), _dbg) when simple_is_unboxable env arg ->
    rewrite_field_access arg GFG.Field.Get_tag
  | Prim (Unary (Block_load { kind; field; _ }, arg), dbg)
    when simple_changed_repr env arg ->
    let kind = P.Block_access_kind.element_kind_for_load kind in
    let field = GFG.Field.Block (Targetint_31_63.to_int field, kind) in
    rewrite_field_access_chg_repr arg field dbg
  | Prim (Unary (Project_value_slot { value_slot; _ }, arg), dbg)
    when simple_changed_repr env arg ->
    rewrite_field_access_chg_repr arg (GFG.Field.Value_slot value_slot) dbg
  | Prim (Unary (Is_int { variant_only = true }, arg), dbg)
    when simple_changed_repr env arg ->
    rewrite_field_access_chg_repr arg GFG.Field.Is_int dbg
  | Prim (Unary (Get_tag, arg), dbg) when simple_changed_repr env arg ->
    rewrite_field_access_chg_repr arg GFG.Field.Get_tag dbg
  | Prim (prim, dbg) ->
    let prim = P.map_args (rewrite_simple env) prim in
    Named.create_prim prim dbg
  | Set_of_closures s -> Named.create_set_of_closures s (* Already rewritten *)
  | Static_consts sc ->
    Named.create_static_consts (rewrite_static_const_group env sc)
  | Rec_info r -> Named.create_rec_info r

let rewrite_apply_cont_expr env ac =
  let cont = Apply_cont_expr.continuation ac in
  let args = Apply_cont_expr.args ac in
  if List.exists
       (fun arg ->
         Simple.pattern_match arg
           ~name:(fun name ~coercion:_ ->
             (not (DS.has_source env.uses (Code_id_or_name.name name)))
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
      get_args env args_to_keep args
    in
    Some (Apply_cont_expr.with_continuation_and_args ac cont ~args)

let make_apply_wrapper env
    (make_apply : continuation:Apply_expr.Result_continuation.t -> Apply_expr.t)
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
    let rev_args_or_invalid =
      (* TODO if the decisions are equal, don't introduce the wrapper. Not
         really important but this will be simpler for debugging *)
      List.fold_left2
        (fun (rev_args_or_invalid : _ Or_invalid.t) apply_decision func_decision
             : _ Or_invalid.t ->
          match rev_args_or_invalid with
          | Invalid -> Invalid
          | Ok (i, rev_args) -> (
            match apply_decision, func_decision with
            | Unbox _, (Keep _ | Delete) | (Keep _ | Delete), Unbox _ ->
              let[@inline] error () =
                Misc.fatal_errorf
                  "Inconsistent apply (%a) and func (%a) decisions:@ %a@."
                  print_param_decision apply_decision print_param_decision
                  func_decision Apply.print apply
              in
              let direct_or_indirect =
                match Apply.call_kind apply with
                | Function { function_call = Direct _; _ } -> error ()
                | Function { function_call = Indirect_known_arity; _ } ->
                  GFG.Direct_code_pointer
                | Function { function_call = Indirect_unknown_arity; _ }
                | C_call _ | Method _ | Effect _ ->
                  GFG.Indirect_code_pointer
              in
              let field =
                GFG.Field.Apply (direct_or_indirect, GFG.Field.Normal i)
              in
              let has_any_source =
                DS.not_local_field_has_source env.uses
                  (Simple.pattern_match
                     (Option.get (Apply.callee apply))
                     ~name:(fun name ~coercion:_ -> Code_id_or_name.name name)
                     ~const:(fun _ -> assert false))
                  field
              in
              if has_any_source then error () else Invalid
            | Delete, _ -> Ok (i + 1, rev_args)
            | Keep (_, _), Keep (v, _) -> Ok (i + 1, Simple.var v :: rev_args)
            | Keep (_, kind), Delete ->
              Ok (i + 1, poison (KS.kind kind) :: rev_args)
            | Unbox fields_apply, Unbox fields_func ->
              Ok
                ( i + 1,
                  fold2_unboxed_subset_with_kind
                    (fun _kind _var_apply var_func rev_args ->
                      Simple.var var_func :: rev_args)
                    fields_apply fields_func rev_args )))
        (Or_invalid.Ok (0, []))
        apply_decisions return_decisions
    in
    let cont_handler =
      let return_parameters = get_parameters return_decisions in
      let handler =
        match rev_args_or_invalid with
        | Ok (_, rev_args) ->
          let args = List.rev rev_args in
          let apply_cont =
            Apply_cont_expr.create return_cont ~args ~dbg:(Apply.dbg apply)
          in
          RE.from_expr
            ~expr:(Expr.create_apply_cont apply_cont)
            ~free_names:(Apply_cont_expr.free_names apply_cont)
        | Invalid ->
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
    RE.create_non_recursive_let_cont return_cont_wrapper cont_handler ~body

let rewrite_call_kind env (call_kind : Call_kind.t) =
  let rewrite_simple = rewrite_simple env in
  match call_kind with
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

let decide_whether_apply_needs_calling_convention_change env apply =
  let call_kind = rewrite_call_kind env (Apply.call_kind apply) in
  let code_id_actually_called, call_kind, _should_break_call =
    let called c alloc_mode call_kind was_indirect_unknown_arity =
      let code_id =
        Simple.pattern_match c
          ~const:(fun _ -> None)
          ~name:(fun name ~coercion:_ ->
            DS.code_id_actually_called env.uses name)
      in
      match code_id with
      | None -> None, call_kind, false
      | Some (code_id, num_already_applied_params) ->
        if num_already_applied_params <> 0 then failwith "todo";
        (* XXX *)
        let new_call_kind = Call_kind.direct_function_call code_id alloc_mode in
        Some code_id, new_call_kind, was_indirect_unknown_arity
    in
    match call_kind with
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
          if DS.has_use env.uses (Code_id_or_name.code_id code_id)
          then call_kind
          else Call_kind.indirect_function_call_known_arity alloc_mode
        in
        called c alloc_mode call_kind false
      | None | Some _ -> Some code_id, call_kind, false)
    | Function { function_call = Indirect_unknown_arity; alloc_mode = _ } ->
      (* called (Option.get (Apply.callee apply)) alloc_mode call_kind true *)
      None, call_kind, false
    | Function { function_call = Indirect_known_arity; alloc_mode } ->
      called (Option.get (Apply.callee apply)) alloc_mode call_kind false
    | C_call _ | Method _ | Effect _ -> None, call_kind, false
  in
  match code_id_actually_called with
  | None -> Not_changing_calling_convention, call_kind
  | Some code_id -> (
    (* Format.eprintf "CODE ID %a: %a@." (Format.pp_print_option Simple.print)
       (Apply.callee apply) Code_id.print code_id; *)
    match Code_id.Map.find_opt code_id env.code_deps with
    | None -> Not_changing_calling_convention, call_kind
    | Some _ ->
      let cannot_change_calling_convention =
        DS.cannot_change_calling_convention env.uses code_id
      in
      if cannot_change_calling_convention
      then Not_changing_calling_convention, call_kind
      else Changing_calling_convention code_id, call_kind)

let rebuild_apply env apply =
  (* CR ncourant: we never rewrite alloc_mode. This is currently ok because we
     never remove begin- or end-region primitives, but might be needed later if
     we chose to handle them. *)
  let updating_calling_convention, call_kind =
    decide_whether_apply_needs_calling_convention_change env apply
  in
  let exn_continuation = Apply.exn_continuation apply in
  let exn_continuation =
    let exn_handler = Exn_continuation.exn_handler exn_continuation in
    let extra_args =
      let selected_extra_args =
        let extra_args = Exn_continuation.extra_args exn_continuation in
        (* try *)
        let args_to_keep =
          Continuation.Map.find exn_handler env.cont_params_to_keep |> List.tl
          (* This contains the exn argument that is not part of the extra
             args *)
        in
        get_args_with_kinds env args_to_keep (List.map fst extra_args)
        (* with Not_found -> (* Not defined in cont_params_to_keep *)
           extra_args *)
      in
      (* List.map (fun (simple, kind) -> rewrite_simple env simple, kind) *)
      selected_extra_args
    in
    Exn_continuation.create ~exn_handler ~extra_args
  in
  (* TODO rewrite arities *)
  (* XXX mshinwell: does this "rewrite arities" need to be done now? *)
  match updating_calling_convention with
  | Not_changing_calling_convention ->
    (* Format.eprintf "NOT CHANGING CALLING CONVENTION %a@." Apply.print
       apply; *)
    let args = List.map (rewrite_simple env) (Apply.args apply) in
    let args_arity = Apply.args_arity apply in
    let return_arity = Apply.return_arity apply in
    let make_apply =
      Apply.create
      (* Note here that callee is rewritten with [rewrite_simple_opt], which
         will put [None] as the callee instead of a dummy value, as a dummy
         value would then be further used in a later simplify pass to refine the
         call kind and produce an invalid. *)
        ~callee:(rewrite_simple_opt env (Apply.callee apply))
        exn_continuation ~args ~args_arity ~return_arity ~call_kind
        (Apply.dbg apply) ~inlined:(Apply.inlined apply)
        ~inlining_state:(Apply.inlining_state apply)
        ~probe:(Apply.probe apply) ~position:(Apply.position apply)
        ~relative_history:(Apply.relative_history apply)
    in
    let func_decisions =
      List.map
        (fun kind -> Keep (Variable.create "function_return", kind))
        (Flambda_arity.unarized_components return_arity)
    in
    make_apply_wrapper env make_apply (Apply.continuation apply) func_decisions
  | Changing_calling_convention code_id ->
    (* Format.eprintf "CHANGING CALLING CONVENTION %a %a@." Code_id.print
       code_id Apply.print apply; *)
    let args_from_unboxed_callee, callee =
      match Apply.callee apply with
      | Some callee when simple_is_unboxable env callee ->
        let fields = get_simple_unboxable env callee in
        let new_args =
          fold_unboxed_with_kind
            (fun kind v acc -> (Simple.var v, KS.anything kind) :: acc)
            fields []
        in
        new_args, None
      | (None | Some _) as callee ->
        ( [],
          (* Note here that callee is rewritten with [rewrite_simple_opt], which
             will put [None] as the callee instead of a dummy value, as a dummy
             value would then be further used in a later simplify pass to refine
             the call kind and produce an invalid. *)
          rewrite_simple_opt env callee )
    in
    let params_decisions =
      match Code_id.Map.find_opt code_id env.function_params_to_keep with
      | None -> assert false
      | Some p -> p
    in
    let params_decisions =
      Flambda_arity.group_by_parameter (Apply.args_arity apply) params_decisions
    in
    let args =
      Flambda_arity.group_by_parameter (Apply.args_arity apply)
        (Apply.args apply)
    in
    let args = List.map2 (get_args_with_kinds env) params_decisions args in
    let args =
      match args with
      | [] -> assert false
      | first :: rest -> (args_from_unboxed_callee @ first) :: rest
    in
    let args_arity =
      let components_for args =
        Flambda_arity.Component_for_creation.Unboxed_product
          (List.map
             (fun (_, k) -> Flambda_arity.Component_for_creation.Singleton k)
             args)
      in
      Flambda_arity.create (List.map components_for args)
    in
    let return_decisions =
      Code_id.Map.find code_id env.function_return_decision
    in
    let return_arity = Flambda_arity.unarize_t (get_arity return_decisions) in
    let args = List.map fst (List.flatten args) in
    let make_apply ~continuation =
      Apply.create ~callee ~continuation exn_continuation ~args ~args_arity
        ~return_arity ~call_kind (Apply.dbg apply)
        ~inlined:(Apply.inlined apply)
        ~inlining_state:(Apply.inlining_state apply)
        ~probe:(Apply.probe apply) ~position:(Apply.position apply)
        ~relative_history:(Apply.relative_history apply)
    in
    make_apply_wrapper env make_apply (Apply.continuation apply)
      return_decisions

let load_field_from_value_which_is_being_unboxed env ~to_bind field arg dbg
    ~hole =
  let oarg = arg in
  let arg =
    Simple.pattern_match arg
      ~const:(fun _ -> Misc.fatal_error "Loading unboxed from constant")
      ~name:(fun name ~coercion:_ -> name)
  in
  let arg = Code_id_or_name.name arg in
  match DS.get_unboxed_fields env.uses arg with
  | Some arg -> bind_fields (Unboxed to_bind) (Field.Map.find field arg) hole
  | None -> (
    assert (Option.is_some (DS.get_changed_representation env.uses arg));
    let arg = Option.get (DS.get_changed_representation env.uses arg) in
    match arg with
    | Block_representation (arg_fields, _size) ->
      let arg = Field.Map.find field arg_fields in
      fold2_unboxed_subset
        (fun var (field, kind) hole ->
          let bp =
            Bound_pattern.singleton (Bound_var.create var Name_mode.normal)
          in
          let named =
            Named.create_prim
              (P.Unary
                 ( Block_load
                     { field = Targetint_31_63.of_int field;
                       kind;
                       mut = Immutable
                     },
                   oarg ))
              dbg
          in
          RE.create_let bp named ~body:hole)
        (Unboxed to_bind) arg hole
    | Closure_representation (arg_fields, function_slots, current_function_slot)
      ->
      let arg = Field.Map.find field arg_fields in
      fold2_unboxed_subset
        (fun var value_slot hole ->
          let bp =
            Bound_pattern.singleton (Bound_var.create var Name_mode.normal)
          in
          let named =
            Named.create_prim
              (P.Unary
                 ( Project_value_slot
                     { value_slot;
                       project_from =
                         Function_slot.Map.find current_function_slot
                           function_slots
                     },
                   oarg ))
              dbg
          in
          RE.create_let bp named ~body:hole)
        (Unboxed to_bind) arg hole)

let rebuild_singleton_binding_which_is_being_unboxed env bv
    ~(defining_expr : Rev_expr.rev_named) ~hole =
  let to_bind =
    Option.get
      (DS.get_unboxed_fields env.uses (Code_id_or_name.var (Bound_var.var bv)))
  in
  match[@ocaml.warning "-fragile-match"] defining_expr with
  | Named named -> (
    match[@ocaml.warning "-fragile-match"] named with
    | Prim (Variadic (Make_block (kind, _, _), args), _dbg) ->
      Field.Map.fold
        (fun (field : GFG.Field.t) (var : _ DS.unboxed_fields) hole ->
          let arg : _ Either.t =
            match field with
            | Block (nth, field_kind) ->
              let arg =
                if nth < List.length args
                then
                  let arg = List.nth args nth in
                  if K.equal field_kind (get_simple_kind env arg)
                  then arg
                  else poison field_kind
                else poison field_kind
              in
              if simple_is_unboxable env arg
              then Right (get_simple_unboxable env arg)
              else Left arg
            | Is_int -> Left Simple.untagged_const_false
            | Get_tag ->
              let tag, _ = P.Block_kind.to_shape kind in
              Left (Simple.untagged_const_int (Tag.to_targetint_31_63 tag))
            | Value_slot _ | Function_slot _ | Code_of_closure | Apply _
            | Code_id_of_call_witness _ ->
              assert false
          in
          match arg with
          | Left simple ->
            let var =
              match var with
              | Not_unboxed var -> var
              | Unboxed _ -> Misc.fatal_errorf "Trying to unbox non-unboxable"
            in
            let bp =
              Bound_pattern.singleton (Bound_var.create var Name_mode.normal)
            in
            RE.create_let bp (Named.create_simple simple) ~body:hole
          | Right arg_fields -> bind_fields var (Unboxed arg_fields) hole)
        to_bind hole
    (* | Prim ( Unary (Opaque_identity { middle_end_only = true; _ }, arg), _dbg
       ) -> (* XXX TO REMOVE *) bind_fields (DS.Unboxed to_bind) (DS.Unboxed
       (get_simple_unboxable env arg)) hole *)
    | Prim (Unary (Block_load { field; kind; _ }, arg), dbg) ->
      let field =
        Field.Block
          ( Targetint_31_63.to_int field,
            P.Block_access_kind.element_kind_for_load kind )
      in
      load_field_from_value_which_is_being_unboxed env ~to_bind field arg dbg
        ~hole
    | Prim
        (Unary (Project_value_slot { value_slot; project_from = _ }, arg), dbg)
      ->
      let field = Field.Value_slot value_slot in
      load_field_from_value_which_is_being_unboxed env ~to_bind field arg dbg
        ~hole
    | Prim (Unary (Project_function_slot _, arg), _) | Simple arg ->
      bind_fields (Unboxed to_bind)
        (Unboxed (get_simple_unboxable env arg))
        hole
    | named ->
      Format.printf "BOUM ? %a@." Named.print named;
      assert false)
  | _ -> assert false

let rebuild_set_of_closures_binding_which_is_being_unboxed env bvs
    ~(defining_expr : Rev_expr.rev_named) ~hole =
  assert (
    List.for_all
      (fun bv ->
        (not (DS.has_use env.uses (Code_id_or_name.var (Bound_var.var bv))))
        || Option.is_some
             (DS.get_unboxed_fields env.uses
                (Code_id_or_name.var (Bound_var.var bv))))
      bvs);
  List.fold_left
    (fun hole bv ->
      if not (DS.has_use env.uses (Code_id_or_name.var (Bound_var.var bv)))
      then hole
      else
        let to_bind =
          Option.get
            (DS.get_unboxed_fields env.uses
               (Code_id_or_name.var (Bound_var.var bv)))
        in
        let value_slots =
          match[@ocaml.warning "-fragile-match"] defining_expr with
          | Named (Set_of_closures _set) ->
            (* Possible ? *)
            assert false
            (* Set_of_closures.value_slots set *)
          | Set_of_closures set -> set.value_slots
          | _ -> assert false
        in
        Field.Map.fold
          (fun (field : GFG.Field.t) (var : _ DS.unboxed_fields) hole ->
            match field with
            | Value_slot value_slot ->
              let arg = Value_slot.Map.find value_slot value_slots in
              if simple_is_unboxable env arg
              then bind_fields var (Unboxed (get_simple_unboxable env arg)) hole
              else
                let var =
                  match var with
                  | Not_unboxed var -> var
                  | Unboxed _ ->
                    Misc.fatal_errorf "Trying to unbox non-unboxable"
                in
                let bp =
                  Bound_pattern.singleton
                    (Bound_var.create var Name_mode.normal)
                in
                RE.create_let bp (Named.create_simple arg) ~body:hole
            | Block _ | Is_int | Get_tag | Function_slot _ | Code_of_closure
            | Apply _ | Code_id_of_call_witness _ ->
              assert false)
          to_bind hole)
    hole bvs

let rebuild_singleton_binding_whose_representation_is_being_changed env bp bv
    ~(orig_defining_expr : Rev_expr.rev_named) ~(new_defining_expr : Named.t)
    ~hole =
  (* TODO when this block is stored anywhere else, the subkind is no longer
     correct... we need to fix that somehow *)
  match[@ocaml.warning "-fragile-match"] orig_defining_expr with
  | Named
      (Prim (Unary (Project_function_slot { move_from; move_to }, arg), dbg)) ->
    let fields =
      Option.get
        (DS.get_changed_representation env.uses
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
  | Named (Prim (Variadic (Make_block (kind, _mut, alloc_mode), args), dbg)) ->
    let fields =
      Option.get
        (DS.get_changed_representation env.uses
           (Code_id_or_name.var (Bound_var.var bv)))
    in
    let fields, size =
      match fields with
      | Block_representation (fields, size) -> fields, size
      | Closure_representation _ -> assert false
    in
    let mp =
      Field.Map.fold
        (fun f (uf : _ DS.unboxed_fields) mp ->
          match (f : Field.t) with
          | Block (i, _kind) -> (
            let arg = List.nth args i in
            if simple_is_unboxable env arg
            then
              fold2_unboxed_subset
                (fun (ff, _) var mp -> Int.Map.add ff (Simple.var var) mp)
                uf
                (Unboxed (get_simple_unboxable env arg))
                mp
            else
              match uf with
              | Not_unboxed (ff, _) ->
                Int.Map.add ff (rewrite_simple env arg) mp
              | Unboxed _ -> Misc.fatal_errorf "trying to unbox simple")
          | Get_tag -> (
            let tag =
              match kind with
              | Values (tag, _) | Mixed (tag, _) ->
                Tag.to_targetint_31_63 (Tag.Scannable.to_tag tag)
              | Naked_floats -> Tag.to_targetint_31_63 Tag.double_array_tag
            in
            match uf with
            | Not_unboxed (ff, _) ->
              Int.Map.add ff (rewrite_simple env (Simple.const_int tag)) mp
            | Unboxed _ -> Misc.fatal_errorf "trying to unbox simple")
          | Is_int -> (
            match uf with
            | Not_unboxed (ff, _) ->
              Int.Map.add ff (rewrite_simple env Simple.const_one) mp
            | Unboxed _ -> Misc.fatal_errorf "trying to unbox simple")
          | Value_slot _ | Function_slot _ | Code_of_closure | Apply _
          | Code_id_of_call_witness _ ->
            assert false)
        fields Int.Map.empty
    in
    let args =
      List.init size (fun i ->
          match Int.Map.find_opt i mp with
          | None -> Simple.const_zero
          | Some x -> x)
    in
    let named =
      Named.create_prim
        (P.Variadic
           ( Make_block
               ( P.Block_kind.Values
                   (Tag.Scannable.zero, List.map (fun _ -> KS.any_value) args),
                 Immutable,
                 alloc_mode ),
             args ))
        dbg
    in
    RE.create_let bp named ~body:hole
  | _ ->
    (* In a situation such as:
     *   x is a variable whose representation is being changed
     *   y = (x, 0)  (assume the representation of [y] is not changed)
     *   z = fst y
     * then [z] will be marked as having its representation changed, because
     * it is equal to [x].  However we don't need to rewrite the [fst y]
     * primitive, which brings us to this case. *)
    let defining_expr = rebuild_named_default_case env new_defining_expr in
    RE.create_let bp defining_expr ~body:hole

let rebuild_set_of_closures_binding_whose_representation_is_being_changed env
    res bp bvs ~(orig_defining_expr : Rev_expr.rev_named) ~hole =
  let bound = List.map (fun bv -> Name.var (Bound_var.var bv)) bvs in
  let set =
    match[@ocaml.warning "-fragile-match"] orig_defining_expr with
    | Named (Set_of_closures _set) ->
      (* Possible ? *)
      assert false
      (* Set_of_closures.value_slots set *)
    | Set_of_closures set -> set
    | _ -> assert false
  in
  let set_of_closures, res = rewrite_set_of_closures env res ~bound set in
  ( RE.create_let bp (Named.create_set_of_closures set_of_closures) ~body:hole,
    res )

let rebuild_make_block_default_case env (bp : Bound_pattern.t)
    ~(block_kind : P.Block_kind.t) ~mutability ~alloc_mode ~fields ~hole dbg =
  let bound_name =
    match bp with
    | Singleton v -> Name.var (Bound_var.var v)
    | Set_of_closures _ | Static _ -> assert false
  in
  let _tag, block_shape = P.Block_kind.to_shape block_kind in
  let block_kind =
    match block_kind with
    | Mixed _ | Naked_floats -> block_kind
    | Values (tag, subkinds) -> (
      let ks =
        KS.create K.value
          (Variant
             { consts = Targetint_31_63.Set.empty;
               non_consts =
                 Tag.Scannable.Map.singleton tag (block_shape, subkinds)
             })
          Non_nullable
      in
      let ks = DS.rewrite_kind_with_subkind env.uses bound_name ks in
      let[@local] with_subkinds subkinds =
        P.Block_kind.Values (tag, subkinds)
      in
      let[@local] default () =
        with_subkinds (List.map (fun ks -> KS.erase_subkind ks) subkinds)
      in
      match[@ocaml.warning "-fragile-match"] KS.non_null_value_subkind ks with
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
        let kind = K.Block_shape.element_kind block_shape i in
        let f = GFG.Field.Block (i, kind) in
        if DS.field_used env.uses bound_name f
        then rewrite_simple env field
        else poison kind)
      fields
  in
  RE.create_let bp
    (Named.create_prim
       (Variadic (Make_block (block_kind, mutability, alloc_mode), fields))
       dbg)
    ~body:hole

let rebuild_let_expr_holed0 (env : env) res ~(bound_pattern : Bound_pattern.t)
    ~(defining_expr : Rev_expr.rev_named) ~new_defining_expr ~hole :
    RE.t * rebuild_result =
  match (bound_pattern : Bound_pattern.t) with
  | Set_of_closures bvs when bound_vars_will_be_unboxed env bvs ->
    ( rebuild_set_of_closures_binding_which_is_being_unboxed env bvs
        ~defining_expr ~hole,
      res )
  | Singleton bv when bound_vars_will_be_unboxed env [bv] ->
    ( rebuild_singleton_binding_which_is_being_unboxed env bv ~defining_expr
        ~hole,
      res )
  | Singleton bv when bound_vars_will_have_their_representation_changed env [bv]
    ->
    ( rebuild_singleton_binding_whose_representation_is_being_changed env
        bound_pattern bv ~orig_defining_expr:defining_expr ~new_defining_expr
        ~hole,
      res )
  | Set_of_closures bvs
    when bound_vars_will_have_their_representation_changed env bvs ->
    rebuild_set_of_closures_binding_whose_representation_is_being_changed env
      res bound_pattern bvs ~orig_defining_expr:defining_expr ~hole
  | Singleton _ | Set_of_closures _ | Static _ -> (
    match[@ocaml.warning "-fragile-match"] new_defining_expr with
    | Flambda.Prim
        (Variadic (Make_block (block_kind, mutability, alloc_mode), fields), dbg)
      ->
      ( rebuild_make_block_default_case env bound_pattern ~block_kind
          ~mutability ~alloc_mode ~fields ~hole dbg,
        res )
    | _ ->
      let defining_expr = rebuild_named_default_case env new_defining_expr in
      RE.create_let bound_pattern defining_expr ~body:hole, res)

let rec default_defining_expr_for_rebuilding_let env res
    (bound_pattern : Bound_pattern.t) (defining_expr : Rev_expr.rev_named) =
  match defining_expr with
  | Named defining_expr -> bound_pattern, defining_expr, res
  | Static_consts group ->
    let bound_static =
      match bound_pattern with
      | Static l -> l
      | Set_of_closures _ | Singleton _ ->
        (* Bound pattern is static consts, so can't bind something else *)
        assert false
    in
    let bound_and_group =
      List.filter_map
        (fun ((p, e) as arg :
               Bound_static.Pattern.t * Rev_expr.rev_static_const_or_code) ->
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
              Some (p, Rev_expr.Deleted_code))
          | Block_like sym -> if is_symbol_used env sym then Some arg else None
          | Set_of_closures m ->
            if Function_slot.Lmap.exists
                 (fun _ sym ->
                   assert (
                     not
                       (Option.is_some
                          (DS.get_changed_representation env.uses
                             (Code_id_or_name.symbol sym))));
                   is_symbol_used env sym)
                 m
            then Some arg
            else None)
        (List.combine (Bound_static.to_list bound_static) group)
    in
    let bound_static, _group = List.split bound_and_group in
    let res, group_members =
      List.fold_left_map
        (fun res pat_and_rev ->
          let static_const_or_code, res =
            rebuild_static_const_or_code env res pat_and_rev
          in
          res, static_const_or_code)
        res bound_and_group
    in
    let group = Static_const_group.create group_members in
    ( Bound_pattern.static (Bound_static.create bound_static),
      Named.create_static_consts group,
      res )
  | Set_of_closures set_of_closures ->
    let bound =
      match bound_pattern with
      | Set_of_closures bound_vars ->
        List.map Name.var (List.map Bound_var.var bound_vars)
      | Static _ | Singleton _ ->
        (* Pattern is a set of closures *)
        assert false
    in
    let set_of_closures, res =
      rewrite_set_of_closures env res ~bound set_of_closures
    in
    let is_phantom =
      Name_mode.is_phantom (Bound_pattern.name_mode bound_pattern)
    in
    let res =
      { res with
        all_slot_offsets =
          Slot_offsets.add_set_of_closures res.all_slot_offsets ~is_phantom
            set_of_closures
      }
    in
    bound_pattern, Named.create_set_of_closures set_of_closures, res

and rebuild_let_expr_holed (env : env) res ~(bound_pattern : Bound_pattern.t)
    ~(defining_expr : Rev_expr.rev_named) ~parent ~hole : RE.t * rebuild_result
    =
  let bound_pattern, new_defining_expr, res =
    default_defining_expr_for_rebuilding_let env res bound_pattern defining_expr
  in
  let subexpr, res =
    match (bound_pattern : Bound_pattern.t) with
    | Set_of_closures _ | Static _ ->
      rebuild_let_expr_holed0 env res ~bound_pattern ~defining_expr
        ~new_defining_expr ~hole
    | Singleton v ->
      let v = Bound_var.var v in
      (* CR ncourant: we should probably properly track regions *)
      let is_begin_region =
        match defining_expr with
        | Named (Prim (prim, _)) -> P.is_begin_region prim
        | Named (Simple _ | Set_of_closures _ | Static_consts _ | Rec_info _)
        | Set_of_closures _ | Static_consts _ ->
          false
      in
      if is_begin_region || is_var_used env v
      then
        rebuild_let_expr_holed0 env res ~bound_pattern ~defining_expr
          ~new_defining_expr ~hole
      else hole, res
  in
  rebuild_holed env res parent subexpr

and rebuild_holed (env : env) res (rev_expr : Rev_expr.rev_expr_holed)
    (hole : RE.t) : RE.t * rebuild_result =
  match rev_expr with
  | Hole -> hole, res
  | Let { bound_pattern; defining_expr; parent } ->
    rebuild_let_expr_holed env res ~bound_pattern ~defining_expr ~parent ~hole
  | Let_cont { cont; parent; handler } ->
    if not (Name_occurrences.mem_continuation hole.free_names cont)
    then rebuild_holed env res parent hole
    else
      let { Rev_expr.bound_parameters; expr; is_exn_handler; is_cold } =
        handler
      in
      let parameters_to_keep =
        Continuation.Map.find cont env.cont_params_to_keep
      in
      let cont_handler, res =
        let handler, res =
          match
            List.filter (is_dead_var env)
              (Bound_parameters.vars bound_parameters)
          with
          | [] -> rebuild_expr env res expr
          | _ :: _ as dead_vars ->
            let msg =
              Format.asprintf
                "Continuation is never called because of dead parameters: [%a]."
                (Format.pp_print_list
                   ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
                   Variable.print)
                dead_vars
            in
            ( RE.from_expr
                ~expr:(Expr.create_invalid (Message msg))
                ~free_names:Name_occurrences.empty,
              res )
        in
        let l = get_parameters parameters_to_keep in
        let l =
          List.concat_map
            (fun param ->
              let v = Bound_parameter.var param in
              match DS.get_unboxed_fields env.uses (Code_id_or_name.var v) with
              | None -> [param]
              | Some fields ->
                fold_unboxed_with_kind
                  (fun kind v acc ->
                    Bound_parameter.create v (KS.anything kind) :: acc)
                  fields [])
            l
        in
        ( RE.create_continuation_handler
            (Bound_parameters.create l)
            ~handler ~is_exn_handler ~is_cold,
          res )
      in
      let let_cont_expr =
        RE.create_non_recursive_let_cont cont cont_handler ~body:hole
      in
      rebuild_holed env res parent let_cont_expr
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
    let res, handlers =
      Continuation.Lmap.fold_left_map
        (fun res cont handler ->
          let { Rev_expr.bound_parameters; expr; is_exn_handler; is_cold } =
            handler
          in
          let bound_parameters = filter_params cont bound_parameters in
          let handler, res = rebuild_expr env res expr in
          let cont_handler =
            RE.create_continuation_handler bound_parameters ~handler
              ~is_exn_handler ~is_cold
          in
          res, cont_handler)
        res handlers
    in
    let invariant_params =
      filter_params (fst (Continuation.Lmap.choose handlers)) invariant_params
    in
    let let_cont_expr =
      RE.create_recursive_let_cont ~invariant_params handlers ~body:hole
    in
    rebuild_holed env res parent let_cont_expr

and rebuild_expr (env : env) (res : rebuild_result)
    (rev_expr : Rev_expr.rev_expr) : RE.t * rebuild_result =
  let { Rev_expr.expr; holed_expr } = rev_expr in
  let expr =
    match expr with
    | Invalid { message } ->
      RE.from_expr
        ~expr:(Expr.create_invalid (Message message))
        ~free_names:Name_occurrences.empty
    | Apply_cont ac -> (
      match rewrite_apply_cont_expr env ac with
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
          (fun _ -> rewrite_apply_cont_expr env)
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
            ~scrutinee:(rewrite_simple env (Switch_expr.scrutinee switch))
            ~arms
        in
        let expr = Expr.create_switch switch in
        let free_names = Switch_expr.free_names switch in
        RE.from_expr ~expr ~free_names
    | Apply apply -> rebuild_apply env apply
  in
  rebuild_holed env res holed_expr expr

and rebuild_function_params_and_body (env : env) res code_metadata
    (params_and_body : Rev_expr.rev_params_and_body) =
  let { Rev_expr.return_continuation;
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
        DS.cannot_change_calling_convention env.uses code_id
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
    match List.filter (is_dead_var env) all_vars with
    | [] -> rebuild_expr env res body
    | _ :: _ as dead_vars ->
      let msg =
        Format.asprintf
          "Function is never called because of dead parameters: [%a]."
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
             Variable.print)
          dead_vars
      in
      ( RE.from_expr
          ~expr:(Expr.create_invalid (Message msg))
          ~free_names:Name_occurrences.empty,
        res )
  in
  match updating_calling_convention with
  | Not_changing_calling_convention ->
    let body, res = rebuild_body () in
    (* Format.eprintf "REBUILD %a FREE %a@." Code_id.print code_id
       Name_occurrences.print body.free_names; *)
    ( Function_params_and_body.create ~return_continuation ~exn_continuation
        params ~body:body.expr ~free_names_of_body:(Known body.free_names)
        ~my_closure ~my_region ~my_ghost_region ~my_depth,
      code_metadata,
      res )
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
                 (DS.get_unboxed_fields env.uses
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
        DS.get_unboxed_fields env.uses (Code_id_or_name.var my_closure)
      with
      | None -> [], code_metadata
      | Some fields ->
        ( fold_unboxed_with_kind
            (fun kind v acc ->
              Bound_parameter.create v (KS.anything kind) :: acc)
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
      Code_metadata.with_never_called_indirectly true
        (Code_metadata.with_params_arity params_arity code_metadata)
    in
    let body, res = rebuild_body () in
    (* Format.eprintf "REBUILD %a FREE %a@." Code_id.print code_id
       Name_occurrences.print body.free_names; *)
    (* assert (List.exists Fun.id (Continuation.Map.find return_continuation
       env.cont_params_to_keep)); *)
    ( Function_params_and_body.create ~return_continuation ~exn_continuation
        (Bound_parameters.create (List.flatten params))
        ~body:body.expr ~free_names_of_body:(Known body.free_names) ~my_closure
        ~my_region ~my_ghost_region ~my_depth,
      code_metadata,
      res )

and rebuild_static_const_or_code env res
    ( (bound_to : Bound_static.Pattern.t),
      (static_const_or_code : Rev_expr.rev_static_const_or_code) ) =
  match static_const_or_code with
  | Deleted_code -> Static_const_or_code.deleted_code, res
  | Code { params_and_body; code_metadata; free_names_of_params_and_body = _ }
    ->
    let is_my_closure_used = is_var_used env params_and_body.my_closure in
    let code_metadata =
      if Bool.equal is_my_closure_used
           (Code_metadata.is_my_closure_used code_metadata)
      then code_metadata
      else (
        assert (not is_my_closure_used);
        Code_metadata.with_is_my_closure_used is_my_closure_used code_metadata)
    in
    let params_and_body, code_metadata, res =
      rebuild_function_params_and_body env res code_metadata params_and_body
    in
    let code =
      Code.create_with_metadata ~params_and_body ~code_metadata
        ~free_names_of_params_and_body:
          (function_params_and_body_free_names params_and_body)
    in
    assert (
      Compilation_unit.is_current
        (Code_id.get_compilation_unit (Code.code_id code)));
    let res =
      { res with
        all_code = Code_id.Map.add (Code.code_id code) code res.all_code
      }
    in
    Static_const_or_code.create_code code, res
  | Static_const (Set_of_closures set_of_closures) ->
    let bound_to =
      match bound_to with
      | Set_of_closures function_slots -> Function_slot.Lmap.data function_slots
      | Code _ | Block_like _ -> Misc.fatal_error "Expected Set_of_closures"
    in
    let bound_to = List.map Name.symbol bound_to in
    let set_of_closures, res =
      rewrite_set_of_closures env res ~bound:bound_to set_of_closures
    in
    let static_const_or_code =
      SC.set_of_closures set_of_closures
      |> Static_const_or_code.create_static_const
    in
    static_const_or_code, res
  | Static_const (Other static_const) -> (
    match static_const with
    | Block _ | Boxed_float32 _ | Boxed_float _ | Boxed_int32 _ | Boxed_int64 _
    | Boxed_nativeint _ | Boxed_vec128 _ | Immutable_float_block _
    | Immutable_float_array _ | Immutable_float32_array _
    | Immutable_int32_array _ | Immutable_int64_array _
    | Immutable_nativeint_array _ | Immutable_vec128_array _
    | Immutable_value_array _ | Empty_array _ | Mutable_string _
    | Immutable_string _ ->
      Static_const_or_code.create_static_const static_const, res
    | Set_of_closures _ ->
      Misc.fatal_errorf
        "Set_of_closures is not permitted in conjunction with Other in the \
         Static_const case:@ %a"
        SC.print static_const)

type result =
  { body : Expr.t;
    free_names : Name_occurrences.t;
    all_code : Code.t Code_id.Map.t;
    slot_offsets : Slot_offsets.t
  }

let rebuild ~(code_deps : Traverse_acc.code_dep Code_id.Map.t)
    ~(continuation_info : Traverse_acc.continuation_info Continuation.Map.t)
    ~fixed_arity_continuations kinds (solved_dep : DS.result) get_code_metadata
    holed =
  let should_keep_function_param code_id =
    let cannot_change_calling_convention =
      DS.cannot_change_calling_convention solved_dep code_id
    in
    if cannot_change_calling_convention
    then (fun var kind ->
      assert (
        Option.is_none
          (DS.get_unboxed_fields solved_dep (Code_id_or_name.var var)));
      Keep (var, kind))
    else
      fun param kind ->
      match DS.get_unboxed_fields solved_dep (Code_id_or_name.var param) with
      | None ->
        let is_var_used =
          raw_is_var_used solved_dep param (K.With_subkind.kind kind)
        in
        (* XXX what should happen to this "if"? *)
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
          DS.cannot_change_calling_convention solved_dep code_id
        in
        let metadata = get_code_metadata code_id in
        let result_kinds =
          Flambda_arity.unarized_components
            (Code_metadata.result_arity metadata)
        in
        if cannot_change_calling_convention
        then
          List.map2 (fun v kind -> Keep (v, kind)) code_dep.return result_kinds
        else
          (* Format.eprintf "DIRECT: %a@." Code_id.print code_id; *)
          List.map2
            (fun v kind ->
              match
                DS.get_unboxed_fields solved_dep (Code_id_or_name.var v)
              with
              | None ->
                let is_var_used =
                  raw_is_var_used solved_dep v (K.With_subkind.kind kind)
                in
                let kind =
                  DS.rewrite_kind_with_subkind solved_dep (Name.var v) kind
                in
                (* TODO: fix this, needs the mapping between code ids of
                   functions and their return continuations *)
                if true || is_var_used then Keep (v, kind) else Delete
              | Some fields -> Unbox fields)
            code_dep.return result_kinds)
      code_deps
  in
  let should_keep_param cont param kind =
    let keep_all_parameters =
      Continuation.Set.mem cont fixed_arity_continuations
    in
    match DS.get_unboxed_fields solved_dep (Code_id_or_name.var param) with
    | None ->
      if keep_all_parameters
         ||
         let is_var_used =
           raw_is_var_used solved_dep param (K.With_subkind.kind kind)
         in
         is_var_used
         ||
         let info = Continuation.Map.find cont continuation_info in
         info.is_exn_handler && Variable.equal param (List.hd info.params)
      then
        Keep
          (param, DS.rewrite_kind_with_subkind solved_dep (Name.var param) kind)
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
      function_return_decision;
      kinds
    }
  in
  let res =
    { all_slot_offsets = Slot_offsets.empty; all_code = Code_id.Map.empty }
  in
  let rebuilt_expr, { all_slot_offsets; all_code } =
    Profile.record_call ~accumulate:true "up" (fun () ->
        rebuild_expr env res holed)
  in
  { body = rebuilt_expr.expr;
    free_names = rebuilt_expr.free_names;
    all_code;
    slot_offsets = all_slot_offsets
  }
