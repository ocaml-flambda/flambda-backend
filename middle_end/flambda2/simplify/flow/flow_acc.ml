(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*    Pierre Chambart and Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2021--2021 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module T = Flow_types
module EPA = Continuation_extra_params_and_args

type t = T.Acc.t

type cont_info = T.Continuation_info.t

let print = T.Acc.print

(* Creation *)
(* ******** *)

let wrong_dummy_toplevel_cont_name = "wrong toplevel cont"

let empty () =
  let wrong_dummy_toplevel_cont =
    Continuation.create ~name:wrong_dummy_toplevel_cont_name ()
  in
  let res : t =
    { stack = [];
      map = Continuation.Map.empty;
      extra = Continuation.Map.empty;
      dummy_toplevel_cont = wrong_dummy_toplevel_cont
    }
  in
  res

(* Updates *)
(* ******* *)

let add_extra_params_and_args cont extra (t : t) =
  let extra =
    Continuation.Map.update cont
      (function
        | Some _ -> Misc.fatal_errorf "Continuation extended a second time"
        | None -> Some extra)
      t.extra
  in
  { t with extra }

let enter_continuation continuation ~recursive ~is_exn_handler params (t : t) =
  let parent_continuation =
    match t.stack with [] -> None | parent :: _ -> Some parent.continuation
  in
  let used_in_handler =
    if not is_exn_handler
    then Name_occurrences.empty
    else
      (* The first param of an exn_handler is unconditionally used *)
      let first_param =
        Bound_parameter.var (List.hd (Bound_parameters.to_list params))
      in
      Name_occurrences.singleton_variable first_param Name_mode.normal
  in
  let cont_info : cont_info =
    { continuation;
      recursive;
      is_exn_handler;
      params;
      parent_continuation;
      bindings = Name.Map.empty;
      direct_aliases = Variable.Map.empty;
      mutable_let_prims_rev = [];
      defined = Variable.Set.empty;
      code_ids = Code_id.Map.empty;
      value_slots = Value_slot.Map.empty;
      used_in_handler;
      apply_cont_args = Continuation.Map.empty
    }
  in
  { t with stack = cont_info :: t.stack }

let init_toplevel ~dummy_toplevel_cont params _t =
  enter_continuation dummy_toplevel_cont ~recursive:false ~is_exn_handler:false
    params
    { (empty ()) with dummy_toplevel_cont }

let exit_continuation cont (t : t) =
  match t.stack with
  | [] -> Misc.fatal_errorf "Empty stack of variable uses"
  | ({ continuation; _ } as elt) :: stack ->
    assert (Continuation.equal cont continuation);
    let map = Continuation.Map.add cont elt t.map in
    { t with stack; map }

let update_top_of_stack ~(t : t) ~f =
  match t.stack with
  | [] -> Misc.fatal_errorf "Empty stack of variable uses"
  | elt :: stack -> { t with stack = f elt :: stack }

let record_defined_var var t =
  update_top_of_stack ~t ~f:(fun elt ->
      let defined = Variable.Set.add var elt.defined in
      { elt with defined })

let record_var_binding var name_occurrences ~generate_phantom_lets t =
  update_top_of_stack ~t ~f:(fun elt ->
      let bindings =
        Name.Map.update (Name.var var)
          (function
            | None -> Some name_occurrences
            | Some _ ->
              Misc.fatal_errorf
                "The following variable has been bound twice: %a" Variable.print
                var)
          elt.bindings
      in
      let used_in_handler =
        if Variable.user_visible var && generate_phantom_lets
        then
          Name_occurrences.add_variable elt.used_in_handler var
            Name_mode.phantom
        else elt.used_in_handler
      in
      let defined = Variable.Set.add var elt.defined in
      { elt with bindings; used_in_handler; defined })

let record_var_alias var definition t =
  update_top_of_stack ~t ~f:(fun elt ->
      let direct_aliases =
        Variable.Map.update var
          (function
            | None -> Some definition
            | Some _ ->
              Misc.fatal_errorf
                "The following variable has been bound twice: %a" Variable.print
                var)
          elt.direct_aliases
      in
      let defined = Variable.Set.add var elt.defined in
      { elt with direct_aliases; defined })

let record_ref_named named_rewrite_id ~bound_to ~original_prim ~prim (t : t) =
  update_top_of_stack ~t ~f:(fun cont_info ->
      let mutable_let_prim : T.Mutable_let_prim.t =
        { bound_var = bound_to; named_rewrite_id; original_prim; prim }
      in
      let mutable_let_prims_rev =
        mutable_let_prim :: cont_info.mutable_let_prims_rev
      in
      { cont_info with mutable_let_prims_rev })

let record_symbol_projection var name_occurrences t =
  update_top_of_stack ~t ~f:(fun elt ->
      let bindings =
        Name.Map.update (Name.var var)
          (function
            | None -> Some name_occurrences
            | Some prior_occurences as original ->
              if Name_occurrences.equal prior_occurences name_occurrences
              then original
              else
                Misc.fatal_errorf
                  "@[<v>The following projection has been bound to different \
                   symbols:%a@ previously bound to:@ %a@ and now to@ %a@]"
                  Variable.print var Name_occurrences.print prior_occurences
                  Name_occurrences.print name_occurrences)
          elt.bindings
      in
      { elt with bindings })

let record_symbol_binding symbol name_occurrences t =
  update_top_of_stack ~t ~f:(fun elt ->
      let bindings =
        Name.Map.update (Name.symbol symbol)
          (function
            | None -> Some name_occurrences
            | Some _ ->
              Misc.fatal_errorf "The following symbol has been bound twice: %a"
                Symbol.print symbol)
          elt.bindings
      in
      { elt with bindings })

let record_code_id_binding code_id name_occurrences t =
  update_top_of_stack ~t ~f:(fun elt ->
      let code_ids =
        Code_id.Map.update code_id
          (function
            | None -> Some name_occurrences
            | Some _ ->
              Misc.fatal_errorf "The following code_id has been bound twice: %a"
                Code_id.print code_id)
          elt.code_ids
      in
      { elt with code_ids })

let record_value_slot src value_slot dst t =
  update_top_of_stack ~t ~f:(fun elt ->
      let value_slots =
        Value_slot.Map.update value_slot
          (function
            | None -> Some (Name.Map.singleton src dst)
            | Some map ->
              Some
                (Name.Map.update src
                   (function
                     | None -> Some dst
                     | Some dst' -> Some (Name_occurrences.union dst dst'))
                   map))
          elt.value_slots
      in
      { elt with value_slots })

let add_used_in_current_handler name_occurrences t =
  update_top_of_stack ~t ~f:(fun elt ->
      let used_in_handler =
        Name_occurrences.union elt.used_in_handler name_occurrences
      in
      { elt with used_in_handler })

let add_apply_conts ~result_cont ~exn_cont ~result_arity t =
  update_top_of_stack ~t ~f:(fun elt ->
      let add_func_result cont rewrite_id ~result_arity ~extra_args
          apply_cont_args =
        Continuation.Map.update cont
          (fun (rewrite_map_opt :
                 T.Cont_arg.t Numeric_types.Int.Map.t
                 Apply_cont_rewrite_id.Map.t
                 option) ->
            let rewrite_map =
              Option.value ~default:Apply_cont_rewrite_id.Map.empty
                rewrite_map_opt
            in
            let rewrite_map =
              Apply_cont_rewrite_id.Map.update rewrite_id
                (function
                  | Some _ ->
                    Misc.fatal_errorf "Introducing a rewrite id twice %a"
                      Apply_cont_rewrite_id.print rewrite_id
                  | None ->
                    let map =
                      Numeric_types.Int.Map.of_list
                        (List.init result_arity (fun i ->
                             i, T.Cont_arg.Function_result))
                    in
                    let _, map =
                      List.fold_left
                        (fun (i, map) (extra_arg, _kind) ->
                          let map =
                            Numeric_types.Int.Map.add i
                              (T.Cont_arg.Simple extra_arg) map
                          in
                          i + 1, map)
                        (1, map) extra_args
                    in
                    Some map)
                rewrite_map
            in
            Some rewrite_map)
          apply_cont_args
      in
      let apply_cont_args =
        let rewrite_id, exn_cont = exn_cont in
        add_func_result
          (Exn_continuation.exn_handler exn_cont)
          rewrite_id ~result_arity:1
          ~extra_args:(Exn_continuation.extra_args exn_cont)
          elt.apply_cont_args
      in
      let apply_cont_args =
        match result_cont with
        | None -> apply_cont_args
        | Some (rewrite_id, result_cont) ->
          add_func_result result_cont rewrite_id
            ~result_arity:(Flambda_arity.cardinal_unarized result_arity)
            ~extra_args:[] apply_cont_args
      in
      { elt with apply_cont_args })

let add_apply_cont_args ~rewrite_id cont arg_name_simples t =
  update_top_of_stack ~t ~f:(fun elt ->
      let apply_cont_args =
        Continuation.Map.update cont
          (fun (rewrite_map_opt :
                 T.Cont_arg.t Numeric_types.Int.Map.t
                 Apply_cont_rewrite_id.Map.t
                 option) ->
            let rewrite_map =
              Option.value ~default:Apply_cont_rewrite_id.Map.empty
                rewrite_map_opt
            in
            let rewrite_map =
              Apply_cont_rewrite_id.Map.update rewrite_id
                (function
                  | Some _ ->
                    Misc.fatal_errorf "Introducing a rewrite id twice %a"
                      Apply_cont_rewrite_id.print rewrite_id
                  | None ->
                    let map, _ =
                      List.fold_left
                        (fun (map, i) arg_simple ->
                          let map =
                            Numeric_types.Int.Map.add i
                              (T.Cont_arg.Simple arg_simple) map
                          in
                          map, i + 1)
                        (Numeric_types.Int.Map.empty, 0)
                        arg_name_simples
                    in
                    Some map)
                rewrite_map
            in
            Some rewrite_map)
          elt.apply_cont_args
      in
      { elt with apply_cont_args })

let get_block_and_constant_field ~block ~field =
  Simple.pattern_match field
    ~name:(fun _ ~coercion:_ -> None)
    ~const:(fun const ->
      Simple.pattern_match' block
        ~const:(fun _ -> None)
        ~symbol:(fun _ ~coercion:_ -> None)
        ~var:(fun var ~coercion:_ ->
          let field =
            match[@ocaml.warning "-4"] Reg_width_const.descr const with
            | Tagged_immediate i -> Targetint_31_63.to_int i
            | _ -> assert false
          in
          Some (var, field)))

let record_let_binding ~rewrite_id ~generate_phantom_lets ~let_bound
    ~simplified_defining_expr t =
  match (simplified_defining_expr : Simplified_named.t) with
  | { free_names; named; cost_metrics = _ } -> (
    let record_var_bindings t free_names =
      Bound_pattern.fold_all_bound_vars let_bound ~init:t ~f:(fun t v ->
          record_var_binding (Bound_var.var v) free_names ~generate_phantom_lets
            t)
    in
    match[@ocaml.warning "-4"] named with
    | Simple simple ->
      let bound_var = Bound_pattern.must_be_singleton let_bound in
      let var = Bound_var.var bound_var in
      record_var_alias var simple t
    | Set_of_closures _ | Rec_info _ -> record_var_bindings t free_names
    | Prim (original_prim, _) -> (
      let bound_var = Bound_pattern.must_be_singleton let_bound in
      let var = Bound_var.var bound_var in
      match[@ocaml.warning "-4"] original_prim with
      | Unary (End_region { ghost = _ }, _region) ->
        (* Uses of region variables in [End_region] don't count as uses. *)
        t
      | Unary (Is_int _, simple) -> (
        match Simple.must_be_var simple with
        | Some (v, _) ->
          record_var_bindings
            (record_ref_named rewrite_id ~bound_to:var ~original_prim
               ~prim:(Is_int v) t)
            Name_occurrences.empty
        | None -> record_var_bindings t free_names)
      | Unary (Get_tag, simple) -> (
        match Simple.must_be_var simple with
        | Some (v, _) ->
          record_var_bindings
            (record_ref_named rewrite_id ~bound_to:var ~original_prim
               ~prim:(Get_tag v) t)
            Name_occurrences.empty
        | None -> record_var_bindings t free_names)
      | Binary (Block_load (bak, mut), block, field) -> (
        match get_block_and_constant_field ~block ~field with
        | Some (block, field) ->
          record_var_bindings
            (record_ref_named rewrite_id ~bound_to:var ~original_prim
               ~prim:(Block_load { bak; mut; block; field })
               t)
            Name_occurrences.empty
        | None -> record_var_bindings t free_names)
      | Ternary (Block_set (bak, _), block, field, value) -> (
        match get_block_and_constant_field ~block ~field with
        | Some (block, field) ->
          record_ref_named rewrite_id ~bound_to:var ~original_prim
            ~prim:(Block_set { bak; block; field; value })
            t
        | None -> add_used_in_current_handler free_names t)
      | Variadic (Make_block (kind, mut, alloc_mode), fields) ->
        record_var_bindings
          (record_ref_named rewrite_id ~bound_to:var ~original_prim
             ~prim:(Make_block { kind; mut; alloc_mode; fields })
             t)
          Name_occurrences.empty
      | _ ->
        if Flambda_primitive.at_most_generative_effects original_prim
        then (* the primitive can be removed *)
          record_var_bindings t free_names
        else
          let t = record_defined_var var t in
          add_used_in_current_handler free_names t))

(* Normalisation *)
(* ************* *)

let add_extra_args_to_call ~extra_args rewrite_id original_args =
  match Apply_cont_rewrite_id.Map.find rewrite_id extra_args with
  | exception Not_found -> Some original_args
  | Or_invalid.Invalid -> None
  | Or_invalid.Ok extra_args ->
    let args_acc =
      if Numeric_types.Int.Map.is_empty original_args
      then 0, Numeric_types.Int.Map.empty
      else
        let max_arg, _ = Numeric_types.Int.Map.max_binding original_args in
        max_arg + 1, original_args
    in
    let extra_args =
      List.map
        (function
          | EPA.Extra_arg.Already_in_scope s -> T.Cont_arg.Simple s
          | EPA.Extra_arg.New_let_binding (v, prim) ->
            T.Cont_arg.New_let_binding (v, Flambda_primitive.free_names prim)
          | EPA.Extra_arg.New_let_binding_with_named_args (v, _) ->
            T.Cont_arg.New_let_binding (v, Name_occurrences.empty))
        extra_args
    in
    let _, args =
      List.fold_left
        (fun (i, args) extra_arg ->
          i + 1, Numeric_types.Int.Map.add i extra_arg args)
        args_acc extra_args
    in
    Some args

let extend_args_with_extra_args (t : T.Acc.t) =
  let map =
    Continuation.Map.map
      (fun (elt : T.Continuation_info.t) ->
        let apply_cont_args =
          Continuation.Map.mapi
            (fun cont rewrite_ids ->
              match Continuation.Map.find cont t.extra with
              | exception Not_found -> rewrite_ids
              | epa ->
                let extra_args = EPA.extra_args epa in
                Apply_cont_rewrite_id.Map.filter_map
                  (add_extra_args_to_call ~extra_args)
                  rewrite_ids)
            elt.apply_cont_args
        in
        { elt with apply_cont_args })
      t.map
  in
  let map =
    Continuation.Map.map
      (fun (elt : T.Continuation_info.t) ->
        let defined =
          Continuation.Map.fold
            (fun callee_cont rewrite_ids defined ->
              match Continuation.Map.find callee_cont t.extra with
              | exception Not_found -> defined
              | epa ->
                Apply_cont_rewrite_id.Map.fold
                  (fun rewrite_id _args defined ->
                    match
                      Apply_cont_rewrite_id.Map.find rewrite_id
                        (EPA.extra_args epa)
                    with
                    | exception Not_found -> defined
                    | Invalid -> defined
                    | Ok extra_args ->
                      let defined =
                        List.fold_left
                          (fun defined -> function
                            | EPA.Extra_arg.Already_in_scope _ -> defined
                            | EPA.Extra_arg.New_let_binding (v, _)
                            | EPA.Extra_arg.New_let_binding_with_named_args
                                (v, _) ->
                              Variable.Set.add v defined)
                          defined extra_args
                      in
                      defined)
                  rewrite_ids defined)
            elt.apply_cont_args elt.defined
        in
        { elt with defined })
      map
  in
  let map =
    Continuation.Map.fold
      (fun cont epa map ->
        let elt : T.Continuation_info.t = Continuation.Map.find cont map in
        let elt =
          let params =
            Bound_parameters.append elt.params (EPA.extra_params epa)
          in
          { elt with params }
        in
        Continuation.Map.add cont elt map)
      t.extra map
  in
  { t with map; extra = Continuation.Map.empty }
