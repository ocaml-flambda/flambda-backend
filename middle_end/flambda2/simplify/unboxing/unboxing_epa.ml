(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Simplify_import
module U = Unboxing_types
module Extra_param_and_args = U.Extra_param_and_args

exception Invalid_apply_cont

exception Prevent_current_unboxing

let prevent_current_unboxing () = raise Prevent_current_unboxing

type unboxed_arg =
  | Poison (* used for recursive calls *)
  | Available of Simple.t
  | Generated of Variable.t
  | Added_by_wrapper_at_rewrite_use of { nth_arg : int }

let _print_unboxed_arg ppf = function
  | Poison -> Format.fprintf ppf "poison"
  | Available simple -> Format.fprintf ppf "simple: %a" Simple.print simple
  | Generated v -> Format.fprintf ppf "generated: %a" Variable.print v
  | Added_by_wrapper_at_rewrite_use { nth_arg } ->
    Format.fprintf ppf "added_by_wrapper(%d)" nth_arg

let type_of_arg_being_unboxed unboxed_arg =
  match unboxed_arg with
  | Poison -> None
  | Available simple -> Some (T.alias_type_of K.value simple)
  | Generated _ -> Some (T.unknown K.value)
  | Added_by_wrapper_at_rewrite_use _ -> prevent_current_unboxing ()

let unbox_arg (unboxer : Unboxers.unboxer) ~typing_env_at_use arg_being_unboxed
    =
  match arg_being_unboxed with
  | Poison ->
    let extra_arg =
      EPA.Extra_arg.Already_in_scope (Simple.const unboxer.poison_const)
    in
    extra_arg, Poison
  | Available arg_at_use -> (
    let arg_type = T.alias_type_of K.value arg_at_use in
    match
      unboxer.prove_simple typing_env_at_use arg_type
        ~min_name_mode:Name_mode.normal
    with
    | Invalid -> raise Invalid_apply_cont
    | Known_result simple ->
      EPA.Extra_arg.Already_in_scope simple, Available simple
    | Need_meet ->
      let var = Variable.create unboxer.var_name in
      let prim = unboxer.unboxing_prim arg_at_use in
      let extra_arg = EPA.Extra_arg.New_let_binding (var, prim) in
      extra_arg, Generated var)
  | Generated var ->
    let arg_at_use = Simple.var var in
    let var = Variable.create unboxer.var_name in
    let prim = unboxer.unboxing_prim arg_at_use in
    let extra_arg = EPA.Extra_arg.New_let_binding (var, prim) in
    extra_arg, Generated var
  | Added_by_wrapper_at_rewrite_use { nth_arg } ->
    let var = Variable.create "unboxed_field" in
    ( EPA.Extra_arg.New_let_binding_with_named_args
        ( var,
          fun args ->
            let arg_simple = List.nth args nth_arg in
            unboxer.unboxing_prim arg_simple ),
      Generated var )

(* Helpers for the variant case *)
(* **************************** *)

type variant_argument =
  | Not_a_constant_constructor
  | Maybe_constant_constructor of
      { is_int : Simple.t;
        arg_being_unboxed : unboxed_arg
      }

let extra_arg_for_is_int = function
  | Maybe_constant_constructor { is_int; _ } ->
    EPA.Extra_arg.Already_in_scope is_int
  | Not_a_constant_constructor ->
    EPA.Extra_arg.Already_in_scope Simple.untagged_const_false

let extra_arg_for_ctor ~typing_env_at_use = function
  | Not_a_constant_constructor ->
    EPA.Extra_arg.Already_in_scope
      (Simple.untagged_const_int (Targetint_31_63.of_int 0))
  | Maybe_constant_constructor { arg_being_unboxed; _ } -> (
    match type_of_arg_being_unboxed arg_being_unboxed with
    | None ->
      EPA.Extra_arg.Already_in_scope
        (Simple.untagged_const_int (Targetint_31_63.of_int 0))
    | Some arg_type -> (
      match
        T.meet_tagging_of_simple typing_env_at_use
          ~min_name_mode:Name_mode.normal arg_type
      with
      | Known_result simple -> EPA.Extra_arg.Already_in_scope simple
      | Need_meet -> prevent_current_unboxing ()
      | Invalid -> raise Invalid_apply_cont))

let extra_args_for_const_ctor_of_variant
    (const_ctors_decision : U.const_ctors_decision) ~typing_env_at_use
    rewrite_id variant_arg : U.const_ctors_decision =
  match const_ctors_decision with
  | Zero -> (
    match variant_arg with
    | Not_a_constant_constructor -> const_ctors_decision
    | Maybe_constant_constructor _ -> raise Invalid_apply_cont)
  | At_least_one { ctor = Do_not_unbox reason; is_int } ->
    let is_int =
      Extra_param_and_args.update_param_args is_int rewrite_id
        (extra_arg_for_is_int variant_arg)
    in
    At_least_one { ctor = Do_not_unbox reason; is_int }
  | At_least_one { ctor = Unbox (Number (Naked_immediate, ctor)); is_int } -> (
    let is_int =
      Extra_param_and_args.update_param_args is_int rewrite_id
        (extra_arg_for_is_int variant_arg)
    in
    try
      let ctor =
        Extra_param_and_args.update_param_args ctor rewrite_id
          (extra_arg_for_ctor ~typing_env_at_use variant_arg)
      in
      At_least_one { ctor = Unbox (Number (Naked_immediate, ctor)); is_int }
    with Prevent_current_unboxing ->
      At_least_one { ctor = Do_not_unbox Not_enough_information_at_use; is_int }
    )
  | At_least_one
      { ctor =
          Unbox
            ( Unique_tag_and_size _ | Variant _ | Closure_single_entry _
            | Number
                ( ( Naked_float | Naked_float32 | Naked_int32 | Naked_int64
                  | Naked_nativeint | Naked_vec128 ),
                  _ ) );
        is_int = _
      } ->
    Misc.fatal_errorf
      "Bad kind for unboxing the constant constructor of a variant"

(* Helpers for the number case *)
(* *************************** *)

let compute_extra_arg_for_number kind unboxer epa rewrite_id ~typing_env_at_use
    arg_being_unboxed : U.decision =
  let extra_arg, _new_arg_being_unboxed =
    unbox_arg unboxer ~typing_env_at_use arg_being_unboxed
  in
  let epa = Extra_param_and_args.update_param_args epa rewrite_id extra_arg in
  Unbox (Number (kind, epa))

(* Recursive descent on decisions *)
(* ****************************** *)

let rec compute_extra_args_for_one_decision_and_use ~(pass : U.pass) rewrite_id
    ~typing_env_at_use arg_being_unboxed decision : U.decision =
  try
    compute_extra_args_for_one_decision_and_use_aux ~pass rewrite_id
      ~typing_env_at_use arg_being_unboxed decision
  with Prevent_current_unboxing -> (
    match pass with
    | Filter -> Do_not_unbox Not_enough_information_at_use
    | Compute_all_extra_args ->
      Misc.fatal_errorf "This case should have been filtered out before.")

and compute_extra_args_for_one_decision_and_use_aux ~(pass : U.pass) rewrite_id
    ~typing_env_at_use arg_being_unboxed (decision : U.decision) : U.decision =
  match decision with
  | Do_not_unbox _ -> decision
  | Unbox (Unique_tag_and_size { tag; shape; fields }) ->
    compute_extra_args_for_block ~pass rewrite_id ~typing_env_at_use
      arg_being_unboxed tag shape fields
  | Unbox (Closure_single_entry { function_slot; vars_within_closure }) ->
    compute_extra_args_for_closure ~pass rewrite_id ~typing_env_at_use
      arg_being_unboxed function_slot vars_within_closure
  | Unbox
      (Variant { tag; const_ctors = const_ctors_from_decision; fields_by_tag })
    -> (
    match type_of_arg_being_unboxed arg_being_unboxed with
    | None ->
      compute_extra_args_for_variant ~pass rewrite_id ~typing_env_at_use
        arg_being_unboxed ~tag_from_decision:tag ~const_ctors_from_decision
        ~fields_by_tag_from_decision:fields_by_tag
        ~const_ctors_at_use:(Or_unknown.Known Targetint_31_63.Set.empty)
        ~non_const_ctors_with_sizes_at_use:Tag.Scannable.Map.empty
    | Some arg_type -> (
      match T.meet_variant_like typing_env_at_use arg_type with
      | Need_meet -> prevent_current_unboxing ()
      | Invalid -> raise Invalid_apply_cont
      | Known_result { const_ctors; non_const_ctors_with_sizes } ->
        compute_extra_args_for_variant ~pass rewrite_id ~typing_env_at_use
          arg_being_unboxed ~tag_from_decision:tag ~const_ctors_from_decision
          ~fields_by_tag_from_decision:fields_by_tag
          ~const_ctors_at_use:const_ctors
          ~non_const_ctors_with_sizes_at_use:non_const_ctors_with_sizes))
  | Unbox (Number (Naked_float32, epa)) ->
    compute_extra_arg_for_number Naked_float32 Unboxers.Float32.unboxer epa
      rewrite_id ~typing_env_at_use arg_being_unboxed
  | Unbox (Number (Naked_float, epa)) ->
    compute_extra_arg_for_number Naked_float Unboxers.Float.unboxer epa
      rewrite_id ~typing_env_at_use arg_being_unboxed
  | Unbox (Number (Naked_int32, epa)) ->
    compute_extra_arg_for_number Naked_int32 Unboxers.Int32.unboxer epa
      rewrite_id ~typing_env_at_use arg_being_unboxed
  | Unbox (Number (Naked_int64, epa)) ->
    compute_extra_arg_for_number Naked_int64 Unboxers.Int64.unboxer epa
      rewrite_id ~typing_env_at_use arg_being_unboxed
  | Unbox (Number (Naked_nativeint, epa)) ->
    compute_extra_arg_for_number Naked_nativeint Unboxers.Nativeint.unboxer epa
      rewrite_id ~typing_env_at_use arg_being_unboxed
  | Unbox (Number (Naked_immediate, epa)) ->
    compute_extra_arg_for_number Naked_immediate Unboxers.Immediate.unboxer epa
      rewrite_id ~typing_env_at_use arg_being_unboxed
  | Unbox (Number (Naked_vec128, epa)) ->
    compute_extra_arg_for_number Naked_vec128 Unboxers.Vec128.unboxer epa
      rewrite_id ~typing_env_at_use arg_being_unboxed

and compute_extra_args_for_block ~pass rewrite_id ~typing_env_at_use
    arg_being_unboxed tag (shape : K.Block_shape.t) fields : U.decision =
  let size = Or_unknown.Known (Targetint_31_63.of_int (List.length fields)) in
  let access_kind_and_const index : P.Block_access_kind.t * _ =
    match shape with
    | Value_only ->
      ( Values
          { size;
            tag = Known (Option.get (Tag.Scannable.of_tag tag));
            field_kind = Any_value
          },
        Const.const_zero )
    | Float_record ->
      ( Naked_floats { size },
        Const.naked_float Numeric_types.Float_by_bit_pattern.zero )
    | Mixed_record shape ->
      let field_kind, const =
        let field_kind = (K.Mixed_block_shape.field_kinds shape).(index) in
        if index < K.Mixed_block_shape.value_prefix_size shape
        then
          (* CR vlaviron: we're not trying to infer if this can only be an
             immediate. In most cases it should be fine, as the primitive will
             get simplified away. *)
          ( P.Mixed_block_access_field_kind.Value_prefix Any_value,
            Const.const_zero )
        else
          ( P.Mixed_block_access_field_kind.Flat_suffix field_kind,
            Const.of_int_of_kind field_kind 0 )
      in
      let tag = Or_unknown.Known (Option.get (Tag.Scannable.of_tag tag)) in
      Mixed { tag; size; shape; field_kind }, const
  in
  let _, fields =
    List.fold_left_map
      (fun field_nth ({ epa; decision; kind } : U.field_decision) :
           (_ * U.field_decision) ->
        let bak, poison_const =
          access_kind_and_const (Targetint_31_63.to_int field_nth)
        in
        let unboxer =
          Unboxers.Field.unboxer ~poison_const bak ~index:field_nth
        in
        let new_extra_arg, new_arg_being_unboxed =
          unbox_arg unboxer ~typing_env_at_use arg_being_unboxed
        in
        let epa =
          Extra_param_and_args.update_param_args epa rewrite_id new_extra_arg
        in
        let decision =
          compute_extra_args_for_one_decision_and_use ~pass rewrite_id
            ~typing_env_at_use new_arg_being_unboxed decision
        in
        Targetint_31_63.(add one field_nth), { epa; decision; kind })
      Targetint_31_63.zero fields
  in
  Unbox (Unique_tag_and_size { tag; shape; fields })

and compute_extra_args_for_closure ~pass rewrite_id ~typing_env_at_use
    arg_being_unboxed function_slot vars_within_closure : U.decision =
  let vars_within_closure =
    Value_slot.Map.mapi
      (fun var ({ epa; decision; kind } : U.field_decision) : U.field_decision ->
        let unboxer = Unboxers.Closure_field.unboxer function_slot var in
        let new_extra_arg, new_arg_being_unboxed =
          unbox_arg unboxer ~typing_env_at_use arg_being_unboxed
        in
        let epa =
          Extra_param_and_args.update_param_args epa rewrite_id new_extra_arg
        in
        let decision =
          compute_extra_args_for_one_decision_and_use ~pass rewrite_id
            ~typing_env_at_use new_arg_being_unboxed decision
        in
        { epa; decision; kind })
      vars_within_closure
  in
  Unbox (Closure_single_entry { function_slot; vars_within_closure })

and compute_extra_args_for_variant ~pass rewrite_id ~typing_env_at_use
    arg_being_unboxed ~tag_from_decision ~const_ctors_from_decision
    ~fields_by_tag_from_decision ~const_ctors_at_use
    ~non_const_ctors_with_sizes_at_use : U.decision =
  let are_there_const_ctors_at_use =
    match (const_ctors_at_use : _ Or_unknown.t) with
    | Unknown -> true
    | Known set -> not (Targetint_31_63.Set.is_empty set)
  in
  let are_there_non_const_ctors_at_use =
    not (Tag.Scannable.Map.is_empty non_const_ctors_with_sizes_at_use)
  in
  let const_ctors =
    if not are_there_const_ctors_at_use
    then
      extra_args_for_const_ctor_of_variant const_ctors_from_decision
        ~typing_env_at_use rewrite_id Not_a_constant_constructor
    else if not are_there_non_const_ctors_at_use
    then
      extra_args_for_const_ctor_of_variant const_ctors_from_decision
        ~typing_env_at_use rewrite_id
        (Maybe_constant_constructor
           { arg_being_unboxed; is_int = Simple.untagged_const_true })
    else
      (* CR-someday gbury: one might want to try and use the cse at use to allow
         unboxing when the tag is not known statically but can be recovered
         through the cse. *)
      prevent_current_unboxing ()
  in
  let tag_at_use_site =
    if not are_there_non_const_ctors_at_use
    then Tag.Scannable.zero
    else
      match
        Tag.Scannable.Map.get_singleton non_const_ctors_with_sizes_at_use
      with
      | None -> prevent_current_unboxing ()
      | Some (tag, _) -> tag
  in
  let tag_extra_arg =
    tag_at_use_site |> Tag.Scannable.to_targetint
    |> Targetint_31_63.of_targetint |> Const.untagged_const_int |> Simple.const
    |> fun x -> EPA.Extra_arg.Already_in_scope x
  in
  let tag =
    Extra_param_and_args.update_param_args tag_from_decision rewrite_id
      tag_extra_arg
  in
  let fields_by_tag =
    Tag.Scannable.Map.mapi
      (fun tag_decision (shape, block_fields) ->
        let size = List.length block_fields in
        (* See doc/unboxing.md about invalid constants, poison and aliases. *)
        let poison_const = Const.const_int (Targetint_31_63.of_int 0xbaba) in
        let bak index : Flambda_primitive.Block_access_kind.t =
          match (shape : K.Block_shape.t) with
          | Value_only ->
            Values
              { size = Known (Targetint_31_63.of_int size);
                tag = Known tag_decision;
                field_kind = Any_value
              }
          | Float_record ->
            (* CR vlaviron: I suspect that this case is unreachable. At least
               the previous version of this code didn't handle it, and it seems
               likely that float records were handled by the unique tag and size
               case instead. *)
            Naked_floats { size = Known (Targetint_31_63.of_int size) }
          | Mixed_record shape ->
            let field_kind =
              let field_kind =
                (K.Mixed_block_shape.field_kinds shape).(index)
              in
              if index < K.Mixed_block_shape.value_prefix_size shape
              then
                (* CR vlaviron: we're not trying to infer if this can only be an
                   immediate. In most cases it should be fine, as the primitive
                   will get simplified away. *)
                P.Mixed_block_access_field_kind.Value_prefix Any_value
              else P.Mixed_block_access_field_kind.Flat_suffix field_kind
            in
            Mixed
              { tag = Known tag_decision;
                size = Known (Targetint_31_63.of_int size);
                shape;
                field_kind
              }
        in
        let new_fields_decisions, _ =
          List.fold_left
            (fun (new_decisions, field_nth)
                 ({ epa; decision; kind } : U.field_decision) ->
              let bak = bak (Targetint_31_63.to_int field_nth) in
              let new_extra_arg, new_arg_being_unboxed =
                if are_there_non_const_ctors_at_use
                   && Tag.Scannable.equal tag_at_use_site tag_decision
                then
                  let unboxer =
                    Unboxers.Field.unboxer ~poison_const bak ~index:field_nth
                  in
                  unbox_arg unboxer ~typing_env_at_use arg_being_unboxed
                else
                  ( EPA.Extra_arg.Already_in_scope (Simple.const poison_const),
                    Poison )
              in
              let epa =
                Extra_param_and_args.update_param_args epa rewrite_id
                  new_extra_arg
              in
              let decision =
                compute_extra_args_for_one_decision_and_use ~pass rewrite_id
                  ~typing_env_at_use new_arg_being_unboxed decision
              in
              let field_decision : U.field_decision = { epa; decision; kind } in
              let new_decisions = field_decision :: new_decisions in
              new_decisions, Targetint_31_63.(add one field_nth))
            ([], Targetint_31_63.zero) block_fields
        in
        shape, List.rev new_fields_decisions)
      fields_by_tag_from_decision
  in
  Unbox (Variant { tag; const_ctors; fields_by_tag })

let add_extra_params_and_args extra_params_and_args ~invalids decision =
  let rec aux extra_params_and_args (decision : U.decision) =
    match decision with
    | Do_not_unbox _ -> extra_params_and_args
    | Unbox (Unique_tag_and_size { tag = _; shape = _; fields }) ->
      List.fold_left
        (fun extra_params_and_args ({ epa; decision; kind } : U.field_decision) ->
          let extra_param = BP.create epa.param kind in
          let extra_params_and_args =
            EPA.add extra_params_and_args ~invalids ~extra_param
              ~extra_args:epa.args
          in
          aux extra_params_and_args decision)
        extra_params_and_args fields
    | Unbox (Closure_single_entry { function_slot = _; vars_within_closure }) ->
      Value_slot.Map.fold
        (fun _ ({ epa; decision; kind } : U.field_decision)
             extra_params_and_args ->
          let extra_param = BP.create epa.param kind in
          let extra_params_and_args =
            EPA.add extra_params_and_args ~invalids ~extra_param
              ~extra_args:epa.args
          in
          aux extra_params_and_args decision)
        vars_within_closure extra_params_and_args
    | Unbox (Variant { tag; const_ctors; fields_by_tag }) ->
      let extra_params_and_args =
        Tag.Scannable.Map.fold
          (fun _ (_shape, block_fields) extra_params_and_args ->
            List.fold_left
              (fun extra_params_and_args
                   ({ epa; decision; kind } : U.field_decision) ->
                let extra_param = BP.create epa.param kind in
                let extra_params_and_args =
                  EPA.add extra_params_and_args ~invalids ~extra_param
                    ~extra_args:epa.args
                in
                aux extra_params_and_args decision)
              extra_params_and_args block_fields)
          fields_by_tag extra_params_and_args
      in
      let extra_params_and_args =
        match const_ctors with
        | Zero -> extra_params_and_args
        | At_least_one { is_int; ctor = Do_not_unbox _; _ } ->
          let extra_param =
            BP.create is_int.param K.With_subkind.naked_immediate
          in
          EPA.add extra_params_and_args ~invalids ~extra_param
            ~extra_args:is_int.args
        | At_least_one { is_int; ctor = Unbox (Number (Naked_immediate, ctor)) }
          ->
          let extra_param =
            BP.create is_int.param K.With_subkind.naked_immediate
          in
          let extra_params_and_args =
            EPA.add extra_params_and_args ~invalids ~extra_param
              ~extra_args:is_int.args
          in
          let extra_param =
            BP.create ctor.param K.With_subkind.naked_immediate
          in
          EPA.add extra_params_and_args ~invalids ~extra_param
            ~extra_args:ctor.args
        | At_least_one
            { ctor =
                Unbox
                  ( Unique_tag_and_size _ | Variant _ | Closure_single_entry _
                  | Number
                      ( ( Naked_float32 | Naked_float | Naked_int32
                        | Naked_int64 | Naked_vec128 | Naked_nativeint ),
                        _ ) );
              is_int = _
            } ->
          Misc.fatal_errorf
            "Trying to unbox the constant constructor of a variant with a kind \
             other than Naked_immediate."
      in
      let extra_param = BP.create tag.param K.With_subkind.naked_immediate in
      EPA.add extra_params_and_args ~invalids ~extra_param ~extra_args:tag.args
    | Unbox (Number (naked_number_kind, epa)) ->
      let kind_with_subkind =
        K.With_subkind.of_naked_number_kind naked_number_kind
      in
      let extra_param = BP.create epa.param kind_with_subkind in
      EPA.add extra_params_and_args ~invalids ~extra_param ~extra_args:epa.args
  in
  aux extra_params_and_args decision
