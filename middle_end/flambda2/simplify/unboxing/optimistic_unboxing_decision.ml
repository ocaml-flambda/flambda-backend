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

let pp_tag print_tag ppf tag =
  if print_tag then Format.fprintf ppf "_%d" (Tag.to_int tag)

(* Internal control knobs *)
let unbox_numbers = true

let unbox_blocks = true

let unbox_variants = true

let unbox_closures = true

let make_optimistic_const_ctor () : U.const_ctors_decision =
  let is_int = Extra_param_and_args.create ~name:"is_int" in
  let unboxed_const_ctor =
    Extra_param_and_args.create ~name:"unboxed_const_ctor"
  in
  let ctor = U.Unbox (Number (Naked_immediate, unboxed_const_ctor)) in
  At_least_one { is_int; ctor }

let make_optimistic_number_decision tenv param_type
    (decider : Unboxers.number_decider) : U.decision option =
  match decider.prove_is_a_boxed_number tenv param_type with
  | Proved () ->
    let naked_number = Extra_param_and_args.create ~name:decider.param_name in
    Some (Unbox (Number (decider.kind, naked_number)))
  | Unknown -> None

let decide tenv param_type deciders : U.decision option =
  List.find_map (make_optimistic_number_decision tenv param_type) deciders

let deciders =
  [ Unboxers.Immediate.decider;
    Unboxers.Float.decider;
    Unboxers.Float32.decider;
    Unboxers.Int32.decider;
    Unboxers.Int64.decider;
    Unboxers.Nativeint.decider;
    Unboxers.Vec128.decider ]

let rec make_optimistic_decision ~depth ~recursive tenv ~param_type : U.decision
    =
  let param_type_is_alias_to_symbol =
    (* The parameter types will have been computed from the types of the
       continuation's arguments at the use site(s), which in turn will have been
       computed from simplified [Simple]s. As such we shouldn't need to
       canonicalise any alias again here. *)
    T.is_alias_to_a_symbol param_type
  in
  let param_kind_is_not_value =
    match T.kind param_type with
    | Value -> false
    | Naked_number _ | Region | Rec_info -> true
  in
  if param_type_is_alias_to_symbol
  then Do_not_unbox Not_beneficial
  else if param_kind_is_not_value
  then Do_not_unbox Not_of_kind_value
  else
    match decide tenv param_type deciders with
    | Some decision ->
      if unbox_numbers then decision else Do_not_unbox Incomplete_parameter_type
    | None -> (
      if depth >= Flambda_features.Expert.max_unboxing_depth ()
      then Do_not_unbox Max_depth_exceeded
      else
        match T.prove_unique_tag_and_size tenv param_type with
        | Proved (tag, shape, size) when unbox_blocks -> (
          let fields =
            make_optimistic_fields ~add_tag_to_name:false ~depth ~recursive tenv
              param_type tag shape size
          in
          match fields with
          | Some fields -> Unbox (Unique_tag_and_size { tag; shape; fields })
          | None -> Do_not_unbox All_fields_invalid)
        | Proved _ | Unknown -> (
          match T.prove_variant_like tenv param_type with
          | Proved { const_ctors; non_const_ctors_with_sizes }
            when unbox_variants && not recursive -> (
            let tag = Extra_param_and_args.create ~name:"tag" in
            let const_ctors : U.const_ctors_decision =
              match const_ctors with
              | Known set when Targetint_31_63.Set.is_empty set -> Zero
              | Unknown | Known _ -> make_optimistic_const_ctor ()
            in
            let fields_by_tag =
              Tag.Scannable.Map.filter_map
                (fun scannable_tag (size, shape) ->
                  let tag = Tag.Scannable.to_tag scannable_tag in
                  match
                    make_optimistic_fields ~add_tag_to_name:true ~depth
                      ~recursive tenv param_type tag shape size
                  with
                  | None -> None
                  | Some decision -> Some (shape, decision))
                non_const_ctors_with_sizes
            in
            if Tag.Scannable.Map.is_empty fields_by_tag
            then Do_not_unbox All_fields_invalid
            else
              match
                const_ctors, Tag.Scannable.Map.get_singleton fields_by_tag
              with
              | Zero, Some (scannable_tag, (shape, fields)) ->
                let tag = Tag.Scannable.to_tag scannable_tag in
                Unbox (Unique_tag_and_size { tag; shape; fields })
              | (Zero | At_least_one _), _ ->
                Unbox (Variant { tag; const_ctors; fields_by_tag }))
          | Proved _ | Unknown -> (
            match T.prove_single_closures_entry tenv param_type with
            | Proved (function_slot, _, closures_entry, _fun_decl)
              when unbox_closures && not recursive ->
              let vars_within_closure =
                make_optimistic_vars_within_closure ~depth ~recursive tenv
                  closures_entry
              in
              Unbox
                (Closure_single_entry { function_slot; vars_within_closure })
            | Proved _ | Unknown -> Do_not_unbox Incomplete_parameter_type)))

and make_optimistic_fields ~add_tag_to_name ~depth ~recursive tenv param_type
    (tag : Tag.t) (shape : K.Block_shape.t) size =
  let field_base_name =
    match shape with
    | Scannable Value_only -> "unboxed_field"
    | Scannable (Mixed_record _) -> "unboxed_mixed_field"
    | Float_record -> "unboxed_float_field"
  in
  let field_name n =
    Format.asprintf "%s%a_%d" field_base_name (pp_tag add_tag_to_name) tag n
  in
  let field_vars =
    List.init (Targetint_31_63.to_int size) (fun i ->
        Extra_param_and_args.create ~name:(field_name i))
  in
  let type_of_var index (epa : Extra_param_and_args.t) =
    T.alias_type_of
      (K.Block_shape.element_kind shape index)
      (Simple.var epa.param)
  in
  let field_types = List.mapi type_of_var field_vars in
  let tenv =
    Misc.Stdlib.List.fold_lefti
      (fun index acc { Extra_param_and_args.param = var; args = _ } ->
        let name = Bound_name.create (Name.var var) Name_mode.normal in
        TE.add_definition acc name (K.Block_shape.element_kind shape index))
      tenv field_vars
  in
  let shape =
    T.immutable_block ~is_unique:false tag ~shape ~fields:field_types
      (Alloc_mode.For_types.unknown ())
  in
  match T.meet tenv param_type shape with
  | Bottom -> None
  | exception (Misc.Fatal_error as exn) ->
    Format.eprintf
      "Context is meet of type: %a@\nwith shape: %a@\nin env: @\n%a@." T.print
      param_type T.print shape TE.print tenv;
    raise exn
  | Ok (_, env_extension) ->
    let tenv = TE.add_env_extension tenv env_extension in
    let fields =
      List.map2
        (fun epa var_type : U.field_decision ->
          let decision =
            make_optimistic_decision ~depth:(depth + 1) ~recursive tenv
              ~param_type:var_type
          in
          { epa; decision; kind = K.With_subkind.anything (T.kind var_type) })
        field_vars field_types
    in
    Some fields

and make_optimistic_vars_within_closure ~depth ~recursive tenv closures_entry =
  let map = T.Closures_entry.value_slot_types closures_entry in
  Value_slot.Map.mapi
    (fun value_slot var_type : U.field_decision ->
      let epa =
        Extra_param_and_args.create ~name:(Value_slot.to_string value_slot)
      in
      let decision =
        make_optimistic_decision ~depth:(depth + 1) ~recursive tenv
          ~param_type:var_type
      in
      let kind =
        K.With_subkind.create
          (Flambda2_types.kind var_type)
          K.With_subkind.Subkind.Anything
      in
      { epa; decision; kind })
    map
