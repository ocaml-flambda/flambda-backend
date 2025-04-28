(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Basile Clément, OCamlPro                         *)
(*                                                                        *)
(*   Copyright 2025 OCamlPro SAS                                          *)
(*   Copyright 2025 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module TE = Typing_env
module TEE = Typing_env_extension
module TG = Type_grammar
module ET = Expand_head.Expanded_type

type renaming =
  { mutable left_renaming : Variable.t Variable.Map.t;
    mutable right_renaming : Variable.t Variable.Map.t
  }

let create_renaming () =
  { left_renaming = Variable.Map.empty; right_renaming = Variable.Map.empty }

let link_and_check renaming var1 var2 check =
  match Variable.Map.find_opt var1 renaming.left_renaming with
  | Some var1' -> Variable.equal var1' var2
  | None -> (
    match Variable.Map.find_opt var2 renaming.right_renaming with
    | Some _ -> false
    | None ->
      renaming.left_renaming
        <- Variable.Map.add var1 var2 renaming.left_renaming;
      renaming.right_renaming
        <- Variable.Map.add var2 var1 renaming.right_renaming;
      check ())

type env =
  { parent_env : TE.t;
    left_env : TE.t;
    right_env : TE.t;
    meet_type : TE.meet_type;
    renaming : renaming
  }

let create_env ~meet_type parent_env left_env right_env =
  { parent_env; left_env; right_env; meet_type; renaming = create_renaming () }

let extension_env env left_env right_env = { env with left_env; right_env }

let add_env_extension env ext1 ext2 =
  extension_env env
    (TE.add_env_extension ~meet_type:env.meet_type env.left_env ext1)
    (TE.add_env_extension ~meet_type:env.meet_type env.right_env ext2)

let add_env_extension_strict env ext1 ext2 =
  ( TE.add_env_extension_strict ~meet_type:env.meet_type env.left_env ext1,
    TE.add_env_extension_strict ~meet_type:env.meet_type env.right_env ext2 )

let exists_in_parent_env env name =
  TE.mem ~min_name_mode:Name_mode.in_types env.parent_env name

let simple_exists_in_parent_env env simple =
  TE.mem_simple ~min_name_mode:Name_mode.in_types env.parent_env simple

let equal_bottom equal (x1 : _ Or_bottom.t) (x2 : _ Or_bottom.t) =
  match x1, x2 with
  | Bottom, Bottom -> true
  | Bottom, Ok _ | Ok _, Bottom -> false
  | Ok x1, Ok x2 -> equal x1 x2

let equal_row_like_index_domain ~equal_lattice (t1 : _ TG.row_like_index_domain)
    (t2 : _ TG.row_like_index_domain) =
  match t1, t2 with
  | Known t1, Known t2 -> equal_lattice t1 t2
  | Known _, At_least _ | At_least _, Known _ -> false
  | At_least t1, At_least t2 -> equal_lattice t1 t2

let equal_row_like_index ~equal_lattice ~equal_shape
    (t1 : (_, _) TG.row_like_index) (t2 : (_, _) TG.row_like_index) =
  equal_row_like_index_domain ~equal_lattice t1.domain t2.domain
  && equal_shape t1.shape t2.shape

let names_with_non_equal_types_env_extension ~equal_type env
    (ext1 : TG.env_extension) (ext2 : TG.env_extension) =
  (* Only consider names that are defined in the parent environment and have a
     new equation in at least one environment.

     Note that there is a loss of precision here with existential variables (not
     present in the parent env): if two existential variables have been (or will
     be!) linked, we don't check that the new types added by each extension for
     the linked variables are equal. *)
  let shared_names =
    Name.Map.merge
      (fun name ty1 ty2 ->
        match ty1, ty2 with
        | (Some ty, _ | _, Some ty) when exists_in_parent_env env name ->
          Some (TG.kind ty)
        | _ -> None)
      (TEE.to_map ext1) (TEE.to_map ext2)
  in
  let env = add_env_extension env ext1 ext2 in
  Name.Map.keys
    (Name.Map.filter
       (fun name kind ->
         let left_ty =
           ET.to_type
             (Expand_head.expand_head env.left_env
                (TG.alias_type_of kind (Simple.name name)))
         in
         let right_ty =
           ET.to_type
             (Expand_head.expand_head env.right_env
                (TG.alias_type_of kind (Simple.name name)))
         in
         not (equal_type env left_ty right_ty))
       shared_names)

let equal_env_extension ~equal_type env ext1 ext2 =
  Name.Set.is_empty
    (names_with_non_equal_types_env_extension ~equal_type env ext1 ext2)

let equal_row_like_case ~equal_type ~equal_maps_to ~equal_lattice ~equal_shape
    env (t1 : (_, _, _) TG.row_like_case) (t2 : (_, _, _) TG.row_like_case) =
  match
    ( TE.add_env_extension_strict env.left_env t1.env_extension
        ~meet_type:env.meet_type,
      TE.add_env_extension_strict env.right_env t2.env_extension
        ~meet_type:env.meet_type )
  with
  | Or_bottom.Bottom, Or_bottom.Bottom -> true
  | Or_bottom.Ok _, Or_bottom.Bottom | Or_bottom.Bottom, Or_bottom.Ok _ -> false
  | Or_bottom.Ok left_env, Or_bottom.Ok right_env ->
    let both_env = extension_env env left_env right_env in
    equal_row_like_index ~equal_lattice ~equal_shape t1.index t2.index
    && equal_maps_to both_env t1.maps_to t2.maps_to
    && equal_env_extension ~equal_type both_env t1.env_extension
         t2.env_extension

let equal_array eq a1 a2 =
  Array.length a1 = Array.length a2 && Array.for_all2 eq a1 a2

let equal_row_like_block_case ~equal_type env (t1 : TG.row_like_block_case)
    (t2 : TG.row_like_block_case) =
  equal_row_like_case ~equal_type ~equal_lattice:TG.Block_size.equal
    ~equal_shape:Flambda_kind.Block_shape.equal
    ~equal_maps_to:(fun env -> equal_array (equal_type env))
    env t1 t2

let equal_row_like_for_blocks ~equal_type env (t1 : TG.row_like_for_blocks)
    (t2 : TG.row_like_for_blocks) =
  Tag.Map.equal
    (Or_unknown.equal (equal_row_like_block_case ~equal_type env))
    t1.known_tags t2.known_tags
  && equal_bottom
       (equal_row_like_block_case ~equal_type env)
       t1.other_tags t2.other_tags
  && Alloc_mode.For_types.equal t1.alloc_mode t2.alloc_mode

let equal_function_slot_indexed_product ~equal_type env
    (t1 : TG.function_slot_indexed_product)
    (t2 : TG.function_slot_indexed_product) =
  Function_slot.Map.equal (equal_type env) t1.function_slot_components_by_index
    t2.function_slot_components_by_index

let equal_value_slot_indexed_product ~equal_type env
    (t1 : TG.value_slot_indexed_product) (t2 : TG.value_slot_indexed_product) =
  Value_slot.Map.equal (equal_type env) t1.value_slot_components_by_index
    t2.value_slot_components_by_index

let equal_function_type ~equal_type env (t1 : TG.function_type)
    (t2 : TG.function_type) =
  Code_id.equal t1.code_id t2.code_id && equal_type env t1.rec_info t2.rec_info

let equal_closures_entry ~equal_type env (t1 : TG.closures_entry)
    (t2 : TG.closures_entry) =
  Function_slot.Map.equal
    (Or_unknown_or_bottom.equal (equal_function_type ~equal_type env))
    t1.function_types t2.function_types
  && equal_function_slot_indexed_product ~equal_type env t1.closure_types
       t2.closure_types
  && equal_value_slot_indexed_product ~equal_type env t1.value_slot_types
       t2.value_slot_types

let equal_row_like_for_closures ~equal_type env (t1 : TG.row_like_for_closures)
    (t2 : TG.row_like_for_closures) =
  let equal_row_like_case =
    equal_row_like_case ~equal_type env
      ~equal_lattice:Set_of_closures_contents.equal
      ~equal_shape:(fun () () -> true)
      ~equal_maps_to:(equal_closures_entry ~equal_type)
  in
  Function_slot.Map.equal equal_row_like_case t1.known_closures
    t2.known_closures
  && equal_bottom equal_row_like_case t1.other_closures t2.other_closures

let equal_array_contents ~equal_type env (t1 : TG.array_contents)
    (t2 : TG.array_contents) =
  match t1, t2 with
  | Mutable, Mutable -> true
  | Mutable, Immutable _ | Immutable _, Mutable -> false
  | Immutable { fields = f1 }, Immutable { fields = f2 } ->
    equal_array (equal_type env) f1 f2

let equal_head_of_kind_value_non_null ~equal_type env
    (t1 : TG.head_of_kind_value_non_null) (t2 : TG.head_of_kind_value_non_null)
    =
  match t1, t2 with
  | Variant t1, Variant t2 -> (
    Bool.equal t1.is_unique t2.is_unique
    &&
    let envs_immediate, envs_block =
      match t1.extensions, t2.extensions with
      | No_extensions, No_extensions ->
        ( (Or_bottom.Ok env.left_env, Or_bottom.Ok env.right_env),
          (Or_bottom.Ok env.left_env, Or_bottom.Ok env.right_env) )
      | Ext { when_immediate; when_block }, No_extensions ->
        ( add_env_extension_strict env when_immediate TEE.empty,
          add_env_extension_strict env when_block TEE.empty )
      | No_extensions, Ext { when_immediate; when_block } ->
        ( add_env_extension_strict env TEE.empty when_immediate,
          add_env_extension_strict env TEE.empty when_block )
      | ( Ext { when_immediate = when_immediate1; when_block = when_block1 },
          Ext { when_immediate = when_immediate2; when_block = when_block2 } )
        ->
        ( add_env_extension_strict env when_immediate1 when_immediate2,
          add_env_extension_strict env when_block1 when_block2 )
    in
    (match envs_immediate with
    | Bottom, Bottom -> true
    | Bottom, Ok _ | Ok _, Bottom -> false
    | Ok left_env, Ok right_env ->
      Or_unknown.equal
        (equal_type (extension_env env left_env right_env))
        t1.immediates t2.immediates)
    &&
    match envs_block with
    | Bottom, Bottom -> true
    | Bottom, Ok _ | Ok _, Bottom -> false
    | Ok left_env, Ok right_env ->
      Or_unknown.equal
        (equal_row_like_for_blocks ~equal_type
           (extension_env env left_env right_env))
        t1.blocks t2.blocks)
  | Mutable_block t1, Mutable_block t2 ->
    Alloc_mode.For_types.equal t1.alloc_mode t2.alloc_mode
  | Boxed_float32 (t1, a1), Boxed_float32 (t2, a2) ->
    equal_type env t1 t2 && Alloc_mode.For_types.equal a1 a2
  | Boxed_float (t1, a1), Boxed_float (t2, a2) ->
    equal_type env t1 t2 && Alloc_mode.For_types.equal a1 a2
  | Boxed_int32 (t1, a1), Boxed_int32 (t2, a2) ->
    equal_type env t1 t2 && Alloc_mode.For_types.equal a1 a2
  | Boxed_int64 (t1, a1), Boxed_int64 (t2, a2) ->
    equal_type env t1 t2 && Alloc_mode.For_types.equal a1 a2
  | Boxed_nativeint (t1, a1), Boxed_nativeint (t2, a2) ->
    equal_type env t1 t2 && Alloc_mode.For_types.equal a1 a2
  | Boxed_vec128 (t1, a1), Boxed_vec128 (t2, a2) ->
    equal_type env t1 t2 && Alloc_mode.For_types.equal a1 a2
  | Closures c1, Closures c2 ->
    equal_row_like_for_closures ~equal_type env c1.by_function_slot
      c2.by_function_slot
    && Alloc_mode.For_types.equal c1.alloc_mode c2.alloc_mode
  | String t1, String t2 -> String_info.Set.equal t1 t2
  | Array t1, Array t2 ->
    Or_unknown_or_bottom.equal Flambda_kind.With_subkind.equal t1.element_kind
      t2.element_kind
    && equal_type env t1.length t2.length
    && Or_unknown.equal
         (equal_array_contents ~equal_type env)
         t1.contents t2.contents
    && Alloc_mode.For_types.equal t1.alloc_mode t2.alloc_mode
  | ( ( Variant _ | Mutable_block _ | Boxed_float _ | Boxed_float32 _
      | Boxed_int32 _ | Boxed_vec128 _ | Boxed_int64 _ | Boxed_nativeint _
      | Closures _ | String _ | Array _ ),
      _ ) ->
    false

let equal_head_of_kind_value ~equal_type env (t1 : TG.head_of_kind_value)
    (t2 : TG.head_of_kind_value) =
  match t1.is_null, t2.is_null with
  | Not_null, Maybe_null | Maybe_null, Not_null -> false
  | Not_null, Not_null | Maybe_null, Maybe_null ->
    Or_unknown_or_bottom.equal
      (equal_head_of_kind_value_non_null ~equal_type env)
      t1.non_null t2.non_null

let equal_head_of_kind_naked_immediate ~equal_type env
    (t1 : TG.head_of_kind_naked_immediate)
    (t2 : TG.head_of_kind_naked_immediate) =
  match t1, t2 with
  | Naked_immediates is1, Naked_immediates is2 ->
    Targetint_31_63.Set.equal is1 is2
  | Is_int t1, Is_int t2 -> equal_type env t1 t2
  | Get_tag t1, Get_tag t2 -> equal_type env t1 t2
  | Is_null t1, Is_null t2 -> equal_type env t1 t2
  | (Naked_immediates _ | Is_int _ | Get_tag _ | Is_null _), _ -> false

let equal_head_of_kind_naked_float32 (t1 : TG.head_of_kind_naked_float32)
    (t2 : TG.head_of_kind_naked_float32) =
  Numeric_types.Float32_by_bit_pattern.Set.equal
    (t1 :> Numeric_types.Float32_by_bit_pattern.Set.t)
    (t2 :> Numeric_types.Float32_by_bit_pattern.Set.t)

let equal_head_of_kind_naked_float (t1 : TG.head_of_kind_naked_float)
    (t2 : TG.head_of_kind_naked_float) =
  Numeric_types.Float_by_bit_pattern.Set.equal
    (t1 :> Numeric_types.Float_by_bit_pattern.Set.t)
    (t2 :> Numeric_types.Float_by_bit_pattern.Set.t)

let equal_head_of_kind_naked_int32 (t1 : TG.head_of_kind_naked_int32)
    (t2 : TG.head_of_kind_naked_int32) =
  Numeric_types.Int32.Set.equal
    (t1 :> Numeric_types.Int32.Set.t)
    (t2 :> Numeric_types.Int32.Set.t)

let equal_head_of_kind_naked_int64 (t1 : TG.head_of_kind_naked_int64)
    (t2 : TG.head_of_kind_naked_int64) =
  Numeric_types.Int64.Set.equal
    (t1 :> Numeric_types.Int64.Set.t)
    (t2 :> Numeric_types.Int64.Set.t)

let equal_head_of_kind_naked_nativeint (t1 : TG.head_of_kind_naked_nativeint)
    (t2 : TG.head_of_kind_naked_nativeint) =
  Targetint_32_64.Set.equal
    (t1 :> Targetint_32_64.Set.t)
    (t2 :> Targetint_32_64.Set.t)

let equal_head_of_kind_naked_vec128 (t1 : TG.head_of_kind_naked_vec128)
    (t2 : TG.head_of_kind_naked_vec128) =
  Vector_types.Vec128.Bit_pattern.Set.equal
    (t1 :> Vector_types.Vec128.Bit_pattern.Set.t)
    (t2 :> Vector_types.Vec128.Bit_pattern.Set.t)

let equal_head_of_kind_rec_info (t1 : TG.head_of_kind_rec_info)
    (t2 : TG.head_of_kind_rec_info) =
  Rec_info_expr.equal t1 t2

let equal_head_of_kind_region (() : TG.head_of_kind_region)
    (() : TG.head_of_kind_region) =
  true

let is_unknown_head_of_kind_value (t : TG.head_of_kind_value) =
  match t.is_null, t.non_null with
  | Maybe_null, Unknown -> true
  | (Not_null | Maybe_null), (Unknown | Bottom | Ok _) -> false

let is_non_obviously_unknown (t : ET.descr) =
  match t with
  | Value head -> is_unknown_head_of_kind_value head
  | Naked_immediate _ | Naked_float32 _ | Naked_float _ | Naked_int32 _
  | Naked_int64 _ | Naked_nativeint _ | Naked_vec128 _ | Rec_info _ | Region _
    ->
    false

let is_bottom_head_of_kind_value (t : TG.head_of_kind_value) =
  match t.is_null, t.non_null with
  | Not_null, Bottom -> true
  | (Not_null | Maybe_null), (Unknown | Bottom | Ok _) -> false

let is_non_obviously_bottom (t : ET.descr) =
  match t with
  | Value head -> is_bottom_head_of_kind_value head
  | Naked_immediate _ | Naked_float32 _ | Naked_float _ | Naked_int32 _
  | Naked_int64 _ | Naked_nativeint _ | Naked_vec128 _ | Rec_info _ | Region _
    ->
    false

let equal_expanded_head ~equal_type env (t1 : ET.t) (t2 : ET.t) =
  match ET.descr t1, ET.descr t2 with
  | Unknown, Unknown -> true
  | Bottom, Bottom -> true
  | Unknown, Ok head | Ok head, Unknown -> is_non_obviously_unknown head
  | Unknown, Bottom | Bottom, Unknown -> false
  | Ok head, Bottom | Bottom, Ok head -> is_non_obviously_bottom head
  | Ok t1, Ok t2 -> (
    match t1, t2 with
    | Value t1, Value t2 -> equal_head_of_kind_value ~equal_type env t1 t2
    | Naked_immediate t1, Naked_immediate t2 ->
      equal_head_of_kind_naked_immediate ~equal_type env t1 t2
    | Naked_float32 t1, Naked_float32 t2 ->
      equal_head_of_kind_naked_float32 t1 t2
    | Naked_float t1, Naked_float t2 -> equal_head_of_kind_naked_float t1 t2
    | Naked_int32 t1, Naked_int32 t2 -> equal_head_of_kind_naked_int32 t1 t2
    | Naked_int64 t1, Naked_int64 t2 -> equal_head_of_kind_naked_int64 t1 t2
    | Naked_nativeint t1, Naked_nativeint t2 ->
      equal_head_of_kind_naked_nativeint t1 t2
    | Naked_vec128 t1, Naked_vec128 t2 -> equal_head_of_kind_naked_vec128 t1 t2
    | Rec_info t1, Rec_info t2 -> equal_head_of_kind_rec_info t1 t2
    | Region t1, Region t2 -> equal_head_of_kind_region t1 t2
    | ( ( Value _ | Naked_immediate _ | Naked_float32 _ | Naked_float _
        | Naked_int32 _ | Naked_int64 _ | Naked_nativeint _ | Naked_vec128 _
        | Rec_info _ | Region _ ),
        _ ) ->
      false)

let rec equal_type env t1 t2 =
  let canonical_simple1 =
    try
      Some
        (TE.get_alias_then_canonical_simple_exn
           ~min_name_mode:Name_mode.in_types env.left_env t1)
    with Not_found -> None
  in
  let canonical_simple2 =
    try
      Some
        (TE.get_alias_then_canonical_simple_exn
           ~min_name_mode:Name_mode.in_types env.right_env t2)
    with Not_found -> None
  in
  match canonical_simple1, canonical_simple2 with
  | Some simple1, Some simple2
    when Simple.equal simple1 simple2 && simple_exists_in_parent_env env simple1
    ->
    true
  | Some simple1, Some simple2 -> (
    match Simple.must_be_var simple1, Simple.must_be_var simple2 with
    | None, None | None, Some _ | Some _, None ->
      (* Allow non-canonical representations of constants. *)
      if Simple.is_const simple1 || Simple.is_const simple2
      then
        equal_expanded_head ~equal_type env
          (Expand_head.expand_head env.left_env t1)
          (Expand_head.expand_head env.right_env t2)
      else false
    | Some (var1, coercion1), Some (var2, coercion2) -> (
      let coercion =
        Coercion.compose_exn coercion1 ~then_:(Coercion.inverse coercion2)
      in
      match
        ( exists_in_parent_env env (Name.var var1),
          exists_in_parent_env env (Name.var var2) )
      with
      | true, true -> Variable.equal var1 var2 && Coercion.is_id coercion
      | true, false | false, true -> false
      | false, false ->
        link_and_check env.renaming var1 var2 (fun () ->
            equal_expanded_head ~equal_type env
              (Expand_head.expand_head env.left_env t1)
              (Expand_head.expand_head env.right_env t2))))
  | (None, Some simple | Some simple, None)
    when (not (Simple.is_const simple))
         && simple_exists_in_parent_env env simple ->
    false
  | None, None | Some _, None | None, Some _ ->
    (* We lose some precision here: if the same named type from one of the
       environments is checked for equality against multiple anonymous types
       from the other environment, we know more equalities in the first
       environment than in the second. This is probably acceptable since this
       check is only intended for debugging. *)
    equal_expanded_head ~equal_type env
      (Expand_head.expand_head env.left_env t1)
      (Expand_head.expand_head env.right_env t2)

let names_with_non_equal_types_level_ignoring_name_mode ~meet_type env level1
    level2 =
  let left_env =
    Typing_env_level.fold_on_defined_vars
      (fun var kind left_env ->
        TE.add_definition left_env
          (Bound_name.create_var (Bound_var.create var Name_mode.in_types))
          kind)
      level1 env
  in
  let right_env =
    Typing_env_level.fold_on_defined_vars
      (fun var kind right_env ->
        TE.add_definition right_env
          (Bound_name.create_var (Bound_var.create var Name_mode.in_types))
          kind)
      level2 env
  in
  names_with_non_equal_types_env_extension ~equal_type
    (create_env ~meet_type env left_env right_env)
    (TEE.from_map (Typing_env_level.equations level1))
    (TEE.from_map (Typing_env_level.equations level2))

let equal_level_ignoring_name_mode ~meet_type env level1 level2 =
  Name.Set.is_empty
    (names_with_non_equal_types_level_ignoring_name_mode ~meet_type env level1
       level2)

let names_with_non_equal_types_env_extension ~meet_type env ext1 ext2 =
  names_with_non_equal_types_env_extension ~equal_type
    (create_env ~meet_type env env env)
    ext1 ext2

let equal_env_extension ~meet_type env ext1 ext2 =
  Name.Set.is_empty
    (names_with_non_equal_types_env_extension ~meet_type env ext1 ext2)

let equal_type ~meet_type env t1 t2 =
  equal_type (create_env ~meet_type env env env) t1 t2
