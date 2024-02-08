(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018--2021 OCamlPro SAS                                    *)
(*   Copyright 2018--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* CR mshinwell: enable warning fragile-match *)

[@@@ocaml.warning "-fragile-match"]

module Float32 = Numeric_types.Float32_by_bit_pattern
module Float = Numeric_types.Float_by_bit_pattern
module Int32 = Numeric_types.Int32
module Int64 = Numeric_types.Int64
module K = Flambda_kind
module TE = Typing_env
module TG = Type_grammar

let is_bottom = Expand_head.is_bottom

let expand_head env ty =
  Expand_head.expand_head env ty |> Expand_head.Expanded_type.descr_oub

let wrong_kind kind_string t =
  Misc.fatal_errorf "Kind error: expected [%s]:@ %a" kind_string TG.print t

type 'a meet_shortcut =
  | Known_result of 'a
  | Need_meet
  | Invalid

type 'a proof_of_property =
  | Proved of 'a
  | Unknown

type 'a generic_proof =
  | Proved of 'a
  | Unknown
  | Invalid

let as_meet_shortcut (p : _ generic_proof) : _ meet_shortcut =
  match p with
  | Proved x -> Known_result x
  | Unknown -> Need_meet
  | Invalid -> Invalid

let as_property (p : _ generic_proof) : _ proof_of_property =
  match p with Proved x -> Proved x | Unknown | Invalid -> Unknown

let prove_equals_to_simple_of_kind_value env t : Simple.t proof_of_property =
  let original_kind = TG.kind t in
  if not (K.equal original_kind K.value)
  then wrong_kind "Value" t
  else
    (* CR pchambart: add TE.get_alias_opt *)
    match TG.get_alias_exn t with
    | exception Not_found ->
      (* CR vlaviron: We could try to turn singleton types into constants here.
         I think there are already a few hacks to generate constant aliases
         instead of singleton types when possible, so we might never end up here
         with a singleton type in practice. *)
      Unknown
    | simple -> (
      match
        TE.get_canonical_simple_exn env simple ~min_name_mode:Name_mode.normal
      with
      | exception Not_found -> Unknown
      | simple -> Proved simple)

(* Note: this function is used for simplifying Obj.is_int, so should not assume
   that the argument represents a variant, unless [variant_only] is [true] *)
let prove_is_int_generic ~variant_only env t : bool generic_proof =
  match expand_head env t with
  | Value (Ok (Variant blocks_imms)) -> (
    match blocks_imms.blocks, blocks_imms.immediates with
    | Unknown, Unknown -> Unknown
    | Unknown, Known imms ->
      if is_bottom env imms then Proved false else Unknown
    | Known blocks, Unknown ->
      if TG.Row_like_for_blocks.is_bottom blocks then Proved true else Unknown
    | Known blocks, Known imms ->
      if TG.Row_like_for_blocks.is_bottom blocks
      then if is_bottom env imms then Invalid else Proved true
      else if is_bottom env imms
      then Proved false
      else Unknown)
  | Value (Ok (Mutable_block _)) -> Proved false
  | Value
      (Ok
        ( Boxed_float _ | Boxed_float32 _ | Boxed_int32 _ | Boxed_int64 _
        | Boxed_vec128 _ | Boxed_nativeint _ | Closures _ | String _ | Array _
          )) ->
    if variant_only then Invalid else Proved false
  | Value Unknown -> Unknown
  | Value Bottom -> Invalid
  | Naked_immediate _ | Naked_float _ | Naked_float32 _ | Naked_int32 _
  | Naked_int64 _ | Naked_nativeint _ | Naked_vec128 _ | Rec_info _ | Region _
    ->
    wrong_kind "Value" t

let prove_is_int env t =
  as_property (prove_is_int_generic ~variant_only:false env t)

let meet_is_int_variant_only env t =
  as_meet_shortcut (prove_is_int_generic ~variant_only:true env t)

(* Note: this function returns a generic proof because we want to propagate the
   Invalid cases to prove_naked_immediates_generic, but it's not suitable for
   implementing [meet_get_tag] because it doesn't ignore the immediates part of
   the variant. *)
(* CR vlaviron: Switch result to Tag.Scannable *)
let prove_get_tag_generic env t : Tag.Set.t generic_proof =
  match expand_head env t with
  | Value (Ok (Variant blocks_imms)) -> (
    match blocks_imms.immediates with
    | Unknown -> Unknown
    | Known imms -> (
      if not (is_bottom env imms)
      then Unknown
      else
        match blocks_imms.blocks with
        | Unknown -> Unknown
        | Known blocks -> (
          (* CR mshinwell: maybe [all_tags] should return the [Invalid] case
             directly? *)
          match TG.Row_like_for_blocks.all_tags blocks with
          | Unknown -> Unknown
          | Known tags -> if Tag.Set.is_empty tags then Invalid else Proved tags
          )))
  | Value
      (Ok
        ( Boxed_float _ | Boxed_float32 _ | Boxed_int32 _ | Boxed_int64 _
        | Boxed_nativeint _ | Boxed_vec128 _ )) ->
    Unknown
  | Value (Ok (Mutable_block _)) -> Unknown
  | Value (Ok (Closures _)) -> Unknown
  | Value (Ok (String _)) -> Unknown
  | Value (Ok (Array _)) -> Unknown
  | Value Unknown -> Unknown
  | Value Bottom -> Invalid
  | Naked_immediate _ | Naked_float _ | Naked_float32 _ | Naked_int32 _
  | Naked_int64 _ | Naked_vec128 _ | Naked_nativeint _ | Rec_info _ | Region _
    ->
    wrong_kind "Value" t

let prove_get_tag env t = as_property (prove_get_tag_generic env t)

let prove_naked_immediates_generic env t : Targetint_31_63.Set.t generic_proof =
  match expand_head env t with
  | Naked_immediate (Ok { immediates = Known is; relations = _ }) ->
    (* No attempt at reduction *)
    if Targetint_31_63.Set.is_empty is then Invalid else Proved is
  | Naked_immediate (Ok { immediates = Unknown; relations }) ->
    (* If we have no known set but some relations, try one step of reduction to
       get a better result *)
    let refine_proof_with_set proof is : _ generic_proof =
      let set_to_proof is : _ generic_proof =
        if Targetint_31_63.Set.is_empty is then Invalid else Proved is
      in
      match proof with
      | Invalid -> Invalid
      | Unknown -> set_to_proof is
      | Proved prev -> set_to_proof (Targetint_31_63.Set.inter prev is)
    in
    let refine_with_is_int (proof : _ generic_proof) name =
      let is_int_set =
        let scrutinee_ty = TE.find env name (Some Flambda_kind.value) in
        match prove_is_int_generic ~variant_only:true env scrutinee_ty with
        | Proved true -> Targetint_31_63.Set.singleton Targetint_31_63.bool_true
        | Proved false ->
          Targetint_31_63.Set.singleton Targetint_31_63.bool_false
        | Unknown ->
          Targetint_31_63.Set.of_list
            [Targetint_31_63.bool_false; Targetint_31_63.bool_true]
        | Invalid -> Targetint_31_63.Set.empty
      in
      refine_proof_with_set proof is_int_set
    in
    let refine_with_get_tag (proof : _ generic_proof) name =
      let block_ty = TE.find env name (Some Flambda_kind.value) in
      match prove_get_tag_generic env block_ty with
      | Proved tags ->
        let tag_set =
          Tag.Set.fold
            (fun tag is ->
              Targetint_31_63.Set.add (Tag.to_targetint_31_63 tag) is)
            tags Targetint_31_63.Set.empty
        in
        refine_proof_with_set proof tag_set
      | Unknown -> proof
      | Invalid -> Invalid
    in
    let refine (rel : TG.relation) proof =
      match rel with
      | Is_int name -> refine_with_is_int proof name
      | Get_tag name -> refine_with_get_tag proof name
    in
    TG.RelationSet.fold refine relations (Unknown : _ generic_proof)
  | Naked_immediate Unknown -> Unknown
  | Naked_immediate Bottom -> Invalid
  | Value _ | Naked_float _ | Naked_float32 _ | Naked_int32 _ | Naked_int64 _
  | Naked_nativeint _ | Naked_vec128 _ | Rec_info _ | Region _ ->
    wrong_kind "Naked_immediate" t

let meet_naked_immediates env t =
  as_meet_shortcut (prove_naked_immediates_generic env t)

let prove_naked_immediates env t =
  as_property (prove_naked_immediates_generic env t)

let prove_equals_tagged_immediates env t : _ proof_of_property =
  match expand_head env t with
  | Value (Ok (Variant { immediates; blocks; is_unique = _ })) -> (
    match blocks with
    | Unknown -> Unknown
    | Known blocks ->
      if TG.Row_like_for_blocks.is_bottom blocks
      then
        match immediates with
        | Unknown -> Unknown
        | Known imms -> (
          match prove_naked_immediates_generic env imms with
          | Proved imms -> Proved imms
          | Invalid -> Proved Targetint_31_63.Set.empty
          | Unknown -> Unknown)
      else Unknown)
  | Value (Ok _ | Unknown | Bottom) -> Unknown
  | Naked_immediate _ | Naked_float _ | Naked_float32 _ | Naked_int32 _
  | Naked_int64 _ | Naked_vec128 _ | Naked_nativeint _ | Rec_info _ | Region _
    ->
    wrong_kind "Value" t

let meet_equals_tagged_immediates env t : _ meet_shortcut =
  match expand_head env t with
  | Value (Ok (Variant { immediates; blocks = _; is_unique = _ })) -> (
    match immediates with
    | Unknown -> Need_meet
    | Known imms -> meet_naked_immediates env imms)
  | Value
      (Ok
        ( Mutable_block _ | Boxed_float _ | Boxed_float32 _ | Boxed_int32 _
        | Boxed_int64 _ | Boxed_vec128 _ | Boxed_nativeint _ | Closures _
        | String _ | Array _ )) ->
    Invalid
  | Value Unknown -> Need_meet
  | Value Bottom
  | Naked_immediate _ | Naked_float _ | Naked_float32 _ | Naked_int32 _
  | Naked_int64 _ | Naked_vec128 _ | Naked_nativeint _ | Rec_info _ | Region _
    ->
    Invalid

let meet_equals_single_tagged_immediate env t : _ meet_shortcut =
  match meet_equals_tagged_immediates env t with
  | Known_result imms -> (
    match Targetint_31_63.Set.get_singleton imms with
    | Some imm -> Known_result imm
    | None -> Need_meet)
  | Need_meet -> Need_meet
  | Invalid -> Invalid

type _ meet_naked_number_kind =
  | Float32 : Float32.Set.t meet_naked_number_kind
  | Float : Float.Set.t meet_naked_number_kind
  | Int32 : Int32.Set.t meet_naked_number_kind
  | Int64 : Int64.Set.t meet_naked_number_kind
  | Nativeint : Targetint_32_64.Set.t meet_naked_number_kind
  | Vec128 : Vector_types.Vec128.Bit_pattern.Set.t meet_naked_number_kind

let[@inline] meet_naked_number (type a) (kind : a meet_naked_number_kind) env t
    : a meet_shortcut =
  let head_to_proof (head : _ Or_unknown_or_bottom.t) coercion ~is_empty :
      _ meet_shortcut =
    match head with
    | Ok set ->
      let set = coercion set in
      if is_empty set then Invalid else Known_result set
    | Unknown -> Need_meet
    | Bottom -> Invalid
  in
  let wrong_kind () =
    let kind_string =
      match kind with
      | Float32 -> "Naked_float32"
      | Float -> "Naked_float"
      | Int32 -> "Naked_int32"
      | Int64 -> "Naked_int64"
      | Nativeint -> "Naked_nativeint"
      | Vec128 -> "Naked_vec128"
    in
    wrong_kind kind_string t
  in
  match expand_head env t with
  | Value _ -> wrong_kind ()
  | Naked_immediate _ -> wrong_kind ()
  | Rec_info _ -> wrong_kind ()
  | Region _ -> wrong_kind ()
  | Naked_float32 fs -> (
    match kind with
    | Float32 ->
      head_to_proof fs
        (fun (fs : TG.head_of_kind_naked_float32) -> (fs :> Float32.Set.t))
        ~is_empty:Float32.Set.is_empty
    | _ -> wrong_kind ())
  | Naked_float fs -> (
    match kind with
    | Float ->
      head_to_proof fs
        (fun (fs : TG.head_of_kind_naked_float) -> (fs :> Float.Set.t))
        ~is_empty:Float.Set.is_empty
    | _ -> wrong_kind ())
  | Naked_int32 is -> (
    match kind with
    | Int32 ->
      head_to_proof is
        (fun (is : TG.head_of_kind_naked_int32) -> (is :> Int32.Set.t))
        ~is_empty:Int32.Set.is_empty
    | _ -> wrong_kind ())
  | Naked_int64 is -> (
    match kind with
    | Int64 ->
      head_to_proof is
        (fun (is : TG.head_of_kind_naked_int64) -> (is :> Int64.Set.t))
        ~is_empty:Int64.Set.is_empty
    | _ -> wrong_kind ())
  | Naked_nativeint is -> (
    match kind with
    | Nativeint ->
      head_to_proof is
        (fun (is : TG.head_of_kind_naked_nativeint) ->
          (is :> Targetint_32_64.Set.t))
        ~is_empty:Targetint_32_64.Set.is_empty
    | _ -> wrong_kind ())
  | Naked_vec128 vs -> (
    match kind with
    | Vec128 ->
      head_to_proof vs
        (fun (fs : TG.head_of_kind_naked_vec128) ->
          (fs :> Vector_types.Vec128.Bit_pattern.Set.t))
        ~is_empty:Vector_types.Vec128.Bit_pattern.Set.is_empty
    | _ -> wrong_kind ())

let meet_naked_float32s = meet_naked_number Float32

let meet_naked_floats = meet_naked_number Float

let meet_naked_int32s = meet_naked_number Int32

let meet_naked_int64s = meet_naked_number Int64

let meet_naked_nativeints = meet_naked_number Nativeint

let meet_naked_vec128s = meet_naked_number Vec128

type variant_like_proof =
  { const_ctors : Targetint_31_63.Set.t Or_unknown.t;
    non_const_ctors_with_sizes : Targetint_31_63.t Tag.Scannable.Map.t
  }

let prove_variant_like_generic env t : variant_like_proof generic_proof =
  match expand_head env t with
  | Value (Ok (Variant blocks_imms)) -> (
    match blocks_imms.blocks with
    | Unknown -> Unknown
    | Known blocks -> (
      match TG.Row_like_for_blocks.all_tags_and_sizes blocks with
      | Unknown -> Unknown
      | Known non_const_ctors_with_sizes -> (
        let non_const_ctors_with_sizes =
          (* CR pchambart: we could ignore non-scannable tags for the meet_
             version *)
          Tag.Map.fold
            (fun tag size (result : _ Or_unknown.t) : _ Or_unknown.t ->
              match result with
              | Unknown -> Unknown
              | Known result -> (
                match Tag.Scannable.of_tag tag with
                | None -> Unknown
                | Some tag -> Known (Tag.Scannable.Map.add tag size result)))
            non_const_ctors_with_sizes
            (Or_unknown.Known Tag.Scannable.Map.empty)
        in
        match non_const_ctors_with_sizes with
        | Unknown -> Unknown
        | Known non_const_ctors_with_sizes ->
          let const_ctors : _ Or_unknown.t =
            match blocks_imms.immediates with
            | Unknown -> Unknown
            | Known imms -> (
              match prove_naked_immediates_generic env imms with
              | Unknown -> Unknown
              | Invalid -> Known Targetint_31_63.Set.empty
              | Proved const_ctors -> Known const_ctors)
          in
          Proved { const_ctors; non_const_ctors_with_sizes })))
  | Value (Ok (Mutable_block _)) -> Unknown
  | Value (Ok (Array _)) ->
    Unknown (* We could return Invalid in a strict mode *)
  | Value
      (Ok
        ( Closures _ | Boxed_float _ | Boxed_float32 _ | Boxed_int32 _
        | Boxed_int64 _ | Boxed_vec128 _ | Boxed_nativeint _ | String _ )) ->
    Invalid
  | Value Unknown -> Unknown
  | Value Bottom -> Invalid
  | Naked_immediate _ | Naked_float _ | Naked_float32 _ | Naked_int32 _
  | Naked_int64 _ | Naked_vec128 _ | Naked_nativeint _ | Rec_info _ | Region _
    ->
    wrong_kind "Value" t

let meet_variant_like env t =
  as_meet_shortcut (prove_variant_like_generic env t)

let prove_variant_like env t = as_property (prove_variant_like_generic env t)

type boxed_or_tagged_number =
  | Boxed of
      Alloc_mode.For_types.t * Flambda_kind.Boxable_number.t * Type_grammar.t
  | Tagged_immediate

(* CR pchambart: Remove fragile matchs and reuse this function *)
let prove_is_a_boxed_or_tagged_number env t :
    boxed_or_tagged_number proof_of_property =
  match expand_head env t with
  | Value Unknown -> Unknown
  | Value (Ok (Variant { blocks; immediates = _; is_unique = _ })) -> (
    match blocks with
    | Unknown -> Unknown
    | Known blocks ->
      if TG.Row_like_for_blocks.is_bottom blocks
      then Proved Tagged_immediate
      else Unknown)
  | Value (Ok (Boxed_float (contents_ty, alloc_mode))) ->
    Proved (Boxed (alloc_mode, Naked_float, contents_ty))
  | Value (Ok (Boxed_float32 (contents_ty, alloc_mode))) ->
    Proved (Boxed (alloc_mode, Naked_float32, contents_ty))
  | Value (Ok (Boxed_int32 (contents_ty, alloc_mode))) ->
    Proved (Boxed (alloc_mode, Naked_int32, contents_ty))
  | Value (Ok (Boxed_int64 (contents_ty, alloc_mode))) ->
    Proved (Boxed (alloc_mode, Naked_int64, contents_ty))
  | Value (Ok (Boxed_nativeint (contents_ty, alloc_mode))) ->
    Proved (Boxed (alloc_mode, Naked_nativeint, contents_ty))
  | Value (Ok (Boxed_vec128 (contents_ty, alloc_mode))) ->
    Proved (Boxed (alloc_mode, Naked_vec128, contents_ty))
  | Value (Bottom | Ok (Mutable_block _ | Closures _ | String _ | Array _)) ->
    Unknown
  | Naked_immediate _ | Naked_float _ | Naked_float32 _ | Naked_int32 _
  | Naked_int64 _ | Naked_vec128 _ | Naked_nativeint _ | Rec_info _ | Region _
    ->
    wrong_kind "Value" t

let prove_is_a_tagged_immediate env t : _ proof_of_property =
  match prove_is_a_boxed_or_tagged_number env t with
  | Proved Tagged_immediate -> Proved ()
  | Proved _ -> Unknown
  | Unknown -> Unknown

let prove_is_a_boxed_float32 env t : _ proof_of_property =
  match expand_head env t with
  | Value Unknown -> Unknown
  | Value (Ok (Boxed_float32 _)) -> Proved ()
  | Value _ -> Unknown
  | Naked_immediate _ | Naked_float _ | Naked_float32 _ | Naked_int32 _
  | Naked_int64 _ | Naked_vec128 _ | Naked_nativeint _ | Rec_info _ | Region _
    ->
    wrong_kind "Value" t

let prove_is_a_boxed_float env t : _ proof_of_property =
  match expand_head env t with
  | Value Unknown -> Unknown
  | Value (Ok (Boxed_float _)) -> Proved ()
  | Value _ -> Unknown
  | Naked_immediate _ | Naked_float _ | Naked_float32 _ | Naked_int32 _
  | Naked_int64 _ | Naked_vec128 _ | Naked_nativeint _ | Rec_info _ | Region _
    ->
    wrong_kind "Value" t

let prove_is_or_is_not_a_boxed_float env t : _ proof_of_property =
  match expand_head env t with
  | Value Unknown -> Unknown
  | Value Bottom -> Unknown
  | Value (Ok (Boxed_float _)) -> Proved true
  | Value (Ok _) -> Proved false
  | Naked_immediate _ | Naked_float _ | Naked_float32 _ | Naked_int32 _
  | Naked_int64 _ | Naked_vec128 _ | Naked_nativeint _ | Rec_info _ | Region _
    ->
    wrong_kind "Value" t

let prove_is_a_boxed_int32 env t : _ proof_of_property =
  match expand_head env t with
  | Value Unknown -> Unknown
  | Value (Ok (Boxed_int32 _)) -> Proved ()
  | Value _ -> Unknown
  | Naked_immediate _ | Naked_float _ | Naked_float32 _ | Naked_int32 _
  | Naked_int64 _ | Naked_vec128 _ | Naked_nativeint _ | Rec_info _ | Region _
    ->
    wrong_kind "Value" t

let prove_is_a_boxed_int64 env t : _ proof_of_property =
  match expand_head env t with
  | Value Unknown -> Unknown
  | Value (Ok (Boxed_int64 _)) -> Proved ()
  | Value _ -> Unknown
  | Naked_immediate _ | Naked_float _ | Naked_float32 _ | Naked_int32 _
  | Naked_int64 _ | Naked_vec128 _ | Naked_nativeint _ | Rec_info _ | Region _
    ->
    wrong_kind "Value" t

let prove_is_a_boxed_nativeint env t : _ proof_of_property =
  match expand_head env t with
  | Value Unknown -> Unknown
  | Value (Ok (Boxed_nativeint _)) -> Proved ()
  | Value _ -> Unknown
  | Naked_immediate _ | Naked_float _ | Naked_float32 _ | Naked_int32 _
  | Naked_int64 _ | Naked_vec128 _ | Naked_nativeint _ | Rec_info _ | Region _
    ->
    wrong_kind "Value" t

let prove_is_a_boxed_vec128 env t : _ proof_of_property =
  match expand_head env t with
  | Value Unknown -> Unknown
  | Value (Ok (Boxed_vec128 _)) -> Proved ()
  | Value _ -> Unknown
  | Naked_immediate _ | Naked_float _ | Naked_float32 _ | Naked_int32 _
  | Naked_int64 _ | Naked_vec128 _ | Naked_nativeint _ | Rec_info _ | Region _
    ->
    wrong_kind "Value" t

let prove_unique_tag_and_size0 env t :
    (Tag_and_size.t * TG.Product.Int_indexed.t * Alloc_mode.For_types.t)
    proof_of_property =
  match expand_head env t with
  | Value (Ok (Variant blocks_imms)) -> (
    match blocks_imms.immediates with
    | Unknown -> Unknown
    | Known immediates ->
      if is_bottom env immediates
      then
        match blocks_imms.blocks with
        | Unknown -> Unknown
        | Known blocks -> (
          match TG.Row_like_for_blocks.get_singleton blocks with
          | None -> Unknown
          | Some (tag_and_size, product, alloc_mode) ->
            Proved (tag_and_size, product, alloc_mode))
      else Unknown)
  | Value (Ok (Mutable_block _)) | Value (Ok _) | Value Unknown | Value Bottom
    ->
    Unknown
  | Naked_immediate _ | Naked_float _ | Naked_float32 _ | Naked_int32 _
  | Naked_int64 _ | Naked_vec128 _ | Naked_nativeint _ | Rec_info _ | Region _
    ->
    wrong_kind "Value" t

let prove_unique_tag_and_size env t :
    (Tag.t * Targetint_31_63.t) proof_of_property =
  match prove_unique_tag_and_size0 env t with
  | Proved (tag_and_size, _, _) -> Proved tag_and_size
  | Unknown -> Unknown

let prove_unique_fully_constructed_immutable_heap_block env t :
    _ proof_of_property =
  match prove_unique_tag_and_size0 env t with
  | Unknown | Proved (_, _, (Heap_or_local | Local)) -> Unknown
  | Proved (tag_and_size, product, Heap) -> (
    let result =
      List.fold_left
        (fun (result : _ proof_of_property) field_ty : _ proof_of_property ->
          match result with
          | Unknown -> result
          | Proved simples_rev -> (
            match TG.get_alias_exn field_ty with
            | exception Not_found -> Unknown
            | simple -> Proved (simple :: simples_rev)))
        (Proved [] : _ proof_of_property)
        (TG.Product.Int_indexed.components product)
    in
    match result with
    | Unknown -> Unknown
    | Proved simples -> Proved (tag_and_size, List.rev simples))

let meet_is_naked_number_array env t naked_number_kind : bool meet_shortcut =
  match expand_head env t with
  | Value Unknown -> Need_meet
  | Value Bottom -> Invalid
  | Value (Ok (Array { element_kind = Unknown; _ })) -> Need_meet
  | Value (Ok (Array { element_kind = Bottom; _ })) ->
    (* Empty array case. We cannot return Invalid, but any other result is
       correct. We arbitrarily pick [false], as this is what we would get if we
       looked at the tag at runtime. *)
    Known_result false
  | Value (Ok (Array { element_kind = Ok element_kind; _ })) -> (
    match K.With_subkind.kind element_kind with
    | Value -> Known_result false
    | Naked_number naked_number_kind'
      when K.Naked_number_kind.equal naked_number_kind naked_number_kind' ->
      Known_result true
    | Naked_number _ | Region | Rec_info ->
      Misc.fatal_errorf "Wrong element kind for array: %a" K.With_subkind.print
        element_kind)
  | Value
      (Ok
        ( Boxed_float _ | Boxed_float32 _ | Boxed_int32 _ | Boxed_int64 _
        | Boxed_nativeint _ | Boxed_vec128 _ | Closures _ | String _ )) ->
    Invalid
  | Value (Ok (Variant _ | Mutable_block _)) ->
    (* In case of untyped code using array primitives on regular blocks *)
    Need_meet
  | Naked_immediate _ | Naked_float _ | Naked_float32 _ | Naked_int32 _
  | Naked_int64 _ | Naked_vec128 _ | Naked_nativeint _ | Rec_info _ | Region _
    ->
    Misc.fatal_errorf "Kind error: expected [Value]:@ %a" TG.print t

let prove_is_immediates_array env t : unit proof_of_property =
  match expand_head env t with
  | Value (Unknown | Bottom) -> Unknown
  | Value (Ok (Array { element_kind = Unknown; _ })) -> Unknown
  | Value (Ok (Array { element_kind = Bottom; _ })) ->
    (* Empty array case. We cannot return Invalid, but it's correct to state
       that any value contained in this array must be an immediate. *)
    Proved ()
  | Value (Ok (Array { element_kind = Ok element_kind; _ })) -> (
    match K.With_subkind.subkind element_kind with
    | Tagged_immediate -> Proved ()
    | Anything | Boxed_float | Boxed_float32 | Boxed_int32 | Boxed_int64
    | Boxed_nativeint | Boxed_vec128 | Variant _ | Float_block _ | Float_array
    | Immediate_array | Value_array | Generic_array | Unboxed_float32_array
    | Unboxed_int32_array | Unboxed_int64_array | Unboxed_nativeint_array ->
      Unknown)
  | Value
      (Ok
        ( Variant _ | Mutable_block _ | Boxed_float _ | Boxed_float32 _
        | Boxed_int32 _ | Boxed_vec128 _ | Boxed_int64 _ | Boxed_nativeint _
        | Closures _ | String _ )) ->
    Unknown
  | Naked_immediate _ | Naked_float _ | Naked_float32 _ | Naked_int32 _
  | Naked_int64 _ | Naked_vec128 _ | Naked_nativeint _ | Rec_info _ | Region _
    ->
    wrong_kind "Value" t

let prove_single_closures_entry_generic env t : _ generic_proof =
  match expand_head env t with
  | Value (Ok (Closures { by_function_slot; alloc_mode })) -> (
    match TG.Row_like_for_closures.get_singleton by_function_slot with
    | None -> Unknown
    | Some ((function_slot, set_of_closures_contents), closures_entry) -> (
      let function_slots =
        Set_of_closures_contents.closures set_of_closures_contents
      in
      assert (Function_slot.Set.mem function_slot function_slots);
      let function_type =
        TG.Closures_entry.find_function_type closures_entry function_slot
      in
      match function_type with
      | Bottom -> Invalid
      | Unknown -> Unknown
      | Ok function_type ->
        Proved (function_slot, alloc_mode, closures_entry, function_type)))
  | Value
      (Ok
        ( Variant _ | Mutable_block _ | Boxed_float _ | Boxed_float32 _
        | Boxed_int32 _ | Boxed_vec128 _ | Boxed_int64 _ | Boxed_nativeint _
        | String _ | Array _ )) ->
    Invalid
  | Value Unknown -> Unknown
  | Value Bottom -> Invalid
  | Naked_immediate _ | Naked_float _ | Naked_float32 _ | Naked_int32 _
  | Naked_int64 _ | Naked_vec128 _ | Naked_nativeint _ | Rec_info _ | Region _
    ->
    wrong_kind "Value" t

let meet_single_closures_entry env t =
  as_meet_shortcut (prove_single_closures_entry_generic env t)

let meet_is_immutable_array env t : _ meet_shortcut =
  match expand_head env t with
  | Value Unknown -> Need_meet
  | Value Bottom -> Invalid
  | Value (Ok (Array { element_kind; length; contents; alloc_mode })) -> (
    match contents with
    | Known (Immutable _) -> Known_result (element_kind, length, alloc_mode)
    | Known Mutable -> Invalid
    | Unknown -> Need_meet)
  | Value (Ok _)
  | Naked_immediate _ | Naked_float _ | Naked_float32 _ | Naked_int32 _
  | Naked_int64 _ | Naked_vec128 _ | Naked_nativeint _ | Rec_info _ | Region _
    ->
    Invalid

let prove_single_closures_entry env t =
  as_property (prove_single_closures_entry_generic env t)

let meet_strings env t : String_info.Set.t meet_shortcut =
  match expand_head env t with
  | Value (Ok (String strs)) -> Known_result strs
  | Value (Ok _) -> Invalid
  | Value Unknown -> Need_meet
  | Value Bottom -> Invalid
  | Naked_immediate _ | Naked_float _ | Naked_float32 _ | Naked_int32 _
  | Naked_int64 _ | Naked_vec128 _ | Naked_nativeint _ | Rec_info _ | Region _
    ->
    Misc.fatal_errorf "Kind error: expected [Value]:@ %a" TG.print t

let prove_strings env t : _ proof_of_property =
  match expand_head env t with
  | Value (Ok (String strs)) ->
    (* At present we only track statically-allocated strings (see
       [Type_grammar]). *)
    Proved (Alloc_mode.For_types.heap, strs)
  | Value (Ok _ | Unknown | Bottom) -> Unknown
  | Naked_immediate _ | Naked_float _ | Naked_float32 _ | Naked_int32 _
  | Naked_int64 _ | Naked_vec128 _ | Naked_nativeint _ | Rec_info _ | Region _
    ->
    Misc.fatal_errorf "Kind error: expected [Value]:@ %a" TG.print t

type tagging_proof_kind =
  | Prove
  | Meet

let[@inline always] inspect_tagging_of_simple proof_kind env ~min_name_mode t :
    Simple.t generic_proof =
  match expand_head env t with
  | Value (Ok (Variant { immediates; blocks; is_unique = _ })) -> (
    let inspect_immediates () =
      match immediates with
      | Unknown -> Unknown
      | Known t -> (
        let from_alias =
          match
            TE.get_canonical_simple_exn env ~min_name_mode (TG.get_alias_exn t)
          with
          | simple -> Some simple
          | exception Not_found -> None
        in
        match from_alias with
        | Some simple -> Proved simple
        | None -> (
          match meet_naked_immediates env t with
          | Need_meet -> Unknown
          | Invalid -> Invalid
          | Known_result imms -> (
            match Targetint_31_63.Set.get_singleton imms with
            | Some imm ->
              Proved (Simple.const (Reg_width_const.naked_immediate imm))
            | None -> Unknown)))
    in
    match proof_kind, blocks with
    | Prove, Unknown -> Unknown
    | Prove, Known blocks ->
      if TG.Row_like_for_blocks.is_bottom blocks
      then inspect_immediates ()
      else Unknown
    | Meet, _ -> inspect_immediates ())
  | Value _ -> Unknown
  | Naked_immediate _ | Naked_float _ | Naked_float32 _ | Naked_int32 _
  | Naked_int64 _ | Naked_vec128 _ | Naked_nativeint _ | Rec_info _ | Region _
    ->
    wrong_kind "Value" t

let prove_tagging_of_simple env ~min_name_mode t =
  as_property (inspect_tagging_of_simple Prove env ~min_name_mode t)

let meet_tagging_of_simple env ~min_name_mode t =
  as_meet_shortcut (inspect_tagging_of_simple Meet env ~min_name_mode t)

let[@inline always] meet_boxed_number_containing_simple
    ~contents_of_boxed_number env ~min_name_mode t : Simple.t meet_shortcut =
  match expand_head env t with
  | Value (Ok ty_value) -> (
    match contents_of_boxed_number ty_value with
    | None -> Invalid
    | Some ty -> (
      match
        TE.get_canonical_simple_exn env ~min_name_mode (TG.get_alias_exn ty)
      with
      | simple -> Known_result simple
      | exception Not_found -> Need_meet))
  | Value Unknown -> Need_meet
  | Value Bottom -> Invalid
  | Naked_immediate _ | Naked_float _ | Naked_float32 _ | Naked_int32 _
  | Naked_int64 _ | Naked_vec128 _ | Naked_nativeint _ | Rec_info _ | Region _
    ->
    Misc.fatal_errorf "Kind error: expected [Value]:@ %a" TG.print t

let meet_boxed_float32_containing_simple =
  meet_boxed_number_containing_simple
    ~contents_of_boxed_number:(fun (ty_value : TG.head_of_kind_value) ->
      match ty_value with
      | Boxed_float32 (ty, _) -> Some ty
      | Variant _ | Mutable_block _ | Boxed_float _ | Boxed_int32 _
      | Boxed_int64 _ | Boxed_vec128 _ | Boxed_nativeint _ | Closures _
      | String _ | Array _ ->
        None)

let meet_boxed_float_containing_simple =
  meet_boxed_number_containing_simple
    ~contents_of_boxed_number:(fun (ty_value : TG.head_of_kind_value) ->
      match ty_value with
      | Boxed_float (ty, _) -> Some ty
      | Variant _ | Mutable_block _ | Boxed_float32 _ | Boxed_int32 _
      | Boxed_int64 _ | Boxed_vec128 _ | Boxed_nativeint _ | Closures _
      | String _ | Array _ ->
        None)

let meet_boxed_int32_containing_simple =
  meet_boxed_number_containing_simple
    ~contents_of_boxed_number:(fun (ty_value : TG.head_of_kind_value) ->
      match ty_value with
      | Boxed_int32 (ty, _) -> Some ty
      | Variant _ | Mutable_block _ | Boxed_float _ | Boxed_float32 _
      | Boxed_int64 _ | Boxed_vec128 _ | Boxed_nativeint _ | Closures _
      | String _ | Array _ ->
        None)

let meet_boxed_int64_containing_simple =
  meet_boxed_number_containing_simple
    ~contents_of_boxed_number:(fun (ty_value : TG.head_of_kind_value) ->
      match ty_value with
      | Boxed_int64 (ty, _) -> Some ty
      | Variant _ | Mutable_block _ | Boxed_float _ | Boxed_float32 _
      | Boxed_int32 _ | Boxed_vec128 _ | Boxed_nativeint _ | Closures _
      | String _ | Array _ ->
        None)

let meet_boxed_nativeint_containing_simple =
  meet_boxed_number_containing_simple
    ~contents_of_boxed_number:(fun (ty_value : TG.head_of_kind_value) ->
      match ty_value with
      | Boxed_nativeint (ty, _) -> Some ty
      | Variant _ | Mutable_block _ | Boxed_float _ | Boxed_float32 _
      | Boxed_int32 _ | Boxed_vec128 _ | Boxed_int64 _ | Closures _ | String _
      | Array _ ->
        None)

let meet_boxed_vec128_containing_simple =
  meet_boxed_number_containing_simple
    ~contents_of_boxed_number:(fun (ty_value : TG.head_of_kind_value) ->
      match ty_value with
      | Boxed_vec128 (ty, _) -> Some ty
      | Variant _ | Mutable_block _ | Boxed_float _ | Boxed_float32 _
      | Boxed_int32 _ | Boxed_nativeint _ | Boxed_int64 _ | Closures _
      | String _ | Array _ ->
        None)

let meet_block_field_simple env ~min_name_mode ~field_kind t field_index :
    Simple.t meet_shortcut =
  match expand_head env t with
  | Value (Ok (Variant { immediates = _; blocks; is_unique = _ })) -> (
    match blocks with
    | Unknown -> Need_meet
    | Known blocks -> (
      if TG.Row_like_for_blocks.is_bottom blocks
      then Invalid
      else
        match
          (TG.Row_like_for_blocks.get_field blocks field_index
            : _ Or_unknown_or_bottom.t)
        with
        | Bottom -> Invalid
        | Unknown -> Need_meet
        | Ok ty -> (
          if (* It's more straightforward to check the kind of [ty] instead of
                examining the row-like structure directly. *)
             not (Flambda_kind.equal (TG.kind ty) field_kind)
          then Invalid
          else
            match TG.get_alias_exn ty with
            | simple -> (
              match TE.get_canonical_simple_exn env ~min_name_mode simple with
              | simple -> Known_result simple
              | exception Not_found -> Need_meet)
            | exception Not_found -> Need_meet)))
  | Value (Ok (Mutable_block _)) -> Need_meet
  | Value (Ok _) -> Invalid
  | Value Unknown -> Need_meet
  | Value Bottom -> Invalid
  | Naked_immediate _ | Naked_float _ | Naked_float32 _ | Naked_int32 _
  | Naked_int64 _ | Naked_vec128 _ | Naked_nativeint _ | Rec_info _ | Region _
    ->
    wrong_kind "Value" t

let meet_project_function_slot_simple env ~min_name_mode t function_slot :
    Simple.t meet_shortcut =
  match expand_head env t with
  | Value (Ok (Closures { by_function_slot; alloc_mode = _ })) -> (
    match
      TG.Row_like_for_closures.get_closure by_function_slot function_slot
    with
    | Unknown -> Need_meet
    | Known ty -> (
      match TG.get_alias_exn ty with
      | simple -> (
        match TE.get_canonical_simple_exn env ~min_name_mode simple with
        | simple -> Known_result simple
        | exception Not_found -> Need_meet)
      | exception Not_found -> Need_meet))
  | Value (Ok _) -> Invalid
  | Value Unknown -> Need_meet
  | Value Bottom -> Invalid
  | Naked_immediate _ | Naked_float _ | Naked_float32 _ | Naked_int32 _
  | Naked_int64 _ | Naked_vec128 _ | Naked_nativeint _ | Rec_info _ | Region _
    ->
    wrong_kind "Value" t

let meet_project_value_slot_simple env ~min_name_mode t value_slot :
    Simple.t meet_shortcut =
  match expand_head env t with
  | Value (Ok (Closures { by_function_slot; alloc_mode = _ })) -> (
    match TG.Row_like_for_closures.get_env_var by_function_slot value_slot with
    | Unknown -> Need_meet
    | Known ty -> (
      if (* It's more straightforward to check the kind of [ty] instead of
            examining the row-like structure directly. *)
         not
           (Flambda_kind.equal (TG.kind ty)
              (Value_slot.kind value_slot |> Flambda_kind.With_subkind.kind))
      then Invalid
      else
        match TG.get_alias_exn ty with
        | simple -> (
          match TE.get_canonical_simple_exn env ~min_name_mode simple with
          | simple -> Known_result simple
          | exception Not_found -> Need_meet)
        | exception Not_found -> Need_meet))
  | Value (Ok _) -> Invalid
  | Value Unknown -> Need_meet
  | Value Bottom -> Invalid
  | Naked_immediate _ | Naked_float _ | Naked_float32 _ | Naked_int32 _
  | Naked_int64 _ | Naked_vec128 _ | Naked_nativeint _ | Rec_info _ | Region _
    ->
    wrong_kind "Value" t

let meet_rec_info env t : Rec_info_expr.t meet_shortcut =
  match expand_head env t with
  | Rec_info (Ok rec_info_expr) -> Known_result rec_info_expr
  | Rec_info Unknown -> Need_meet
  | Rec_info Bottom -> Invalid
  | Value _ | Naked_immediate _ | Naked_float _ | Naked_float32 _
  | Naked_int32 _ | Naked_int64 _ | Naked_vec128 _ | Naked_nativeint _
  | Region _ ->
    wrong_kind "Rec_info" t

let prove_alloc_mode_of_boxed_number env t :
    Alloc_mode.For_types.t proof_of_property =
  match expand_head env t with
  | Value (Ok (Boxed_float32 (_, alloc_mode)))
  | Value (Ok (Boxed_float (_, alloc_mode)))
  | Value (Ok (Boxed_int32 (_, alloc_mode)))
  | Value (Ok (Boxed_int64 (_, alloc_mode)))
  | Value (Ok (Boxed_nativeint (_, alloc_mode)))
  | Value (Ok (Boxed_vec128 (_, alloc_mode))) ->
    Proved alloc_mode
  | Value (Ok (Variant _ | Mutable_block _ | String _ | Array _ | Closures _))
  | Value (Unknown | Bottom) ->
    Unknown
  | Naked_immediate _ | Naked_float _ | Naked_float32 _ | Naked_int32 _
  | Naked_int64 _ | Naked_vec128 _ | Naked_nativeint _ | Rec_info _ | Region _
    ->
    wrong_kind "Value" t

let never_holds_locally_allocated_values env var : _ proof_of_property =
  match TE.find_or_missing env (Name.var var) with
  | None -> Unknown
  | Some ty -> (
    match expand_head env ty with
    | Value (Ok (Variant { blocks; _ })) -> (
      match blocks with
      | Unknown -> Unknown
      | Known blocks -> (
        if TG.Row_like_for_blocks.is_bottom blocks
        then Proved ()
        else
          match blocks.alloc_mode with
          | Heap -> Proved ()
          | Local | Heap_or_local -> Unknown))
    | Value (Ok (Boxed_float32 (_, alloc_mode)))
    | Value (Ok (Boxed_float (_, alloc_mode)))
    | Value (Ok (Boxed_int32 (_, alloc_mode)))
    | Value (Ok (Boxed_int64 (_, alloc_mode)))
    | Value (Ok (Boxed_nativeint (_, alloc_mode)))
    | Value (Ok (Boxed_vec128 (_, alloc_mode)))
    | Value (Ok (Mutable_block { alloc_mode }))
    | Value (Ok (Closures { alloc_mode; _ }))
    | Value (Ok (Array { alloc_mode; _ })) -> (
      match alloc_mode with
      | Heap -> Proved ()
      | Local | Heap_or_local -> Unknown)
    | Value (Ok (String _)) -> Proved ()
    | Value Unknown -> Unknown
    | Value Bottom -> Unknown
    | Naked_immediate _ | Naked_float _ | Naked_float32 _ | Naked_int32 _
    | Naked_int64 _ | Naked_vec128 _ | Naked_nativeint _ | Rec_info _ | Region _
      ->
      Proved ())

let prove_physical_equality env t1 t2 =
  let incompatible_naked_numbers t1 t2 =
    match expand_head env t1, expand_head env t2 with
    | Naked_float32 (Ok s1), Naked_float32 (Ok s2) ->
      let module FS = Numeric_types.Float32_by_bit_pattern.Set in
      FS.is_empty (FS.inter (s1 :> FS.t) (s2 :> FS.t))
    | Naked_float (Ok s1), Naked_float (Ok s2) ->
      let module FS = Numeric_types.Float_by_bit_pattern.Set in
      FS.is_empty (FS.inter (s1 :> FS.t) (s2 :> FS.t))
    | Naked_int32 (Ok s1), Naked_int32 (Ok s2) ->
      let module IS = Numeric_types.Int32.Set in
      IS.is_empty (IS.inter (s1 :> IS.t) (s2 :> IS.t))
    | Naked_int64 (Ok s1), Naked_int64 (Ok s2) ->
      let module IS = Numeric_types.Int64.Set in
      IS.is_empty (IS.inter (s1 :> IS.t) (s2 :> IS.t))
    | Naked_nativeint (Ok s1), Naked_nativeint (Ok s2) ->
      let module IS = Targetint_32_64.Set in
      IS.is_empty (IS.inter (s1 :> IS.t) (s2 :> IS.t))
    | Naked_vec128 (Ok s1), Naked_vec128 (Ok s2) ->
      let module IS = Vector_types.Vec128.Bit_pattern.Set in
      IS.is_empty (IS.inter (s1 :> IS.t) (s2 :> IS.t))
    | ( ( Naked_float _ | Naked_float32 _ | Naked_int32 _ | Naked_int64 _
        | Naked_nativeint _ | Naked_vec128 _ | Value _ | Naked_immediate _
        | Region _ | Rec_info _ ),
        _ ) ->
      false
  in
  let check_heads () : _ proof_of_property =
    match expand_head env t1, expand_head env t2 with
    | ( ( Naked_immediate _ | Naked_float _ | Naked_float32 _ | Naked_int32 _
        | Naked_int64 _ | Naked_vec128 _ | Naked_nativeint _ | Rec_info _
        | Region _ ),
        _ ) ->
      wrong_kind "Value" t1
    | ( _,
        ( Naked_immediate _ | Naked_float _ | Naked_float32 _ | Naked_int32 _
        | Naked_int64 _ | Naked_vec128 _ | Naked_nativeint _ | Rec_info _
        | Region _ ) ) ->
      wrong_kind "Value" t2
    | Value (Unknown | Bottom), _ | _, Value (Unknown | Bottom) -> Unknown
    | Value (Ok head1), Value (Ok head2) -> (
      match head1, head2 with
      (* Basic cases:
       * similar heads -> Unknown
       * incompatible contents -> Proved false
       *)
      | Boxed_float (t1, _), Boxed_float (t2, _) ->
        if incompatible_naked_numbers t1 t2 then Proved false else Unknown
      | Boxed_float32 (t1, _), Boxed_float32 (t2, _) ->
        if incompatible_naked_numbers t1 t2 then Proved false else Unknown
      | Boxed_int32 (t1, _), Boxed_int32 (t2, _) ->
        if incompatible_naked_numbers t1 t2 then Proved false else Unknown
      | Boxed_int64 (t1, _), Boxed_int64 (t2, _) ->
        if incompatible_naked_numbers t1 t2 then Proved false else Unknown
      | Boxed_nativeint (t1, _), Boxed_nativeint (t2, _) ->
        if incompatible_naked_numbers t1 t2 then Proved false else Unknown
      | Boxed_vec128 (t1, _), Boxed_vec128 (t2, _) ->
        if incompatible_naked_numbers t1 t2 then Proved false else Unknown
      | Closures _, Closures _ -> Unknown
      | String s1, String s2 ->
        let module SS = String_info.Set in
        if SS.is_empty (SS.inter s1 s2) then Proved false else Unknown
      (* Immediates and allocated values -> Proved false *)
      | ( Variant { immediates = _; blocks = Known blocks; is_unique = _ },
          ( Mutable_block _ | Boxed_float _ | Boxed_float32 _ | Boxed_int32 _
          | Boxed_int64 _ | Boxed_vec128 _ | Boxed_nativeint _ | Closures _
          | String _ | Array _ ) )
      | ( ( Mutable_block _ | Boxed_float _ | Boxed_float32 _ | Boxed_int32 _
          | Boxed_int64 _ | Boxed_vec128 _ | Boxed_nativeint _ | Closures _
          | String _ | Array _ ),
          Variant { immediates = _; blocks = Known blocks; is_unique = _ } )
        when TG.Row_like_for_blocks.is_bottom blocks ->
        Proved false
      (* Variants:
       * incompatible immediates and incompatible block tags -> Proved false
       * same immediate on both sides, no blocks -> Proved true
       *)
      | ( Variant { immediates = immediates1; blocks = blocks1; is_unique = _ },
          Variant { immediates = immediates2; blocks = blocks2; is_unique = _ }
        ) -> (
        match immediates1, immediates2, blocks1, blocks2 with
        | Known imms, _, _, Known blocks
          when TG.is_obviously_bottom imms
               && TG.Row_like_for_blocks.is_bottom blocks ->
          Proved false
        | _, Known imms, Known blocks, _
          when TG.is_obviously_bottom imms
               && TG.Row_like_for_blocks.is_bottom blocks ->
          Proved false
        | Known imms1, Known imms2, Known blocks1, Known blocks2 -> (
          let immediates_equality : _ generic_proof =
            (* Note: the proof we're returning here has slightly unusual
               semantics. [Invalid] is only returned if neither input can be an
               immediate. [Proved false] means that the inputs cannot be both
               immediates and equal. [Proved true] means that *if* both inputs
               are immediates, then they are equal.

               This is what allows us to combine correctly with the property on
               blocks to return a precise enough result. *)
            match
              ( prove_naked_immediates_generic env imms1,
                prove_naked_immediates_generic env imms2 )
            with
            | Invalid, Invalid -> Invalid
            | Invalid, _ | _, Invalid -> Proved false
            | Unknown, _ | _, Unknown -> Unknown
            | Proved imms1, Proved imms2 -> (
              let module S = Targetint_31_63.Set in
              if S.is_empty (S.inter imms1 imms2)
              then Proved false
              else
                match S.get_singleton imms1, S.get_singleton imms2 with
                | None, _ | _, None -> Unknown
                | Some imm1, Some imm2 ->
                  (* We've ruled out the empty intersection case, so the numbers
                     have to be equal *)
                  assert (Targetint_31_63.equal imm1 imm2);
                  Proved true)
          in
          let blocks_equality : _ generic_proof =
            (* Same semantics as in the immediates case *)
            let is_bottom1 = TG.Row_like_for_blocks.is_bottom blocks1 in
            let is_bottom2 = TG.Row_like_for_blocks.is_bottom blocks2 in
            if is_bottom1 && is_bottom2
            then Invalid
            else if is_bottom1 || is_bottom2
            then Proved false
            else
              match
                ( TG.Row_like_for_blocks.get_singleton blocks1,
                  TG.Row_like_for_blocks.get_singleton blocks2 )
              with
              | None, _ | _, None -> Unknown
              | ( Some ((tag1, size1), _fields1, _alloc_mode1),
                  Some ((tag2, size2), _fields2, _alloc_mode2) ) ->
                if Tag.equal tag1 tag2 && Targetint_31_63.equal size1 size2
                then
                  (* CR vlaviron and chambart: We could add a special case for
                     extension constructors, to try to remove dead branches in
                     try...with handlers *)
                  Unknown
                else
                  (* Different tags or sizes: the blocks can't be physically
                     equal. *)
                  Proved false
          in
          (* Note: the [Proved true, Proved true] case cannot be converted to
             [Proved true] (see comment above on semantics) *)
          match immediates_equality, blocks_equality with
          | Proved b, Invalid | Invalid, Proved b -> Proved b
          | Proved false, Proved false -> Proved false
          | _, _ -> Unknown)
        | _, _, _, _ -> Unknown)
      (* Boxed numbers with non-numbers or different kinds -> Proved *)
      | ( Boxed_float _,
          ( Variant _ | Mutable_block _ | Boxed_float32 _ | Boxed_int32 _
          | Boxed_int64 _ | Boxed_vec128 _ | Boxed_nativeint _ | Closures _
          | String _ | Array _ ) )
      | ( ( Variant _ | Mutable_block _ | Boxed_float32 _ | Boxed_int32 _
          | Boxed_int64 _ | Boxed_vec128 _ | Boxed_nativeint _ | Closures _
          | String _ | Array _ ),
          Boxed_float _ )
      | ( Boxed_int32 _,
          ( Variant _ | Mutable_block _ | Boxed_float32 _ | Boxed_int64 _
          | Boxed_nativeint _ | Boxed_vec128 _ | Closures _ | String _ | Array _
            ) )
      | ( ( Variant _ | Mutable_block _ | Boxed_float32 _ | Boxed_int64 _
          | Boxed_nativeint _ | Boxed_vec128 _ | Closures _ | String _ | Array _
            ),
          Boxed_int32 _ )
      | ( Boxed_int64 _,
          ( Variant _ | Mutable_block _ | Boxed_float32 _ | Boxed_nativeint _
          | Closures _ | Boxed_vec128 _ | String _ | Array _ ) )
      | ( ( Variant _ | Mutable_block _ | Boxed_float32 _ | Boxed_nativeint _
          | Closures _ | Boxed_vec128 _ | String _ | Array _ ),
          Boxed_int64 _ )
      | ( Boxed_vec128 _,
          ( Variant _ | Mutable_block _ | Boxed_float32 _ | Boxed_nativeint _
          | Closures _ | String _ | Array _ ) )
      | ( ( Variant _ | Mutable_block _ | Boxed_float32 _ | Boxed_nativeint _
          | Closures _ | String _ | Array _ ),
          Boxed_vec128 _ )
      | ( Boxed_nativeint _,
          ( Variant _ | Mutable_block _ | Boxed_float32 _ | Closures _
          | String _ | Array _ ) )
      | ( ( Variant _ | Mutable_block _ | Boxed_float32 _ | Closures _
          | String _ | Array _ ),
          Boxed_nativeint _ )
      | ( Boxed_float32 _,
          (Variant _ | Mutable_block _ | Closures _ | String _ | Array _) )
      | ( (Variant _ | Mutable_block _ | Closures _ | String _ | Array _),
          Boxed_float32 _ ) ->
        Proved false
      (* Closures and non-closures -> Proved *)
      | Closures _, (Variant _ | Mutable_block _ | String _ | Array _)
      | (Variant _ | Mutable_block _ | String _ | Array _), Closures _ ->
        Proved false
      (* Strings and non-strings -> Proved *)
      | String _, (Variant _ | Mutable_block _ | Array _)
      | (Variant _ | Mutable_block _ | Array _), String _ ->
        Proved false
      (* Due to various hacks in existing code (including in the compiler), it
         would be dangerous to assume that variants, mutable blocks and arrays
         cannot alias to each other *)
      | ( (Variant _ | Mutable_block _ | Array _),
          (Variant _ | Mutable_block _ | Array _) ) ->
        Unknown)
  in
  match TG.get_alias_opt t1, TG.get_alias_opt t2 with
  | Some s1, Some s2 ->
    let const c1 =
      Simple.pattern_match' s2
        ~const:(fun c2 : _ proof_of_property ->
          if Reg_width_const.equal c1 c2 then Proved true else Proved false)
        ~symbol:(fun _ ~coercion:_ : _ proof_of_property -> Proved false)
        ~var:(fun _ ~coercion:_ -> check_heads ())
    in
    let symbol sym1 ~coercion:_ =
      Simple.pattern_match' s2
        ~const:(fun _ : _ proof_of_property -> Proved false)
        ~symbol:(fun sym2 ~coercion:_ : _ proof_of_property ->
          if Symbol.equal sym1 sym2 then Proved true else Proved false)
        ~var:(fun _ ~coercion:_ -> check_heads ())
    in
    let var var1 ~coercion:_ =
      Simple.pattern_match' s2
        ~const:(fun _ -> check_heads ())
        ~symbol:(fun _ ~coercion:_ -> check_heads ())
        ~var:(fun var2 ~coercion:_ : _ proof_of_property ->
          if Variable.equal var1 var2 then Proved true else check_heads ())
    in
    Simple.pattern_match' s1 ~const ~symbol ~var
  | None, Some _ | Some _, None | None, None -> check_heads ()
