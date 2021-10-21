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

[@@@ocaml.warning "+a-4-30-40-41-42"]
(* CR mshinwell: enable warning 4 *)

module Float = Numeric_types.Float_by_bit_pattern
module Int32 = Numeric_types.Int32
module Int64 = Numeric_types.Int64
module K = Flambda_kind
module TE = Typing_env
module TG = Type_grammar

let is_bottom = Expand_head.is_bottom

let expand_head env ty =
  Expand_head.expand_head env ty |> Expand_head.Expanded_type.descr_oub

type 'a proof =
  | Proved of 'a
  | Unknown
  | Invalid

type 'a proof_allowing_kind_mismatch =
  | Proved of 'a
  | Unknown
  | Invalid
  | Wrong_kind

type var_or_symbol_or_tagged_immediate =
  | Var of Variable.t
  | Symbol of Symbol.t
  | Tagged_immediate of Targetint_31_63.t

let prove_equals_to_var_or_symbol_or_tagged_immediate env t :
    (var_or_symbol_or_tagged_immediate * Coercion.t) proof =
  let original_kind = TG.kind t in
  if not (K.equal original_kind K.value)
  then Misc.fatal_errorf "Type %a is not of kind value" TG.print t;
  (* XXX This probably shouldn't be using [get_alias] *)
  (* XXX This needs to match the equal-to-tagged-immediates function below *)
  match TG.get_alias_exn t with
  | exception Not_found -> Unknown
  | simple ->
    Simple.pattern_match simple
      ~const:(fun cst : _ proof ->
        match Reg_width_const.descr cst with
        | Tagged_immediate imm -> Proved (Tagged_immediate imm, Coercion.id)
        | _ ->
          Misc.fatal_errorf
            "[Simple] %a in the [Equals] field has a kind different from that \
             returned by [kind] (%a):@ %a"
            Simple.print simple K.print original_kind TG.print t)
      ~name:(fun _ ~coercion:_ : _ proof ->
        match
          TE.get_canonical_simple_exn env simple ~min_name_mode:Name_mode.normal
        with
        | exception Not_found -> Unknown
        | simple ->
          (* CR mshinwell: Instead, get all aliases and find a Symbol, to avoid
             relying on the fact that if there is a Symbol alias then it will be
             canonical *)
          Simple.pattern_match simple
            ~const:(fun cst : _ proof ->
              match Reg_width_const.descr cst with
              | Tagged_immediate imm ->
                Proved (Tagged_immediate imm, Coercion.id)
              | _ ->
                let kind = TG.kind t in
                Misc.fatal_errorf
                  "Kind returned by [get_canonical_simple] (%a) doesn't match \
                   the kind of the returned [Simple] %a:@ %a"
                  K.print kind Simple.print simple TG.print t)
            ~name:(fun name ~coercion ->
              Name.pattern_match name
                ~var:
                  (fun var :
                       (var_or_symbol_or_tagged_immediate * Coercion.t) proof ->
                  Proved (Var var, coercion))
                ~symbol:
                  (fun symbol :
                       (var_or_symbol_or_tagged_immediate * Coercion.t) proof ->
                  Proved (Symbol symbol, coercion))))

let prove_single_closures_entry' env t : _ proof_allowing_kind_mismatch =
  match expand_head env t with
  | Value (Ok (Closures closures)) -> begin
    match TG.Row_like_for_closures.get_singleton closures.by_closure_id with
    | None -> Unknown
    | Some ((closure_id, set_of_closures_contents), closures_entry) -> (
      let closure_ids =
        Set_of_closures_contents.closures set_of_closures_contents
      in
      assert (Closure_id.Set.mem closure_id closure_ids);
      let function_type =
        TG.Closures_entry.find_function_type closures_entry closure_id
      in
      match function_type with
      | Bottom -> Invalid
      | Unknown -> Unknown
      | Ok function_type -> Proved (closure_id, closures_entry, function_type))
  end
  | Value (Ok _) -> Invalid
  | Value Unknown -> Unknown
  | Value Bottom -> Invalid
  | Naked_immediate _ -> Wrong_kind
  | Naked_float _ -> Wrong_kind
  | Naked_int32 _ -> Wrong_kind
  | Naked_int64 _ -> Wrong_kind
  | Naked_nativeint _ -> Wrong_kind
  | Rec_info _ -> Wrong_kind

let prove_single_closures_entry env t : _ proof =
  match prove_single_closures_entry' env t with
  | Proved proof -> Proved proof
  | Unknown -> Unknown
  | Invalid -> Invalid
  | Wrong_kind -> Misc.fatal_errorf "Type has wrong kind: %a" TG.print t

(* CR mshinwell: Try to functorise or otherwise factor out across the various
   number kinds. *)
let prove_naked_floats env t : _ proof =
  let wrong_kind () =
    Misc.fatal_errorf "Kind error: expected [Naked_float]:@ %a" TG.print t
  in
  match expand_head env t with
  | Naked_float (Ok fs) -> if Float.Set.is_empty fs then Invalid else Proved fs
  | Naked_float Unknown -> Unknown
  | Naked_float Bottom -> Invalid
  | Value _ -> wrong_kind ()
  | Naked_immediate _ -> wrong_kind ()
  | Naked_int32 _ -> wrong_kind ()
  | Naked_int64 _ -> wrong_kind ()
  | Naked_nativeint _ -> wrong_kind ()
  | Rec_info _ -> wrong_kind ()

let prove_naked_int32s env t : _ proof =
  let wrong_kind () =
    Misc.fatal_errorf "Kind error: expected [Naked_int32]:@ %a" TG.print t
  in
  match expand_head env t with
  | Naked_int32 (Ok is) -> if Int32.Set.is_empty is then Invalid else Proved is
  | Naked_int32 Unknown -> Unknown
  | Naked_int32 Bottom -> Invalid
  | Value _ -> wrong_kind ()
  | Naked_immediate _ -> wrong_kind ()
  | Naked_float _ -> wrong_kind ()
  | Naked_int64 _ -> wrong_kind ()
  | Naked_nativeint _ -> wrong_kind ()
  | Rec_info _ -> wrong_kind ()

let prove_naked_int64s env t : _ proof =
  let wrong_kind () =
    Misc.fatal_errorf "Kind error: expected [Naked_int64]:@ %a" TG.print t
  in
  match expand_head env t with
  | Naked_int64 (Ok is) -> if Int64.Set.is_empty is then Invalid else Proved is
  | Naked_int64 Unknown -> Unknown
  | Naked_int64 Bottom -> Invalid
  | Value _ -> wrong_kind ()
  | Naked_immediate _ -> wrong_kind ()
  | Naked_float _ -> wrong_kind ()
  | Naked_int32 _ -> wrong_kind ()
  | Naked_nativeint _ -> wrong_kind ()
  | Rec_info _ -> wrong_kind ()

let prove_naked_nativeints env t : _ proof =
  let wrong_kind () =
    Misc.fatal_errorf "Kind error: expected [Naked_nativeint]:@ %a" TG.print t
  in
  match expand_head env t with
  | Naked_nativeint (Ok is) ->
    if Targetint_32_64.Set.is_empty is then Invalid else Proved is
  | Naked_nativeint Unknown -> Unknown
  | Naked_nativeint Bottom -> Invalid
  | Value _ -> wrong_kind ()
  | Naked_immediate _ -> wrong_kind ()
  | Naked_float _ -> wrong_kind ()
  | Naked_int32 _ -> wrong_kind ()
  | Naked_int64 _ -> wrong_kind ()
  | Rec_info _ -> wrong_kind ()

let prove_is_int env t : bool proof =
  let wrong_kind () =
    Misc.fatal_errorf "Kind error: expected [Value]:@ %a" TG.print t
  in
  match expand_head env t with
  | Value (Ok (Variant blocks_imms)) -> begin
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
      else Unknown
  end
  | Value
      (Ok
        ( Boxed_float _ | Boxed_int32 _ | Boxed_int64 _ | Boxed_nativeint _
        | Closures _ | String _ | Array _ )) ->
    Proved false
  | Value Unknown -> Unknown
  | Value Bottom -> Invalid
  | Naked_immediate _ -> wrong_kind ()
  | Naked_float _ -> wrong_kind ()
  | Naked_int32 _ -> wrong_kind ()
  | Naked_int64 _ -> wrong_kind ()
  | Naked_nativeint _ -> wrong_kind ()
  | Rec_info _ -> wrong_kind ()

let prove_tags_must_be_a_block env t : Tag.Set.t proof =
  let wrong_kind () =
    Misc.fatal_errorf "Kind error: expected [Value]:@ %a" TG.print t
  in
  match expand_head env t with
  | Value (Ok (Variant blocks_imms)) -> begin
    match blocks_imms.immediates with
    | Unknown -> Unknown
    | Known imms -> (
      if not (is_bottom env imms)
      then
        (* CR vlaviron: This case could be completely ignored if we're only
           interested in the set of tags this type can have. It is there because
           this function used to return Invalid when it couldn't definitively
           prove that this type represents a block, but this caused a number of
           problems so this was switched to Unknown (which, unlike Invalid, is
           always sound). There remain a question of whether we actually want
           two different variants of this function, one for simplification of
           the get_tag primitive which may reasonably expect to error if its
           argument cannot be proved to be a block, and one for simplification
           of CSE parameters containing a get_tag equation, which can occur at
           places where the type is not known to be a block. *)
        Unknown
      else
        match blocks_imms.blocks with
        | Unknown -> Unknown
        | Known blocks -> (
          (* CR mshinwell: maybe [all_tags] should return the [Invalid] case
             directly? *)
          match TG.Row_like_for_blocks.all_tags blocks with
          | Unknown -> Unknown
          | Known tags -> if Tag.Set.is_empty tags then Invalid else Proved tags
          ))
  end
  | Value
      (Ok (Boxed_float _ | Boxed_int32 _ | Boxed_int64 _ | Boxed_nativeint _))
    ->
    Proved (Tag.Set.singleton Tag.custom_tag)
  | Value (Ok (Closures _)) ->
    Proved (Tag.Set.of_list [Tag.closure_tag; Tag.infix_tag])
  | Value (Ok (String _)) -> Proved (Tag.Set.singleton Tag.string_tag)
  | Value (Ok (Array _)) ->
    Proved (Tag.Set.of_list [Tag.zero; Tag.double_array_tag])
  | Value Unknown -> Unknown
  | Value Bottom -> Invalid
  (* CR mshinwell: Here and elsewhere, use or-patterns. *)
  | Naked_immediate _ -> wrong_kind ()
  | Naked_float _ -> wrong_kind ()
  | Naked_int32 _ -> wrong_kind ()
  | Naked_int64 _ -> wrong_kind ()
  | Naked_nativeint _ -> wrong_kind ()
  | Rec_info _ -> wrong_kind ()

let prove_naked_immediates env t : Targetint_31_63.Set.t proof =
  let wrong_kind () =
    Misc.fatal_errorf "Kind error: expected [Naked_immediate]:@ %a" TG.print t
  in
  match expand_head env t with
  | Naked_immediate (Ok (Naked_immediates is)) ->
    if Targetint_31_63.Set.is_empty is then Invalid else Proved is
  | Naked_immediate (Ok (Is_int scrutinee_ty)) -> begin
    match prove_is_int env scrutinee_ty with
    | Proved true ->
      Proved (Targetint_31_63.Set.singleton Targetint_31_63.bool_true)
    | Proved false ->
      Proved (Targetint_31_63.Set.singleton Targetint_31_63.bool_false)
    | Unknown -> Unknown
    | Invalid -> Invalid
  end
  | Naked_immediate (Ok (Get_tag block_ty)) -> begin
    (* CR vlaviron: see the comment in prove_tags_must_be_a_block. See also
       prove_equals_tagged_immediates below, which returns Unknown when the
       blocks part is not bottom. *)
    match prove_tags_must_be_a_block env block_ty with
    | Proved tags ->
      let is =
        Tag.Set.fold
          (fun tag is -> Targetint_31_63.Set.add (Tag.to_target_imm tag) is)
          tags Targetint_31_63.Set.empty
      in
      Proved is
    | Unknown -> Unknown
    | Invalid -> Invalid
  end
  | Naked_immediate Unknown -> Unknown
  | Naked_immediate Bottom -> Invalid
  | Value _ -> wrong_kind ()
  | Naked_float _ -> wrong_kind ()
  | Naked_int32 _ -> wrong_kind ()
  | Naked_int64 _ -> wrong_kind ()
  | Naked_nativeint _ -> wrong_kind ()
  | Rec_info _ -> wrong_kind ()

let prove_equals_tagged_immediates env t : Targetint_31_63.Set.t proof =
  let wrong_kind () =
    Misc.fatal_errorf "Kind error: expected [Value]:@ %a" TG.print t
  in
  match expand_head env t with
  | Value (Ok (Variant blocks_imms)) -> begin
    match blocks_imms.blocks, blocks_imms.immediates with
    | Unknown, Unknown | Unknown, Known _ | Known _, Unknown -> Unknown
    | Known blocks, Known imms ->
      (* CR mshinwell: Check this. Again it depends on the context; is this a
         context where variants are ok? *)
      if not (TG.Row_like_for_blocks.is_bottom blocks)
      then Unknown
      else prove_naked_immediates env imms
  end
  | Value (Ok _) -> Invalid
  | Value Unknown -> Unknown
  | Value Bottom -> Invalid
  | Naked_immediate _ -> wrong_kind ()
  | Naked_float _ -> wrong_kind ()
  | Naked_int32 _ -> wrong_kind ()
  | Naked_int64 _ -> wrong_kind ()
  | Naked_nativeint _ -> wrong_kind ()
  | Rec_info _ -> wrong_kind ()

let prove_equals_single_tagged_immediate env t : _ proof =
  match prove_equals_tagged_immediates env t with
  | Proved imms -> begin
    match Targetint_31_63.Set.get_singleton imms with
    | Some imm -> Proved imm
    | None -> Unknown
  end
  | Unknown -> Unknown
  | Invalid -> Invalid

let prove_tags_and_sizes env t : Targetint_31_63.Imm.t Tag.Map.t proof =
  let wrong_kind () =
    Misc.fatal_errorf "Kind error: expected [Value]:@ %a" TG.print t
  in
  match expand_head env t with
  | Value (Ok (Variant blocks_imms)) -> begin
    match blocks_imms.immediates with
    (* CR mshinwell: Care. Should this return [Unknown] or [Invalid] if there is
       the possibility of the type representing a tagged immediate? *)
    | Unknown -> Unknown
    | Known immediates ->
      if is_bottom env immediates
      then
        match blocks_imms.blocks with
        | Unknown -> Unknown
        | Known blocks -> (
          match TG.Row_like_for_blocks.all_tags_and_sizes blocks with
          | Unknown -> Unknown
          | Known tags_and_sizes -> Proved tags_and_sizes)
      else Unknown
  end
  | Value (Ok _) -> Invalid
  | Value Unknown -> Unknown
  | Value Bottom -> Invalid
  (* CR mshinwell: Here and elsewhere, use or-patterns. *)
  | Naked_immediate _ -> wrong_kind ()
  | Naked_float _ -> wrong_kind ()
  | Naked_int32 _ -> wrong_kind ()
  | Naked_int64 _ -> wrong_kind ()
  | Naked_nativeint _ -> wrong_kind ()
  | Rec_info _ -> wrong_kind ()

let prove_unique_tag_and_size env t :
    (Tag.t * Targetint_31_63.Imm.t) proof_allowing_kind_mismatch =
  if not (Flambda_kind.equal (TG.kind t) Flambda_kind.value)
  then Wrong_kind
  else
    match prove_tags_and_sizes env t with
    | Invalid -> Invalid
    | Unknown -> Unknown
    | Proved tags_to_sizes -> (
      match Tag.Map.get_singleton tags_to_sizes with
      | None -> Unknown
      | Some (tag, size) -> Proved (tag, size))

type variant_like_proof =
  { const_ctors : Targetint_31_63.Set.t Or_unknown.t;
    non_const_ctors_with_sizes : Targetint_31_63.Imm.t Tag.Scannable.Map.t
  }

let prove_variant_like env t : variant_like_proof proof_allowing_kind_mismatch =
  (* Format.eprintf "prove_variant:@ %a\n%!" TG.print t; *)
  match expand_head env t with
  | Value (Ok (Variant blocks_imms)) -> begin
    match blocks_imms.blocks with
    | Unknown -> Unknown
    | Known blocks -> (
      match TG.Row_like_for_blocks.all_tags_and_sizes blocks with
      | Unknown -> Unknown
      | Known non_const_ctors_with_sizes -> (
        let non_const_ctors_with_sizes =
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
            | Known imms -> begin
              match prove_naked_immediates env imms with
              | Unknown -> Unknown
              | Invalid -> Known Targetint_31_63.Set.empty
              | Proved const_ctors -> Known const_ctors
            end
          in
          Proved { const_ctors; non_const_ctors_with_sizes }))
  end
  | Value (Ok _) -> Invalid
  | Value Unknown -> Unknown
  | Value Bottom -> Invalid
  | Naked_immediate _ -> Wrong_kind
  | Naked_float _ -> Wrong_kind
  | Naked_int32 _ -> Wrong_kind
  | Naked_int64 _ -> Wrong_kind
  | Naked_nativeint _ -> Wrong_kind
  | Rec_info _ -> Wrong_kind

let prove_is_a_boxed_number env t :
    Flambda_kind.Boxable_number.t proof_allowing_kind_mismatch =
  match expand_head env t with
  | Value Unknown -> Unknown
  | Value (Ok (Variant { blocks; immediates; is_unique = _ })) -> begin
    match blocks, immediates with
    | Unknown, Unknown -> Unknown
    | Unknown, Known imms -> if is_bottom env imms then Invalid else Unknown
    | Known blocks, Unknown ->
      if TG.Row_like_for_blocks.is_bottom blocks
      then Proved Untagged_immediate
      else Unknown
    | Known blocks, Known imms ->
      if is_bottom env imms
      then Invalid
      else if TG.Row_like_for_blocks.is_bottom blocks
      then Proved Untagged_immediate
      else Unknown
  end
  | Value (Ok (Boxed_float _)) -> Proved Naked_float
  | Value (Ok (Boxed_int32 _)) -> Proved Naked_int32
  | Value (Ok (Boxed_int64 _)) -> Proved Naked_int64
  | Value (Ok (Boxed_nativeint _)) -> Proved Naked_nativeint
  | Value _ -> Invalid
  | _ -> Wrong_kind

let prove_is_a_tagged_immediate env t : _ proof_allowing_kind_mismatch =
  match prove_is_a_boxed_number env t with
  | Proved Untagged_immediate -> Proved ()
  | Proved _ -> Unknown
  | Invalid -> Invalid
  | Wrong_kind -> Wrong_kind
  | Unknown -> Unknown

let prove_is_a_boxed_float env t : _ proof_allowing_kind_mismatch =
  match expand_head env t with
  | Value Unknown -> Unknown
  | Value (Ok (Boxed_float _)) -> Proved ()
  | Value _ -> Invalid
  | _ -> Wrong_kind

let prove_is_a_boxed_int32 env t : _ proof_allowing_kind_mismatch =
  match expand_head env t with
  | Value Unknown -> Unknown
  | Value (Ok (Boxed_int32 _)) -> Proved ()
  | Value _ -> Invalid
  | _ -> Wrong_kind

let prove_is_a_boxed_int64 env t : _ proof_allowing_kind_mismatch =
  match expand_head env t with
  | Value Unknown -> Unknown
  | Value (Ok (Boxed_int64 _)) -> Proved ()
  | Value _ -> Invalid
  | _ -> Wrong_kind

let prove_is_a_boxed_nativeint env t : _ proof_allowing_kind_mismatch =
  match expand_head env t with
  | Value Unknown -> Unknown
  | Value (Ok (Boxed_nativeint _)) -> Proved ()
  | Value _ -> Invalid
  | _ -> Wrong_kind

(* CR mshinwell: Factor out code from the following. *)

let prove_boxed_floats env t : _ proof =
  let result_var = Variable.create "result" in
  let result_var' = Bound_var.create result_var Name_mode.normal in
  let result_simple = Simple.var result_var in
  let result_kind = K.naked_float in
  let shape = TG.box_float (TG.alias_type_of result_kind result_simple) in
  match
    Meet_and_join.meet_shape env t ~shape ~result_var:result_var' ~result_kind
  with
  | Bottom -> Invalid
  | Ok env_extension ->
    let env =
      TE.add_definition env
        (Bound_name.create (Name.var result_var) Name_mode.normal)
        result_kind
    in
    let env =
      TE.add_env_extension env env_extension ~meet_type:Meet_and_join.meet
    in
    let t = TE.find env (Name.var result_var) (Some result_kind) in
    prove_naked_floats env t

let prove_boxed_int32s env t : _ proof =
  let result_var = Variable.create "result" in
  let result_var' = Bound_var.create result_var Name_mode.normal in
  let result_simple = Simple.var result_var in
  let result_kind = K.naked_int32 in
  let shape = TG.box_int32 (TG.alias_type_of result_kind result_simple) in
  match
    Meet_and_join.meet_shape env t ~shape ~result_var:result_var' ~result_kind
  with
  | Bottom -> Invalid
  | Ok env_extension ->
    let env =
      TE.add_definition env
        (Bound_name.create (Name.var result_var) Name_mode.normal)
        result_kind
    in
    let env =
      TE.add_env_extension env env_extension ~meet_type:Meet_and_join.meet
    in
    let t = TE.find env (Name.var result_var) (Some result_kind) in
    prove_naked_int32s env t

let prove_boxed_int64s env t : _ proof =
  let result_var = Variable.create "result" in
  let result_var' = Bound_var.create result_var Name_mode.normal in
  let result_simple = Simple.var result_var in
  let result_kind = K.naked_int64 in
  let shape = TG.box_int64 (TG.alias_type_of result_kind result_simple) in
  match
    Meet_and_join.meet_shape env t ~shape ~result_var:result_var' ~result_kind
  with
  | Bottom -> Invalid
  | Ok env_extension ->
    let env =
      TE.add_definition env
        (Bound_name.create (Name.var result_var) Name_mode.normal)
        result_kind
    in
    let env =
      TE.add_env_extension env env_extension ~meet_type:Meet_and_join.meet
    in
    let t = TE.find env (Name.var result_var) (Some result_kind) in
    prove_naked_int64s env t

let prove_boxed_nativeints env t : _ proof =
  let result_var = Variable.create "result" in
  let result_var' = Bound_var.create result_var Name_mode.normal in
  let result_simple = Simple.var result_var in
  let result_kind = K.naked_nativeint in
  let shape = TG.box_nativeint (TG.alias_type_of result_kind result_simple) in
  match
    Meet_and_join.meet_shape env t ~shape ~result_var:result_var' ~result_kind
  with
  | Bottom -> Invalid
  | Ok env_extension ->
    let env =
      TE.add_definition env
        (Bound_name.create (Name.var result_var) Name_mode.normal)
        result_kind
    in
    let env =
      TE.add_env_extension env env_extension ~meet_type:Meet_and_join.meet
    in
    let t = TE.find env (Name.var result_var) (Some result_kind) in
    prove_naked_nativeints env t

let prove_strings env t : String_info.Set.t proof =
  let wrong_kind () =
    Misc.fatal_errorf "Kind error: expected [Value]:@ %a" TG.print t
  in
  match expand_head env t with
  | Value (Ok (String strs)) -> Proved strs
  | Value (Ok _) -> Invalid
  | Value Unknown -> Unknown
  | Value Bottom -> Invalid
  | Naked_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
  | Naked_nativeint _ | Rec_info _ ->
    wrong_kind ()

type prove_tagging_function =
  | Prove_could_be_tagging_of_simple
  | Prove_is_always_tagging_of_simple

let prove_is_tagging_of_simple ~prove_function env ~min_name_mode t :
    Simple.t proof =
  let wrong_kind () =
    Misc.fatal_errorf "Kind error: expected [Value]:@ %a" TG.print t
  in
  match expand_head env t with
  | Value (Ok (Variant { immediates; blocks; is_unique = _ })) -> begin
    match blocks with
    | Unknown -> Unknown
    | Known blocks -> (
      match prove_function with
      | Prove_is_always_tagging_of_simple
        when not (TG.Row_like_for_blocks.is_bottom blocks) ->
        Unknown
      | Prove_is_always_tagging_of_simple
        (* when (Row_like_for_blocks.is_bottom blocks) *)
      | Prove_could_be_tagging_of_simple -> (
        match immediates with
        | Unknown -> Unknown
        | Known t -> (
          let from_alias =
            match
              TE.get_canonical_simple_exn env ~min_name_mode
                (TG.get_alias_exn t)
            with
            | simple -> Some simple
            | exception Not_found -> None
          in
          match from_alias with
          | Some simple -> Proved simple
          | None -> (
            match prove_naked_immediates env t with
            | Unknown -> Unknown
            | Invalid -> Invalid
            | Proved imms -> (
              match Targetint_31_63.Set.get_singleton imms with
              | Some imm ->
                Proved (Simple.const (Reg_width_const.naked_immediate imm))
              | None -> Unknown)))))
  end
  | Value Unknown -> Unknown
  | Value _ -> Invalid
  | Naked_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
  | Naked_nativeint _ | Rec_info _ ->
    wrong_kind ()

let prove_is_always_tagging_of_simple =
  prove_is_tagging_of_simple ~prove_function:Prove_is_always_tagging_of_simple

let prove_could_be_tagging_of_simple =
  prove_is_tagging_of_simple ~prove_function:Prove_could_be_tagging_of_simple

let[@inline always] prove_boxed_number_containing_simple
    ~contents_of_boxed_number env ~min_name_mode t : Simple.t proof =
  match expand_head env t with
  | Value (Ok ty_value) -> begin
    match contents_of_boxed_number ty_value with
    | None -> Invalid
    | Some ty -> (
      match
        TE.get_canonical_simple_exn env ~min_name_mode (TG.get_alias_exn ty)
      with
      | simple -> Proved simple
      | exception Not_found -> Unknown)
  end
  | Value Unknown -> Unknown
  | Value Bottom -> Invalid
  | Naked_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
  | Naked_nativeint _ | Rec_info _ ->
    Misc.fatal_errorf "Kind error: expected [Value]:@ %a" TG.print t

let prove_boxed_float_containing_simple =
  prove_boxed_number_containing_simple
    ~contents_of_boxed_number:(fun (ty_value : TG.head_of_kind_value) ->
      match ty_value with
      | Boxed_float ty -> Some ty
      | Variant _ | Boxed_int32 _ | Boxed_int64 _ | Boxed_nativeint _
      | Closures _ | String _ | Array _ ->
        None)

let prove_boxed_int32_containing_simple =
  prove_boxed_number_containing_simple
    ~contents_of_boxed_number:(fun (ty_value : TG.head_of_kind_value) ->
      match ty_value with
      | Boxed_int32 ty -> Some ty
      | Variant _ | Boxed_float _ | Boxed_int64 _ | Boxed_nativeint _
      | Closures _ | String _ | Array _ ->
        None)

let prove_boxed_int64_containing_simple =
  prove_boxed_number_containing_simple
    ~contents_of_boxed_number:(fun (ty_value : TG.head_of_kind_value) ->
      match ty_value with
      | Boxed_int64 ty -> Some ty
      | Variant _ | Boxed_float _ | Boxed_int32 _ | Boxed_nativeint _
      | Closures _ | String _ | Array _ ->
        None)

let prove_boxed_nativeint_containing_simple =
  prove_boxed_number_containing_simple
    ~contents_of_boxed_number:(fun (ty_value : TG.head_of_kind_value) ->
      match ty_value with
      | Boxed_nativeint ty -> Some ty
      | Variant _ | Boxed_float _ | Boxed_int32 _ | Boxed_int64 _ | Closures _
      | String _ | Array _ ->
        None)

let[@inline] prove_block_field_simple_aux env ~min_name_mode t get_field :
    Simple.t proof =
  let wrong_kind () =
    Misc.fatal_errorf "Kind error: expected [Value]:@ %a" TG.print t
  in
  match expand_head env t with
  | Value (Ok (Variant { immediates; blocks; is_unique = _ })) -> begin
    match immediates with
    | Unknown -> Unknown
    | Known imms -> begin
      match blocks with
      | Unknown -> Unknown
      | Known blocks -> (
        if TG.Row_like_for_blocks.is_bottom blocks
        then Invalid
        else if not (TG.is_obviously_bottom imms)
        then Unknown
        else
          match (get_field blocks : _ Or_unknown_or_bottom.t) with
          | Bottom -> Invalid
          | Unknown -> Unknown
          | Ok ty -> begin
            match TG.get_alias_exn ty with
            | simple -> begin
              match TE.get_canonical_simple_exn env ~min_name_mode simple with
              | simple -> Proved simple
              | exception Not_found -> Unknown
            end
            | exception Not_found -> Unknown
          end)
    end
  end
  | Value (Ok _) -> Invalid
  | Value Unknown -> Unknown
  | Value Bottom -> Invalid
  | Naked_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
  | Naked_nativeint _ | Rec_info _ ->
    wrong_kind ()

let prove_block_field_simple env ~min_name_mode t field_index =
  let[@inline] get blocks =
    TG.Row_like_for_blocks.get_field blocks field_index
  in
  (prove_block_field_simple_aux [@inlined]) env ~min_name_mode t get

let prove_variant_field_simple env ~min_name_mode t variant_tag field_index =
  let[@inline] get blocks =
    TG.Row_like_for_blocks.get_variant_field blocks variant_tag field_index
  in
  (prove_block_field_simple_aux [@inlined]) env ~min_name_mode t get

let prove_project_var_simple env ~min_name_mode t env_var : Simple.t proof =
  let wrong_kind () =
    Misc.fatal_errorf "Kind error: expected [Value]:@ %a" TG.print t
  in
  match expand_head env t with
  | Value (Ok (Closures { by_closure_id })) -> (
    match TG.Row_like_for_closures.get_env_var by_closure_id env_var with
    | Unknown -> Unknown
    | Known ty -> begin
      match TG.get_alias_exn ty with
      | simple -> begin
        match TE.get_canonical_simple_exn env ~min_name_mode simple with
        | simple -> Proved simple
        | exception Not_found -> Unknown
      end
      | exception Not_found -> Unknown
    end)
  | Value (Ok _) -> Invalid
  | Value Unknown -> Unknown
  | Value Bottom -> Invalid
  | Naked_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
  | Naked_nativeint _ | Rec_info _ ->
    wrong_kind ()

let prove_rec_info env t : Rec_info_expr.t proof =
  let wrong_kind () =
    Misc.fatal_errorf "Kind error: expected [Rec_info]:@ %a" TG.print t
  in
  match expand_head env t with
  | Rec_info (Ok rec_info_expr) -> Proved rec_info_expr
  | Rec_info Unknown -> Unknown
  | Rec_info Bottom -> Invalid
  | Value _ | Naked_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
  | Naked_nativeint _ ->
    wrong_kind ()
