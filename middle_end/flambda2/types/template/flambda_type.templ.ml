(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018--2019 OCamlPro SAS                                    *)
(*   Copyright 2018--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(* CR mshinwell: Decide on doc or non-doc comments in here. There are some
   modules which aren't exposed in the interface but probably require
   documentation. *)

(* CR mshinwell: Remove when warning 60 fixed *)
[@@@ocaml.warning "-60"]

module Float = Numeric_types.Float_by_bit_pattern
module Int32 = Numeric_types.Int32
module Int64 = Numeric_types.Int64
module K = Flambda_kind

(* -- module rec binding here -- *)

include Type_grammar

type flambda_type = t

let meet env t1 t2 : _ Or_bottom.t =
  let meet_env = Meet_env.create env in
  meet meet_env t1 t2

let meet_shape env t ~shape ~result_var ~result_kind : _ Or_bottom.t =
  let result = Name_in_binding_pos.var result_var in
  let env = Typing_env.add_definition env result result_kind in
  match meet env t shape with
  | Bottom -> Bottom
  | Ok (_meet_ty, env_extension) -> Ok env_extension

let join ?bound_name central_env ~left_env ~left_ty ~right_env ~right_ty =
  let join_env = Join_env.create central_env ~left_env ~right_env in
  match join ?bound_name join_env left_ty right_ty with
  | Unknown -> unknown_like left_ty
  | Known ty -> ty

let arity_of_list ts = Flambda_arity.create (List.map kind ts)

type typing_env = Typing_env.t

type typing_env_extension = Typing_env_extension.t

let invariant _env _t = () (* CR mshinwell: implement *)

type 'a type_accessor = Typing_env.t -> 'a

let unknown_types_from_arity arity = List.map (fun kind -> unknown kind) arity

let rec unknown_with_descr (descr : Flambda_kind.With_subkind.descr) =
  match descr with
  | Any_value -> any_value ()
  | Naked_number Naked_immediate -> any_naked_immediate ()
  | Naked_number Naked_float -> any_naked_float ()
  | Naked_number Naked_int32 -> any_naked_int32 ()
  | Naked_number Naked_int64 -> any_naked_int64 ()
  | Naked_number Naked_nativeint -> any_naked_nativeint ()
  | Boxed_float -> any_boxed_float ()
  | Boxed_int32 -> any_boxed_int32 ()
  | Boxed_int64 -> any_boxed_int64 ()
  | Boxed_nativeint -> any_boxed_nativeint ()
  | Tagged_immediate -> any_tagged_immediate ()
  | Rec_info -> any_rec_info ()
  | Block { tag; fields } ->
    assert (not (Tag.equal tag Tag.double_array_tag));
    immutable_block ~is_unique:false tag ~field_kind:Flambda_kind.value
      ~fields:(List.map unknown_with_descr fields)
  | Float_block { num_fields } ->
    immutable_block ~is_unique:false Tag.double_array_tag
      ~field_kind:Flambda_kind.naked_float
      ~fields:(List.init num_fields (fun _ -> any_naked_float ()))

let unknown_with_subkind kind =
  unknown_with_descr (Flambda_kind.With_subkind.descr kind)

let unknown_types_from_arity_with_subkinds arity =
  List.map (fun kind -> unknown_with_subkind kind) arity

let bottom_types_from_arity arity = List.map (fun kind -> bottom kind) arity

let is_bottom env t =
  match expand_head t env with
  | Value Bottom
  | Naked_immediate Bottom
  | Naked_float Bottom
  | Naked_int32 Bottom
  | Naked_int64 Bottom
  | Naked_nativeint Bottom
  | Rec_info Bottom ->
    true
  | Const _ | Value _ | Naked_immediate _ | Naked_float _ | Naked_int32 _
  | Naked_int64 _ | Naked_nativeint _ | Rec_info _ ->
    false

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
  let original_kind = kind t in
  if not (K.equal original_kind K.value)
  then Misc.fatal_errorf "Type %a is not of kind value" print t;
  (* XXX This probably shouldn't be using [get_alias] *)
  (* XXX This needs to match the equal-to-tagged-immediates function below *)
  match get_alias_exn t with
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
            Simple.print simple K.print original_kind print t)
      ~name:(fun _ ~coercion:_ : _ proof ->
        match
          Typing_env.get_canonical_simple_exn env simple
            ~min_name_mode:Name_mode.normal
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
                let kind = kind t in
                Misc.fatal_errorf
                  "Kind returned by [get_canonical_simple] (%a) doesn't match \
                   the kind of the returned [Simple] %a:@ %a"
                  K.print kind Simple.print simple print t)
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
  match expand_head t env with
  | Const _ -> Invalid
  | Value (Ok (Closures closures)) -> begin
    match
      Row_like.For_closures_entry_by_set_of_closures_contents.get_singleton
        closures.by_closure_id
    with
    | None -> Unknown
    | Some ((closure_id, set_of_closures_contents), closures_entry) -> (
      let closure_ids =
        Set_of_closures_contents.closures set_of_closures_contents
      in
      assert (Closure_id.Set.mem closure_id closure_ids);
      let function_decl =
        Closures_entry.find_function_declaration closures_entry closure_id
      in
      match function_decl with
      | Bottom -> Invalid
      | Ok function_decl -> Proved (closure_id, closures_entry, function_decl))
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
  | Wrong_kind -> Misc.fatal_errorf "Type has wrong kind: %a" print t

(* CR mshinwell: Try to functorise or otherwise factor out across the various
   number kinds. *)
let prove_naked_floats env t : _ proof =
  let wrong_kind () =
    Misc.fatal_errorf "Kind error: expected [Naked_float]:@ %a" print t
  in
  match expand_head t env with
  | Const (Naked_float f) -> Proved (Float.Set.singleton f)
  | Const
      ( Naked_immediate _ | Tagged_immediate _ | Naked_int32 _ | Naked_int64 _
      | Naked_nativeint _ ) ->
    wrong_kind ()
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
    Misc.fatal_errorf "Kind error: expected [Naked_int32]:@ %a" print t
  in
  match expand_head t env with
  | Const (Naked_int32 i) -> Proved (Int32.Set.singleton i)
  | Const
      ( Naked_immediate _ | Tagged_immediate _ | Naked_float _ | Naked_int64 _
      | Naked_nativeint _ ) ->
    wrong_kind ()
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
    Misc.fatal_errorf "Kind error: expected [Naked_int64]:@ %a" print t
  in
  match expand_head t env with
  | Const (Naked_int64 i) -> Proved (Int64.Set.singleton i)
  | Const
      ( Naked_immediate _ | Tagged_immediate _ | Naked_float _ | Naked_int32 _
      | Naked_nativeint _ ) ->
    wrong_kind ()
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
    Misc.fatal_errorf "Kind error: expected [Naked_nativeint]:@ %a" print t
  in
  match expand_head t env with
  | Const (Naked_nativeint i) -> Proved (Targetint_32_64.Set.singleton i)
  | Const
      ( Naked_immediate _ | Tagged_immediate _ | Naked_float _ | Naked_int32 _
      | Naked_int64 _ ) ->
    wrong_kind ()
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
    Misc.fatal_errorf "Kind error: expected [Value]:@ %a" print t
  in
  match expand_head t env with
  | Const (Tagged_immediate _) -> Proved true
  | Const _ -> wrong_kind ()
  | Value (Ok (Variant blocks_imms)) -> begin
    match blocks_imms.blocks, blocks_imms.immediates with
    | Unknown, Unknown -> Unknown
    | Unknown, Known imms ->
      if is_bottom env imms then Proved false else Unknown
    | Known blocks, Unknown ->
      if Row_like.For_blocks.is_bottom blocks then Proved true else Unknown
    | Known blocks, Known imms ->
      (* CR mshinwell: Should we tighten things up by causing fatal errors in
         cases such as [blocks] and [imms] both being bottom? *)
      if Row_like.For_blocks.is_bottom blocks
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
    Misc.fatal_errorf "Kind error: expected [Value]:@ %a" print t
  in
  match expand_head t env with
  | Const (Tagged_immediate _) -> Unknown
  | Const _ -> wrong_kind ()
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
          match Row_like.For_blocks.all_tags blocks with
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
    Misc.fatal_errorf "Kind error: expected [Naked_immediate]:@ %a" print t
  in
  match expand_head t env with
  | Const (Naked_immediate i) -> Proved (Targetint_31_63.Set.singleton i)
  | Const
      ( Tagged_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
      | Naked_nativeint _ ) ->
    wrong_kind ()
  | Naked_immediate (Ok (Naked_immediates is)) ->
    (* CR mshinwell: As noted elsewhere, add abstraction to avoid the need for
       these checks *)
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
    Misc.fatal_errorf "Kind error: expected [Value]:@ %a" print t
  in
  match expand_head t env with
  | Const (Tagged_immediate imm) -> Proved (Targetint_31_63.Set.singleton imm)
  | Const
      ( Naked_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
      | Naked_nativeint _ ) ->
    wrong_kind ()
  | Value (Ok (Variant blocks_imms)) -> begin
    match blocks_imms.blocks, blocks_imms.immediates with
    | Unknown, Unknown | Unknown, Known _ | Known _, Unknown -> Unknown
    | Known blocks, Known imms ->
      (* CR mshinwell: Check this. Again it depends on the context; is this a
         context where variants are ok? *)
      if not (Row_like.For_blocks.is_bottom blocks)
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
    Misc.fatal_errorf "Kind error: expected [Value]:@ %a" print t
  in
  match expand_head t env with
  | Const (Tagged_immediate _) -> Unknown
  | Const _ -> wrong_kind ()
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
          match Row_like.For_blocks.all_tags_and_sizes blocks with
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
  if not (Flambda_kind.equal (kind t) Flambda_kind.value)
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
  (* Format.eprintf "prove_variant:@ %a\n%!" print t; *)
  match expand_head t env with
  | Const (Tagged_immediate imm) ->
    Proved
      { const_ctors = Known (Targetint_31_63.Set.singleton imm);
        non_const_ctors_with_sizes = Tag.Scannable.Map.empty
      }
  | Const _ -> Wrong_kind
  | Value (Ok (Variant blocks_imms)) -> begin
    match blocks_imms.blocks with
    | Unknown -> Unknown
    | Known blocks -> (
      match Row_like.For_blocks.all_tags_and_sizes blocks with
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
  match expand_head t env with
  | Const (Tagged_immediate _) -> Proved Untagged_immediate
  | Const _ -> Wrong_kind
  | Value Unknown -> Unknown
  | Value (Ok (Variant { blocks; immediates; is_unique = _ })) -> begin
    match blocks, immediates with
    | Unknown, Unknown -> Unknown
    | Unknown, Known imms -> if is_bottom env imms then Invalid else Unknown
    | Known blocks, Unknown ->
      if Row_like.For_blocks.is_bottom blocks
      then Proved Untagged_immediate
      else Unknown
    | Known blocks, Known imms ->
      if is_bottom env imms
      then Invalid
      else if Row_like.For_blocks.is_bottom blocks
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
  match expand_head t env with
  | Const _ -> Wrong_kind
  | Value Unknown -> Unknown
  | Value (Ok (Boxed_float _)) -> Proved ()
  | Value _ -> Invalid
  | _ -> Wrong_kind

let prove_is_a_boxed_int32 env t : _ proof_allowing_kind_mismatch =
  match expand_head t env with
  | Const _ -> Wrong_kind
  | Value Unknown -> Unknown
  | Value (Ok (Boxed_int32 _)) -> Proved ()
  | Value _ -> Invalid
  | _ -> Wrong_kind

let prove_is_a_boxed_int64 env t : _ proof_allowing_kind_mismatch =
  match expand_head t env with
  | Const _ -> Wrong_kind
  | Value Unknown -> Unknown
  | Value (Ok (Boxed_int64 _)) -> Proved ()
  | Value _ -> Invalid
  | _ -> Wrong_kind

let prove_is_a_boxed_nativeint env t : _ proof_allowing_kind_mismatch =
  match expand_head t env with
  | Const _ -> Wrong_kind
  | Value Unknown -> Unknown
  | Value (Ok (Boxed_nativeint _)) -> Proved ()
  | Value _ -> Invalid
  | _ -> Wrong_kind

(* CR mshinwell: Factor out code from the following. *)

let prove_boxed_floats env t : _ proof =
  let result_var = Variable.create "result" in
  let result_var' = Var_in_binding_pos.create result_var Name_mode.normal in
  let result_simple = Simple.var result_var in
  let result_kind = K.naked_float in
  let shape = box_float (alias_type_of result_kind result_simple) in
  match meet_shape env t ~shape ~result_var:result_var' ~result_kind with
  | Bottom -> Invalid
  | Ok env_extension ->
    let env =
      Typing_env.add_definition env
        (Name_in_binding_pos.create (Name.var result_var) Name_mode.normal)
        result_kind
    in
    let env = Typing_env.add_env_extension env env_extension in
    let t = Typing_env.find env (Name.var result_var) (Some result_kind) in
    prove_naked_floats env t

let prove_boxed_int32s env t : _ proof =
  let result_var = Variable.create "result" in
  let result_var' = Var_in_binding_pos.create result_var Name_mode.normal in
  let result_simple = Simple.var result_var in
  let result_kind = K.naked_int32 in
  let shape = box_int32 (alias_type_of result_kind result_simple) in
  match meet_shape env t ~shape ~result_var:result_var' ~result_kind with
  | Bottom -> Invalid
  | Ok env_extension ->
    let env =
      Typing_env.add_definition env
        (Name_in_binding_pos.create (Name.var result_var) Name_mode.normal)
        result_kind
    in
    let env = Typing_env.add_env_extension env env_extension in
    let t = Typing_env.find env (Name.var result_var) (Some result_kind) in
    prove_naked_int32s env t

let prove_boxed_int64s env t : _ proof =
  let result_var = Variable.create "result" in
  let result_var' = Var_in_binding_pos.create result_var Name_mode.normal in
  let result_simple = Simple.var result_var in
  let result_kind = K.naked_int64 in
  let shape = box_int64 (alias_type_of result_kind result_simple) in
  match meet_shape env t ~shape ~result_var:result_var' ~result_kind with
  | Bottom -> Invalid
  | Ok env_extension ->
    let env =
      Typing_env.add_definition env
        (Name_in_binding_pos.create (Name.var result_var) Name_mode.normal)
        result_kind
    in
    let env = Typing_env.add_env_extension env env_extension in
    let t = Typing_env.find env (Name.var result_var) (Some result_kind) in
    prove_naked_int64s env t

let prove_boxed_nativeints env t : _ proof =
  let result_var = Variable.create "result" in
  let result_var' = Var_in_binding_pos.create result_var Name_mode.normal in
  let result_simple = Simple.var result_var in
  let result_kind = K.naked_nativeint in
  let shape = box_nativeint (alias_type_of result_kind result_simple) in
  match meet_shape env t ~shape ~result_var:result_var' ~result_kind with
  | Bottom -> Invalid
  | Ok env_extension ->
    let env =
      Typing_env.add_definition env
        (Name_in_binding_pos.create (Name.var result_var) Name_mode.normal)
        result_kind
    in
    let env = Typing_env.add_env_extension env env_extension in
    let t = Typing_env.find env (Name.var result_var) (Some result_kind) in
    prove_naked_nativeints env t

let prove_strings env t : String_info.Set.t proof =
  let wrong_kind () =
    Misc.fatal_errorf "Kind error: expected [Value]:@ %a" print t
  in
  match expand_head t env with
  | Const _ -> if K.equal (kind t) K.value then Invalid else wrong_kind ()
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
    Misc.fatal_errorf "Kind error: expected [Value]:@ %a" print t
  in
  match expand_head t env with
  | Const (Tagged_immediate imm) ->
    Proved (Simple.const (Reg_width_const.naked_immediate imm))
  | Value (Ok (Variant { immediates; blocks; is_unique = _ })) -> begin
    match blocks with
    | Unknown -> Unknown
    | Known blocks -> (
      match prove_function with
      | Prove_is_always_tagging_of_simple
        when not (Row_like.For_blocks.is_bottom blocks) ->
        Unknown
      | Prove_is_always_tagging_of_simple
        (* when (Row_like.For_blocks.is_bottom blocks) *)
      | Prove_could_be_tagging_of_simple -> (
        match immediates with
        | Unknown -> Unknown
        | Known t -> (
          let from_alias =
            match
              Typing_env.get_canonical_simple_exn env ~min_name_mode
                (get_alias_exn t)
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
  | Const _ | Naked_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
  | Naked_nativeint _ | Rec_info _ ->
    wrong_kind ()

let prove_is_always_tagging_of_simple =
  prove_is_tagging_of_simple ~prove_function:Prove_is_always_tagging_of_simple

let prove_could_be_tagging_of_simple =
  prove_is_tagging_of_simple ~prove_function:Prove_could_be_tagging_of_simple

let[@inline always] prove_boxed_number_containing_simple
    ~contents_of_boxed_number env ~min_name_mode t : Simple.t proof =
  match expand_head t env with
  | Value (Ok ty_value) -> begin
    match contents_of_boxed_number ty_value with
    | None -> Invalid
    | Some ty -> (
      match
        Typing_env.get_canonical_simple_exn env ~min_name_mode
          (get_alias_exn ty)
      with
      | simple -> Proved simple
      | exception Not_found -> Unknown)
  end
  | Value Unknown -> Unknown
  | Value Bottom -> Invalid
  | Const _ | Naked_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
  | Naked_nativeint _ | Rec_info _ ->
    Misc.fatal_errorf "Kind error: expected [Value]:@ %a" print t

let prove_boxed_float_containing_simple =
  prove_boxed_number_containing_simple
    ~contents_of_boxed_number:(fun (ty_value : Type_of_kind_value0.t) ->
      match ty_value with
      | Boxed_float ty -> Some ty
      | Variant _ | Boxed_int32 _ | Boxed_int64 _ | Boxed_nativeint _
      | Closures _ | String _ | Array _ ->
        None)

let prove_boxed_int32_containing_simple =
  prove_boxed_number_containing_simple
    ~contents_of_boxed_number:(fun (ty_value : Type_of_kind_value0.t) ->
      match ty_value with
      | Boxed_int32 ty -> Some ty
      | Variant _ | Boxed_float _ | Boxed_int64 _ | Boxed_nativeint _
      | Closures _ | String _ | Array _ ->
        None)

let prove_boxed_int64_containing_simple =
  prove_boxed_number_containing_simple
    ~contents_of_boxed_number:(fun (ty_value : Type_of_kind_value0.t) ->
      match ty_value with
      | Boxed_int64 ty -> Some ty
      | Variant _ | Boxed_float _ | Boxed_int32 _ | Boxed_nativeint _
      | Closures _ | String _ | Array _ ->
        None)

let prove_boxed_nativeint_containing_simple =
  prove_boxed_number_containing_simple
    ~contents_of_boxed_number:(fun (ty_value : Type_of_kind_value0.t) ->
      match ty_value with
      | Boxed_nativeint ty -> Some ty
      | Variant _ | Boxed_float _ | Boxed_int32 _ | Boxed_int64 _ | Closures _
      | String _ | Array _ ->
        None)

let[@inline] prove_block_field_simple_aux env ~min_name_mode t get_field :
    Simple.t proof =
  let wrong_kind () =
    Misc.fatal_errorf "Kind error: expected [Value]:@ %a" print t
  in
  match expand_head t env with
  | Const _ -> if K.equal (kind t) K.value then Invalid else wrong_kind ()
  | Value (Ok (Variant { immediates; blocks; is_unique = _ })) -> begin
    match immediates with
    | Unknown -> Unknown
    | Known imms -> begin
      match blocks with
      | Unknown -> Unknown
      | Known blocks -> (
        if Row_like.For_blocks.is_bottom blocks
        then Invalid
        else if not (is_obviously_bottom imms)
        then Unknown
        else
          match (get_field blocks : _ Or_unknown_or_bottom.t) with
          | Bottom -> Invalid
          | Unknown -> Unknown
          | Ok ty -> begin
            match get_alias_exn ty with
            | simple -> begin
              match
                Typing_env.get_canonical_simple_exn env ~min_name_mode simple
              with
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
  let[@inline] get blocks = Row_like.For_blocks.get_field blocks field_index in
  (prove_block_field_simple_aux [@inlined]) env ~min_name_mode t get

let prove_variant_field_simple env ~min_name_mode t variant_tag field_index =
  let[@inline] get blocks =
    Row_like.For_blocks.get_variant_field blocks variant_tag field_index
  in
  (prove_block_field_simple_aux [@inlined]) env ~min_name_mode t get

let prove_project_var_simple env ~min_name_mode t env_var : Simple.t proof =
  let wrong_kind () =
    Misc.fatal_errorf "Kind error: expected [Value]:@ %a" print t
  in
  match expand_head t env with
  | Const _ -> if K.equal (kind t) K.value then Invalid else wrong_kind ()
  | Value (Ok (Closures { by_closure_id })) -> (
    let module RFC = Row_like.For_closures_entry_by_set_of_closures_contents in
    match RFC.get_env_var by_closure_id env_var with
    | Unknown -> Unknown
    | Known ty -> begin
      match get_alias_exn ty with
      | simple -> begin
        match Typing_env.get_canonical_simple_exn env ~min_name_mode simple with
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
    Misc.fatal_errorf "Kind error: expected [Rec_info]:@ %a" print t
  in
  match expand_head t env with
  | Rec_info (Ok rec_info_expr) -> Proved rec_info_expr
  | Rec_info Unknown -> Unknown
  | Rec_info Bottom -> Invalid
  | Const _ | Value _ | Naked_immediate _ | Naked_float _ | Naked_int32 _
  | Naked_int64 _ | Naked_nativeint _ ->
    wrong_kind ()

type to_lift =
  | Immutable_block of
      { tag : Tag.Scannable.t;
        is_unique : bool;
        fields : var_or_symbol_or_tagged_immediate list
      }
  | Boxed_float of Float.t
  | Boxed_int32 of Int32.t
  | Boxed_int64 of Int64.t
  | Boxed_nativeint of Targetint_32_64.t

type reification_result =
  | Lift of to_lift
  | Lift_set_of_closures of
      { closure_id : Closure_id.t;
        function_decls : Function_declaration_type.Inlinable.t Closure_id.Map.t;
        closure_vars : Simple.t Var_within_closure.Map.t
      }
  | Simple of Simple.t
  | Cannot_reify
  | Invalid

(* CR mshinwell: Think more to identify all the cases that should be in this
   function. *)
let reify ?allowed_if_free_vars_defined_in ?additional_free_var_criterion
    ?disallowed_free_vars ?(allow_unique = false) env ~min_name_mode t :
    reification_result =
  let var_allowed var =
    match allowed_if_free_vars_defined_in with
    | None -> false
    | Some allowed_if_free_vars_defined_in -> (
      Typing_env.mem ~min_name_mode allowed_if_free_vars_defined_in
        (Name.var var)
      && begin
           match additional_free_var_criterion with
           | None -> true
           | Some criterion -> criterion var
         end
      &&
      match disallowed_free_vars with
      | None -> true
      | Some disallowed_free_vars ->
        not (Variable.Set.mem var disallowed_free_vars))
  in
  let canonical_simple =
    match
      Typing_env.get_alias_then_canonical_simple_exn env ~min_name_mode t
    with
    | exception Not_found -> None
    | canonical_simple -> Some canonical_simple
  in
  match canonical_simple with
  | Some canonical_simple when Simple.is_symbol canonical_simple ->
    (* Don't lift things that are already bound to symbols. Apart from anything
       else, this could cause aliases between symbols, which are currently
       forbidden (every symbol has the same binding time). *)
    Cannot_reify
  | canonical_simple_opt -> (
    let try_canonical_simple () =
      match canonical_simple_opt with
      | None -> Cannot_reify
      | Some canonical_simple -> Simple canonical_simple
    in
    match expand_head t env with
    | Const const -> Simple (Simple.const_from_descr const)
    | Value (Ok (Variant blocks_imms)) -> (
      if blocks_imms.is_unique && not allow_unique
      then try_canonical_simple ()
      else
        match blocks_imms.blocks, blocks_imms.immediates with
        | Known blocks, Known imms ->
          if is_bottom env imms
          then
            match Row_like.For_blocks.get_singleton blocks with
            | None -> try_canonical_simple ()
            | Some ((tag, size), field_types) -> (
              assert (
                Targetint_31_63.Imm.equal size
                  (Product.Int_indexed.width field_types));
              (* CR mshinwell: Could recognise other things, e.g. tagged
                 immediates and float arrays, supported by [Static_part]. *)
              match Tag.Scannable.of_tag tag with
              | None -> try_canonical_simple ()
              | Some tag ->
                let field_types = Product.Int_indexed.components field_types in
                let vars_or_symbols_or_tagged_immediates =
                  List.filter_map
                    (fun field_type : var_or_symbol_or_tagged_immediate option ->
                      match
                        (* CR mshinwell: Change this to a function
                           [prove_equals_to_simple]? *)
                        prove_equals_to_var_or_symbol_or_tagged_immediate env
                          field_type
                      with
                      | Proved (_, coercion) when not (Coercion.is_id coercion)
                        ->
                        (* CR-someday lmaurer: Support lifting things whose
                           fields have coercions. *)
                        None
                      | Proved (Var var, _) ->
                        if var_allowed var then Some (Var var) else None
                      | Proved (Symbol sym, _) -> Some (Symbol sym)
                      | Proved (Tagged_immediate imm, _) ->
                        Some (Tagged_immediate imm)
                      (* CR mshinwell: [Invalid] should propagate up *)
                      | Unknown | Invalid -> None)
                    field_types
                in
                if List.compare_lengths field_types
                     vars_or_symbols_or_tagged_immediates
                   = 0
                then
                  Lift
                    (Immutable_block
                       { tag;
                         is_unique = blocks_imms.is_unique;
                         fields = vars_or_symbols_or_tagged_immediates
                       })
                else try_canonical_simple ())
          else if Row_like.For_blocks.is_bottom blocks
          then
            match prove_naked_immediates env imms with
            | Proved imms -> begin
              match Targetint_31_63.Set.get_singleton imms with
              | None -> try_canonical_simple ()
              | Some imm ->
                Simple (Simple.const (Reg_width_const.tagged_immediate imm))
            end
            | Unknown -> try_canonical_simple ()
            | Invalid -> Invalid
          else try_canonical_simple ()
        | _, _ -> try_canonical_simple ())
    | Value (Ok (Closures closures)) -> begin
      (* CR mshinwell: Here and above, move to separate function. *)
      match
        Row_like.For_closures_entry_by_set_of_closures_contents.get_singleton
          closures.by_closure_id
      with
      | None -> try_canonical_simple ()
      | Some ((closure_id, contents), closures_entry) ->
        (* CR mshinwell: What about if there were multiple entries in the
           row-like structure for the same closure ID? This is ruled out by
           [get_singleton] at the moment. We should probably choose the best
           entry from the [Row_like] structure. *)
        let closure_ids = Set_of_closures_contents.closures contents in
        (* CR mshinwell: Should probably check
           [Set_of_closures_contents.closure_vars contents]? *)
        if not (Closure_id.Set.mem closure_id closure_ids)
        then
          Misc.fatal_errorf
            "Closure ID %a expected in set-of-closures-contents in closure \
             type@ %a"
            Closure_id.print closure_id print t;
        (* Format.eprintf "Reifying closure %a@.Contents:@.%a@.Entry:@.%a@.Typing env:@.%a@.Backtrace:@.%s@."
         *   Closure_id.print closure_id
         *   Set_of_closures_contents.print contents
         *   Closures_entry.print closures_entry
         *   Typing_env.print env
         *   Printexc.(raw_backtrace_to_string (get_callstack 10)); *)
        let function_decls_with_closure_vars =
          Closure_id.Set.fold
            (fun closure_id function_decls_with_closure_vars ->
              match
                Closures_entry.find_function_declaration closures_entry
                  closure_id
              with
              | Bottom -> function_decls_with_closure_vars
              | Ok function_decl -> (
                match function_decl with
                | Bottom | Unknown | Ok (Non_inlinable _) ->
                  function_decls_with_closure_vars
                | Ok (Inlinable inlinable_decl) ->
                  (* CR mshinwell: We're ignoring [coercion] *)
                  let closure_var_types =
                    Closures_entry.closure_var_types closures_entry
                  in
                  let closure_var_simples =
                    Var_within_closure.Map.filter_map
                      (fun _closure_var closure_var_type ->
                        match
                          prove_equals_to_var_or_symbol_or_tagged_immediate env
                            closure_var_type
                        with
                        | Proved (Var var, coercion) ->
                          if var_allowed var
                          then
                            Some
                              (Simple.with_coercion (Simple.var var) coercion)
                          else None
                        | Proved (Symbol sym, coercion) ->
                          Some
                            (Simple.with_coercion (Simple.symbol sym) coercion)
                        | Proved (Tagged_immediate imm, coercion) ->
                          Some
                            (Simple.with_coercion
                               (Simple.const
                                  (Reg_width_const.tagged_immediate imm))
                               coercion)
                        | Unknown | Invalid -> None)
                      closure_var_types
                  in
                  if Var_within_closure.Map.cardinal closure_var_types
                     <> Var_within_closure.Map.cardinal closure_var_simples
                  then function_decls_with_closure_vars
                  else
                    Closure_id.Map.add closure_id
                      (inlinable_decl, closure_var_simples)
                      function_decls_with_closure_vars))
            closure_ids Closure_id.Map.empty
        in
        if Closure_id.Set.cardinal closure_ids
           <> Closure_id.Map.cardinal function_decls_with_closure_vars
        then try_canonical_simple ()
        else
          let function_decls =
            Closure_id.Map.map
              (fun (function_decl, _) -> function_decl)
              function_decls_with_closure_vars
          in
          let closure_vars =
            Closure_id.Map.fold
              (fun _closure_id (_function_decl, closure_var_simples)
                   all_closure_vars ->
                Var_within_closure.Map.fold
                  (fun closure_var simple all_closure_vars ->
                    begin
                      match
                        Var_within_closure.Map.find closure_var all_closure_vars
                      with
                      | exception Not_found -> ()
                      | existing_simple ->
                        if not (Simple.equal simple existing_simple)
                        then
                          Misc.fatal_errorf
                            "Disagreement on %a and %a (closure var %a)@ \
                             whilst reifying set-of-closures from:@ %a"
                            Simple.print simple Simple.print existing_simple
                            Var_within_closure.print closure_var print t
                    end;
                    Var_within_closure.Map.add closure_var simple
                      all_closure_vars)
                  closure_var_simples all_closure_vars)
              function_decls_with_closure_vars Var_within_closure.Map.empty
          in
          Lift_set_of_closures { closure_id; function_decls; closure_vars }
    end
    (* CR mshinwell: share code with [prove_equals_tagged_immediates], above *)
    | Naked_immediate (Ok (Is_int scrutinee_ty)) -> begin
      match prove_is_int env scrutinee_ty with
      | Proved true -> Simple Simple.untagged_const_true
      | Proved false -> Simple Simple.untagged_const_false
      | Unknown -> try_canonical_simple ()
      | Invalid -> Invalid
    end
    | Naked_immediate (Ok (Get_tag block_ty)) -> begin
      match prove_tags_must_be_a_block env block_ty with
      | Proved tags -> (
        let is =
          Tag.Set.fold
            (fun tag is -> Targetint_31_63.Set.add (Tag.to_target_imm tag) is)
            tags Targetint_31_63.Set.empty
        in
        match Targetint_31_63.Set.get_singleton is with
        | None -> try_canonical_simple ()
        | Some i -> Simple (Simple.const (Reg_width_const.naked_immediate i)))
      | Unknown -> try_canonical_simple ()
      | Invalid -> Invalid
    end
    | Naked_float (Ok fs) -> begin
      match Float.Set.get_singleton fs with
      | None -> try_canonical_simple ()
      | Some f -> Simple (Simple.const (Reg_width_const.naked_float f))
    end
    | Naked_int32 (Ok ns) -> begin
      match Int32.Set.get_singleton ns with
      | None -> try_canonical_simple ()
      | Some n -> Simple (Simple.const (Reg_width_const.naked_int32 n))
    end
    | Naked_int64 (Ok ns) -> begin
      match Int64.Set.get_singleton ns with
      | None -> try_canonical_simple ()
      | Some n -> Simple (Simple.const (Reg_width_const.naked_int64 n))
    end
    | Naked_nativeint (Ok ns) -> begin
      match Targetint_32_64.Set.get_singleton ns with
      | None -> try_canonical_simple ()
      | Some n -> Simple (Simple.const (Reg_width_const.naked_nativeint n))
    end
    | Value (Ok (Boxed_float ty_naked_float)) -> begin
      match prove_naked_floats env ty_naked_float with
      | Unknown -> try_canonical_simple ()
      | Invalid -> Invalid
      | Proved fs -> (
        match Float.Set.get_singleton fs with
        | None -> try_canonical_simple ()
        | Some f -> Lift (Boxed_float f))
    end
    | Value (Ok (Boxed_int32 ty_naked_int32)) -> begin
      match prove_naked_int32s env ty_naked_int32 with
      | Unknown -> try_canonical_simple ()
      | Invalid -> Invalid
      | Proved ns -> (
        match Int32.Set.get_singleton ns with
        | None -> try_canonical_simple ()
        | Some n -> Lift (Boxed_int32 n))
    end
    | Value (Ok (Boxed_int64 ty_naked_int64)) -> begin
      match prove_naked_int64s env ty_naked_int64 with
      | Unknown -> try_canonical_simple ()
      | Invalid -> Invalid
      | Proved ns -> (
        match Int64.Set.get_singleton ns with
        | None -> try_canonical_simple ()
        | Some n -> Lift (Boxed_int64 n))
    end
    | Value (Ok (Boxed_nativeint ty_naked_nativeint)) -> begin
      match prove_naked_nativeints env ty_naked_nativeint with
      | Unknown -> try_canonical_simple ()
      | Invalid -> Invalid
      | Proved ns -> (
        match Targetint_32_64.Set.get_singleton ns with
        | None -> try_canonical_simple ()
        | Some n -> Lift (Boxed_nativeint n))
    end
    | Value Bottom
    | Naked_immediate Bottom
    | Naked_float Bottom
    | Naked_int32 Bottom
    | Naked_int64 Bottom
    | Naked_nativeint Bottom ->
      Invalid
    | _ -> try_canonical_simple ())
