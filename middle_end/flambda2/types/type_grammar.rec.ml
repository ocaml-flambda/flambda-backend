(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

module K = Flambda_kind
module TE = Typing_env
module TEE = Typing_env_extension
module TEEV = Typing_env_extension.With_extra_variables
module T_V = Type_of_kind_value
module T_NI = Type_of_kind_naked_immediate
module T_Nf = Type_of_kind_naked_float
module T_N32 = Type_of_kind_naked_int32
module T_N64 = Type_of_kind_naked_int64
module T_NN = Type_of_kind_naked_nativeint
module T_RI = Type_of_kind_rec_info

type t =
  | Value of T_V.t
  | Naked_immediate of T_NI.t
  | Naked_float of T_Nf.t
  | Naked_int32 of T_N32.t
  | Naked_int64 of T_N64.t
  | Naked_nativeint of T_NN.t
  | Rec_info of T_RI.t

let [@ocamlformat "disable"] print_with_cache ~cache ppf (t : Type_grammar.t) =
  match t with
  | Value ty ->
    Format.fprintf ppf "@[<hov 1>(Val@ %a)@]"
      (T_V.print_with_cache ~cache) ty
  | Naked_immediate ty ->
    Format.fprintf ppf "@[<hov 1>(Naked_immediate@ %a)@]"
      (T_NI.print_with_cache ~cache) ty
  | Naked_float ty ->
    Format.fprintf ppf "@[<hov 1>(Naked_float@ %a)@]"
      (T_Nf.print_with_cache ~cache) ty
  | Naked_int32 ty ->
    Format.fprintf ppf "@[<hov 1>(Naked_int32@ %a)@]"
      (T_N32.print_with_cache ~cache) ty
  | Naked_int64 ty ->
    Format.fprintf ppf "@[<hov 1>(Naked_int64@ %a)@]"
      (T_N64.print_with_cache ~cache) ty
  | Naked_nativeint ty ->
    Format.fprintf ppf "@[<hov 1>(Naked_nativeint@ %a)@]"
      (T_NN.print_with_cache ~cache) ty
  | Rec_info ty ->
    Format.fprintf ppf "@[<hov 1>(Rec_info@ %a)@]"
      (T_RI.print_with_cache ~cache) ty

let [@ocamlformat "disable"] print ppf t =
  let cache : Printing_cache.t = Printing_cache.create () in
  print_with_cache ~cache ppf t

let force_to_kind_value t =
  match t with
  | Value ty -> ty
  | Naked_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
  | Naked_nativeint _ | Rec_info _ ->
    Misc.fatal_errorf "Type has wrong kind (expected [Value]):@ %a" print t

let force_to_kind_naked_immediate t =
  match t with
  | Naked_immediate ty -> ty
  | Value _ | Naked_float _ | Naked_int32 _ | Naked_int64 _ | Naked_nativeint _
  | Rec_info _ ->
    Misc.fatal_errorf "Type has wrong kind (expected [Naked_immediate]):@ %a"
      print t

let force_to_kind_naked_float t =
  match t with
  | Naked_float ty -> ty
  | Value _ | Naked_immediate _ | Naked_int32 _ | Naked_int64 _
  | Naked_nativeint _ | Rec_info _ ->
    Misc.fatal_errorf "Type has wrong kind (expected [Naked_float]):@ %a" print
      t

let force_to_kind_naked_int32 t =
  match t with
  | Naked_int32 ty -> ty
  | Value _ | Naked_immediate _ | Naked_float _ | Naked_int64 _
  | Naked_nativeint _ | Rec_info _ ->
    Misc.fatal_errorf "Type has wrong kind (expected [Naked_int32]):@ %a" print
      t

let force_to_kind_naked_int64 t =
  match t with
  | Naked_int64 ty -> ty
  | Value _ | Naked_immediate _ | Naked_float _ | Naked_int32 _
  | Naked_nativeint _ | Rec_info _ ->
    Misc.fatal_errorf "Type has wrong kind (expected [Naked_number Int64]):@ %a"
      print t

let force_to_kind_naked_nativeint t =
  match t with
  | Naked_nativeint ty -> ty
  | Value _ | Naked_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
  | Rec_info _ ->
    Misc.fatal_errorf
      "Type has wrong kind (expected [Naked_number Nativeint]):@ %a" print t

let force_to_kind_rec_info t =
  match t with
  | Rec_info ty -> ty
  | Value _ | Naked_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
  | Naked_nativeint _ ->
    Misc.fatal_errorf "Type has wrong kind (expected [Rec_info]):@ %a" print t

let apply_renaming t renaming =
  match t with
  | Value ty ->
    let ty' = T_V.apply_renaming ty renaming in
    if ty == ty' then t else Value ty'
  | Naked_immediate ty ->
    let ty' = T_NI.apply_renaming ty renaming in
    if ty == ty' then t else Naked_immediate ty'
  | Naked_float ty ->
    let ty' = T_Nf.apply_renaming ty renaming in
    if ty == ty' then t else Naked_float ty'
  | Naked_int32 ty ->
    let ty' = T_N32.apply_renaming ty renaming in
    if ty == ty' then t else Naked_int32 ty'
  | Naked_int64 ty ->
    let ty' = T_N64.apply_renaming ty renaming in
    if ty == ty' then t else Naked_int64 ty'
  | Naked_nativeint ty ->
    let ty' = T_NN.apply_renaming ty renaming in
    if ty == ty' then t else Naked_nativeint ty'
  | Rec_info ty ->
    let ty' = T_RI.apply_renaming ty renaming in
    if ty == ty' then t else Rec_info ty'

let free_names t =
  match t with
  | Value ty -> T_V.free_names ty
  | Naked_immediate ty -> T_NI.free_names ty
  | Naked_float ty -> T_Nf.free_names ty
  | Naked_int32 ty -> T_N32.free_names ty
  | Naked_int64 ty -> T_N64.free_names ty
  | Naked_nativeint ty -> T_NN.free_names ty
  | Rec_info ty -> T_RI.free_names ty

let all_ids_for_export t =
  match t with
  | Value ty -> T_V.all_ids_for_export ty
  | Naked_immediate ty -> T_NI.all_ids_for_export ty
  | Naked_float ty -> T_Nf.all_ids_for_export ty
  | Naked_int32 ty -> T_N32.all_ids_for_export ty
  | Naked_int64 ty -> T_N64.all_ids_for_export ty
  | Naked_nativeint ty -> T_NN.all_ids_for_export ty
  | Rec_info ty -> T_RI.all_ids_for_export ty

let apply_coercion t coercion : _ Or_bottom.t =
  match t with
  | Value ty -> begin
    match T_V.apply_coercion ty coercion with
    | Ok ty -> Ok (Value ty)
    | Bottom -> Bottom
  end
  | Naked_immediate ty -> begin
    match T_NI.apply_coercion ty coercion with
    | Ok ty -> Ok (Naked_immediate ty)
    | Bottom -> Bottom
  end
  | Naked_float ty -> begin
    match T_Nf.apply_coercion ty coercion with
    | Ok ty -> Ok (Naked_float ty)
    | Bottom -> Bottom
  end
  | Naked_int32 ty -> begin
    match T_N32.apply_coercion ty coercion with
    | Ok ty -> Ok (Naked_int32 ty)
    | Bottom -> Bottom
  end
  | Naked_int64 ty -> begin
    match T_N64.apply_coercion ty coercion with
    | Ok ty -> Ok (Naked_int64 ty)
    | Bottom -> Bottom
  end
  | Naked_nativeint ty -> begin
    match T_NN.apply_coercion ty coercion with
    | Ok ty -> Ok (Naked_nativeint ty)
    | Bottom -> Bottom
  end
  | Rec_info ty -> begin
    match T_RI.apply_coercion ty coercion with
    | Ok ty -> Ok (Rec_info ty)
    | Bottom -> Bottom
  end

let eviscerate t env =
  match t with
  | Value ty ->
    let ty = T_V.eviscerate ~force_to_kind:force_to_kind_value ty env K.value in
    Value ty
  | Naked_immediate ty ->
    let ty =
      T_NI.eviscerate ~force_to_kind:force_to_kind_naked_immediate ty env
        K.naked_immediate
    in
    Naked_immediate ty
  | Naked_float ty ->
    let ty =
      T_Nf.eviscerate ~force_to_kind:force_to_kind_naked_float ty env
        K.naked_float
    in
    Naked_float ty
  | Naked_int32 ty ->
    let ty =
      T_N32.eviscerate ~force_to_kind:force_to_kind_naked_int32 ty env
        K.naked_int32
    in
    Naked_int32 ty
  | Naked_int64 ty ->
    let ty =
      T_N64.eviscerate ~force_to_kind:force_to_kind_naked_int64 ty env
        K.naked_int64
    in
    Naked_int64 ty
  | Naked_nativeint ty ->
    let ty =
      T_NN.eviscerate ~force_to_kind:force_to_kind_naked_nativeint ty env
        K.naked_nativeint
    in
    Naked_nativeint ty
  | Rec_info ty ->
    let ty =
      T_RI.eviscerate ~force_to_kind:force_to_kind_rec_info ty env K.rec_info
    in
    Rec_info ty

let kind t =
  match t with
  | Value _ -> K.value
  | Naked_immediate _ -> K.naked_immediate
  | Naked_float _ -> K.naked_float
  | Naked_int32 _ -> K.naked_int32
  | Naked_int64 _ -> K.naked_int64
  | Naked_nativeint _ -> K.naked_nativeint
  | Rec_info _ -> K.rec_info

let get_alias_exn t =
  match t with
  | Value ty -> T_V.get_alias_exn ty
  | Naked_immediate ty -> T_NI.get_alias_exn ty
  | Naked_float ty -> T_Nf.get_alias_exn ty
  | Naked_int32 ty -> T_N32.get_alias_exn ty
  | Naked_int64 ty -> T_N64.get_alias_exn ty
  | Naked_nativeint ty -> T_NN.get_alias_exn ty
  | Rec_info ty -> T_RI.get_alias_exn ty

(* CR mshinwell: We should have transformations and invariant checks to enforce
   that, when a type can be expressed just using [Equals] (e.g. to a tagged
   immediate [Simple]), then it should be. In the tagged immediate case this
   would mean forbidding Variant with only a single immediate. Although this
   state needs to exist during [meet] or whenever heads are expanded. *)

let is_obviously_bottom (t : t) =
  match t with
  | Value ty -> T_V.is_obviously_bottom ty
  | Naked_immediate ty -> T_NI.is_obviously_bottom ty
  | Naked_float ty -> T_Nf.is_obviously_bottom ty
  | Naked_int32 ty -> T_N32.is_obviously_bottom ty
  | Naked_int64 ty -> T_N64.is_obviously_bottom ty
  | Naked_nativeint ty -> T_NN.is_obviously_bottom ty
  | Rec_info ty -> T_RI.is_obviously_bottom ty

let is_obviously_unknown (t : t) =
  match t with
  | Value ty -> T_V.is_obviously_unknown ty
  | Naked_immediate ty -> T_NI.is_obviously_unknown ty
  | Naked_float ty -> T_Nf.is_obviously_unknown ty
  | Naked_int32 ty -> T_N32.is_obviously_unknown ty
  | Naked_int64 ty -> T_N64.is_obviously_unknown ty
  | Naked_nativeint ty -> T_NN.is_obviously_unknown ty
  | Rec_info ty -> T_RI.is_obviously_unknown ty

let alias_type_of (kind : K.t) name : t =
  match kind with
  | Value -> Value (T_V.create_equals name)
  | Naked_number Naked_immediate -> Naked_immediate (T_NI.create_equals name)
  | Naked_number Naked_float -> Naked_float (T_Nf.create_equals name)
  | Naked_number Naked_int32 -> Naked_int32 (T_N32.create_equals name)
  | Naked_number Naked_int64 -> Naked_int64 (T_N64.create_equals name)
  | Naked_number Naked_nativeint -> Naked_nativeint (T_NN.create_equals name)
  | Rec_info -> Rec_info (T_RI.create_equals name)
  | Fabricated -> Misc.fatal_error "Only used in [Flambda_static] now"

let bottom_value () = Value (T_V.bottom ())

let bottom_naked_immediate () = Naked_immediate (T_NI.bottom ())

let bottom_naked_float () = Naked_float (T_Nf.bottom ())

let bottom_naked_int32 () = Naked_int32 (T_N32.bottom ())

let bottom_naked_int64 () = Naked_int64 (T_N64.bottom ())

let bottom_naked_nativeint () = Naked_nativeint (T_NN.bottom ())

let bottom_rec_info () = Rec_info (T_RI.bottom ())

let bottom (kind : K.t) =
  match kind with
  | Value -> bottom_value ()
  | Naked_number Naked_immediate -> bottom_naked_immediate ()
  | Naked_number Naked_float -> bottom_naked_float ()
  | Naked_number Naked_int32 -> bottom_naked_int32 ()
  | Naked_number Naked_int64 -> bottom_naked_int64 ()
  | Naked_number Naked_nativeint -> bottom_naked_nativeint ()
  | Rec_info -> bottom_rec_info ()
  | Fabricated -> Misc.fatal_error "Only used in [Flambda_static] now"

let bottom_like t = bottom (kind t)

let any_value () = Value (T_V.unknown ())

let any_naked_immediate () = Naked_immediate (T_NI.unknown ())

let any_naked_float () = Naked_float (T_Nf.unknown ())

let any_naked_int32 () = Naked_int32 (T_N32.unknown ())

let any_naked_int64 () = Naked_int64 (T_N64.unknown ())

let any_naked_nativeint () = Naked_nativeint (T_NN.unknown ())

let any_rec_info () = Rec_info (T_RI.unknown ())

let unknown (kind : K.t) =
  match kind with
  | Value -> any_value ()
  | Naked_number Naked_immediate -> any_naked_immediate ()
  | Naked_number Naked_float -> any_naked_float ()
  | Naked_number Naked_int32 -> any_naked_int32 ()
  | Naked_number Naked_int64 -> any_naked_int64 ()
  | Naked_number Naked_nativeint -> any_naked_nativeint ()
  | Rec_info -> any_rec_info ()
  | Fabricated -> Misc.fatal_error "Only used in [Flambda_static] now"

let unknown_like t = unknown (kind t)

let this_naked_immediate i : t =
  Naked_immediate
    (T_NI.create_equals (Simple.const (Reg_width_const.naked_immediate i)))

let this_naked_float f : t =
  Naked_float
    (T_Nf.create_equals (Simple.const (Reg_width_const.naked_float f)))

let this_naked_int32 i : t =
  Naked_int32
    (T_N32.create_equals (Simple.const (Reg_width_const.naked_int32 i)))

let this_naked_int64 i : t =
  Naked_int64
    (T_N64.create_equals (Simple.const (Reg_width_const.naked_int64 i)))

let this_naked_nativeint i : t =
  Naked_nativeint
    (T_NN.create_equals (Simple.const (Reg_width_const.naked_nativeint i)))

let these_naked_immediates0 ~no_alias is =
  match Targetint_31_63.Set.get_singleton is with
  | Some i when not no_alias -> this_naked_immediate i
  | _ ->
    if Targetint_31_63.Set.is_empty is
    then bottom K.naked_immediate
    else Naked_immediate (T_NI.create_no_alias (Ok (Naked_immediates is)))

let these_naked_floats0 ~no_alias fs =
  match Float.Set.get_singleton fs with
  | Some f when not no_alias -> this_naked_float f
  | _ ->
    if Float.Set.is_empty fs
    then bottom K.naked_float
    else Naked_float (T_Nf.create_no_alias (Ok fs))

let these_naked_int32s0 ~no_alias is =
  match Int32.Set.get_singleton is with
  | Some i when not no_alias -> this_naked_int32 i
  | _ ->
    if Int32.Set.is_empty is
    then bottom K.naked_int32
    else Naked_int32 (T_N32.create_no_alias (Ok is))

let these_naked_int64s0 ~no_alias is =
  match Int64.Set.get_singleton is with
  | Some i when not no_alias -> this_naked_int64 i
  | _ ->
    if Int64.Set.is_empty is
    then bottom K.naked_int64
    else Naked_int64 (T_N64.create_no_alias (Ok is))

let these_naked_nativeints0 ~no_alias is =
  match Targetint_32_64.Set.get_singleton is with
  | Some i when not no_alias -> this_naked_nativeint i
  | _ ->
    if Targetint_32_64.Set.is_empty is
    then bottom K.naked_nativeint
    else Naked_nativeint (T_NN.create_no_alias (Ok is))

let this_naked_immediate_without_alias i =
  these_naked_immediates0 ~no_alias:true (Targetint_31_63.Set.singleton i)

let this_naked_float_without_alias f =
  these_naked_floats0 ~no_alias:true (Float.Set.singleton f)

let this_naked_int32_without_alias i =
  these_naked_int32s0 ~no_alias:true (Int32.Set.singleton i)

let this_naked_int64_without_alias i =
  these_naked_int64s0 ~no_alias:true (Int64.Set.singleton i)

let this_naked_nativeint_without_alias i =
  these_naked_nativeints0 ~no_alias:true (Targetint_32_64.Set.singleton i)

let these_naked_immediates is = these_naked_immediates0 ~no_alias:false is

let these_naked_floats fs = these_naked_floats0 ~no_alias:false fs

let these_naked_int32s is = these_naked_int32s0 ~no_alias:false is

let these_naked_int64s is = these_naked_int64s0 ~no_alias:false is

let these_naked_nativeints is = these_naked_nativeints0 ~no_alias:false is

let box_float (t : t) : t =
  match t with
  | Naked_float _ -> Value (T_V.create (Boxed_float t))
  | Value _ | Naked_immediate _ | Naked_int32 _ | Naked_int64 _
  | Naked_nativeint _ | Rec_info _ ->
    Misc.fatal_errorf "Type of wrong kind for [box_float]: %a" print t

let box_int32 (t : t) : t =
  match t with
  | Naked_int32 _ -> Value (T_V.create (Boxed_int32 t))
  | Value _ | Naked_immediate _ | Naked_float _ | Naked_int64 _
  | Naked_nativeint _ | Rec_info _ ->
    Misc.fatal_errorf "Type of wrong kind for [box_int32]: %a" print t

let box_int64 (t : t) : t =
  match t with
  | Naked_int64 _ -> Value (T_V.create (Boxed_int64 t))
  | Value _ | Naked_immediate _ | Naked_float _ | Naked_int32 _
  | Naked_nativeint _ | Rec_info _ ->
    Misc.fatal_errorf "Type of wrong kind for [box_int64]: %a" print t

let box_nativeint (t : t) : t =
  match t with
  | Naked_nativeint _ -> Value (T_V.create (Boxed_nativeint t))
  | Value _ | Naked_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
  | Rec_info _ ->
    Misc.fatal_errorf "Type of wrong kind for [box_nativeint]: %a" print t

let any_tagged_immediate () : t =
  Value
    (T_V.create_no_alias
       (Ok
          (Variant
             (Variant.create ~is_unique:false ~immediates:Unknown
                ~blocks:(Known (Row_like.For_blocks.create_bottom ()))))))

let this_tagged_immediate imm : t =
  Value
    (T_V.create_equals (Simple.const (Reg_width_const.tagged_immediate imm)))

let these_tagged_immediates0 ~no_alias imms : t =
  match Targetint_31_63.Set.get_singleton imms with
  | Some imm when not no_alias -> this_tagged_immediate imm
  | _ ->
    if Targetint_31_63.Set.is_empty imms
    then bottom K.value
    else
      Value
        (T_V.create_no_alias
           (Ok
              (Variant
                 (Variant.create ~is_unique:false
                    ~immediates:(Known (these_naked_immediates imms))
                    ~blocks:(Known (Row_like.For_blocks.create_bottom ()))))))

let these_tagged_immediates imms = these_tagged_immediates0 ~no_alias:false imms

let this_tagged_immediate_without_alias imm =
  these_tagged_immediates0 ~no_alias:true (Targetint_31_63.Set.singleton imm)

let tag_immediate t : t =
  match t with
  | Naked_immediate _ ->
    Value
      (T_V.create_no_alias
         (Ok
            (Variant
               (Variant.create ~is_unique:false ~immediates:(Known t)
                  ~blocks:(Known (Row_like.For_blocks.create_bottom ()))))))
  | Value _ | Naked_float _ | Naked_int32 _ | Naked_int64 _ | Naked_nativeint _
  | Rec_info _ ->
    Misc.fatal_errorf "Type of wrong kind for [tag_immediate]: %a" print t

let tagged_immediate_alias_to ~naked_immediate : t =
  tag_immediate
    (Naked_immediate (T_NI.create_equals (Simple.var naked_immediate)))

let any_block () : t =
  Value
    (T_V.create_no_alias
       (Ok
          (Variant
             (Variant.create ~is_unique:false
                ~immediates:(Known (bottom K.naked_immediate))
                ~blocks:Unknown))))

let is_int_for_scrutinee ~scrutinee : t =
  Naked_immediate (T_NI.create (Is_int (alias_type_of K.value scrutinee)))

let get_tag_for_block ~block : t =
  Naked_immediate (T_NI.create (Get_tag (alias_type_of K.value block)))

let any_tagged_bool () = these_tagged_immediates Targetint_31_63.all_bools

let any_naked_bool () = these_naked_immediates Targetint_31_63.all_bools

let this_boxed_float f = box_float (this_naked_float f)

let this_boxed_int32 i = box_int32 (this_naked_int32 i)

let this_boxed_int64 i = box_int64 (this_naked_int64 i)

let this_boxed_nativeint i = box_nativeint (this_naked_nativeint i)

let these_boxed_floats fs = box_float (these_naked_floats fs)

let these_boxed_int32s is = box_int32 (these_naked_int32s is)

let these_boxed_int64s is = box_int64 (these_naked_int64s is)

let these_boxed_nativeints is = box_nativeint (these_naked_nativeints is)

let boxed_float_alias_to ~naked_float =
  box_float (Naked_float (T_Nf.create_equals (Simple.var naked_float)))

let boxed_int32_alias_to ~naked_int32 =
  box_int32 (Naked_int32 (T_N32.create_equals (Simple.var naked_int32)))

let boxed_int64_alias_to ~naked_int64 =
  box_int64 (Naked_int64 (T_N64.create_equals (Simple.var naked_int64)))

let boxed_nativeint_alias_to ~naked_nativeint =
  box_nativeint
    (Naked_nativeint (T_NN.create_equals (Simple.var naked_nativeint)))

let blocks_with_these_tags tags : _ Or_unknown.t =
  if Tag.Set.for_all Tag.is_structured_block tags
  then
    let blocks =
      Row_like.For_blocks.create_blocks_with_these_tags
        ~field_kind:Flambda_kind.value tags
    in
    (* CR vlaviron: There is a potential soundness issue as this forbids Array
       values, which could have tag 0. *)
    Known
      (Value
         (T_V.create_no_alias
            (Ok
               (Variant
                  (Variant.create ~is_unique:false
                     ~immediates:(Known (bottom K.naked_immediate))
                     ~blocks:(Known blocks))))))
  else Unknown

let immutable_block ~is_unique tag ~field_kind ~fields =
  match Targetint_31_63.Imm.of_int_option (List.length fields) with
  | None ->
    (* CR mshinwell: This should be a special kind of error. *)
    Misc.fatal_error "Block too long for target"
  | Some _size ->
    Value
      (T_V.create_no_alias
         (Ok
            (Variant
               (Variant.create ~is_unique
                  ~immediates:(Known (bottom K.naked_immediate))
                  ~blocks:
                    (Known
                       (Row_like.For_blocks.create ~field_kind ~field_tys:fields
                          (Closed tag)))))))

let immutable_block_with_size_at_least ~tag ~n ~field_kind ~field_n_minus_one =
  let n = Targetint_31_63.Imm.to_int n in
  let field_tys =
    List.init n (fun index ->
        if index < n - 1
        then unknown field_kind
        else alias_type_of field_kind (Simple.var field_n_minus_one))
  in
  Value
    (T_V.create_no_alias
       (Ok
          (Variant
             (Variant.create ~is_unique:false
                ~immediates:(Known (bottom K.naked_immediate))
                ~blocks:
                  (Known
                     (Row_like.For_blocks.create ~field_kind ~field_tys
                        (Open tag)))))))

let variant ~const_ctors ~non_const_ctors =
  let blocks =
    let field_tys_by_tag =
      Tag.Scannable.Map.fold
        (fun tag ty non_const_ctors ->
          Tag.Map.add (Tag.Scannable.to_tag tag) ty non_const_ctors)
        non_const_ctors Tag.Map.empty
    in
    Row_like.For_blocks.create_exactly_multiple ~field_tys_by_tag
  in
  Value
    (T_V.create_no_alias
       (Ok
          (Variant
             (Variant.create ~is_unique:false ~immediates:(Known const_ctors)
                ~blocks:(Known blocks)))))

let open_variant_from_const_ctors_type ~const_ctors =
  Value
    (T_V.create_no_alias
       (Ok
          (Variant
             (Variant.create ~is_unique:false ~immediates:(Known const_ctors)
                ~blocks:Unknown))))

let open_variant_from_non_const_ctor_with_size_at_least ~n ~field_n_minus_one =
  let n = Targetint_31_63.Imm.to_int n in
  let field_tys =
    List.init n (fun index ->
        if index < n - 1
        then any_value ()
        else alias_type_of K.value (Simple.var field_n_minus_one))
  in
  Value
    (T_V.create_no_alias
       (Ok
          (Variant
             (Variant.create ~is_unique:false ~immediates:Unknown
                ~blocks:
                  (Known
                     (Row_like.For_blocks.create ~field_kind:K.value ~field_tys
                        (Open Unknown)))))))

let this_immutable_string str =
  (* CR mshinwell: Use "length" not "size" for strings *)
  let size = Targetint_31_63.Imm.of_int (String.length str) in
  let string_info =
    String_info.Set.singleton
      (String_info.create ~contents:(Contents str) ~size)
  in
  Value (T_V.create (String string_info))

let mutable_string ~size =
  let size = Targetint_31_63.Imm.of_int size in
  let string_info =
    String_info.Set.singleton
      (String_info.create ~contents:Unknown_or_mutable ~size)
  in
  Value (T_V.create (String string_info))

let any_boxed_float () = box_float (any_naked_float ())

let any_boxed_int32 () = box_int32 (any_naked_int32 ())

let any_boxed_int64 () = box_int64 (any_naked_int64 ())

let any_boxed_nativeint () = box_nativeint (any_naked_nativeint ())

let create_function_declaration ~code ~rec_info =
  Function_declaration_type.create ~code ~rec_info

let create_non_inlinable_function_declaration ~code_id =
  Function_declaration_type.create_non_inlinable ~code_id

let exactly_this_closure closure_id ~all_function_decls_in_set:function_decls
    ~all_closures_in_set:closure_types
    ~all_closure_vars_in_set:closure_var_types =
  let closure_types =
    Product.Closure_id_indexed.create Flambda_kind.value closure_types
  in
  let closures_entry =
    let closure_var_types =
      Product.Var_within_closure_indexed.create Flambda_kind.value
        closure_var_types
    in
    Closures_entry.create ~function_decls ~closure_types ~closure_var_types
  in
  let by_closure_id =
    let set_of_closures_contents =
      Set_of_closures_contents.create
        (Closure_id.Map.keys function_decls)
        (Var_within_closure.Map.keys closure_var_types)
    in
    Row_like.For_closures_entry_by_set_of_closures_contents.create_exactly
      closure_id set_of_closures_contents closures_entry
  in
  Value (T_V.create (Closures { by_closure_id }))

let at_least_the_closures_with_ids ~this_closure closure_ids_and_bindings =
  let closure_ids_and_types =
    Closure_id.Map.map
      (fun bound_to -> alias_type_of K.value bound_to)
      closure_ids_and_bindings
  in
  let function_decls =
    Closure_id.Map.map
      (fun _ -> Or_unknown_or_bottom.Unknown)
      closure_ids_and_bindings
  in
  let closure_types =
    Product.Closure_id_indexed.create Flambda_kind.value closure_ids_and_types
  in
  let closures_entry =
    Closures_entry.create ~function_decls ~closure_types
      ~closure_var_types:
        (Product.Var_within_closure_indexed.create_top Flambda_kind.value)
  in
  let by_closure_id =
    let set_of_closures_contents =
      Set_of_closures_contents.create
        (Closure_id.Map.keys closure_ids_and_types)
        Var_within_closure.Set.empty
    in
    Row_like.For_closures_entry_by_set_of_closures_contents.create_at_least
      this_closure set_of_closures_contents closures_entry
  in
  Value (T_V.create (Closures { by_closure_id }))

let closure_with_at_least_these_closure_vars ~this_closure closure_vars : t =
  let closure_var_types =
    let type_of_var v = alias_type_of K.value (Simple.var v) in
    let map = Var_within_closure.Map.map type_of_var closure_vars in
    Product.Var_within_closure_indexed.create Flambda_kind.value map
  in
  let closures_entry =
    Closures_entry.create ~function_decls:Closure_id.Map.empty
      ~closure_types:(Product.Closure_id_indexed.create_top Flambda_kind.value)
      ~closure_var_types
  in
  let by_closure_id =
    let set_of_closures_contents =
      Set_of_closures_contents.create Closure_id.Set.empty
        (Var_within_closure.Map.keys closure_vars)
    in
    Row_like.For_closures_entry_by_set_of_closures_contents.create_at_least
      this_closure set_of_closures_contents closures_entry
  in
  Value (T_V.create (Closures { by_closure_id }))

let closure_with_at_least_this_closure_var ~this_closure closure_var
    ~closure_element_var : t =
  closure_with_at_least_these_closure_vars ~this_closure
    (Var_within_closure.Map.singleton closure_var closure_element_var)

let array_of_length ~length = Value (T_V.create (Array { length }))

let type_for_const const =
  match Reg_width_const.descr const with
  | Naked_immediate i -> this_naked_immediate i
  | Tagged_immediate i -> this_tagged_immediate i
  | Naked_float f -> this_naked_float f
  | Naked_int32 n -> this_naked_int32 n
  | Naked_int64 n -> this_naked_int64 n
  | Naked_nativeint n -> this_naked_nativeint n

let kind_for_const const = kind (type_for_const const)

let this_rec_info (rec_info_expr : Rec_info_expr.t) =
  match rec_info_expr with
  | Var dv -> Rec_info (T_RI.create_equals (Simple.var dv))
  | Const _ | Succ _ | Unroll_to _ -> Rec_info (T_RI.create rec_info_expr)

let expand_head t env : Resolved_type.t =
  match t with
  | Value ty ->
    let head =
      T_V.expand_head ~force_to_kind:force_to_kind_value ty env K.value
    in
    Value head
  | Naked_immediate ty ->
    let head =
      T_NI.expand_head ~force_to_kind:force_to_kind_naked_immediate ty env
        K.naked_immediate
    in
    Naked_immediate head
  | Naked_float ty ->
    let head =
      T_Nf.expand_head ~force_to_kind:force_to_kind_naked_float ty env
        K.naked_float
    in
    Naked_float head
  | Naked_int32 ty ->
    let head =
      T_N32.expand_head ~force_to_kind:force_to_kind_naked_int32 ty env
        K.naked_int32
    in
    Naked_int32 head
  | Naked_int64 ty ->
    let head =
      T_N64.expand_head ~force_to_kind:force_to_kind_naked_int64 ty env
        K.naked_int64
    in
    Naked_int64 head
  | Naked_nativeint ty ->
    let head =
      T_NN.expand_head ~force_to_kind:force_to_kind_naked_nativeint ty env
        K.naked_nativeint
    in
    Naked_nativeint head
  | Rec_info ty ->
    let head =
      T_RI.expand_head ~force_to_kind:force_to_kind_rec_info ty env K.rec_info
    in
    Rec_info head

let expand_head' t env : t =
  match t with
  | Value ty ->
    Value (T_V.expand_head' ~force_to_kind:force_to_kind_value ty env K.value)
  | Naked_immediate ty ->
    Naked_immediate
      (T_NI.expand_head' ~force_to_kind:force_to_kind_naked_immediate ty env
         K.naked_immediate)
  | Naked_float ty ->
    Naked_float
      (T_Nf.expand_head' ~force_to_kind:force_to_kind_naked_float ty env
         K.naked_float)
  | Naked_int32 ty ->
    Naked_int32
      (T_N32.expand_head' ~force_to_kind:force_to_kind_naked_int32 ty env
         K.naked_int32)
  | Naked_int64 ty ->
    Naked_int64
      (T_N64.expand_head' ~force_to_kind:force_to_kind_naked_int64 ty env
         K.naked_int64)
  | Naked_nativeint ty ->
    Naked_nativeint
      (T_NN.expand_head' ~force_to_kind:force_to_kind_naked_nativeint ty env
         K.naked_nativeint)
  | Rec_info ty ->
    Rec_info
      (T_RI.expand_head' ~force_to_kind:force_to_kind_rec_info ty env K.rec_info)

let missing_kind env free_names =
  Name_occurrences.fold_variables free_names ~init:false
    ~f:(fun missing_kind var ->
      missing_kind || TE.variable_is_from_missing_cmx_file env (Name.var var))

(* CR mshinwell: There is a subtlety here: the presence of a name in
   [suitable_for] doesn't mean that we should blindly return "=name". The type
   of the name in [suitable_for] might be (much) worse than the one in the
   environment [t]. *)
(* CR mshinwell: This is related to a trivial join - think about this some
   more. *)
let rec make_suitable_for_environment0_core t env ~depth ~suitable_for level =
  let free_names = free_names t in
  if Name_occurrences.no_variables free_names
  then level, t
  else if missing_kind env free_names
  then level, unknown (kind t)
  else
    let to_erase =
      let var free_var = not (TE.mem suitable_for (Name.var free_var)) in
      Name_occurrences.filter_names free_names ~f:(fun free_name ->
          Name.pattern_match free_name ~var ~symbol:(fun _ -> true))
    in
    if Name_occurrences.is_empty to_erase
    then level, t
    else if depth > 1
    then level, unknown (kind t)
    else
      let level, renaming =
        (* To avoid writing an erasure operation, we define irrelevant fresh
           variables in the returned [Typing_env_level], and swap them with the
           variables that we wish to erase throughout the type. *)
        Name_occurrences.fold_names to_erase ~init:(level, Renaming.empty)
          ~f:(fun ((level, renaming) as acc) to_erase_name ->
            Name.pattern_match to_erase_name
              ~symbol:(fun _ -> acc)
              ~var:(fun to_erase ->
                let original_type = TE.find env to_erase_name None in
                let kind = kind original_type in
                let fresh_var = Variable.rename to_erase in
                let level =
                  let level, ty =
                    match
                      TE.get_canonical_simple_exn env
                        ~min_name_mode:Name_mode.in_types (Simple.var to_erase)
                    with
                    | exception Not_found -> level, unknown kind
                    | canonical_simple ->
                      if TE.mem_simple suitable_for canonical_simple
                      then level, alias_type_of kind canonical_simple
                      else
                        let t = TE.find env (Name.var to_erase) (Some kind) in
                        let t = expand_head' t env in
                        make_suitable_for_environment0_core t env
                          ~depth:(depth + 1) ~suitable_for level
                  in
                  TEEV.add_definition level fresh_var kind ty
                in
                let renaming =
                  Renaming.add_variable renaming to_erase fresh_var
                in
                level, renaming))
      in
      level, apply_renaming t renaming

let make_suitable_for_environment0 t env ~suitable_for level =
  make_suitable_for_environment0_core t env ~depth:0 ~suitable_for level

let make_suitable_for_environment t env ~suitable_for ~bind_to =
  (* if TE.mem env bind_to then begin Misc.fatal_errorf "[bind_to] %a must not
     be bound in the \ source environment:@ %a" Name.print bind_to TE.print env
     end; *)
  if not (TE.mem suitable_for bind_to)
  then
    Misc.fatal_errorf
      "[bind_to] %a is expected to be bound in the [suitable_for] \
       environment:@ %a"
      Name.print bind_to TE.print suitable_for;
  let level, t =
    make_suitable_for_environment0 t env ~suitable_for (TEEV.empty ())
  in
  let level = TEEV.add_or_replace_equation level bind_to t in
  level

let meet env t1 t2 =
  match t1, t2 with
  | Value ty1, Value ty2 ->
    T_V.meet env K.value t1 t2 ty1 ty2 ~force_to_kind:force_to_kind_value
      ~to_type:(fun ty -> Value ty)
  | Naked_immediate ty1, Naked_immediate ty2 ->
    T_NI.meet env K.naked_immediate t1 t2 ty1 ty2
      ~force_to_kind:force_to_kind_naked_immediate ~to_type:(fun ty ->
        Naked_immediate ty)
  | Naked_float ty1, Naked_float ty2 ->
    T_Nf.meet env K.naked_float t1 t2 ty1 ty2
      ~force_to_kind:force_to_kind_naked_float ~to_type:(fun ty ->
        Naked_float ty)
  | Naked_int32 ty1, Naked_int32 ty2 ->
    T_N32.meet env K.naked_int32 t1 t2 ty1 ty2
      ~force_to_kind:force_to_kind_naked_int32 ~to_type:(fun ty ->
        Naked_int32 ty)
  | Naked_int64 ty1, Naked_int64 ty2 ->
    T_N64.meet env K.naked_int64 t1 t2 ty1 ty2
      ~force_to_kind:force_to_kind_naked_int64 ~to_type:(fun ty ->
        Naked_int64 ty)
  | Naked_nativeint ty1, Naked_nativeint ty2 ->
    T_NN.meet env K.naked_nativeint t1 t2 ty1 ty2
      ~force_to_kind:force_to_kind_naked_nativeint ~to_type:(fun ty ->
        Naked_nativeint ty)
  | Rec_info ty1, Rec_info ty2 ->
    T_RI.meet env K.rec_info t1 t2 ty1 ty2 ~force_to_kind:force_to_kind_rec_info
      ~to_type:(fun ty -> Rec_info ty)
  | ( ( Value _ | Naked_immediate _ | Naked_float _ | Naked_int32 _
      | Naked_int64 _ | Naked_nativeint _ | Rec_info _ ),
      _ ) ->
    Misc.fatal_errorf "Kind mismatch upon meet:@ %a@ versus@ %a" print t1 print
      t2

let join ?bound_name env t1 t2 =
  match t1, t2 with
  | Value ty1, Value ty2 ->
    T_V.join ?bound_name env K.value t1 t2 ty1 ty2
      ~force_to_kind:force_to_kind_value ~to_type:(fun ty -> Value ty)
  | Naked_immediate ty1, Naked_immediate ty2 ->
    T_NI.join ?bound_name env K.naked_immediate t1 t2 ty1 ty2
      ~force_to_kind:force_to_kind_naked_immediate ~to_type:(fun ty ->
        Naked_immediate ty)
  | Naked_float ty1, Naked_float ty2 ->
    T_Nf.join ?bound_name env K.naked_float t1 t2 ty1 ty2
      ~force_to_kind:force_to_kind_naked_float ~to_type:(fun ty ->
        Naked_float ty)
  | Naked_int32 ty1, Naked_int32 ty2 ->
    T_N32.join ?bound_name env K.naked_int32 t1 t2 ty1 ty2
      ~force_to_kind:force_to_kind_naked_int32 ~to_type:(fun ty ->
        Naked_int32 ty)
  | Naked_int64 ty1, Naked_int64 ty2 ->
    T_N64.join ?bound_name env K.naked_int64 t1 t2 ty1 ty2
      ~force_to_kind:force_to_kind_naked_int64 ~to_type:(fun ty ->
        Naked_int64 ty)
  | Naked_nativeint ty1, Naked_nativeint ty2 ->
    T_NN.join ?bound_name env K.naked_nativeint t1 t2 ty1 ty2
      ~force_to_kind:force_to_kind_naked_nativeint ~to_type:(fun ty ->
        Naked_nativeint ty)
  | Rec_info ty1, Rec_info ty2 ->
    T_RI.join ?bound_name env K.rec_info t1 t2 ty1 ty2
      ~force_to_kind:force_to_kind_rec_info ~to_type:(fun ty -> Rec_info ty)
  | ( ( Value _ | Naked_immediate _ | Naked_float _ | Naked_int32 _
      | Naked_int64 _ | Naked_nativeint _ | Rec_info _ ),
      _ ) ->
    Misc.fatal_errorf "Kind mismatch upon join:@ %a@ versus@ %a" print t1 print
      t2

let is_alias_of_name ty name =
  match get_alias_exn ty with
  | exception Not_found -> false
  | simple ->
    Simple.pattern_match simple
      ~name:(fun name' ~coercion:_ -> Name.equal name name')
      ~const:(fun _ -> false)

let check_equation name ty =
  if Flambda_features.check_invariants ()
  then
    if is_alias_of_name ty name
    then
      Misc.fatal_errorf "Directly recursive equation@ %a = %a@ disallowed"
        Name.print name print ty
