(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

module Float = Numeric_types.Float_by_bit_pattern
module Int32 = Numeric_types.Int32
module Int64 = Numeric_types.Int64
module K = Flambda_kind
module MTC = More_type_creators
module TE = Typing_env
module TG = Type_grammar
module TEEV = Typing_env_extension.With_extra_variables

module Expanded_type : sig
  type t

  val of_non_alias_type : ?coercion:Coercion.t -> TG.t -> t

  val create_value : Type_grammar.head_of_kind_value -> t

  val create_naked_immediate : Type_grammar.head_of_kind_naked_immediate -> t

  val create_naked_float : Type_grammar.head_of_kind_naked_float -> t

  val create_naked_int32 : Type_grammar.head_of_kind_naked_int32 -> t

  val create_naked_int64 : Type_grammar.head_of_kind_naked_int64 -> t

  val create_naked_nativeint : Type_grammar.head_of_kind_naked_nativeint -> t

  val create_rec_info : Type_grammar.head_of_kind_rec_info -> t

  val create_bottom : Flambda_kind.t -> t

  val create_unknown : Flambda_kind.t -> t

  val bottom_like : t -> t

  val unknown_like : t -> t

  val is_bottom : t -> bool

  val is_unknown : t -> bool

  val to_type : t -> Type_grammar.t

  type descr = private
    | Value of Type_grammar.head_of_kind_value
    | Naked_immediate of Type_grammar.head_of_kind_naked_immediate
    | Naked_float of Type_grammar.head_of_kind_naked_float
    | Naked_int32 of Type_grammar.head_of_kind_naked_int32
    | Naked_int64 of Type_grammar.head_of_kind_naked_int64
    | Naked_nativeint of Type_grammar.head_of_kind_naked_nativeint
    | Rec_info of Type_grammar.head_of_kind_rec_info

  val descr : t -> descr Or_unknown_or_bottom.t

  type descr_oub = private
    | Value of Type_grammar.head_of_kind_value Or_unknown_or_bottom.t
    | Naked_immediate of
        Type_grammar.head_of_kind_naked_immediate Or_unknown_or_bottom.t
    | Naked_float of
        Type_grammar.head_of_kind_naked_float Or_unknown_or_bottom.t
    | Naked_int32 of
        Type_grammar.head_of_kind_naked_int32 Or_unknown_or_bottom.t
    | Naked_int64 of
        Type_grammar.head_of_kind_naked_int64 Or_unknown_or_bottom.t
    | Naked_nativeint of
        Type_grammar.head_of_kind_naked_nativeint Or_unknown_or_bottom.t
    | Rec_info of Type_grammar.head_of_kind_rec_info Or_unknown_or_bottom.t

  val descr_oub : t -> descr_oub
end = struct
  type descr =
    | Value of TG.head_of_kind_value
    | Naked_immediate of TG.head_of_kind_naked_immediate
    | Naked_float of TG.head_of_kind_naked_float
    | Naked_int32 of TG.head_of_kind_naked_int32
    | Naked_int64 of TG.head_of_kind_naked_int64
    | Naked_nativeint of TG.head_of_kind_naked_nativeint
    | Rec_info of TG.head_of_kind_rec_info

  type t =
    { kind : K.t;
      descr : descr Or_unknown_or_bottom.t
    }

  let descr t = t.descr

  let create_value head = { kind = K.value; descr = Ok (Value head) }

  let create_naked_immediate head =
    { kind = K.naked_immediate; descr = Ok (Naked_immediate head) }

  let create_naked_float head =
    { kind = K.naked_float; descr = Ok (Naked_float head) }

  let create_naked_int32 head =
    { kind = K.naked_int32; descr = Ok (Naked_int32 head) }

  let create_naked_int64 head =
    { kind = K.naked_int64; descr = Ok (Naked_int64 head) }

  let create_naked_nativeint head =
    { kind = K.naked_nativeint; descr = Ok (Naked_nativeint head) }

  let create_rec_info head = { kind = K.rec_info; descr = Ok (Rec_info head) }

  let create_bottom kind = { kind; descr = Bottom }

  let create_unknown kind = { kind; descr = Unknown }

  let bottom_like t = create_bottom t.kind

  let unknown_like t = create_unknown t.kind

  let is_bottom t =
    match t.descr with Bottom -> true | Unknown | Ok _ -> false

  let is_unknown t =
    match t.descr with Unknown -> true | Bottom | Ok _ -> false

  let of_non_alias_type ?coercion ty : t =
    match TG.descr ty with
    | Value Unknown -> create_unknown K.value
    | Value Bottom -> create_bottom K.value
    | Value (Ok (No_alias head)) -> (
      match coercion with
      | None -> create_value head
      | Some coercion -> (
        match TG.apply_coercion_head_of_kind_value head coercion with
        | Bottom -> create_bottom K.value
        | Ok head -> create_value head))
    | Naked_immediate Unknown -> create_unknown K.naked_immediate
    | Naked_immediate Bottom -> create_bottom K.naked_immediate
    | Naked_immediate (Ok (No_alias head)) -> (
      match coercion with
      | None -> create_naked_immediate head
      | Some coercion -> (
        match TG.apply_coercion_head_of_kind_naked_immediate head coercion with
        | Bottom -> create_bottom K.naked_immediate
        | Ok head -> create_naked_immediate head))
    | Naked_float Unknown -> create_unknown K.naked_float
    | Naked_float Bottom -> create_bottom K.naked_float
    | Naked_float (Ok (No_alias head)) -> (
      match coercion with
      | None -> create_naked_float head
      | Some coercion -> (
        match TG.apply_coercion_head_of_kind_naked_float head coercion with
        | Bottom -> create_bottom K.naked_float
        | Ok head -> create_naked_float head))
    | Naked_int32 Unknown -> create_unknown K.naked_int32
    | Naked_int32 Bottom -> create_bottom K.naked_int32
    | Naked_int32 (Ok (No_alias head)) -> (
      match coercion with
      | None -> create_naked_int32 head
      | Some coercion -> (
        match TG.apply_coercion_head_of_kind_naked_int32 head coercion with
        | Bottom -> create_bottom K.naked_int32
        | Ok head -> create_naked_int32 head))
    | Naked_int64 Unknown -> create_unknown K.naked_int64
    | Naked_int64 Bottom -> create_bottom K.naked_int64
    | Naked_int64 (Ok (No_alias head)) -> (
      match coercion with
      | None -> create_naked_int64 head
      | Some coercion -> (
        match TG.apply_coercion_head_of_kind_naked_int64 head coercion with
        | Bottom -> create_bottom K.naked_int64
        | Ok head -> create_naked_int64 head))
    | Naked_nativeint Unknown -> create_unknown K.naked_nativeint
    | Naked_nativeint Bottom -> create_bottom K.naked_nativeint
    | Naked_nativeint (Ok (No_alias head)) -> (
      match coercion with
      | None -> create_naked_nativeint head
      | Some coercion -> (
        match TG.apply_coercion_head_of_kind_naked_nativeint head coercion with
        | Bottom -> create_bottom K.naked_nativeint
        | Ok head -> create_naked_nativeint head))
    | Rec_info Unknown -> create_unknown K.rec_info
    | Rec_info Bottom -> create_bottom K.rec_info
    | Rec_info (Ok (No_alias head)) -> (
      match coercion with
      | None -> create_rec_info head
      | Some coercion -> (
        match TG.apply_coercion_head_of_kind_rec_info head coercion with
        | Bottom -> create_bottom K.rec_info
        | Ok head -> create_rec_info head))
    | Value (Ok (Equals _))
    | Naked_immediate (Ok (Equals _))
    | Naked_float (Ok (Equals _))
    | Naked_int32 (Ok (Equals _))
    | Naked_int64 (Ok (Equals _))
    | Naked_nativeint (Ok (Equals _))
    | Rec_info (Ok (Equals _)) ->
      Misc.fatal_errorf "Type cannot be an alias type:@ %a" TG.print ty

  let to_type (t : t) =
    match t.descr with
    | Unknown -> MTC.unknown t.kind
    | Bottom -> MTC.bottom t.kind
    | Ok descr -> (
      match descr with
      | Value head -> TG.create_from_head_value head
      | Naked_immediate head -> TG.create_from_head_naked_immediate head
      | Naked_float head -> TG.create_from_head_naked_float head
      | Naked_int32 head -> TG.create_from_head_naked_int32 head
      | Naked_int64 head -> TG.create_from_head_naked_int64 head
      | Naked_nativeint head -> TG.create_from_head_naked_nativeint head
      | Rec_info head -> TG.create_from_head_rec_info head)

  type descr_oub =
    | Value of Type_grammar.head_of_kind_value Or_unknown_or_bottom.t
    | Naked_immediate of
        Type_grammar.head_of_kind_naked_immediate Or_unknown_or_bottom.t
    | Naked_float of
        Type_grammar.head_of_kind_naked_float Or_unknown_or_bottom.t
    | Naked_int32 of
        Type_grammar.head_of_kind_naked_int32 Or_unknown_or_bottom.t
    | Naked_int64 of
        Type_grammar.head_of_kind_naked_int64 Or_unknown_or_bottom.t
    | Naked_nativeint of
        Type_grammar.head_of_kind_naked_nativeint Or_unknown_or_bottom.t
    | Rec_info of Type_grammar.head_of_kind_rec_info Or_unknown_or_bottom.t

  let descr_oub t : descr_oub =
    match t.descr with
    | Unknown -> (
      match t.kind with
      | Value -> Value Unknown
      | Naked_number Naked_immediate -> Naked_immediate Unknown
      | Naked_number Naked_float -> Naked_float Unknown
      | Naked_number Naked_int32 -> Naked_int32 Unknown
      | Naked_number Naked_int64 -> Naked_int64 Unknown
      | Naked_number Naked_nativeint -> Naked_nativeint Unknown
      | Rec_info -> Rec_info Unknown
      | Fabricated -> Misc.fatal_error "Unused kind, to be removed")
    | Bottom -> (
      match t.kind with
      | Value -> Value Bottom
      | Naked_number Naked_immediate -> Naked_immediate Bottom
      | Naked_number Naked_float -> Naked_float Bottom
      | Naked_number Naked_int32 -> Naked_int32 Bottom
      | Naked_number Naked_int64 -> Naked_int64 Bottom
      | Naked_number Naked_nativeint -> Naked_nativeint Bottom
      | Rec_info -> Rec_info Bottom
      | Fabricated -> Misc.fatal_error "Unused kind, to be removed")
    | Ok (Value head) -> Value (Ok head)
    | Ok (Naked_immediate head) -> Naked_immediate (Ok head)
    | Ok (Naked_float head) -> Naked_float (Ok head)
    | Ok (Naked_int32 head) -> Naked_int32 (Ok head)
    | Ok (Naked_int64 head) -> Naked_int64 (Ok head)
    | Ok (Naked_nativeint head) -> Naked_nativeint (Ok head)
    | Ok (Rec_info head) -> Rec_info (Ok head)
end

module ET = Expanded_type

let expand_head0 simple env kind =
  let min_name_mode = Name_mode.min_in_types in
  match TE.get_canonical_simple_exn env simple ~min_name_mode with
  | exception Not_found ->
    (* This can happen when [simple] is of [Phantom] name mode. We're not
       interested in propagating types for phantom variables, so [Unknown] is
       fine here. *)
    ET.of_non_alias_type (MTC.unknown kind)
  | simple ->
    let[@inline always] name name ~coercion =
      let ty = TE.find env name (Some kind) in
      match TG.get_alias_exn ty with
      | exception Not_found ->
        let coercion =
          if Coercion.is_id coercion then None else Some coercion
        in
        ET.of_non_alias_type ?coercion ty
      | _alias ->
        Misc.fatal_errorf
          "Canonical alias %a should never have [Equals] type %a:@\n\n%a"
          Simple.print simple TG.print ty TE.print env
    in
    Simple.pattern_match simple
      ~const:(fun const ->
        match Reg_width_const.descr const with
        | Naked_immediate i ->
          ET.create_naked_immediate
            (TG.Head_of_kind_naked_immediate.create_naked_immediates
               (Targetint_31_63.Set.singleton i))
        | Tagged_immediate i ->
          ET.create_value (TG.Head_of_kind_value.create_tagged_immediate i)
        | Naked_float f -> ET.create_naked_float (Float.Set.singleton f)
        | Naked_int32 i -> ET.create_naked_int32 (Int32.Set.singleton i)
        | Naked_int64 i -> ET.create_naked_int64 (Int64.Set.singleton i)
        | Naked_nativeint i ->
          ET.create_naked_nativeint (Targetint_32_64.Set.singleton i))
      ~name

let expand_head env ty =
  match TG.get_alias_exn ty with
  | exception Not_found -> ET.of_non_alias_type ty
  | simple -> expand_head0 simple env (TG.kind ty)

let[@inline always] get_canonical_simples_and_expand_heads ~left_env ~left_ty
    ~right_env ~right_ty : Simple.t option * ET.t * Simple.t option * ET.t =
  let canonical_simple1 =
    match
      TE.get_alias_then_canonical_simple_exn left_env left_ty
        ~min_name_mode:Name_mode.in_types
    with
    | exception Not_found -> None
    | canonical_simple -> Some canonical_simple
  in
  let head1 = expand_head left_env left_ty in
  let canonical_simple2 =
    match
      TE.get_alias_then_canonical_simple_exn right_env right_ty
        ~min_name_mode:Name_mode.in_types
    with
    | exception Not_found -> None
    | canonical_simple -> Some canonical_simple
  in
  let head2 = expand_head right_env right_ty in
  canonical_simple1, head1, canonical_simple2, head2

let is_bottom env t = ET.is_bottom (expand_head env t)

let is_unknown env t = ET.is_unknown (expand_head env t)

let missing_kind env free_names =
  Name_occurrences.fold_variables free_names ~init:false
    ~f:(fun missing_kind var ->
      missing_kind || TE.variable_is_from_missing_cmx_file env (Name.var var))

type to_erase =
  | Everything_not_in of Typing_env.t
  | All_variables_except of Variable.Set.t

(* CR mshinwell: There is a subtlety here: the presence of a name in
   [suitable_for] doesn't mean that we should blindly return "=name". The type
   of the name in [suitable_for] might be (much) worse than the one in the
   environment [t]. *)
let rec make_suitable_for_environment0_core env t ~depth (to_erase : to_erase)
    level =
  let[@inline always] should_erase simple =
    match to_erase with
    | Everything_not_in suitable_for -> not (TE.mem_simple suitable_for simple)
    | All_variables_except to_keep ->
      Simple.pattern_match' simple
        ~var:(fun var ~coercion:_ -> not (Variable.Set.mem var to_keep))
        ~const:(fun _ -> false)
        ~symbol:(fun _ ~coercion:_ -> false)
  in
  let free_names = TG.free_names t in
  if Name_occurrences.no_variables free_names
  then level, t
  else if missing_kind env free_names
  then level, MTC.unknown (TG.kind t)
  else
    let to_erase_names =
      let var free_var = should_erase (Simple.var free_var) in
      Name_occurrences.filter_names free_names ~f:(fun free_name ->
          Name.pattern_match free_name ~var ~symbol:(fun _ -> true))
    in
    if Name_occurrences.is_empty to_erase_names
    then level, t
    else if depth > 1
    then level, MTC.unknown (TG.kind t)
    else
      let level, renaming =
        (* To avoid writing an erasure operation, we define irrelevant fresh
           variables in the returned [TEL], and swap them with the variables
           that we wish to erase throughout the type. *)
        Name_occurrences.fold_names to_erase_names ~init:(level, Renaming.empty)
          ~f:(fun ((level, renaming) as acc) to_erase_name ->
            Name.pattern_match to_erase_name
              ~symbol:(fun _ -> acc)
              ~var:(fun to_erase_var ->
                let original_type = TE.find env to_erase_name None in
                let kind = TG.kind original_type in
                let fresh_var = Variable.rename to_erase_var in
                let level =
                  let level, ty =
                    match
                      TE.get_canonical_simple_exn env
                        ~min_name_mode:Name_mode.in_types
                        (Simple.var to_erase_var)
                    with
                    | exception Not_found -> level, MTC.unknown kind
                    | canonical_simple ->
                      if not (should_erase canonical_simple)
                      then level, TG.alias_type_of kind canonical_simple
                      else
                        let t =
                          TE.find env (Name.var to_erase_var) (Some kind)
                        in
                        let t = expand_head env t |> ET.to_type in
                        make_suitable_for_environment0_core env t
                          ~depth:(depth + 1) to_erase level
                  in
                  TEEV.add_definition level fresh_var kind ty
                in
                let renaming =
                  Renaming.add_variable renaming to_erase_var fresh_var
                in
                level, renaming))
      in
      level, TG.apply_renaming t renaming

let make_suitable_for_environment0 env t to_erase level =
  make_suitable_for_environment0_core env t ~depth:0 to_erase level

let make_suitable_for_environment env t (to_erase : to_erase) ~bind_to =
  (match to_erase with
  | Everything_not_in suitable_for ->
    if not (TE.mem suitable_for bind_to)
    then
      Misc.fatal_errorf
        "[bind_to] %a is expected to be\n\
        \   bound in the [suitable_for] environment:@ %a" Name.print bind_to
        TE.print suitable_for
  | All_variables_except _ -> ());
  let level, t = make_suitable_for_environment0 env t to_erase TEEV.empty in
  let level = TEEV.add_or_replace_equation level bind_to t in
  level
