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

let expand_head_of_alias_type env kind
    ~known_canonical_simple_at_in_types_mode:simple =
  let[@inline always] name name ~coercion =
    let ty = TE.find env name (Some kind) in
    match TG.get_alias_exn ty with
    | exception Not_found ->
      let coercion = if Coercion.is_id coercion then None else Some coercion in
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

let expand_head0 env ty ~known_canonical_simple_at_in_types_mode =
  match TG.get_alias_exn ty with
  | exception Not_found -> ET.of_non_alias_type ty
  | _ -> (
    match known_canonical_simple_at_in_types_mode with
    | Some simple ->
      expand_head_of_alias_type env (TG.kind ty)
        ~known_canonical_simple_at_in_types_mode:simple
    | None ->
      (* See comment below in [expand_head] about this case. *)
      ET.of_non_alias_type (MTC.unknown (TG.kind ty)))

let expand_head env ty =
  match TG.get_alias_exn ty with
  | exception Not_found -> ET.of_non_alias_type ty
  | simple -> (
    let kind = TG.kind ty in
    match
      TE.get_canonical_simple_exn env simple ~min_name_mode:Name_mode.in_types
    with
    | exception Not_found ->
      (* This can happen when [simple] is of [Phantom] name mode. We're not
         interested in propagating types for phantom variables, so [Unknown] is
         fine here. *)
      ET.of_non_alias_type (MTC.unknown kind)
    | simple ->
      expand_head_of_alias_type ~known_canonical_simple_at_in_types_mode:simple
        env kind)

let is_bottom env t = ET.is_bottom (expand_head env t)

let is_unknown env t = ET.is_unknown (expand_head env t)

let missing_kind env free_names =
  Name_occurrences.fold_variables free_names ~init:false
    ~f:(fun missing_kind var ->
      missing_kind || TE.variable_is_from_missing_cmx_file env (Name.var var))

type to_erase =
  | Everything_not_in of Typing_env.t
  | All_variables_except of Variable.Set.t

exception Missing_cmx_file

let free_variables_transitive env already_seen ty =
  let rec free_variables_transitive0 ty ~result =
    (* We don't need to look at symbols because the assumption (see the .mli) is
       that all symbols have valid types in the target environment. *)
    let free_vars = TG.free_names ty |> Name_occurrences.with_only_variables in
    if missing_kind env free_vars
    then raise Missing_cmx_file
    else
      let to_traverse = Name_occurrences.diff free_vars result in
      let result = Name_occurrences.union result to_traverse in
      Name_occurrences.fold_names to_traverse ~init:result
        ~f:(fun result name ->
          let ty = TE.find env name None in
          free_variables_transitive0 ty ~result)
  in
  free_variables_transitive0 ty ~result:already_seen

let make_suitable_for_environment env (to_erase : to_erase) bind_to_and_types =
  (match to_erase with
  | Everything_not_in suitable_for ->
    List.iter
      (fun (bind_to, _ty) ->
        if not (TE.mem suitable_for bind_to)
        then
          Misc.fatal_errorf
            "Variable to be bound %a is expected to already be\n\
            \   bound in the [suitable_for] environment:@ %a" Name.print bind_to
            TE.print suitable_for)
      bind_to_and_types
  | All_variables_except _ -> ());
  (* Do a quick free variables check first to try to catch easy cases. *)
  let free_vars =
    List.fold_left
      (fun free_vars (_bind_to, ty) ->
        Name_occurrences.union free_vars
          (Name_occurrences.with_only_variables (TG.free_names ty)))
      Name_occurrences.empty bind_to_and_types
  in
  if Name_occurrences.is_empty free_vars
  then
    List.fold_left
      (fun result (bind_to, ty) ->
        TEEV.add_or_replace_equation result bind_to ty)
      TEEV.empty bind_to_and_types
  else
    (* Now collect all of the free variables, transitively (see comment on
       function above). *)
    match
      bind_to_and_types |> List.map snd
      |> List.fold_left (free_variables_transitive env) Name_occurrences.empty
    with
    | exception Missing_cmx_file ->
      (* Just forget everything if there is a .cmx file missing. *)
      List.fold_left
        (fun result (bind_to, ty) ->
          TEEV.add_or_replace_equation result bind_to (MTC.unknown_like ty))
        TEEV.empty bind_to_and_types
    | free_vars ->
      (* Determine which variables will be unavailable and thus need fresh ones
         assigning to them. *)
      let unavailable_vars =
        match to_erase with
        | Everything_not_in suitable_for ->
          Name_occurrences.fold_variables free_vars ~init:[]
            ~f:(fun unavailable_vars var ->
              if not (TE.mem suitable_for (Name.var var))
              then var :: unavailable_vars
              else unavailable_vars)
        | All_variables_except to_keep ->
          Name_occurrences.fold_variables free_vars ~init:[]
            ~f:(fun unavailable_vars var ->
              if not (Variable.Set.mem var to_keep)
              then var :: unavailable_vars
              else unavailable_vars)
      in
      (* Fetch the type equation for each free variable. Also add in the
         equations about the "bind-to" names provided to this function. If any
         of the "bind-to" names are already defined in [env], the type given in
         [bind_to_and_types] takes precedence over such definition. *)
      let equations =
        ListLabels.fold_left unavailable_vars ~init:[] ~f:(fun equations var ->
            let name = Name.var var in
            let ty = TE.find env name None in
            (name, ty) :: equations)
      in
      let equations =
        List.fold_left
          (fun equations (bind_to, ty) ->
            (* The [bind_to] variables are not expected to be unavailable, so
               this shouldn't cause duplicates. *)
            (bind_to, ty) :: equations)
          equations bind_to_and_types
      in
      (* Make fresh variables for the unavailable variables and form a
         renaming. *)
      let unavailable_to_fresh_vars =
        List.map (fun var -> var, Variable.rename var) unavailable_vars
        |> Variable.Map.of_list
      in
      let renaming =
        Variable.Map.fold
          (fun unavailable_var fresh_var renaming ->
            Renaming.add_fresh_variable renaming unavailable_var
              ~guaranteed_fresh:fresh_var)
          unavailable_to_fresh_vars Renaming.empty
      in
      (* Now replace any unavailable variables with their fresh counterparts, on
         both sides of the equations map. At the same time identify which
         equations now have fresh variables on their left-hand sides. *)
      let equations =
        List.map
          (fun (lhs, ty) ->
            let lhs' = Renaming.apply_name renaming lhs in
            let ty = TG.apply_renaming ty renaming in
            lhs', ty)
          equations
      in
      (* Finally form an environment extension with extra variables: the
         existentials are the fresh variables. *)
      let bind_to_vars =
        List.fold_left
          (fun bind_to_vars (bind_to, _) ->
            Name.pattern_match bind_to
              ~var:(fun var -> Variable.Set.add var bind_to_vars)
              ~symbol:(fun _ -> bind_to_vars))
          Variable.Set.empty bind_to_and_types
      in
      List.fold_left
        (fun env_extension (lhs, ty) ->
          let env_extension =
            Name.pattern_match lhs
              ~var:(fun lhs ->
                if not (Variable.Set.mem lhs bind_to_vars)
                then TEEV.add_definition env_extension lhs (TG.kind ty)
                else env_extension)
              ~symbol:(fun _ -> env_extension)
          in
          if TG.is_obviously_unknown ty
          then env_extension
          else TEEV.add_or_replace_equation env_extension lhs ty)
        TEEV.empty equations
