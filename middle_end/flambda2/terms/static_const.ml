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

let fprintf = Format.fprintf

type t =
  | Set_of_closures of Set_of_closures.t
  | Block of Tag.Scannable.t * Mutability.t * Field_of_static_block.t list
  | Boxed_float of Numeric_types.Float_by_bit_pattern.t Or_variable.t
  | Boxed_int32 of Int32.t Or_variable.t
  | Boxed_int64 of Int64.t Or_variable.t
  | Boxed_nativeint of Targetint_32_64.t Or_variable.t
  | Immutable_float_block of
      Numeric_types.Float_by_bit_pattern.t Or_variable.t list
  | Immutable_float_array of
      Numeric_types.Float_by_bit_pattern.t Or_variable.t list
  | Empty_array
  | Mutable_string of { initial_value : string }
  | Immutable_string of string

let [@ocamlformat "disable"] print ppf t =
  match t with
  | Set_of_closures set ->
    fprintf ppf "@[<hov 1>(@<0>%sSet_of_closures@<0>%s@ %a)@]"
      (Flambda_colours.static_part ())
      (Flambda_colours.normal ())
      Set_of_closures.print set
  | Block (tag, mut, fields) ->
    fprintf ppf "@[<hov 1>(@<0>%s%sblock@<0>%s@ (tag %a)@ (%a))@]"
      (Flambda_colours.static_part ())
      (match mut with
        | Immutable -> "Immutable_"
        | Immutable_unique -> "Unique_"
        | Mutable -> "Mutable_")
      (Flambda_colours.normal ())
      Tag.Scannable.print tag
      (Format.pp_print_list ~pp_sep:Format.pp_print_space
        Field_of_static_block.print) fields
  | Boxed_float or_var ->
    fprintf ppf "@[<hov 1>(@<0>%sBoxed_float@<0>%s@ %a)@]"
      (Flambda_colours.static_part ())
      (Flambda_colours.normal ())
      (Or_variable.print Numeric_types.Float_by_bit_pattern.print) or_var
  | Boxed_int32 or_var ->
    fprintf ppf "@[<hov 1>(@<0>%sBoxed_int32@<0>%s@ %a)@]"
      (Flambda_colours.static_part ())
      (Flambda_colours.normal ())
      (Or_variable.print Numeric_types.Int32.print) or_var
  | Boxed_int64 or_var ->
    fprintf ppf "@[<hov 1>(@<0>%sBoxed_int64@<0>%s@ %a)@]"
      (Flambda_colours.static_part ())
      (Flambda_colours.normal ())
      (Or_variable.print Numeric_types.Int64.print) or_var
  | Boxed_nativeint or_var ->
    fprintf ppf "@[<hov 1>(@<0>%sBoxed_nativeint@<0>%s@ %a)@]"
      (Flambda_colours.static_part ())
      (Flambda_colours.normal ())
      (Or_variable.print Targetint_32_64.print) or_var
  | Immutable_float_block fields ->
    fprintf ppf "@[<hov 1>(@<0>%sImmutable_float_block@<0>%s@ @[[| %a |]@])@]"
      (Flambda_colours.static_part ())
      (Flambda_colours.normal ())
      (Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.pp_print_string ppf "@; ")
        (Or_variable.print Numeric_types.Float_by_bit_pattern.print))
      fields
  | Immutable_float_array fields ->
    fprintf ppf "@[<hov 1>(@<0>%sImmutable_float_array@<0>%s@ @[[| %a |]@])@]"
      (Flambda_colours.static_part ())
      (Flambda_colours.normal ())
      (Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.pp_print_string ppf "@; ")
        (Or_variable.print Numeric_types.Float_by_bit_pattern.print))
      fields
  | Empty_array ->
    fprintf ppf "@<0>%sEmpty_array@<0>%s"
      (Flambda_colours.static_part ())
      (Flambda_colours.normal ())
  | Mutable_string { initial_value = s; } ->
    fprintf ppf "@[<hov 1>(@<0>%sMutable_string@<0>%s@ %S)@]"
      (Flambda_colours.static_part ())
      (Flambda_colours.normal ())
      s
  | Immutable_string s ->
    fprintf ppf "@[<hov 1>(@<0>%sImmutable_string@<0>%s@ %S)@]"
      (Flambda_colours.static_part ())
      (Flambda_colours.normal ())
      s

include Container_types.Make (struct
  type nonrec t = t

  let print = print

  let compare t1 t2 =
    match t1, t2 with
    | Set_of_closures set1, Set_of_closures set2 ->
      Set_of_closures.compare set1 set2
    | Block (tag1, mut1, fields1), Block (tag2, mut2, fields2) ->
      let c = Tag.Scannable.compare tag1 tag2 in
      if c <> 0
      then c
      else
        let c = Mutability.compare mut1 mut2 in
        if c <> 0
        then c
        else
          Misc.Stdlib.List.compare Field_of_static_block.compare fields1 fields2
    | Boxed_float or_var1, Boxed_float or_var2 ->
      Or_variable.compare Numeric_types.Float_by_bit_pattern.compare or_var1
        or_var2
    | Boxed_int32 or_var1, Boxed_int32 or_var2 ->
      Or_variable.compare Numeric_types.Int32.compare or_var1 or_var2
    | Boxed_int64 or_var1, Boxed_int64 or_var2 ->
      Or_variable.compare Numeric_types.Int64.compare or_var1 or_var2
    | Boxed_nativeint or_var1, Boxed_nativeint or_var2 ->
      Or_variable.compare Targetint_32_64.compare or_var1 or_var2
    | Immutable_float_block fields1, Immutable_float_array fields2 ->
      Misc.Stdlib.List.compare
        (Or_variable.compare Numeric_types.Float_by_bit_pattern.compare)
        fields1 fields2
    | Immutable_float_array fields1, Immutable_float_array fields2 ->
      Misc.Stdlib.List.compare
        (Or_variable.compare Numeric_types.Float_by_bit_pattern.compare)
        fields1 fields2
    | Empty_array, Empty_array -> 0
    | ( Mutable_string { initial_value = s1 },
        Mutable_string { initial_value = s2 } )
    | Immutable_string s1, Immutable_string s2 ->
      String.compare s1 s2
    | Block _, _ -> -1
    | _, Block _ -> 1
    | Set_of_closures _, _ -> -1
    | _, Set_of_closures _ -> 1
    | Boxed_float _, _ -> -1
    | _, Boxed_float _ -> 1
    | Boxed_int32 _, _ -> -1
    | _, Boxed_int32 _ -> 1
    | Boxed_int64 _, _ -> -1
    | _, Boxed_int64 _ -> 1
    | Boxed_nativeint _, _ -> -1
    | _, Boxed_nativeint _ -> 1
    | Immutable_float_block _, _ -> -1
    | _, Immutable_float_block _ -> 1
    | Immutable_float_array _, _ -> -1
    | _, Immutable_float_array _ -> 1
    | Empty_array, _ -> -1
    | _, Empty_array -> 1
    | Mutable_string _, _ -> -1
    | Immutable_string _, Mutable_string _ -> 1

  let equal t1 t2 = compare t1 t2 = 0

  let hash _t = Misc.fatal_error "Not yet implemented"
end)

let free_names t =
  match t with
  | Set_of_closures set -> Set_of_closures.free_names set
  | Block (_tag, _mut, fields) ->
    List.fold_left
      (fun fvs field ->
        Name_occurrences.union fvs (Field_of_static_block.free_names field))
      Name_occurrences.empty fields
  | Boxed_float or_var -> Or_variable.free_names or_var
  | Boxed_int32 or_var -> Or_variable.free_names or_var
  | Boxed_int64 or_var -> Or_variable.free_names or_var
  | Boxed_nativeint or_var -> Or_variable.free_names or_var
  | Mutable_string { initial_value = _ } | Immutable_string _ | Empty_array ->
    Name_occurrences.empty
  | Immutable_float_block fields | Immutable_float_array fields ->
    List.fold_left
      (fun fns (field : _ Or_variable.t) ->
        match field with
        | Var (v, _dbg) -> Name_occurrences.add_variable fns v Name_mode.normal
        | Const _ -> fns)
      Name_occurrences.empty fields

let apply_renaming t renaming =
  if Renaming.is_empty renaming
  then t
  else
    match t with
    | Set_of_closures set ->
      let set' = Set_of_closures.apply_renaming set renaming in
      if set == set' then t else Set_of_closures set'
    | Block (tag, mut, fields) ->
      let changed = ref false in
      let fields =
        List.map
          (fun field ->
            let field' = Field_of_static_block.apply_renaming field renaming in
            if not (field == field') then changed := true;
            field')
          fields
      in
      if not !changed then t else Block (tag, mut, fields)
    | Boxed_float or_var ->
      let or_var' = Or_variable.apply_renaming or_var renaming in
      if or_var == or_var' then t else Boxed_float or_var'
    | Boxed_int32 or_var ->
      let or_var' = Or_variable.apply_renaming or_var renaming in
      if or_var == or_var' then t else Boxed_int32 or_var'
    | Boxed_int64 or_var ->
      let or_var' = Or_variable.apply_renaming or_var renaming in
      if or_var == or_var' then t else Boxed_int64 or_var'
    | Boxed_nativeint or_var ->
      let or_var' = Or_variable.apply_renaming or_var renaming in
      if or_var == or_var' then t else Boxed_nativeint or_var'
    | Mutable_string { initial_value = _ } | Immutable_string _ -> t
    | Immutable_float_block fields ->
      let changed = ref false in
      let fields =
        List.map
          (fun (field : _ Or_variable.t) ->
            let field' : _ Or_variable.t =
              match field with
              | Var (v, dbg) -> Var (Renaming.apply_variable renaming v, dbg)
              | Const _ -> field
            in
            if not (field == field') then changed := true;
            field')
          fields
      in
      if not !changed then t else Immutable_float_block fields
    | Immutable_float_array fields ->
      let changed = ref false in
      let fields =
        List.map
          (fun (field : _ Or_variable.t) ->
            let field' : _ Or_variable.t =
              match field with
              | Var (v, dbg) -> Var (Renaming.apply_variable renaming v, dbg)
              | Const _ -> field
            in
            if not (field == field') then changed := true;
            field')
          fields
      in
      if not !changed then t else Immutable_float_array fields
    | Empty_array -> Empty_array

let all_ids_for_export t =
  match t with
  | Set_of_closures set -> Set_of_closures.all_ids_for_export set
  | Block (_tag, _mut, fields) ->
    List.fold_left
      (fun ids field ->
        Ids_for_export.union ids
          (Field_of_static_block.all_ids_for_export field))
      Ids_for_export.empty fields
  | Boxed_float (Var (var, _dbg))
  | Boxed_int32 (Var (var, _dbg))
  | Boxed_int64 (Var (var, _dbg))
  | Boxed_nativeint (Var (var, _dbg)) ->
    Ids_for_export.add_variable Ids_for_export.empty var
  | Boxed_float (Const _)
  | Boxed_int32 (Const _)
  | Boxed_int64 (Const _)
  | Boxed_nativeint (Const _)
  | Mutable_string { initial_value = _ }
  | Immutable_string _ ->
    Ids_for_export.empty
  | Immutable_float_block fields ->
    List.fold_left
      (fun ids (field : _ Or_variable.t) ->
        match field with
        | Var (var, _dbg) -> Ids_for_export.add_variable ids var
        | Const _ -> ids)
      Ids_for_export.empty fields
  | Immutable_float_array fields ->
    List.fold_left
      (fun ids (field : _ Or_variable.t) ->
        match field with
        | Var (var, _dbg) -> Ids_for_export.add_variable ids var
        | Const _ -> ids)
      Ids_for_export.empty fields
  | Empty_array -> Ids_for_export.empty

let is_block t =
  match t with
  | Block _ | Boxed_float _ | Boxed_int32 _ | Boxed_int64 _ | Boxed_nativeint _
  | Immutable_float_block _ | Immutable_float_array _ | Immutable_string _
  | Mutable_string _ | Empty_array ->
    true
  | Set_of_closures _ -> false

let is_set_of_closures t =
  match t with
  | Set_of_closures _ -> true
  | Block _ | Boxed_float _ | Boxed_int32 _ | Boxed_int64 _ | Boxed_nativeint _
  | Immutable_float_block _ | Immutable_float_array _ | Immutable_string _
  | Mutable_string _ | Empty_array ->
    false

let is_fully_static t = free_names t |> Name_occurrences.no_variables

let can_share0 t =
  match t with
  | Block (_, Immutable, _)
  | Set_of_closures _ | Boxed_float _ | Boxed_int32 _ | Boxed_int64 _
  | Boxed_nativeint _ | Immutable_float_block _ | Immutable_float_array _
  | Immutable_string _ | Empty_array ->
    true
  | Block (_, (Mutable | Immutable_unique), _) | Mutable_string _ -> false

let can_share t = can_share0 t && is_fully_static t

let must_be_set_of_closures t =
  match t with
  | Set_of_closures set -> set
  | Block _ | Boxed_float _ | Boxed_int32 _ | Boxed_int64 _ | Boxed_nativeint _
  | Immutable_float_block _ | Immutable_float_array _ | Empty_array
  | Immutable_string _ | Mutable_string _ ->
    Misc.fatal_errorf "Not a set of closures:@ %a" print t

let match_against_bound_static_pattern t (pat : Bound_static.Pattern.t)
    ~set_of_closures:set_of_closures_callback ~block_like:block_like_callback =
  match t, pat with
  | Set_of_closures set_of_closures, Set_of_closures closure_symbols ->
    let function_slots =
      Set_of_closures.function_decls set_of_closures
      |> Function_declarations.funs_in_order |> Function_slot.Lmap.keys
    in
    let function_slots' = Function_slot.Lmap.keys closure_symbols in
    let function_slots_match =
      (* Note that we check the order here. *)
      Misc.Stdlib.List.compare Function_slot.compare function_slots
        function_slots'
      = 0
    in
    if not function_slots_match
    then
      Misc.fatal_errorf "Mismatch on declared function slots:@ %a@ =@ %a"
        Bound_static.Pattern.print pat print t;
    set_of_closures_callback ~closure_symbols set_of_closures
  | ( ( Block _ | Boxed_float _ | Boxed_int32 _ | Boxed_int64 _
      | Boxed_nativeint _ | Immutable_float_block _ | Immutable_float_array _
      | Empty_array | Immutable_string _ | Mutable_string _ ),
      Block_like symbol ) ->
    block_like_callback symbol t
  | Set_of_closures _, (Block_like _ | Code _)
  | ( ( Block _ | Boxed_float _ | Boxed_int32 _ | Boxed_int64 _
      | Boxed_nativeint _ | Immutable_float_block _ | Immutable_float_array _
      | Empty_array | Immutable_string _ | Mutable_string _ ),
      (Set_of_closures _ | Code _) ) ->
    Misc.fatal_errorf "Mismatch on variety of [Static_const]:@ %a@ =@ %a"
      Bound_static.Pattern.print pat print t
