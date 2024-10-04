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

let fprintf = Format.fprintf

type t =
  | Set_of_closures of Set_of_closures.t
  | Block of
      Tag.Scannable.t
      * Mutability.t
      * Flambda_kind.Scannable_block_shape.t
      * Simple.With_debuginfo.t list
  | Boxed_float32 of Numeric_types.Float32_by_bit_pattern.t Or_variable.t
  | Boxed_float of Numeric_types.Float_by_bit_pattern.t Or_variable.t
  | Boxed_int32 of Int32.t Or_variable.t
  | Boxed_int64 of Int64.t Or_variable.t
  | Boxed_nativeint of Targetint_32_64.t Or_variable.t
  | Boxed_vec128 of Vector_types.Vec128.Bit_pattern.t Or_variable.t
  | Immutable_float_block of
      Numeric_types.Float_by_bit_pattern.t Or_variable.t list
  | Immutable_float_array of
      Numeric_types.Float_by_bit_pattern.t Or_variable.t list
  | Immutable_float32_array of
      Numeric_types.Float32_by_bit_pattern.t Or_variable.t list
  | Immutable_int32_array of Int32.t Or_variable.t list
  | Immutable_int64_array of Int64.t Or_variable.t list
  | Immutable_nativeint_array of Targetint_32_64.t Or_variable.t list
  | Immutable_vec128_array of
      Vector_types.Vec128.Bit_pattern.t Or_variable.t list
  | Immutable_value_array of Simple.With_debuginfo.t list
  | Empty_array of Empty_array_kind.t
  | Mutable_string of { initial_value : string }
  | Immutable_string of string

let set_of_closures set = Set_of_closures set

let block tag mutability shape fields = Block (tag, mutability, shape, fields)

let boxed_float32 or_var = Boxed_float32 or_var

let boxed_float or_var = Boxed_float or_var

let boxed_int32 or_var = Boxed_int32 or_var

let boxed_int64 or_var = Boxed_int64 or_var

let boxed_nativeint or_var = Boxed_nativeint or_var

let boxed_vec128 or_var = Boxed_vec128 or_var

let immutable_float_block fields = Immutable_float_block fields

let immutable_float_array fields =
  match fields with
  | [] -> Empty_array Values_or_immediates_or_naked_floats
  | _ :: _ -> Immutable_float_array fields

let immutable_float32_array fields =
  match fields with
  | [] -> Empty_array Naked_float32s
  | _ :: _ -> Immutable_float32_array fields

let immutable_int64_array fields =
  match fields with
  | [] -> Empty_array Naked_int64s
  | _ :: _ -> Immutable_int64_array fields

let immutable_int32_array fields =
  match fields with
  | [] -> Empty_array Naked_int32s
  | _ :: _ -> Immutable_int32_array fields

let immutable_nativeint_array fields =
  match fields with
  | [] -> Empty_array Naked_nativeints
  | _ :: _ -> Immutable_nativeint_array fields

let immutable_vec128_array fields =
  match fields with
  | [] -> Empty_array Naked_vec128s
  | _ :: _ -> Immutable_vec128_array fields

let immutable_value_array fields =
  match fields with
  | [] -> Empty_array Values_or_immediates_or_naked_floats
  | _ :: _ -> Immutable_value_array fields

let empty_array kind = Empty_array kind

let mutable_string ~initial_value = Mutable_string { initial_value }

let immutable_string str = Immutable_string str

let add_mutability_prefix (mut : Mutability.t) ~suffix =
  let prefix =
    match mut with
    | Immutable -> "Immutable_"
    | Immutable_unique -> "Unique_"
    | Mutable -> "Mutable_"
  in
  prefix ^ suffix

let [@ocamlformat "disable"] print ppf t =
  match t with
  | Set_of_closures set ->
    fprintf ppf "@[<hov 1>(%tSet_of_closures%t@ %a)@]"
      Flambda_colours.static_part
      Flambda_colours.pop
      Set_of_closures.print set
  | Block (tag, mut, shape, fields) ->
    fprintf ppf "@[<hov 1>(%t%s%t@ (tag %a)@ (shape %a)@ (%a))@]"
      Flambda_colours.static_part
      (add_mutability_prefix mut ~suffix:"block")
      Flambda_colours.pop
      Tag.Scannable.print tag
      Flambda_kind.Scannable_block_shape.print shape
      (Format.pp_print_list ~pp_sep:Format.pp_print_space
        Simple.With_debuginfo.print) fields
  | Boxed_float32 or_var ->
    fprintf ppf "@[<hov 1>(%tBoxed_float32%t@ %a)@]"
      Flambda_colours.static_part
      Flambda_colours.pop
      (Or_variable.print Numeric_types.Float32_by_bit_pattern.print) or_var
  | Boxed_float or_var ->
    fprintf ppf "@[<hov 1>(%tBoxed_float%t@ %a)@]"
      Flambda_colours.static_part
      Flambda_colours.pop
      (Or_variable.print Numeric_types.Float_by_bit_pattern.print) or_var
  | Boxed_int32 or_var ->
    fprintf ppf "@[<hov 1>(%tBoxed_int32%t@ %a)@]"
      Flambda_colours.static_part
      Flambda_colours.pop
      (Or_variable.print Numeric_types.Int32.print) or_var
  | Boxed_int64 or_var ->
    fprintf ppf "@[<hov 1>(%tBoxed_int64%t@ %a)@]"
      Flambda_colours.static_part
      Flambda_colours.pop
      (Or_variable.print Numeric_types.Int64.print) or_var
  | Boxed_nativeint or_var ->
    fprintf ppf "@[<hov 1>(%tBoxed_nativeint%t@ %a)@]"
      Flambda_colours.static_part
      Flambda_colours.pop
      (Or_variable.print Targetint_32_64.print) or_var
  | Boxed_vec128 (or_var) ->
    fprintf ppf "@[<hov 1>(%tBoxed_vec128%t@ %a)@]"
      Flambda_colours.static_part
      Flambda_colours.pop
      (Or_variable.print Vector_types.Vec128.Bit_pattern.print) or_var
  | Immutable_float_block fields ->
    fprintf ppf "@[<hov 1>(%tImmutable_float_block%t@ @[[| %a |]@])@]"
      Flambda_colours.static_part
      Flambda_colours.pop
      (Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.pp_print_string ppf "@; ")
        (Or_variable.print Numeric_types.Float_by_bit_pattern.print))
      fields
  | Immutable_float_array fields ->
    fprintf ppf "@[<hov 1>(%tImmutable_float_array%t@ @[[| %a |]@])@]"
      Flambda_colours.static_part
      Flambda_colours.pop
      (Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.pp_print_string ppf "@; ")
        (Or_variable.print Numeric_types.Float_by_bit_pattern.print))
      fields
  | Immutable_float32_array fields ->
    fprintf ppf "@[<hov 1>(%tImmutable_float32_array%t@ @[[| %a |]@])@]"
      Flambda_colours.static_part
      Flambda_colours.pop
      (Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.pp_print_string ppf "@; ")
    (Or_variable.print Numeric_types.Float32_by_bit_pattern.print))
      fields
  | Immutable_int32_array fields ->
    fprintf ppf "@[<hov 1>(%tImmutable_int32_array%t@ @[[| %a |]@])@]"
      Flambda_colours.static_part
      Flambda_colours.pop
      (Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.pp_print_string ppf "@; ")
        (Or_variable.print (fun ppf n -> Format.fprintf ppf "%ld" n)))
      fields
  | Immutable_int64_array fields ->
    fprintf ppf "@[<hov 1>(%tImmutable_int64_array%t@ @[[| %a |]@])@]"
      Flambda_colours.static_part
      Flambda_colours.pop
      (Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.pp_print_string ppf "@; ")
        (Or_variable.print (fun ppf n -> Format.fprintf ppf "%Ld" n)))
      fields
  | Immutable_nativeint_array fields ->
    fprintf ppf "@[<hov 1>(%tImmutable_nativeint_array%t@ @[[| %a |]@])@]"
      Flambda_colours.static_part
      Flambda_colours.pop
      (Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.pp_print_string ppf "@; ")
        (Or_variable.print Targetint_32_64.print))
      fields
  | Immutable_vec128_array fields ->
    fprintf ppf "@[<hov 1>(%tImmutable_vec128_array%t@ @[[| %a |]@])@]"
      Flambda_colours.static_part
      Flambda_colours.pop
      (Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.pp_print_string ppf "@; ")
        (Or_variable.print Vector_types.Vec128.Bit_pattern.print))
      fields
  | Immutable_value_array fields ->
    fprintf ppf "@[<hov 1>(%tImmutable_value_array%t@ (%a))@]"
      Flambda_colours.static_part
      Flambda_colours.pop
      (Format.pp_print_list ~pp_sep:Format.pp_print_space
        Simple.With_debuginfo.print) fields
  | Empty_array array_kind ->
    fprintf ppf "%tEmpty_array(%a)%t"
      Flambda_colours.static_part
      Empty_array_kind.print array_kind
      Flambda_colours.pop
  | Mutable_string { initial_value = s; } ->
    fprintf ppf "@[<hov 1>(%tMutable_string%t@ %S)@]"
      Flambda_colours.static_part
      Flambda_colours.pop
      s
  | Immutable_string s ->
    fprintf ppf "@[<hov 1>(%tImmutable_string%t@ %S)@]"
      Flambda_colours.static_part
      Flambda_colours.pop
      s

include Container_types.Make (struct
  type nonrec t = t

  let print = print

  let compare t1 t2 =
    match t1, t2 with
    | Set_of_closures set1, Set_of_closures set2 ->
      Set_of_closures.compare set1 set2
    | Block (tag1, mut1, shape1, fields1), Block (tag2, mut2, shape2, fields2)
      ->
      let c = Tag.Scannable.compare tag1 tag2 in
      if c <> 0
      then c
      else
        let c = Mutability.compare mut1 mut2 in
        if c <> 0
        then c
        else
          let c = Flambda_kind.Scannable_block_shape.compare shape1 shape2 in
          if c <> 0
          then c
          else
            Misc.Stdlib.List.compare Simple.With_debuginfo.compare fields1
              fields2
    | Boxed_float32 or_var1, Boxed_float32 or_var2 ->
      Or_variable.compare Numeric_types.Float32_by_bit_pattern.compare or_var1
        or_var2
    | Boxed_float or_var1, Boxed_float or_var2 ->
      Or_variable.compare Numeric_types.Float_by_bit_pattern.compare or_var1
        or_var2
    | Boxed_int32 or_var1, Boxed_int32 or_var2 ->
      Or_variable.compare Numeric_types.Int32.compare or_var1 or_var2
    | Boxed_int64 or_var1, Boxed_int64 or_var2 ->
      Or_variable.compare Numeric_types.Int64.compare or_var1 or_var2
    | Boxed_nativeint or_var1, Boxed_nativeint or_var2 ->
      Or_variable.compare Targetint_32_64.compare or_var1 or_var2
    | Boxed_vec128 or_var1, Boxed_vec128 or_var2 ->
      Or_variable.compare Vector_types.Vec128.Bit_pattern.compare or_var1
        or_var2
    | Immutable_float_block fields1, Immutable_float_array fields2 ->
      Misc.Stdlib.List.compare
        (Or_variable.compare Numeric_types.Float_by_bit_pattern.compare)
        fields1 fields2
    | Immutable_float_array fields1, Immutable_float_array fields2 ->
      Misc.Stdlib.List.compare
        (Or_variable.compare Numeric_types.Float_by_bit_pattern.compare)
        fields1 fields2
    | Immutable_float32_array fields1, Immutable_float32_array fields2 ->
      Misc.Stdlib.List.compare
        (Or_variable.compare Numeric_types.Float32_by_bit_pattern.compare)
        fields1 fields2
    | Immutable_int64_array fields1, Immutable_int64_array fields2 ->
      Misc.Stdlib.List.compare
        (Or_variable.compare Int64.compare)
        fields1 fields2
    | Immutable_int32_array fields1, Immutable_int32_array fields2 ->
      Misc.Stdlib.List.compare
        (Or_variable.compare Int32.compare)
        fields1 fields2
    | Immutable_nativeint_array fields1, Immutable_nativeint_array fields2 ->
      Misc.Stdlib.List.compare
        (Or_variable.compare Targetint_32_64.compare)
        fields1 fields2
    | Immutable_vec128_array fields1, Immutable_vec128_array fields2 ->
      Misc.Stdlib.List.compare
        (Or_variable.compare Vector_types.Vec128.Bit_pattern.compare)
        fields1 fields2
    | Immutable_value_array fields1, Immutable_value_array fields2 ->
      Misc.Stdlib.List.compare Simple.With_debuginfo.compare fields1 fields2
    | Empty_array array_kind1, Empty_array array_kind2 ->
      Empty_array_kind.compare array_kind1 array_kind2
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
    | Boxed_float32 _, _ -> -1
    | _, Boxed_float32 _ -> 1
    | Boxed_int32 _, _ -> -1
    | _, Boxed_int32 _ -> 1
    | Boxed_int64 _, _ -> -1
    | _, Boxed_int64 _ -> 1
    | Boxed_nativeint _, _ -> -1
    | _, Boxed_nativeint _ -> 1
    | Boxed_vec128 _, _ -> -1
    | _, Boxed_vec128 _ -> 1
    | Immutable_float_block _, _ -> -1
    | _, Immutable_float_block _ -> 1
    | Immutable_float_array _, _ -> -1
    | _, Immutable_float_array _ -> 1
    | Immutable_float32_array _, _ -> -1
    | _, Immutable_float32_array _ -> 1
    | Immutable_int64_array _, _ -> -1
    | _, Immutable_int64_array _ -> 1
    | Immutable_int32_array _, _ -> -1
    | _, Immutable_int32_array _ -> 1
    | Immutable_nativeint_array _, _ -> -1
    | _, Immutable_nativeint_array _ -> 1
    | Immutable_vec128_array _, _ -> -1
    | _, Immutable_vec128_array _ -> 1
    | Immutable_value_array _, _ -> -1
    | _, Immutable_value_array _ -> 1
    | Empty_array _, _ -> -1
    | _, Empty_array _ -> 1
    | Mutable_string _, _ -> -1
    | _, Mutable_string _ -> 1

  let equal t1 t2 = compare t1 t2 = 0

  let hash _t = Misc.fatal_error "Not yet implemented"
end)

let free_names_of_fields fields =
  List.fold_left
    (fun fvs field ->
      Name_occurrences.union fvs (Simple.With_debuginfo.free_names field))
    Name_occurrences.empty fields

let free_names t =
  let[@inline] free_names_for_numeric_fields fields =
    List.fold_left
      (fun fns (field : _ Or_variable.t) ->
        match field with
        | Var (v, _dbg) -> Name_occurrences.add_variable fns v Name_mode.normal
        | Const _ -> fns)
      Name_occurrences.empty fields
  in
  match t with
  | Set_of_closures set -> Set_of_closures.free_names set
  | Block (_tag, _mut, _shape, fields) -> free_names_of_fields fields
  | Boxed_float32 or_var -> Or_variable.free_names or_var
  | Boxed_float or_var -> Or_variable.free_names or_var
  | Boxed_int32 or_var -> Or_variable.free_names or_var
  | Boxed_int64 or_var -> Or_variable.free_names or_var
  | Boxed_nativeint or_var -> Or_variable.free_names or_var
  | Boxed_vec128 or_var -> Or_variable.free_names or_var
  | Mutable_string { initial_value = _ } | Immutable_string _ | Empty_array _ ->
    Name_occurrences.empty
  | Immutable_float_block fields | Immutable_float_array fields ->
    free_names_for_numeric_fields fields
  | Immutable_float32_array fields -> free_names_for_numeric_fields fields
  | Immutable_int32_array fields -> free_names_for_numeric_fields fields
  | Immutable_int64_array fields -> free_names_for_numeric_fields fields
  | Immutable_nativeint_array fields -> free_names_for_numeric_fields fields
  | Immutable_vec128_array fields -> free_names_for_numeric_fields fields
  | Immutable_value_array fields -> free_names_of_fields fields

let apply_renaming_number_array_fields renaming fields =
  Misc.Stdlib.List.map_sharing
    (fun (field : _ Or_variable.t) : _ Or_variable.t ->
      match field with
      | Var (v, dbg) ->
        let v' = Renaming.apply_variable renaming v in
        if v == v' then field else Var (v', dbg)
      | Const _ -> field)
    fields

let apply_renaming t renaming =
  if Renaming.is_identity renaming
  then t
  else
    match t with
    | Set_of_closures set ->
      let set' = Set_of_closures.apply_renaming set renaming in
      if set == set' then t else Set_of_closures set'
    | Block (tag, mut, shape, fields) ->
      let fields' =
        Misc.Stdlib.List.map_sharing
          (fun field -> Simple.With_debuginfo.apply_renaming field renaming)
          fields
      in
      if fields' == fields then t else Block (tag, mut, shape, fields')
    | Boxed_float32 or_var ->
      let or_var' = Or_variable.apply_renaming or_var renaming in
      if or_var == or_var' then t else Boxed_float32 or_var'
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
    | Boxed_vec128 or_var ->
      let or_var' = Or_variable.apply_renaming or_var renaming in
      if or_var == or_var' then t else Boxed_vec128 or_var'
    | Mutable_string { initial_value = _ } | Immutable_string _ -> t
    | Immutable_float_block fields ->
      let fields' = apply_renaming_number_array_fields renaming fields in
      if fields' == fields then t else Immutable_float_block fields'
    | Immutable_float_array fields ->
      let fields' = apply_renaming_number_array_fields renaming fields in
      if fields' == fields then t else Immutable_float_array fields'
    | Immutable_float32_array fields ->
      let fields' = apply_renaming_number_array_fields renaming fields in
      if fields' == fields then t else Immutable_float32_array fields'
    | Immutable_int32_array fields ->
      let fields' = apply_renaming_number_array_fields renaming fields in
      if fields' == fields then t else Immutable_int32_array fields'
    | Immutable_int64_array fields ->
      let fields' = apply_renaming_number_array_fields renaming fields in
      if fields' == fields then t else Immutable_int64_array fields'
    | Immutable_nativeint_array fields ->
      let fields' = apply_renaming_number_array_fields renaming fields in
      if fields' == fields then t else Immutable_nativeint_array fields'
    | Immutable_vec128_array fields ->
      let fields' = apply_renaming_number_array_fields renaming fields in
      if fields' == fields then t else Immutable_vec128_array fields'
    | Immutable_value_array fields ->
      let fields' =
        Misc.Stdlib.List.map_sharing
          (fun field -> Simple.With_debuginfo.apply_renaming field renaming)
          fields
      in
      if fields == fields' then t else Immutable_value_array fields'
    | Empty_array _ as res -> res

let ids_for_export_fields fields =
  List.fold_left
    (fun ids field ->
      Ids_for_export.union ids (Simple.With_debuginfo.ids_for_export field))
    Ids_for_export.empty fields

let ids_for_export_number_array_fields fields =
  List.fold_left
    (fun ids (field : _ Or_variable.t) ->
      match field with
      | Var (var, _dbg) -> Ids_for_export.add_variable ids var
      | Const _ -> ids)
    Ids_for_export.empty fields

let ids_for_export t =
  match t with
  | Set_of_closures set -> Set_of_closures.ids_for_export set
  | Block (_tag, _mut, _shape, fields) -> ids_for_export_fields fields
  | Boxed_float (Var (var, _dbg))
  | Boxed_float32 (Var (var, _dbg))
  | Boxed_int32 (Var (var, _dbg))
  | Boxed_int64 (Var (var, _dbg))
  | Boxed_nativeint (Var (var, _dbg))
  | Boxed_vec128 (Var (var, _dbg)) ->
    Ids_for_export.add_variable Ids_for_export.empty var
  | Boxed_float (Const _)
  | Boxed_float32 (Const _)
  | Boxed_int32 (Const _)
  | Boxed_int64 (Const _)
  | Boxed_nativeint (Const _)
  | Boxed_vec128 (Const _)
  | Mutable_string { initial_value = _ }
  | Immutable_string _ ->
    Ids_for_export.empty
  | Immutable_float_block fields -> ids_for_export_number_array_fields fields
  | Immutable_float_array fields -> ids_for_export_number_array_fields fields
  | Immutable_float32_array fields -> ids_for_export_number_array_fields fields
  | Immutable_int32_array fields -> ids_for_export_number_array_fields fields
  | Immutable_int64_array fields -> ids_for_export_number_array_fields fields
  | Immutable_nativeint_array fields ->
    ids_for_export_number_array_fields fields
  | Immutable_vec128_array fields -> ids_for_export_number_array_fields fields
  | Immutable_value_array fields -> ids_for_export_fields fields
  | Empty_array _ -> Ids_for_export.empty

let is_block t =
  match t with
  | Block _ | Boxed_float _ | Boxed_float32 _ | Boxed_int32 _ | Boxed_int64 _
  | Boxed_nativeint _ | Boxed_vec128 _ | Immutable_float_block _
  | Immutable_float_array _ | Immutable_float32_array _
  | Immutable_int32_array _ | Immutable_int64_array _
  | Immutable_nativeint_array _ | Immutable_vec128_array _ | Immutable_string _
  | Mutable_string _ | Empty_array _ | Immutable_value_array _ ->
    true
  | Set_of_closures _ -> false

let is_set_of_closures t =
  match t with
  | Set_of_closures _ -> true
  | Block _ | Boxed_float _ | Boxed_float32 _ | Boxed_int32 _ | Boxed_int64 _
  | Boxed_nativeint _ | Boxed_vec128 _ | Immutable_float_block _
  | Immutable_float_array _ | Immutable_float32_array _
  | Immutable_int32_array _ | Immutable_int64_array _
  | Immutable_nativeint_array _ | Immutable_vec128_array _ | Immutable_string _
  | Mutable_string _ | Empty_array _ | Immutable_value_array _ ->
    false

let is_fully_static t = free_names t |> Name_occurrences.no_variables

let can_share0 t =
  match t with
  | Block (_, Immutable, _, _)
  | Set_of_closures _ | Boxed_float _ | Boxed_float32 _ | Boxed_int32 _
  | Boxed_int64 _ | Boxed_vec128 _ | Boxed_nativeint _ | Immutable_float_block _
  | Immutable_float_array _ | Immutable_float32_array _ | Immutable_string _
  | Empty_array _ | Immutable_int32_array _ | Immutable_int64_array _
  | Immutable_nativeint_array _ | Immutable_vec128_array _
  | Immutable_value_array _ ->
    true
  | Block (_, (Mutable | Immutable_unique), _, _) | Mutable_string _ -> false

let can_share t = can_share0 t && is_fully_static t

let must_be_set_of_closures t =
  match t with
  | Set_of_closures set -> set
  | Block _ | Boxed_float _ | Boxed_float32 _ | Boxed_int32 _ | Boxed_int64 _
  | Boxed_nativeint _ | Boxed_vec128 _ | Immutable_float_block _
  | Immutable_float_array _ | Immutable_float32_array _
  | Immutable_int32_array _ | Immutable_int64_array _
  | Immutable_nativeint_array _ | Immutable_vec128_array _ | Empty_array _
  | Immutable_value_array _ | Immutable_string _ | Mutable_string _ ->
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
  | ( ( Block _ | Boxed_float _ | Boxed_float32 _ | Boxed_int32 _
      | Boxed_int64 _ | Boxed_vec128 _ | Boxed_nativeint _
      | Immutable_float_block _ | Immutable_float_array _
      | Immutable_float32_array _ | Immutable_int32_array _
      | Immutable_int64_array _ | Immutable_nativeint_array _
      | Immutable_vec128_array _ | Immutable_value_array _ | Empty_array _
      | Immutable_string _ | Mutable_string _ ),
      Block_like symbol ) ->
    block_like_callback symbol t
  | Set_of_closures _, (Block_like _ | Code _)
  | ( ( Block _ | Boxed_float _ | Boxed_float32 _ | Boxed_int32 _
      | Boxed_int64 _ | Boxed_vec128 _ | Boxed_nativeint _
      | Immutable_float_block _ | Immutable_float_array _
      | Immutable_float32_array _ | Immutable_int32_array _
      | Immutable_int64_array _ | Immutable_nativeint_array _
      | Immutable_vec128_array _ | Immutable_value_array _ | Empty_array _
      | Immutable_string _ | Mutable_string _ ),
      (Set_of_closures _ | Code _) ) ->
    Misc.fatal_errorf "Mismatch on variety of [Static_const]:@ %a@ =@ %a"
      Bound_static.Pattern.print pat print t
