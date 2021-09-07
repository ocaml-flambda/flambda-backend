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

let fprintf = Format.fprintf

module K = Flambda_kind

module Field_of_block = struct
  type t =
    | Symbol of Symbol.t
    | Tagged_immediate of Targetint_31_63.t
    | Dynamically_computed of Variable.t

  include Container_types.Make (struct
    type nonrec t = t

    let compare t1 t2 =
      match t1, t2 with
      | Symbol s1, Symbol s2 -> Symbol.compare s1 s2
      | Tagged_immediate t1, Tagged_immediate t2 ->
        Targetint_31_63.compare t1 t2
      | Dynamically_computed v1, Dynamically_computed v2 ->
        Variable.compare v1 v2
      | Symbol _, Tagged_immediate _ -> -1
      | Tagged_immediate _, Symbol _ -> 1
      | Symbol _, Dynamically_computed _ -> -1
      | Dynamically_computed _, Symbol _ -> 1
      | Tagged_immediate _, Dynamically_computed _ -> -1
      | Dynamically_computed _, Tagged_immediate _ -> 1

    let equal t1 t2 =
      compare t1 t2 = 0

    let hash t =
      match t with
      | Symbol symbol -> Hashtbl.hash (0, Symbol.hash symbol)
      | Tagged_immediate immediate ->
        Hashtbl.hash (1, Targetint_31_63.hash immediate)
      | Dynamically_computed var -> Hashtbl.hash (2, Variable.hash var)

    let [@ocamlformat "disable"] print ppf t =
      match t with
      | Symbol symbol -> Symbol.print ppf symbol
      | Tagged_immediate immediate -> Targetint_31_63.print ppf immediate
      | Dynamically_computed var -> Variable.print ppf var

    let output chan t =
      print (Format.formatter_of_out_channel chan) t
  end)

  let apply_renaming t renaming =
    match t with
    | Tagged_immediate _ -> t
    | Symbol symbol ->
      let symbol' = Renaming.apply_symbol renaming symbol in
      if symbol == symbol' then t
      else Symbol symbol'
    | Dynamically_computed var ->
      let var' = Renaming.apply_variable renaming var in
      if var == var' then t
      else Dynamically_computed var'

  let free_names t =
    match t with
    | Dynamically_computed var ->
      Name_occurrences.singleton_variable var Name_mode.normal
    | Symbol sym ->
      Name_occurrences.singleton_symbol sym Name_mode.normal
    | Tagged_immediate _ -> Name_occurrences.empty

  let all_ids_for_export t =
    match t with
    | Dynamically_computed var ->
      Ids_for_export.add_variable Ids_for_export.empty var
    | Symbol sym ->
      Ids_for_export.add_symbol Ids_for_export.empty sym
    | Tagged_immediate _ -> Ids_for_export.empty
end

type t =
  | Code of Code.t
  | Set_of_closures of Set_of_closures.t
  | Block of Tag.Scannable.t * Mutability.t * (Field_of_block.t list)
  | Boxed_float of Numeric_types.Float_by_bit_pattern.t Or_variable.t
  | Boxed_int32 of Int32.t Or_variable.t
  | Boxed_int64 of Int64.t Or_variable.t
  | Boxed_nativeint of Targetint_32_64.t Or_variable.t
  | Immutable_float_block of Numeric_types.Float_by_bit_pattern.t Or_variable.t list
  | Immutable_float_array of Numeric_types.Float_by_bit_pattern.t Or_variable.t list
  | Mutable_string of { initial_value : string; }
  | Immutable_string of string

type static_const = t

let [@ocamlformat "disable"] print_with_cache ~cache ppf t =
  match t with
  | Code code ->
    fprintf ppf "@[<hov 1>(@<0>%sCode@<0>%s@ %a)@]"
      (Flambda_colours.static_part ())
      (Flambda_colours.normal ())
      (Code.print_with_cache ~cache) code
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
        Field_of_block.print) fields
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

  let [@ocamlformat "disable"] print ppf t =
    print_with_cache ~cache:(Printing_cache.create ()) ppf t

  let compare t1 t2 =
    match t1, t2 with
    | Code code1, Code code2 -> Code.compare code1 code2
    | Set_of_closures set1, Set_of_closures set2 ->
      Set_of_closures.compare set1 set2
    | Block (tag1, mut1, fields1), Block (tag2, mut2, fields2) ->
      let c = Tag.Scannable.compare tag1 tag2 in
      if c <> 0 then c
      else
        let c = Mutability.compare mut1 mut2 in
        if c <> 0 then c
        else Misc.Stdlib.List.compare Field_of_block.compare fields1 fields2
    | Boxed_float or_var1, Boxed_float or_var2 ->
      Or_variable.compare Numeric_types.Float_by_bit_pattern.compare or_var1 or_var2
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
    | Mutable_string { initial_value = s1; },
      Mutable_string { initial_value = s2; }
    | Immutable_string s1, Immutable_string s2 -> String.compare s1 s2
    | Block _, _ -> -1
    | _, Block _ -> 1
    | Code _, _ -> -1
    | _, Code _ -> 1
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
    | Mutable_string _, _ -> -1
    | Immutable_string _, Mutable_string _ -> 1

  let equal t1 t2 = (compare t1 t2 = 0)

  let hash _t = Misc.fatal_error "Not yet implemented"

  let output _ _ = Misc.fatal_error "Not yet implemented"
end)

let free_names t =
  match t with
  | Code code -> Code.free_names code
  | Set_of_closures set -> Set_of_closures.free_names set
  | Block (_tag, _mut, fields) ->
    List.fold_left (fun fvs field ->
        Name_occurrences.union fvs (Field_of_block.free_names field))
      (Name_occurrences.empty)
      fields
  | Boxed_float or_var -> Or_variable.free_names or_var
  | Boxed_int32 or_var -> Or_variable.free_names or_var
  | Boxed_int64 or_var -> Or_variable.free_names or_var
  | Boxed_nativeint or_var -> Or_variable.free_names or_var
  | Mutable_string { initial_value = _; }
  | Immutable_string _ -> Name_occurrences.empty
  | Immutable_float_block fields | Immutable_float_array fields ->
    List.fold_left (fun fns (field : _ Or_variable.t) ->
        match field with
        | Var v ->
          Name_occurrences.add_variable fns v Name_mode.normal
        | Const _ -> fns)
      (Name_occurrences.empty)
      fields

let apply_renaming t renaming =
  if Renaming.is_empty renaming then t
  else
    match t with
    | Code code ->
      let code' = Code.apply_renaming code renaming in
      if code == code' then t
      else Code code'
    | Set_of_closures set ->
      let set' = Set_of_closures.apply_renaming set renaming in
      if set == set' then t
      else Set_of_closures set'
    | Block (tag, mut, fields) ->
      let changed = ref false in
      let fields =
        List.map (fun field ->
            let field' = Field_of_block.apply_renaming field renaming in
            if not (field == field') then begin
              changed := true
            end;
            field')
          fields
      in
      if not !changed then t
      else Block (tag, mut, fields)
    | Boxed_float or_var ->
      let or_var' = Or_variable.apply_renaming or_var renaming in
      if or_var == or_var' then t
      else Boxed_float or_var'
    | Boxed_int32 or_var ->
      let or_var' = Or_variable.apply_renaming or_var renaming in
      if or_var == or_var' then t
      else Boxed_int32 or_var'
    | Boxed_int64 or_var ->
      let or_var' = Or_variable.apply_renaming or_var renaming in
      if or_var == or_var' then t
      else Boxed_int64 or_var'
    | Boxed_nativeint or_var ->
      let or_var' = Or_variable.apply_renaming or_var renaming in
      if or_var == or_var' then t
      else Boxed_nativeint or_var'
    | Mutable_string { initial_value = _; }
    | Immutable_string _ -> t
    | Immutable_float_block fields ->
      let changed = ref false in
      let fields =
        List.map (fun (field : _ Or_variable.t) ->
            let field' : _ Or_variable.t =
              match field with
              | Var v -> Var (Renaming.apply_variable renaming v)
              | Const _ -> field
            in
            if not (field == field') then begin
              changed := true
            end;
            field')
          fields
      in
      if not !changed then t
      else Immutable_float_block fields
    | Immutable_float_array fields ->
      let changed = ref false in
      let fields =
        List.map (fun (field : _ Or_variable.t) ->
            let field' : _ Or_variable.t =
              match field with
              | Var v -> Var (Renaming.apply_variable renaming v)
              | Const _ -> field
            in
            if not (field == field') then begin
              changed := true
            end;
            field')
          fields
      in
      if not !changed then t
      else Immutable_float_array fields

let all_ids_for_export t =
  match t with
  | Code code -> Code.all_ids_for_export code
  | Set_of_closures set -> Set_of_closures.all_ids_for_export set
  | Block (_tag, _mut, fields) ->
    List.fold_left (fun ids field ->
        Ids_for_export.union ids (Field_of_block.all_ids_for_export field))
      Ids_for_export.empty
      fields
  | Boxed_float (Var var)
  | Boxed_int32 (Var var)
  | Boxed_int64 (Var var)
  | Boxed_nativeint (Var var) ->
    Ids_for_export.add_variable Ids_for_export.empty var
  | Boxed_float (Const _)
  | Boxed_int32 (Const _)
  | Boxed_int64 (Const _)
  | Boxed_nativeint (Const _)
  | Mutable_string { initial_value = _; }
  | Immutable_string _ -> Ids_for_export.empty
  | Immutable_float_block fields ->
    List.fold_left (fun ids (field : _ Or_variable.t) ->
        match field with
        | Var v ->
          Ids_for_export.add_variable ids v
        | Const _ -> ids)
      Ids_for_export.empty
      fields
  | Immutable_float_array fields ->
    List.fold_left (fun ids (field : _ Or_variable.t) ->
        match field with
        | Var v ->
          Ids_for_export.add_variable ids v
        | Const _ -> ids)
      Ids_for_export.empty
      fields

let is_fully_static t =
  free_names t
  |> Name_occurrences.no_variables

let can_share0 t =
  match t with
  | Block (_, Immutable, _)
  | Code _
    (* Code will never actually be shared since the [compare] function
       looks at the code ID. *)
  | Set_of_closures _
  | Boxed_float _
  | Boxed_int32 _
  | Boxed_int64 _
  | Boxed_nativeint _
  | Immutable_float_block _
  | Immutable_float_array _
  | Immutable_string _ -> true
  | Block (_, (Mutable | Immutable_unique), _)
  | Mutable_string _ -> false

let can_share t =
  can_share0 t && is_fully_static t

let to_code t =
  match t with
  | Code code -> Some code
  | Set_of_closures _
  | Block _
  | Boxed_float _
  | Boxed_int32 _
  | Boxed_int64 _
  | Boxed_nativeint _
  | Immutable_float_block _
  | Immutable_float_array _
  | Immutable_string _
  | Mutable_string _ -> None

let must_be_set_of_closures t =
  match t with
  | Set_of_closures set -> set
  | Block _
  | Code _
  | Boxed_float _
  | Boxed_int32 _
  | Boxed_int64 _
  | Boxed_nativeint _
  | Immutable_float_block _
  | Immutable_float_array _
  | Immutable_string _
  | Mutable_string _ -> Misc.fatal_errorf "Not a set of closures:@ %a" print t

let match_against_bound_symbols_pattern t (pat : Bound_symbols.Pattern.t)
      ~code:code_callback ~set_of_closures:set_of_closures_callback
      ~block_like:block_like_callback =
  match t, pat with
  | Code code, Code code_id ->
    if not (Code_id.equal (Code.code_id code) code_id) then begin
      Misc.fatal_errorf "Mismatch on declared code IDs:@ %a@ =@ %a"
        Bound_symbols.Pattern.print pat
        print t
    end;
    code_callback code_id code
  | Set_of_closures set_of_closures, Set_of_closures closure_symbols ->
    let closure_ids =
      Set_of_closures.function_decls set_of_closures
      |> Function_declarations.funs_in_order
      |> Closure_id.Lmap.keys
    in
    let closure_ids' = Closure_id.Lmap.keys closure_symbols in
    let closure_ids_match =
      (* Note that we check the order here. *)
      Misc.Stdlib.List.compare Closure_id.compare closure_ids closure_ids' = 0
    in
    if not closure_ids_match then begin
      Misc.fatal_errorf "Mismatch on declared closure IDs:@ %a@ =@ %a"
        Bound_symbols.Pattern.print pat
        print t
    end;
    set_of_closures_callback ~closure_symbols set_of_closures
  | (Block _ | Boxed_float _ | Boxed_int32 _ | Boxed_int64 _
      | Boxed_nativeint _ | Immutable_float_block _ | Immutable_float_array _
      | Immutable_string _ | Mutable_string _), Block_like symbol ->
    block_like_callback symbol t
  | Code _, (Set_of_closures _ | Block_like _)
  | Set_of_closures _, (Code _ | Block_like _)
  | (Block _ | Boxed_float _ | Boxed_int32 _ | Boxed_int64 _
      | Boxed_nativeint _ | Immutable_float_block _ | Immutable_float_array _
      | Immutable_string _ | Mutable_string _), (Code _ | Set_of_closures _) ->
    Misc.fatal_errorf "Mismatch on variety of [Static_const]:@ %a@ =@ %a"
      Bound_symbols.Pattern.print pat
      print t

module Group = struct
  type nonrec t = t list

  let create static_consts = static_consts
  let to_list t = t

  let empty = []

  let [@ocamlformat "disable"] print_with_cache ~cache ppf t =
    Format.fprintf ppf "@[<hov 1>(%a)@]"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space
        (print_with_cache ~cache))
      t

  let [@ocamlformat "disable"] print ppf t =
    Format.fprintf ppf "@[<hov 1>(%a)@]"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space print) t

  let free_names t =
    List.map free_names t
    |> Name_occurrences.union_list

  let apply_renaming t renaming =
    List.map (fun static_const -> apply_renaming static_const renaming) t

  let all_ids_for_export t =
    List.map all_ids_for_export t
    |> Ids_for_export.union_list

  let match_against_bound_symbols t bound_symbols ~init ~code:code_callback
        ~set_of_closures:set_of_closures_callback
        ~block_like:block_like_callback =
    let bound_symbol_pats = Bound_symbols.to_list bound_symbols in
    if List.compare_lengths t bound_symbol_pats <> 0 then begin
      Misc.fatal_errorf "Mismatch between length of [Bound_symbols.t] and \
          [Static_const.t list]:@ %a@ =@ %a"
        Bound_symbols.print bound_symbols
        print t
    end;
    ListLabels.fold_left2 t bound_symbol_pats ~init
      ~f:(fun acc static_const bound_symbols_pat ->
        match_against_bound_symbols_pattern static_const bound_symbols_pat
          ~code:(fun code_id code -> code_callback acc code_id code)
          ~set_of_closures:(fun ~closure_symbols set_of_closures ->
            set_of_closures_callback acc ~closure_symbols set_of_closures)
          ~block_like:(fun symbol static_const ->
            block_like_callback acc symbol static_const))

  let pieces_of_code t =
    List.filter_map to_code t
    |> List.filter_map (fun code ->
      if Code.is_deleted code then None
      else Some (Code.code_id code, code))
    |> Code_id.Map.of_list

  let pieces_of_code' t =
    pieces_of_code t
    |> Code_id.Map.data

  let is_fully_static t = List.for_all is_fully_static t

  let concat t1 t2 = t1 @ t2

  let map t ~f = List.map f t
end
