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

module K = Flambda_kind
module Float = Numeric_types.Float_by_bit_pattern
module Int32 = Numeric_types.Int32
module Int64 = Numeric_types.Int64
module RWC = Reg_width_const
module TD = Type_descr
open Or_bottom.Let_syntax
open Or_unknown.Let_syntax

module Block_size = struct
  include Targetint_31_63.Imm

  (** [subset t1 t2] is true if [t1] is a subset of [t2] *)
  let subset t1 t2 = Stdlib.( <= ) (compare t1 t2) 0

  (* An integer [i] represents all the values smaller than i, hence a smaller
     number is included in a bigger *)
  let union t1 t2 = Targetint_31_63.Imm.max t1 t2

  let inter t1 t2 = Targetint_31_63.Imm.min t1 t2
end

(* The grammar of Flambda types. *)
type t =
  | Value of head_of_kind_value TD.t
  | Naked_immediate of head_of_kind_naked_immediate TD.t
  | Naked_float of head_of_kind_naked_float TD.t
  | Naked_int32 of head_of_kind_naked_int32 TD.t
  | Naked_int64 of head_of_kind_naked_int64 TD.t
  | Naked_nativeint of head_of_kind_naked_nativeint TD.t
  | Rec_info of head_of_kind_rec_info TD.t

and head_of_kind_value =
  | Variant of
      { immediates : t Or_unknown.t;
        blocks : row_like_for_blocks Or_unknown.t;
        is_unique : bool
      }
  | Boxed_float of t
  | Boxed_int32 of t
  | Boxed_int64 of t
  | Boxed_nativeint of t
  | Closures of { by_closure_id : row_like_for_closures }
  | String of String_info.Set.t
  | Array of { length : t }

and head_of_kind_naked_immediate =
  | Naked_immediates of Targetint_31_63.Set.t
  | Is_int of t
  | Get_tag of t

and head_of_kind_naked_float = Float.Set.t

and head_of_kind_naked_int32 = Int32.Set.t

and head_of_kind_naked_int64 = Int64.Set.t

and head_of_kind_naked_nativeint = Targetint_32_64.Set.t

and head_of_kind_rec_info = Rec_info_expr.t

(* For row-like, ['index] must not contain any names. *)

(* Note: it wouldn't require many changes to change this to an interval:
 * type 'index row_like_index = { at_least : 'index; at_most : 'index }
 * representing { x | at_least \subset x /\ x \subset at_most }
 *)
and 'index row_like_index =
  | Known of 'index  (** [Known x] represents the singleton set: { x } *)
  | At_least of 'index
      (** [At_least x] represents the set { y | x \subset y } *)

and ('index, 'maps_to) row_like_case =
  { maps_to : 'maps_to;
        (** Kinds different from [Value] are only allowed in cases with known
            tags. Currently cases with tag 254 must have fields of kind
            [Naked_float] and all other must have fields of kind [Value]. *)
    index : 'index row_like_index;
    env_extension : env_extension
  }

and row_like_for_blocks =
  { known_tags : (Block_size.t, int_indexed_product) row_like_case Tag.Map.t;
    other_tags : (Block_size.t, int_indexed_product) row_like_case Or_bottom.t
  }

and row_like_for_closures =
  { known_closures :
      (Set_of_closures_contents.t, closures_entry) row_like_case
      Closure_id.Map.t;
    other_closures :
      (Set_of_closures_contents.t, closures_entry) row_like_case Or_bottom.t
  }

and closures_entry =
  { function_types : function_type Or_unknown_or_bottom.t Closure_id.Map.t;
    closure_types : closure_id_indexed_product;
    closure_var_types : var_within_closure_indexed_product
  }

(* Products are a set of constraints: each new field reduces the concrete set.
   The empty product is top. There is no bottom. All components must be of the
   same kind.

   { 1 => Unknown; 2 => V } is equal to { 2 => V } *)
and closure_id_indexed_product =
  { closure_id_components_by_index : t Closure_id.Map.t }

and var_within_closure_indexed_product =
  { var_within_closure_components_by_index : t Var_within_closure.Map.t }

and int_indexed_product =
  { fields : t array;
    kind : Flambda_kind.t
  }

and function_type =
  { code_id : Code_id.t;
    rec_info : t
  }

and env_extension = { equations : t Name.Map.t } [@@unboxed]

type flambda_type = t

let row_like_is_bottom ~known ~(other : _ Or_bottom.t) ~is_empty_map_known =
  is_empty_map_known known && match other with Bottom -> true | Ok _ -> false

let apply_renaming t renaming =
  if Renaming.is_empty renaming
  then t
  else
    match t with
    | Value ty ->
      let ty' = TD.apply_renaming ty renaming in
      if ty == ty' then t else Value ty'
    | Naked_immediate ty ->
      let ty' = TD.apply_renaming ty renaming in
      if ty == ty' then t else Naked_immediate ty'
    | Naked_float ty ->
      let ty' = TD.apply_renaming ty renaming in
      if ty == ty' then t else Naked_float ty'
    | Naked_int32 ty ->
      let ty' = TD.apply_renaming ty renaming in
      if ty == ty' then t else Naked_int32 ty'
    | Naked_int64 ty ->
      let ty' = TD.apply_renaming ty renaming in
      if ty == ty' then t else Naked_int64 ty'
    | Naked_nativeint ty ->
      let ty' = TD.apply_renaming ty renaming in
      if ty == ty' then t else Naked_nativeint ty'
    | Rec_info ty ->
      let ty' = TD.apply_renaming ty renaming in
      if ty == ty' then t else Rec_info ty'

let rec apply_renaming_head_of_kind_value head renaming =
  match head with
  | Variant { blocks; immediates; is_unique } ->
    let immediates' =
      let>+ immediates = immediates in
      apply_renaming immediates renaming
    in
    let blocks' =
      let>+ blocks = blocks in
      apply_renaming_row_like_for_blocks blocks renaming
    in
    if immediates == immediates' && blocks == blocks'
    then head
    else Variant { is_unique; blocks = blocks'; immediates = immediates' }
  | Boxed_float ty ->
    let ty' = apply_renaming ty renaming in
    if ty == ty' then head else Boxed_float ty'
  | Boxed_int32 ty ->
    let ty' = apply_renaming ty renaming in
    if ty == ty' then head else Boxed_int32 ty'
  | Boxed_int64 ty ->
    let ty' = apply_renaming ty renaming in
    if ty == ty' then head else Boxed_int64 ty'
  | Boxed_nativeint ty ->
    let ty' = apply_renaming ty renaming in
    if ty == ty' then head else Boxed_nativeint ty'
  | Closures { by_closure_id } ->
    let by_closure_id' =
      apply_renaming_row_like_for_closures by_closure_id renaming
    in
    if by_closure_id == by_closure_id'
    then head
    else Closures { by_closure_id = by_closure_id' }
  | String _ -> head
  | Array { length } ->
    let length' = apply_renaming length renaming in
    if length == length' then head else Array { length = length' }

and apply_renaming_head_of_kind_naked_immediate head renaming =
  match head with
  | Naked_immediates _ -> head
  | Is_int ty ->
    let ty' = apply_renaming ty renaming in
    if ty == ty' then head else Is_int ty'
  | Get_tag ty ->
    let ty' = apply_renaming ty renaming in
    if ty == ty' then head else Get_tag ty'

and apply_renaming_head_of_kind_naked_float head _ = head

and apply_renaming_head_of_kind_naked_int32 head _ = head

and apply_renaming_head_of_kind_naked_int64 head _ = head

and apply_renaming_head_of_kind_naked_nativeint head _ = head

and apply_renaming_head_of_kind_rec_info head renaming =
  Rec_info_expr.apply_renaming head renaming

and apply_renaming_row_like :
      'index 'maps_to 'known.
      apply_renaming_index:('index -> Renaming.t -> 'index) ->
      apply_renaming_maps_to:('maps_to -> Renaming.t -> 'maps_to) ->
      known:'known ->
      other:('index, 'maps_to) row_like_case Or_bottom.t ->
      map_known:
        ((('index, 'maps_to) row_like_case -> ('index, 'maps_to) row_like_case) ->
        'known ->
        'known) ->
      Renaming.t ->
      ('known * ('index, 'maps_to) row_like_case Or_bottom.t) option =
 fun ~apply_renaming_index ~apply_renaming_maps_to ~known ~other ~map_known
     renaming ->
  let rename_index = function
    | Known index -> Known (apply_renaming_index index renaming)
    | At_least index -> At_least (apply_renaming_index index renaming)
  in
  let known' =
    map_known
      (fun { index; maps_to; env_extension } ->
        { index = rename_index index;
          env_extension = apply_renaming_env_extension env_extension renaming;
          maps_to = apply_renaming_maps_to maps_to renaming
        })
      known
  in
  let other' : _ Or_bottom.t =
    match other with
    | Bottom -> Bottom
    | Ok { index; maps_to; env_extension } ->
      Ok
        { index = rename_index index;
          env_extension = apply_renaming_env_extension env_extension renaming;
          maps_to = apply_renaming_maps_to maps_to renaming
        }
  in
  if known == known' && other == other' then None else Some (known', other')

and apply_renaming_row_like_for_blocks
    ({ known_tags; other_tags } as row_like_for_tags) renaming =
  match
    apply_renaming_row_like
      ~apply_renaming_index:(fun block_size _ -> block_size)
      ~apply_renaming_maps_to:apply_renaming_int_indexed_product
      ~known:known_tags ~other:other_tags ~map_known:Tag.Map.map_sharing
      renaming
  with
  | None -> row_like_for_tags
  | Some (known_tags, other_tags) -> { known_tags; other_tags }

and apply_renaming_row_like_for_closures
    ({ known_closures; other_closures } as row_like_for_closures) renaming =
  match
    apply_renaming_row_like
      ~apply_renaming_index:Set_of_closures_contents.apply_renaming
      ~apply_renaming_maps_to:apply_renaming_closures_entry
      ~known:known_closures ~other:other_closures
      ~map_known:Closure_id.Map.map_sharing renaming
  with
  | None -> row_like_for_closures
  | Some (known_closures, other_closures) -> { known_closures; other_closures }

and apply_renaming_closures_entry
    { function_types; closure_types; closure_var_types } renaming =
  { function_types =
      Closure_id.Map.map_sharing
        (fun function_type ->
          Or_unknown_or_bottom.map function_type ~f:(fun function_type ->
              apply_renaming_function_type function_type renaming))
        function_types;
    closure_types =
      apply_renaming_closure_id_indexed_product closure_types renaming;
    closure_var_types =
      apply_renaming_var_within_closure_indexed_product closure_var_types
        renaming
  }

and apply_renaming_closure_id_indexed_product { closure_id_components_by_index }
    renaming =
  let closure_id_components_by_index =
    Closure_id.Map.map_sharing
      (fun ty -> apply_renaming ty renaming)
      closure_id_components_by_index
  in
  { closure_id_components_by_index }

and apply_renaming_var_within_closure_indexed_product
    { var_within_closure_components_by_index } renaming =
  let var_within_closure_components_by_index =
    (* CR-someday mshinwell: some loss of sharing here, potentially *)
    Var_within_closure.Map.filter_map
      (fun closure_var ty ->
        if not (Renaming.closure_var_is_used renaming closure_var)
        then None
        else Some (apply_renaming ty renaming))
      var_within_closure_components_by_index
  in
  { var_within_closure_components_by_index }

and apply_renaming_int_indexed_product { fields; kind } renaming =
  let fields = Array.copy fields in
  for i = 0 to Array.length fields - 1 do
    fields.(i) <- apply_renaming fields.(i) renaming
  done;
  { fields; kind }

and apply_renaming_function_type ({ code_id; rec_info } as function_type)
    renaming =
  let code_id' = Renaming.apply_code_id renaming code_id in
  let rec_info' = apply_renaming rec_info renaming in
  if code_id == code_id' && rec_info == rec_info'
  then function_type
  else { code_id = code_id'; rec_info = rec_info' }

and apply_renaming_env_extension ({ equations } as env_extension) renaming =
  let changed = ref false in
  let equations' =
    Name.Map.fold
      (fun name ty acc ->
        let ty' = apply_renaming ty renaming in
        let name' = Renaming.apply_name renaming name in
        if not (ty == ty' && name == name') then changed := true;
        Name.Map.add name' ty' acc)
      equations Name.Map.empty
  in
  if !changed then { equations = equations' } else env_extension

let rec free_names t =
  match t with
  | Value ty ->
    TD.free_names ~apply_renaming_head:apply_renaming_head_of_kind_value
      ~free_names_head:free_names_head_of_kind_value ty
  | Naked_immediate ty ->
    TD.free_names
      ~apply_renaming_head:apply_renaming_head_of_kind_naked_immediate
      ~free_names_head:free_names_head_of_kind_naked_immediate ty
  | Naked_float ty ->
    TD.free_names ~apply_renaming_head:apply_renaming_head_of_kind_naked_float
      ~free_names_head:free_names_head_of_kind_naked_float ty
  | Naked_int32 ty ->
    TD.free_names ~apply_renaming_head:apply_renaming_head_of_kind_naked_int32
      ~free_names_head:free_names_head_of_kind_naked_int32 ty
  | Naked_int64 ty ->
    TD.free_names ~apply_renaming_head:apply_renaming_head_of_kind_naked_int64
      ~free_names_head:free_names_head_of_kind_naked_int64 ty
  | Naked_nativeint ty ->
    TD.free_names
      ~apply_renaming_head:apply_renaming_head_of_kind_naked_nativeint
      ~free_names_head:free_names_head_of_kind_naked_nativeint ty
  | Rec_info ty ->
    TD.free_names ~apply_renaming_head:Rec_info_expr.apply_renaming
      ~free_names_head:free_names_head_of_kind_rec_info ty

and free_names_head_of_kind_value head =
  match head with
  | Variant { blocks; immediates; is_unique = _ } ->
    Name_occurrences.union
      (Or_unknown.free_names free_names_row_like_for_blocks blocks)
      (Or_unknown.free_names free_names immediates)
  | Boxed_float ty -> free_names ty
  | Boxed_int32 ty -> free_names ty
  | Boxed_int64 ty -> free_names ty
  | Boxed_nativeint ty -> free_names ty
  | Closures { by_closure_id } -> free_names_row_like_for_closures by_closure_id
  | String _ -> Name_occurrences.empty
  | Array { length } -> free_names length

and free_names_head_of_kind_naked_immediate head =
  match head with
  | Naked_immediates _ -> Name_occurrences.empty
  | Is_int ty | Get_tag ty -> free_names ty

and free_names_head_of_kind_naked_float _ = Name_occurrences.empty

and free_names_head_of_kind_naked_int32 _ = Name_occurrences.empty

and free_names_head_of_kind_naked_int64 _ = Name_occurrences.empty

and free_names_head_of_kind_naked_nativeint _ = Name_occurrences.empty

and free_names_head_of_kind_rec_info head =
  Rec_info_expr.free_names_in_types head

and free_names_row_like :
      'row_tag 'index 'maps_to 'known.
      free_names_maps_to:('maps_to -> Name_occurrences.t) ->
      known:'known ->
      other:('index, 'maps_to) row_like_case Or_bottom.t ->
      fold_known:
        (('row_tag -> ('index, 'maps_to) row_like_case -> 'acc -> 'acc) ->
        'known ->
        'acc ->
        'acc) ->
      Name_occurrences.t =
 fun ~free_names_maps_to ~known ~other ~fold_known ->
  let from_known =
    fold_known
      (fun _ { maps_to; env_extension; index = _ } free_names ->
        Name_occurrences.union free_names
          (Name_occurrences.union
             (free_names_env_extension env_extension)
             (free_names_maps_to maps_to)))
      known Name_occurrences.empty
  in
  match other with
  | Bottom -> from_known
  | Ok { maps_to; env_extension; index = _ } ->
    Name_occurrences.union
      (free_names_maps_to maps_to)
      (Name_occurrences.union from_known
         (free_names_env_extension env_extension))

and free_names_row_like_for_blocks { known_tags; other_tags } =
  free_names_row_like ~free_names_maps_to:free_names_int_indexed_product
    ~known:known_tags ~other:other_tags ~fold_known:Tag.Map.fold

and free_names_row_like_for_closures { known_closures; other_closures } =
  free_names_row_like ~free_names_maps_to:free_names_closures_entry
    ~known:known_closures ~other:other_closures ~fold_known:Closure_id.Map.fold

and free_names_closures_entry
    { function_types; closure_types; closure_var_types } =
  let function_types_free_names =
    Closure_id.Map.fold
      (fun _closure_id function_decl free_names ->
        Name_occurrences.union free_names
          (free_names_function_type function_decl))
      function_types Name_occurrences.empty
  in
  Name_occurrences.union function_types_free_names
    (Name_occurrences.union
       (free_names_closure_id_indexed_product closure_types)
       (free_names_var_within_closure_indexed_product closure_var_types))

and free_names_closure_id_indexed_product { closure_id_components_by_index } =
  Closure_id.Map.fold
    (fun _ t free_names_acc ->
      Name_occurrences.union (free_names t) free_names_acc)
    closure_id_components_by_index Name_occurrences.empty

and free_names_var_within_closure_indexed_product
    { var_within_closure_components_by_index } =
  Var_within_closure.Map.fold
    (fun _ t free_names_acc ->
      Name_occurrences.union (free_names t) free_names_acc)
    var_within_closure_components_by_index Name_occurrences.empty

and free_names_int_indexed_product { fields; kind = _ } =
  Array.fold_left
    (fun free_names_acc t ->
      Name_occurrences.union (free_names t) free_names_acc)
    Name_occurrences.empty fields

and free_names_function_type (function_type : _ Or_unknown_or_bottom.t) =
  match function_type with
  | Bottom | Unknown -> Name_occurrences.empty
  | Ok { code_id; rec_info } ->
    Name_occurrences.add_code_id (free_names rec_info) code_id Name_mode.in_types

and free_names_env_extension { equations } =
  Name.Map.fold
    (fun name t acc ->
      let acc = Name_occurrences.union acc (free_names t) in
      Name_occurrences.add_name acc name Name_mode.in_types)
    equations Name_occurrences.empty

let rec print ppf t =
  let no_renaming thing _ = thing in
  let no_free_names _ = Name_occurrences.empty in
  match t with
  | Value ty ->
    Format.fprintf ppf "@[<hov 1>(Val@ %a)@]"
      (TD.print ~print_head:print_head_of_kind_value
         ~apply_renaming_head:apply_renaming_head_of_kind_value
         ~free_names_head:free_names_head_of_kind_value)
      ty
  | Naked_immediate ty ->
    Format.fprintf ppf "@[<hov 1>(Naked_immediate@ %a)@]"
      (TD.print ~print_head:print_head_of_kind_naked_immediate
         ~apply_renaming_head:no_renaming ~free_names_head:no_free_names)
      ty
  | Naked_float ty ->
    Format.fprintf ppf "@[<hov 1>(Naked_float@ %a)@]"
      (TD.print ~print_head:print_head_of_kind_naked_float
         ~apply_renaming_head:no_renaming ~free_names_head:no_free_names)
      ty
  | Naked_int32 ty ->
    Format.fprintf ppf "@[<hov 1>(Naked_int32@ %a)@]"
      (TD.print ~print_head:print_head_of_kind_naked_int32
         ~apply_renaming_head:no_renaming ~free_names_head:no_free_names)
      ty
  | Naked_int64 ty ->
    Format.fprintf ppf "@[<hov 1>(Naked_int64@ %a)@]"
      (TD.print ~print_head:print_head_of_kind_naked_int64
         ~apply_renaming_head:no_renaming ~free_names_head:no_free_names)
      ty
  | Naked_nativeint ty ->
    Format.fprintf ppf "@[<hov 1>(Naked_nativeint@ %a)@]"
      (TD.print ~print_head:print_head_of_kind_naked_nativeint
         ~apply_renaming_head:no_renaming ~free_names_head:no_free_names)
      ty
  | Rec_info ty ->
    Format.fprintf ppf "@[<hov 1>(Rec_info@ %a)@]"
      (TD.print ~print_head:print_head_of_kind_rec_info
         ~apply_renaming_head:apply_renaming_head_of_kind_rec_info
         ~free_names_head:free_names_head_of_kind_rec_info)
      ty

and print_head_of_kind_value ppf head =
  match head with
  | Variant { blocks; immediates; is_unique } ->
    (* CR mshinwell: Improve so that we elide blocks and/or immediates when
       they're empty. *)
    Format.fprintf ppf
      "@[<hov 1>(Variant%s@ @[<hov 1>(blocks@ %a)@]@ @[<hov 1>(tagged_imms@ \
       %a)@])@]"
      (if is_unique then " unique" else "")
      (Or_unknown.print print_row_like_for_blocks)
      blocks (Or_unknown.print print) immediates
  | Boxed_float ty -> Format.fprintf ppf "@[<hov 1>(Boxed_float@ %a)@]" print ty
  | Boxed_int32 ty -> Format.fprintf ppf "@[<hov 1>(Boxed_int32@ %a)@]" print ty
  | Boxed_int64 ty -> Format.fprintf ppf "@[<hov 1>(Boxed_int64@ %a)@]" print ty
  | Boxed_nativeint ty ->
    Format.fprintf ppf "@[<hov 1>(Boxed_nativeint@ %a)@]" print ty
  | Closures { by_closure_id } -> print_row_like_for_closures ppf by_closure_id
  | String str_infos ->
    Format.fprintf ppf "@[<hov 1>(Strings@ (%a))@]" String_info.Set.print
      str_infos
  | Array { length } ->
    Format.fprintf ppf "@[<hov 1>(Array@ (length@ %a))@]" print length

and print_head_of_kind_naked_immediate ppf head =
  match head with
  | Naked_immediates is ->
    Format.fprintf ppf "@[<hov 1>(%a)@]" Targetint_31_63.Set.print is
  | Is_int ty -> Format.fprintf ppf "@[<hov 1>(Is_int@ %a)@]" print ty
  | Get_tag ty -> Format.fprintf ppf "@[<hov 1>(Get_tag@ %a)@]" print ty

and print_head_of_kind_naked_float ppf head =
  Format.fprintf ppf "@[(Naked_float@ (%a))@]" Float.Set.print head

and print_head_of_kind_naked_int32 ppf head =
  Format.fprintf ppf "@[(Naked_int32@ (%a))@]" Int32.Set.print head

and print_head_of_kind_naked_int64 ppf head =
  Format.fprintf ppf "@[(Naked_int64@ (%a))@]" Int64.Set.print head

and print_head_of_kind_naked_nativeint ppf head =
  Format.fprintf ppf "@[(Naked_nativeint@ (%a))@]" Targetint_32_64.Set.print
    head

and print_head_of_kind_rec_info ppf head = Rec_info_expr.print ppf head

and print_row_like :
      'index 'maps_to 'known.
      print_index:(Format.formatter -> 'index -> unit) ->
      print_maps_to:(Format.formatter -> 'maps_to -> unit) ->
      print_known_map:
        ((Format.formatter -> ('index, 'maps_to) row_like_case -> unit) ->
        Format.formatter ->
        'known ->
        unit) ->
      is_empty_map_known:('known -> bool) ->
      known:'known ->
      other:('index, 'maps_to) row_like_case Or_bottom.t ->
      Format.formatter ->
      unit =
 fun ~print_index ~print_maps_to ~print_known_map ~is_empty_map_known ~known
     ~other ppf ->
  let print_index ppf = function
    | Known index -> Format.fprintf ppf "(Known @[<2>%a@])" print_index index
    | At_least min_index ->
      Format.fprintf ppf "(At_least @[<2>%a@])" print_index min_index
  in
  if row_like_is_bottom ~known ~other ~is_empty_map_known
  then
    (* CR mshinwell: factor out (and elsewhere) *)
    let colour = Flambda_colours.top_or_bottom_type () in
    if Flambda_features.unicode ()
    then
      Format.fprintf ppf "@<0>%s@<1>\u{22a5}@<0>%s" colour
        (Flambda_colours.normal ())
    else Format.fprintf ppf "%s_|_%s" colour (Flambda_colours.normal ())
  else
    let pp_env_extension ppf env_extension =
      if not (Name.Map.is_empty env_extension.equations)
      then Format.fprintf ppf "@ %a" print_env_extension env_extension
    in
    let print ppf { maps_to; index; env_extension } =
      Format.fprintf ppf "=> %a,@ %a%a" print_index index print_maps_to maps_to
        pp_env_extension env_extension
    in
    Format.fprintf ppf
      "@[<hov 1>(@[<hov 1>(known@ %a)@]@ @[<hov 1>(other@ %a)@])@]"
      (print_known_map print) known (Or_bottom.print print) other

and print_row_like_for_blocks ppf { known_tags; other_tags } =
  print_row_like ~print_index:Block_size.print
    ~print_maps_to:print_int_indexed_product ~print_known_map:Tag.Map.print
    ~is_empty_map_known:Tag.Map.is_empty ~known:known_tags ~other:other_tags ppf

and print_row_like_for_closures ppf { known_closures; other_closures } =
  print_row_like ~print_index:Set_of_closures_contents.print
    ~print_maps_to:print_closures_entry ~print_known_map:Closure_id.Map.print
    ~is_empty_map_known:Closure_id.Map.is_empty ~known:known_closures
    ~other:other_closures ppf

and print_closures_entry ppf
    { function_types; closure_types; closure_var_types } =
  Format.fprintf ppf
    "@[<hov 1>(@[<hov 1>(function_types@ %a)@]@ @[<hov 1>(closure_types@ \
     %a)@]@ @[<hov 1>(closure_var_types@ %a)@])@]"
    (Closure_id.Map.print (Or_unknown_or_bottom.print print_function_type))
    function_types print_closure_id_indexed_product closure_types
    print_var_within_closure_indexed_product closure_var_types

and print_closure_id_indexed_product ppf { closure_id_components_by_index } =
  Format.fprintf ppf
    "@[<hov 1>(@[<hov 1>(closure_id_components_by_index@ %a)@])@]"
    (Closure_id.Map.print print)
    closure_id_components_by_index

and print_var_within_closure_indexed_product ppf
    { var_within_closure_components_by_index } =
  Format.fprintf ppf
    "@[<hov 1>(@[<hov 1>(var_within_closure_components_by_index@ %a)@])@]"
    (Var_within_closure.Map.print print)
    var_within_closure_components_by_index

and print_int_indexed_product ppf { fields; kind } =
  Format.fprintf ppf "@[<hov 1>((kind %a)@ %a)@]" K.print kind
    (Format.pp_print_list ~pp_sep:Format.pp_print_space print)
    (Array.to_list fields)

and print_function_type ppf { code_id; rec_info } =
  Format.fprintf ppf
    "@[<hov 1>(function_type@ @[<hov 1>(code_id@ %a)@]@ @[<hov 1>(rec_info@ \
     %a)@])@]"
    Code_id.print code_id print rec_info

and print_env_extension ppf { equations } =
  let print_equations ppf equations =
    let equations = Name.Map.bindings equations in
    match equations with
    | [] -> Format.pp_print_string ppf "()"
    | _ :: _ ->
      Format.pp_print_string ppf "(";
      Format.pp_print_list ~pp_sep:Format.pp_print_space
        (fun ppf (name, t) ->
          Format.fprintf ppf "@[<hov 1>%a@ :@ %a@]" Name.print name print t)
        ppf equations;
      Format.pp_print_string ppf ")"
  in
  Format.fprintf ppf "@[<hov 1>(equations@ @[<v 1>%a@])@]" print_equations
    equations

let rec all_ids_for_export t =
  match t with
  | Value ty ->
    TD.all_ids_for_export ~apply_renaming_head:apply_renaming_head_of_kind_value
      ~free_names_head:free_names_head_of_kind_value
      ~all_ids_for_export_head:all_ids_for_export_head_of_kind_value ty
  | Naked_immediate ty ->
    TD.all_ids_for_export
      ~apply_renaming_head:apply_renaming_head_of_kind_naked_immediate
      ~free_names_head:free_names_head_of_kind_naked_immediate
      ~all_ids_for_export_head:all_ids_for_export_head_of_kind_naked_immediate
      ty
  | Naked_float ty ->
    TD.all_ids_for_export
      ~apply_renaming_head:apply_renaming_head_of_kind_naked_float
      ~free_names_head:free_names_head_of_kind_naked_float
      ~all_ids_for_export_head:all_ids_for_export_head_of_kind_naked_float ty
  | Naked_int32 ty ->
    TD.all_ids_for_export
      ~apply_renaming_head:apply_renaming_head_of_kind_naked_int32
      ~free_names_head:free_names_head_of_kind_naked_int32
      ~all_ids_for_export_head:all_ids_for_export_head_of_kind_naked_int32 ty
  | Naked_int64 ty ->
    TD.all_ids_for_export
      ~apply_renaming_head:apply_renaming_head_of_kind_naked_int64
      ~free_names_head:free_names_head_of_kind_naked_int64
      ~all_ids_for_export_head:all_ids_for_export_head_of_kind_naked_int64 ty
  | Naked_nativeint ty ->
    TD.all_ids_for_export
      ~apply_renaming_head:apply_renaming_head_of_kind_naked_nativeint
      ~free_names_head:free_names_head_of_kind_naked_nativeint
      ~all_ids_for_export_head:all_ids_for_export_head_of_kind_naked_nativeint
      ty
  | Rec_info ty ->
    TD.all_ids_for_export
      ~apply_renaming_head:apply_renaming_head_of_kind_rec_info
      ~free_names_head:free_names_head_of_kind_rec_info
      ~all_ids_for_export_head:all_ids_for_export_head_of_kind_rec_info ty

and all_ids_for_export_head_of_kind_value head =
  match head with
  | Variant { blocks; immediates; is_unique = _ } ->
    Ids_for_export.union
      (Or_unknown.all_ids_for_export all_ids_for_export_row_like_for_blocks
         blocks)
      (Or_unknown.all_ids_for_export all_ids_for_export immediates)
  | Boxed_float t -> all_ids_for_export t
  | Boxed_int32 t -> all_ids_for_export t
  | Boxed_int64 t -> all_ids_for_export t
  | Boxed_nativeint t -> all_ids_for_export t
  | Closures { by_closure_id } ->
    all_ids_for_export_row_like_for_closures by_closure_id
  | String _ -> Ids_for_export.empty
  | Array { length } -> all_ids_for_export length

and all_ids_for_export_head_of_kind_naked_immediate head =
  match head with
  | Naked_immediates _ -> Ids_for_export.empty
  | Is_int t | Get_tag t -> all_ids_for_export t

and all_ids_for_export_head_of_kind_naked_float _ = Ids_for_export.empty

and all_ids_for_export_head_of_kind_naked_int32 _ = Ids_for_export.empty

and all_ids_for_export_head_of_kind_naked_int64 _ = Ids_for_export.empty

and all_ids_for_export_head_of_kind_naked_nativeint _ = Ids_for_export.empty

and all_ids_for_export_head_of_kind_rec_info head =
  Rec_info_expr.all_ids_for_export head

and all_ids_for_export_row_like :
      'row_tag 'index 'maps_to 'known.
      all_ids_for_export_maps_to:('maps_to -> Ids_for_export.t) ->
      known:'known ->
      other:('index, 'maps_to) row_like_case Or_bottom.t ->
      fold_known:
        (('row_tag -> ('index, 'maps_to) row_like_case -> 'acc -> 'acc) ->
        'known ->
        'acc ->
        'acc) ->
      Ids_for_export.t =
 fun ~all_ids_for_export_maps_to ~known ~other ~fold_known ->
  let from_known =
    fold_known
      (fun _tag { maps_to; env_extension; index = _ } ids ->
        Ids_for_export.union ids
          (Ids_for_export.union
             (all_ids_for_export_maps_to maps_to)
             (all_ids_for_export_env_extension env_extension)))
      known Ids_for_export.empty
  in
  match other with
  | Bottom -> from_known
  | Ok { maps_to; env_extension; index = _ } ->
    Ids_for_export.union
      (all_ids_for_export_maps_to maps_to)
      (Ids_for_export.union from_known
         (all_ids_for_export_env_extension env_extension))

and all_ids_for_export_row_like_for_blocks { known_tags; other_tags } =
  all_ids_for_export_row_like
    ~all_ids_for_export_maps_to:all_ids_for_export_int_indexed_product
    ~known:known_tags ~other:other_tags ~fold_known:Tag.Map.fold

and all_ids_for_export_row_like_for_closures { known_closures; other_closures }
    =
  all_ids_for_export_row_like
    ~all_ids_for_export_maps_to:all_ids_for_export_closures_entry
    ~known:known_closures ~other:other_closures ~fold_known:Closure_id.Map.fold

and all_ids_for_export_closures_entry
    { function_types; closure_types; closure_var_types } =
  let function_types_ids =
    Closure_id.Map.fold
      (fun _closure_id (function_type : _ Or_unknown_or_bottom.t) ids ->
        match function_type with
        | Unknown | Bottom -> ids
        | Ok function_type ->
          Ids_for_export.union ids
            (all_ids_for_export_function_type function_type))
      function_types Ids_for_export.empty
  in
  Ids_for_export.union function_types_ids
    (Ids_for_export.union
       (all_ids_for_export_closure_id_indexed_product closure_types)
       (all_ids_for_export_var_within_closure_indexed_product closure_var_types))

and all_ids_for_export_closure_id_indexed_product
    { closure_id_components_by_index } =
  Closure_id.Map.fold
    (fun _ t ids -> Ids_for_export.union ids (all_ids_for_export t))
    closure_id_components_by_index Ids_for_export.empty

and all_ids_for_export_var_within_closure_indexed_product
    { var_within_closure_components_by_index } =
  Var_within_closure.Map.fold
    (fun _ t ids -> Ids_for_export.union ids (all_ids_for_export t))
    var_within_closure_components_by_index Ids_for_export.empty

and all_ids_for_export_int_indexed_product { fields; kind = _ } =
  Array.fold_left
    (fun ids field -> Ids_for_export.union ids (all_ids_for_export field))
    Ids_for_export.empty fields

and all_ids_for_export_function_type { code_id; rec_info } =
  Ids_for_export.union
    (Ids_for_export.singleton_code_id code_id)
    (all_ids_for_export rec_info)

and all_ids_for_export_env_extension { equations } =
  Name.Map.fold
    (fun name t ids ->
      Ids_for_export.add_name
        (Ids_for_export.union ids (all_ids_for_export t))
        name)
    equations Ids_for_export.empty

(* We need to be very careful here. A non-trivial coercion expects to be dealing
   with some very specific type. As of this writing, the only non-trivial
   coercions are depth changes, so they operate on closures. Thus if we see
   something like [(t1, t2) @ depth 0 -> 3], where [@] is the coerce operator
   and [depth 0 -> 3] is just some non-trivial coercion, _this is a type error_
   and we should return bottom.

   If we ever need to apply coercions inside (for instance) tuples, this isn't
   hard so long as we introduce tuples _of coercions_. Then [(t1, t2) @ (co1,
   co2)] is just [(t1 @ co1, t2 @ co2)]. Any bit of type syntax should be
   liftable: a coercion on blocks has a coercion for each field, a coercion on
   variants has a coercion for each branch, etc. *)
(* CR-soon lmaurer Return just [t] from here once we're confident that returning
   bottom is a genuine error condition. (Currently, there's a wrapper below that
   checks for bottom and throws a fatal error.) *)
let rec apply_coercion t coercion : t Or_bottom.t =
  if Coercion.is_id coercion
  then Ok t
  else
    match t with
    | Value ty ->
      let<+ ty' =
        TD.apply_coercion ~apply_coercion_head:apply_coercion_head_of_kind_value
          ~apply_renaming_head:apply_renaming_head_of_kind_value
          ~free_names_head:free_names_head_of_kind_value coercion ty
      in
      if ty == ty' then t else Value ty'
    | Naked_immediate ty ->
      let<+ ty' =
        TD.apply_coercion
          ~apply_coercion_head:apply_coercion_head_of_kind_naked_immediate
          ~apply_renaming_head:apply_renaming_head_of_kind_naked_immediate
          ~free_names_head:free_names_head_of_kind_naked_immediate coercion ty
      in
      if ty == ty' then t else Naked_immediate ty'
    | Naked_float ty ->
      let<+ ty' =
        TD.apply_coercion
          ~apply_coercion_head:apply_coercion_head_of_kind_naked_float
          ~apply_renaming_head:apply_renaming_head_of_kind_naked_float
          ~free_names_head:free_names_head_of_kind_naked_float coercion ty
      in
      if ty == ty' then t else Naked_float ty'
    | Naked_int32 ty ->
      let<+ ty' =
        TD.apply_coercion
          ~apply_coercion_head:apply_coercion_head_of_kind_naked_int32
          ~apply_renaming_head:apply_renaming_head_of_kind_naked_int32
          ~free_names_head:free_names_head_of_kind_naked_int32 coercion ty
      in
      if ty == ty' then t else Naked_int32 ty'
    | Naked_int64 ty ->
      let<+ ty' =
        TD.apply_coercion
          ~apply_coercion_head:apply_coercion_head_of_kind_naked_int64
          ~apply_renaming_head:apply_renaming_head_of_kind_naked_int64
          ~free_names_head:free_names_head_of_kind_naked_int64 coercion ty
      in
      if ty == ty' then t else Naked_int64 ty'
    | Naked_nativeint ty ->
      let<+ ty' =
        TD.apply_coercion
          ~apply_coercion_head:apply_coercion_head_of_kind_naked_nativeint
          ~apply_renaming_head:apply_renaming_head_of_kind_naked_nativeint
          ~free_names_head:free_names_head_of_kind_naked_nativeint coercion ty
      in
      if ty == ty' then t else Naked_nativeint ty'
    | Rec_info ty ->
      let<+ ty' =
        TD.apply_coercion
          ~apply_coercion_head:apply_coercion_head_of_kind_rec_info
          ~apply_renaming_head:apply_renaming_head_of_kind_rec_info
          ~free_names_head:free_names_head_of_kind_rec_info coercion ty
      in
      if ty == ty' then t else Rec_info ty'

and apply_coercion_head_of_kind_value head coercion : _ Or_bottom.t =
  match head with
  | Closures { by_closure_id } ->
    let<+ by_closure_id' =
      apply_coercion_row_like_for_closures by_closure_id coercion
    in
    if by_closure_id == by_closure_id'
    then head
    else Closures { by_closure_id = by_closure_id' }
  | Variant _ ->
    (* See the comment on [apply_coercion]. The situation for variants (sums) is
       similar to that for tuples (products): we would want a coercion for each
       branch. *)
    if Coercion.is_id coercion then Ok head else Bottom
  | Boxed_float _ ->
    (* Even if we had coercions that would act on float constants, we would want
       to have a [Boxed_float] wrapper that would lift a float coercion to a
       value coercion. *)
    if Coercion.is_id coercion then Ok head else Bottom
  | Boxed_int32 _ | Boxed_int64 _ | Boxed_nativeint _ | String _ ->
    (* Similarly, we don't have lifted coercions for these. *)
    if Coercion.is_id coercion then Ok head else Bottom
  | Array { length = _ } ->
    (* This one's a bit more obvious: we wouldn't want to accidentally treat a
       coercion on integers as a coercion on array lengths. *)
    if Coercion.is_id coercion then Ok head else Bottom

and apply_coercion_head_of_kind_naked_immediate head coercion : _ Or_bottom.t =
  if Coercion.is_id coercion then Ok head else Bottom

and apply_coercion_head_of_kind_naked_float head coercion : _ Or_bottom.t =
  if Coercion.is_id coercion then Ok head else Bottom

and apply_coercion_head_of_kind_naked_int32 head coercion : _ Or_bottom.t =
  if Coercion.is_id coercion then Ok head else Bottom

and apply_coercion_head_of_kind_naked_int64 head coercion : _ Or_bottom.t =
  if Coercion.is_id coercion then Ok head else Bottom

and apply_coercion_head_of_kind_naked_nativeint head coercion : _ Or_bottom.t =
  if Coercion.is_id coercion then Ok head else Bottom

and apply_coercion_head_of_kind_rec_info head coercion : _ Or_bottom.t =
  (* Currently no coercion has an effect on a depth variable and
     [Rec_info_expr.t] does not contain any other variety of name. *)
  if Coercion.is_id coercion then Ok head else Bottom

and apply_coercion_row_like :
      'index 'maps_to 'row_tag 'known.
      apply_coercion_maps_to:
        ('row_tag option -> 'maps_to -> Coercion.t -> 'maps_to Or_bottom.t) ->
      known:'known ->
      other:('index, 'maps_to) row_like_case Or_bottom.t ->
      is_empty_map_known:('known -> bool) ->
      filter_map_known:
        (('row_tag ->
         ('index, 'maps_to) row_like_case ->
         ('index, 'maps_to) row_like_case option) ->
        'known ->
        'known) ->
      Coercion.t ->
      ('known * ('index, 'maps_to) row_like_case Or_bottom.t) Or_bottom.t =
 fun ~apply_coercion_maps_to ~known ~other ~is_empty_map_known ~filter_map_known
     coercion ->
  let known =
    filter_map_known
      (fun row_tag { maps_to; index; env_extension } ->
        match apply_coercion_maps_to (Some row_tag) maps_to coercion with
        | Bottom -> None
        | Ok maps_to -> (
          match apply_coercion_env_extension env_extension coercion with
          | Bottom -> None
          | Ok env_extension -> Some { maps_to; index; env_extension }))
      known
  in
  let other : _ Or_bottom.t =
    match other with
    | Bottom -> Bottom
    | Ok { maps_to; index; env_extension } ->
      let<* maps_to = apply_coercion_maps_to None maps_to coercion in
      let<+ env_extension =
        apply_coercion_env_extension env_extension coercion
      in
      { maps_to; index; env_extension }
  in
  if row_like_is_bottom ~known ~other ~is_empty_map_known
  then Bottom
  else Ok (known, other)

and apply_coercion_row_like_for_closures { known_closures; other_closures }
    coercion : row_like_for_closures Or_bottom.t =
  let<+ known, other =
    apply_coercion_row_like
      ~apply_coercion_maps_to:apply_coercion_closures_entry
      ~known:known_closures ~other:other_closures
      ~is_empty_map_known:Closure_id.Map.is_empty
      ~filter_map_known:Closure_id.Map.filter_map coercion
  in
  { known_closures = known; other_closures = other }

and apply_coercion_closures_entry row_tag
    { function_types; closure_types; closure_var_types } coercion :
    _ Or_bottom.t =
  let bottom = ref false in
  let function_coercion =
    (* We're being naughty here. Properly, a coercion acting on a row-like type
       should itself be row-like, with one sub-coercion per row (generalizing
       the example in the comment on [apply_coercion]). Then, what _should_
       happen here is that we pull out the sub-coercion corresponding to
       [row_tag] and apply it. But we don't have a row-like - all we have is
       either the identity or a [change_depth] coercion. Thus we cheat and let
       the same [change_depth] coercion apply at _either_ the level of the
       variant-like closure type _or_ the level of a component function type.
       Since we only expect coercions to apply in cases where there's a single
       branch (either [my_closure], a known projection out of [my_closure], or a
       [let] binding arising from inlining a known function), we're getting away
       with it for the time being.

       This is why we don't similarly recurse into variants (or blocks): in
       general, a coercion acting on a variant or block _does not_ act directly
       on the components; rather, it should contain sub-coercions that do - and
       we don't currently have any such compound coercions. (See the comment on
       [apply_coercion].)

       CR-someday lmaurer: Fix this if necessary. It's unlikely to be worthwhile
       to do so before we already have row-like coercions for other reasons. *)
    ignore (row_tag : Closure_id.t option);
    coercion
  in
  let function_types =
    (* Somewhat hackily apply the same coercion to everything in the set of
       closures. After all, we're only adjusting recursion depth, and all
       closures in the same set have the same depth.

       CR lmaurer: Check that this is consistent with the simplifier's behavior.
       In particular, [select_closure] should return a closure at the same depth
       as the original closure.

       Exhaustingly, this is _entirely orthogonal_ to the issue with closures
       having row-like types. *)
    Closure_id.Map.map
      (fun function_type ->
        match apply_coercion_function_type function_type function_coercion with
        | Ok function_type -> function_type
        | Bottom ->
          bottom := true;
          (* This [function_type] will never be looked at: *)
          function_type)
      function_types
  in
  if !bottom
  then Bottom
  else
    let<* closure_types =
      apply_coercion_to_closure_types_in_set closure_types coercion
    in
    let<+ closure_var_types =
      apply_coercion_to_closure_var_types_in_set closure_var_types coercion
    in
    { function_types; closure_types; closure_var_types }

and apply_coercion_to_closure_types_in_set
    ({ closure_id_components_by_index } as product) coercion : _ Or_bottom.t =
  let found_bottom = ref false in
  let closure_id_components_by_index' =
    (* Again, just apply the same coercion to everything in the set of closures
       (see comment on [function_types] above).

       We're _also_ once again conflating the coercion applied to the
       variant-like closure type with the coercion applied to the row. If we had
       row-like coercions, we would instead construct a singleton for each
       closure (just as [product] has a singleton row-like type for each
       closure).

       CR-someday lmaurer: Fix this once we fix [apply_coercion_closures_entry]
       (presumably by adding row-like coercions). *)
    Closure_id.Map.map_sharing
      (fun t ->
        match apply_coercion t coercion with
        | Bottom ->
          found_bottom := true;
          t
        | Ok t -> t)
      closure_id_components_by_index
  in
  if !found_bottom
  then Bottom
  else if closure_id_components_by_index == closure_id_components_by_index'
  then Ok product
  else Ok { closure_id_components_by_index = closure_id_components_by_index' }

and apply_coercion_to_closure_var_types_in_set product _coercion : _ Or_bottom.t
    =
  (* The coercion applies only to the function type, not the environment. For
     example, changing the depth of a closure [inner] that refers to some other
     closure [outer] (with which it's not mutually recursive) only changes the
     depth of [inner]. *)
  Ok product

and apply_coercion_function_type
    (function_type : function_type Or_unknown_or_bottom.t)
    (coercion : Coercion.t) : _ Or_bottom.t =
  match coercion with
  | Id -> Ok function_type
  | Change_depth { from; to_ } -> (
    match function_type with
    | Unknown | Bottom -> Ok function_type
    | Ok { code_id; rec_info } ->
      (* CR lmaurer: We should really be checking that [from] matches the
         current [rec_info], but that requires either passing in a typing
         environment or making absolutely sure that rec_infos get
         canonicalized. *)
      ignore (from, rec_info);
      let rec_info = Rec_info (TD.create to_) in
      Ok (Or_unknown_or_bottom.Ok { code_id; rec_info }))

and apply_coercion_env_extension { equations } coercion : _ Or_bottom.t =
  let<+ equations =
    Name.Map.fold
      (fun name t result ->
        let<* result = result in
        let<+ t = apply_coercion t coercion in
        Name.Map.add name t result)
      equations (Or_bottom.Ok Name.Map.empty)
  in
  { equations }

let apply_coercion t coercion =
  match apply_coercion t coercion with
  | Ok t -> t
  | Bottom ->
    Misc.fatal_errorf "Cannot apply coercion %a@ to type %a" Coercion.print
      coercion print t

let kind t =
  match t with
  | Value _ -> K.value
  | Naked_immediate _ -> K.naked_immediate
  | Naked_float _ -> K.naked_float
  | Naked_int32 _ -> K.naked_int32
  | Naked_int64 _ -> K.naked_int64
  | Naked_nativeint _ -> K.naked_nativeint
  | Rec_info _ -> K.rec_info

let create_variant ~is_unique ~(immediates : _ Or_unknown.t) ~blocks =
  begin
    match immediates with
    | Unknown -> ()
    | Known immediates ->
      if not (K.equal (kind immediates) K.naked_immediate)
      then
        Misc.fatal_errorf
          "Cannot create [immediates] with type that is not of kind \
           [Naked_immediate]:@ %a"
          print immediates
  end;
  Value (TD.create (Variant { immediates; blocks; is_unique }))

let create_closures by_closure_id =
  Value (TD.create (Closures { by_closure_id }))

module Function_type = struct
  type t = function_type

  let create code_id ~rec_info = { code_id; rec_info }

  let code_id t = t.code_id

  let rec_info t = t.rec_info
end

module Closures_entry = struct
  type t = closures_entry

  let create ~function_types ~closure_types ~closure_var_types =
    { function_types; closure_types; closure_var_types }

  let find_function_type t closure_id : _ Or_unknown_or_bottom.t =
    match Closure_id.Map.find closure_id t.function_types with
    | exception Not_found -> Bottom
    | func_decl -> func_decl

  let closure_var_types { closure_var_types; _ } =
    closure_var_types.var_within_closure_components_by_index
end

module Product = struct
  module Closure_id_indexed = struct
    type t = closure_id_indexed_product

    let create closure_id_components_by_index =
      (* CR mshinwell: Check that the types are all of kind [Value] *)
      { closure_id_components_by_index }

    let top = { closure_id_components_by_index = Closure_id.Map.empty }

    let width t =
      Targetint_31_63.Imm.of_int
        (Closure_id.Map.cardinal t.closure_id_components_by_index)

    let components t = Closure_id.Map.data t.closure_id_components_by_index
  end

  module Var_within_closure_indexed = struct
    type t = var_within_closure_indexed_product

    let create var_within_closure_components_by_index =
      (* CR mshinwell: Check that the types are all of kind [Value] *)
      { var_within_closure_components_by_index }

    let top =
      { var_within_closure_components_by_index = Var_within_closure.Map.empty }

    let width t =
      Targetint_31_63.Imm.of_int
        (Var_within_closure.Map.cardinal
           t.var_within_closure_components_by_index)

    let components t =
      Var_within_closure.Map.data t.var_within_closure_components_by_index
  end

  module Int_indexed = struct
    type t = int_indexed_product

    let field_kind t = t.kind

    let create_from_list kind tys = { kind; fields = Array.of_list tys }

    let create_from_array kind fields = { kind; fields }

    let create_top kind = { kind; fields = [||] }

    let width t = Targetint_31_63.Imm.of_int (Array.length t.fields)

    let components t = Array.to_list t.fields
  end
end

module Row_like_index = struct
  type 'index t = 'index row_like_index

  let known index = Known index

  let at_least index = At_least index
end

module Row_like_case = struct
  type ('index, 'maps_to) t = ('index, 'maps_to) row_like_case

  let create ~maps_to ~index ~env_extension = { maps_to; index; env_extension }
end

module Row_like_for_blocks = struct
  type t = row_like_for_blocks

  type open_or_closed =
    | Open of Tag.t Or_unknown.t
    | Closed of Tag.t

  let bottom = { known_tags = Tag.Map.empty; other_tags = Bottom }

  let is_bottom { known_tags; other_tags } =
    Tag.Map.is_empty known_tags && other_tags = Or_bottom.Bottom

  let all_tags { known_tags; other_tags } : Tag.Set.t Or_unknown.t =
    match other_tags with
    | Ok _ -> Unknown
    | Bottom -> Known (Tag.Map.keys known_tags)

  let create_exactly tag index maps_to =
    { known_tags =
        Tag.Map.singleton tag
          { maps_to;
            index = Known index;
            env_extension = { equations = Name.Map.empty }
          };
      other_tags = Bottom
    }

  let create_at_least tag index maps_to =
    { known_tags =
        Tag.Map.singleton tag
          { maps_to;
            index = At_least index;
            env_extension = { equations = Name.Map.empty }
          };
      other_tags = Bottom
    }

  let create_at_least_unknown_tag index maps_to =
    { known_tags = Tag.Map.empty;
      other_tags =
        Ok
          { maps_to;
            index = At_least index;
            env_extension = { equations = Name.Map.empty }
          }
    }

  let create ~(field_kind : Flambda_kind.t) ~field_tys
      (open_or_closed : open_or_closed) =
    let field_kind' =
      List.map kind field_tys |> Flambda_kind.Set.of_list
      |> Flambda_kind.Set.get_singleton
    in
    (* CR pchambart: move to invariant check *)
    begin
      match field_kind' with
      | None ->
        if List.length field_tys <> 0
        then Misc.fatal_error "[field_tys] must all be of the same kind"
      | Some field_kind' ->
        if not (Flambda_kind.equal field_kind field_kind')
        then
          Misc.fatal_errorf "Declared field kind %a doesn't match [field_tys]"
            Flambda_kind.print field_kind
    end;

    let tag : _ Or_unknown.t =
      let tag : _ Or_unknown.t =
        match open_or_closed with
        | Open (Known tag) -> Known tag
        | Open Unknown -> Unknown
        | Closed tag -> Known tag
      in
      match tag with
      | Unknown -> begin
        match field_kind with
        | Value -> Unknown
        | Naked_number Naked_float -> Known Tag.double_array_tag
        | Naked_number Naked_immediate
        | Naked_number Naked_int32
        | Naked_number Naked_int64
        | Naked_number Naked_nativeint
        | Fabricated | Rec_info ->
          Misc.fatal_errorf "Bad kind %a for fields" Flambda_kind.print
            field_kind
      end
      | Known tag -> begin
        match field_kind with
        | Value -> begin
          match Tag.Scannable.of_tag tag with
          | Some _ -> Known tag
          | None ->
            Misc.fatal_error
              "Blocks full of [Value]s must have a tag less than [No_scan_tag]"
        end
        | Naked_number Naked_float ->
          if not (Tag.equal tag Tag.double_array_tag)
          then
            Misc.fatal_error
              "Blocks full of naked floats must have tag [Tag.double_array_tag]";
          Known tag
        | Naked_number Naked_immediate
        | Naked_number Naked_int32
        | Naked_number Naked_int64
        | Naked_number Naked_nativeint
        | Fabricated | Rec_info ->
          Misc.fatal_errorf "Bad kind %a for fields" Flambda_kind.print
            field_kind
      end
    in
    let product = { kind = field_kind; fields = Array.of_list field_tys } in
    let size = Targetint_31_63.Imm.of_int (List.length field_tys) in
    match open_or_closed with
    | Open _ -> begin
      match tag with
      | Known tag -> create_at_least tag size product
      | Unknown -> create_at_least_unknown_tag size product
    end
    | Closed _ -> (
      match tag with
      | Known tag -> create_exactly tag size product
      | Unknown -> assert false)
  (* see above *)

  let create_blocks_with_these_tags ~field_kind tags =
    let maps_to = Product.Int_indexed.create_top field_kind in
    let case =
      { maps_to;
        index = At_least Targetint_31_63.Imm.zero;
        env_extension = { equations = Name.Map.empty }
      }
    in
    { known_tags = Tag.Map.of_set (fun _ -> case) tags; other_tags = Bottom }

  let create_exactly_multiple ~field_tys_by_tag =
    let known_tags =
      Tag.Map.map
        (fun field_tys ->
          (* CR mshinwell: Validate [field_tys] like [create] does, above *)
          let field_kind =
            match field_tys with
            | [] -> Flambda_kind.value
            | field_ty :: _ -> kind field_ty
          in
          let maps_to =
            { kind = field_kind; fields = Array.of_list field_tys }
          in
          let size = Targetint_31_63.Imm.of_int (List.length field_tys) in
          { maps_to;
            index = Known size;
            env_extension = { equations = Name.Map.empty }
          })
        field_tys_by_tag
    in
    { known_tags; other_tags = Bottom }

  let create_raw ~known_tags ~other_tags =
    (* CR mshinwell: add invariant check? *)
    { known_tags; other_tags }

  let all_tags_and_indexes { known_tags; other_tags } : _ Or_unknown.t =
    match other_tags with
    | Ok _ -> Unknown
    | Bottom -> Known (Tag.Map.map (fun case -> case.index) known_tags)

  let all_tags_and_sizes t : Targetint_31_63.Imm.t Tag.Map.t Or_unknown.t =
    match all_tags_and_indexes t with
    | Unknown -> Unknown
    | Known tags_and_indexes ->
      let any_unknown = ref false in
      let by_tag =
        Tag.Map.map
          (fun index ->
            match index with
            | Known index -> index
            | At_least index ->
              any_unknown := true;
              index)
          tags_and_indexes
      in
      if !any_unknown then Unknown else Known by_tag

  let get_singleton { known_tags; other_tags } =
    match other_tags with
    | Ok _ -> None
    | Bottom -> (
      match Tag.Map.get_singleton known_tags with
      | None -> None
      | Some (tag, { maps_to; index; env_extension = _ }) -> (
        (* If this is a singleton all the information from the env_extension is
           already part of the environment *)
        match index with
        | At_least _ -> None
        | Known index -> Some ((tag, index), maps_to)))

  let project_int_indexed_product { fields; kind = _ } index : _ Or_unknown.t =
    if Array.length fields <= index then Unknown else Known fields.(index)

  let get_field t field_index : _ Or_unknown_or_bottom.t =
    match get_singleton t with
    | None -> Unknown
    | Some ((_tag, size), maps_to) -> (
      let index = Targetint_31_63.to_targetint field_index in
      if Targetint_31_63.Imm.( <= ) size index
      then Bottom
      else
        match
          project_int_indexed_product maps_to (Targetint_31_63.Imm.to_int index)
        with
        | Unknown -> Unknown
        | Known res -> Ok res)

  let get_variant_field t variant_tag field_index : _ Or_unknown_or_bottom.t =
    let index = Targetint_31_63.to_targetint field_index in
    let aux { index = size; maps_to; env_extension = _ } :
        _ Or_unknown_or_bottom.t =
      match size with
      | Known i when i <= index -> Bottom
      | Known _ | At_least _ -> (
        match
          project_int_indexed_product maps_to (Targetint_31_63.Imm.to_int index)
        with
        | Unknown -> Unknown
        | Known res -> Ok res)
    in
    match Tag.Map.find variant_tag t.known_tags with
    | case -> aux case
    | exception Not_found -> begin
      match t.other_tags with Bottom -> Bottom | Ok case -> aux case
    end
end

module Row_like_for_closures = struct
  type t = row_like_for_closures

  let create_exactly (closure_id : Closure_id.t)
      (contents : Set_of_closures_contents.t) (closures_entry : closures_entry)
      =
    let known_closures =
      Closure_id.Map.singleton closure_id
        { index = Known contents;
          maps_to = closures_entry;
          env_extension = { equations = Name.Map.empty }
        }
    in
    { known_closures; other_closures = Bottom }

  let create_at_least (closure_id : Closure_id.t)
      (contents : Set_of_closures_contents.t) (closures_entry : closures_entry)
      =
    let known_closures =
      Closure_id.Map.singleton closure_id
        { index = At_least contents;
          maps_to = closures_entry;
          env_extension = { equations = Name.Map.empty }
        }
    in
    { known_closures; other_closures = Bottom }

  let create_raw ~known_closures ~other_closures =
    (* CR mshinwell: add invariant check? *)
    { known_closures; other_closures }

  let get_singleton { known_closures; other_closures } =
    match other_closures with
    | Ok _ -> None
    | Bottom -> (
      match Closure_id.Map.get_singleton known_closures with
      | None -> None
      | Some (tag, { maps_to; index; env_extension = _ }) -> (
        (* If this is a singleton all the information from the env_extension is
           already part of the environment *)
        match index with
        | At_least _ -> None
        | Known index -> Some ((tag, index), maps_to)))

  let get_env_var t env_var : _ Or_unknown.t =
    match get_singleton t with
    | None -> Unknown
    | Some ((_tag, index), maps_to) ->
      if not
           (Var_within_closure.Set.mem env_var
              (Set_of_closures_contents.closure_vars index))
      then Unknown
      else
        let env_var_ty =
          try
            Var_within_closure.Map.find env_var
              maps_to.closure_var_types.var_within_closure_components_by_index
          with Not_found ->
            Misc.fatal_errorf
              "Environment variable %a is bound in index but not in \
               maps_to@.Index:@ %a@.Maps_to:@ %a"
              Var_within_closure.print env_var Set_of_closures_contents.print
              index print_closures_entry maps_to
        in
        Known env_var_ty
end

module Env_extension = struct
  type t = env_extension

  let empty = { equations = Name.Map.empty }

  let create ~equations = { equations }

  let all_ids_for_export = all_ids_for_export_env_extension

  let apply_renaming = apply_renaming_env_extension

  let free_names = free_names_env_extension

  let print = print_env_extension

  let to_map t = t.equations
end

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
  | Value ty ->
    TD.get_alias_exn ty ~apply_renaming_head:apply_renaming_head_of_kind_value
      ~free_names_head:free_names_head_of_kind_value
  | Naked_immediate ty ->
    TD.get_alias_exn ty
      ~apply_renaming_head:apply_renaming_head_of_kind_naked_immediate
      ~free_names_head:free_names_head_of_kind_naked_immediate
  | Naked_float ty ->
    TD.get_alias_exn ty
      ~apply_renaming_head:apply_renaming_head_of_kind_naked_float
      ~free_names_head:free_names_head_of_kind_naked_float
  | Naked_int32 ty ->
    TD.get_alias_exn ty
      ~apply_renaming_head:apply_renaming_head_of_kind_naked_int32
      ~free_names_head:free_names_head_of_kind_naked_int32
  | Naked_int64 ty ->
    TD.get_alias_exn ty
      ~apply_renaming_head:apply_renaming_head_of_kind_naked_int64
      ~free_names_head:free_names_head_of_kind_naked_int64
  | Naked_nativeint ty ->
    TD.get_alias_exn ty
      ~apply_renaming_head:apply_renaming_head_of_kind_naked_nativeint
      ~free_names_head:free_names_head_of_kind_naked_nativeint
  | Rec_info ty ->
    TD.get_alias_exn ty
      ~apply_renaming_head:apply_renaming_head_of_kind_rec_info
      ~free_names_head:free_names_head_of_kind_rec_info

let is_obviously_bottom t =
  match t with
  | Value ty -> TD.is_obviously_bottom ty
  | Naked_immediate ty -> TD.is_obviously_bottom ty
  | Naked_float ty -> TD.is_obviously_bottom ty
  | Naked_int32 ty -> TD.is_obviously_bottom ty
  | Naked_int64 ty -> TD.is_obviously_bottom ty
  | Naked_nativeint ty -> TD.is_obviously_bottom ty
  | Rec_info ty -> TD.is_obviously_bottom ty

let is_obviously_unknown t =
  match t with
  | Value ty -> TD.is_obviously_unknown ty
  | Naked_immediate ty -> TD.is_obviously_unknown ty
  | Naked_float ty -> TD.is_obviously_unknown ty
  | Naked_int32 ty -> TD.is_obviously_unknown ty
  | Naked_int64 ty -> TD.is_obviously_unknown ty
  | Naked_nativeint ty -> TD.is_obviously_unknown ty
  | Rec_info ty -> TD.is_obviously_unknown ty

let alias_type_of (kind : K.t) name : t =
  match kind with
  | Value -> Value (TD.create_equals name)
  | Naked_number Naked_immediate -> Naked_immediate (TD.create_equals name)
  | Naked_number Naked_float -> Naked_float (TD.create_equals name)
  | Naked_number Naked_int32 -> Naked_int32 (TD.create_equals name)
  | Naked_number Naked_int64 -> Naked_int64 (TD.create_equals name)
  | Naked_number Naked_nativeint -> Naked_nativeint (TD.create_equals name)
  | Rec_info -> Rec_info (TD.create_equals name)
  | Fabricated -> Misc.fatal_error "Unused kind, to be removed"

let bottom_value = Value TD.bottom

let bottom_naked_immediate = Naked_immediate TD.bottom

let bottom_naked_float = Naked_float TD.bottom

let bottom_naked_int32 = Naked_int32 TD.bottom

let bottom_naked_int64 = Naked_int64 TD.bottom

let bottom_naked_nativeint = Naked_nativeint TD.bottom

let bottom_rec_info = Rec_info TD.bottom

let any_value = Value TD.unknown

let any_naked_immediate = Naked_immediate TD.unknown

let any_naked_float = Naked_float TD.unknown

let any_naked_int32 = Naked_int32 TD.unknown

let any_naked_int64 = Naked_int64 TD.unknown

let any_naked_nativeint = Naked_nativeint TD.unknown

let any_rec_info = Rec_info TD.unknown

let this_naked_immediate i : t =
  Naked_immediate (TD.create_equals (Simple.const (RWC.naked_immediate i)))

let this_naked_float f : t =
  Naked_float (TD.create_equals (Simple.const (RWC.naked_float f)))

let this_naked_int32 i : t =
  Naked_int32 (TD.create_equals (Simple.const (RWC.naked_int32 i)))

let this_naked_int64 i : t =
  Naked_int64 (TD.create_equals (Simple.const (RWC.naked_int64 i)))

let this_naked_nativeint i : t =
  Naked_nativeint (TD.create_equals (Simple.const (RWC.naked_nativeint i)))

let these_naked_immediates ~no_alias is =
  match Targetint_31_63.Set.get_singleton is with
  | Some i when not no_alias -> this_naked_immediate i
  | _ ->
    if Targetint_31_63.Set.is_empty is
    then bottom_naked_immediate
    else Naked_immediate (TD.create (Naked_immediates is))

let these_naked_floats ~no_alias fs =
  match Float.Set.get_singleton fs with
  | Some f when not no_alias -> this_naked_float f
  | _ ->
    if Float.Set.is_empty fs
    then bottom_naked_float
    else Naked_float (TD.create fs)

let these_naked_int32s ~no_alias is =
  match Int32.Set.get_singleton is with
  | Some i when not no_alias -> this_naked_int32 i
  | _ ->
    if Int32.Set.is_empty is
    then bottom_naked_int32
    else Naked_int32 (TD.create is)

let these_naked_int64s ~no_alias is =
  match Int64.Set.get_singleton is with
  | Some i when not no_alias -> this_naked_int64 i
  | _ ->
    if Int64.Set.is_empty is
    then bottom_naked_int64
    else Naked_int64 (TD.create is)

let these_naked_nativeints ~no_alias is =
  match Targetint_32_64.Set.get_singleton is with
  | Some i when not no_alias -> this_naked_nativeint i
  | _ ->
    if Targetint_32_64.Set.is_empty is
    then bottom_naked_nativeint
    else Naked_nativeint (TD.create is)

let box_float (t : t) : t =
  match t with
  | Naked_float _ -> Value (TD.create (Boxed_float t))
  | Value _ | Naked_immediate _ | Naked_int32 _ | Naked_int64 _
  | Naked_nativeint _ | Rec_info _ ->
    Misc.fatal_errorf "Type of wrong kind for [box_float]: %a" print t

let box_int32 (t : t) : t =
  match t with
  | Naked_int32 _ -> Value (TD.create (Boxed_int32 t))
  | Value _ | Naked_immediate _ | Naked_float _ | Naked_int64 _
  | Naked_nativeint _ | Rec_info _ ->
    Misc.fatal_errorf "Type of wrong kind for [box_int32]: %a" print t

let box_int64 (t : t) : t =
  match t with
  | Naked_int64 _ -> Value (TD.create (Boxed_int64 t))
  | Value _ | Naked_immediate _ | Naked_float _ | Naked_int32 _
  | Naked_nativeint _ | Rec_info _ ->
    Misc.fatal_errorf "Type of wrong kind for [box_int64]: %a" print t

let box_nativeint (t : t) : t =
  match t with
  | Naked_nativeint _ -> Value (TD.create (Boxed_nativeint t))
  | Value _ | Naked_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
  | Rec_info _ ->
    Misc.fatal_errorf "Type of wrong kind for [box_nativeint]: %a" print t

let this_tagged_immediate imm : t =
  Value (TD.create_equals (Simple.const (RWC.tagged_immediate imm)))

let tag_immediate t : t =
  match t with
  | Naked_immediate _ ->
    Value
      (TD.create
         (Variant
            { is_unique = false;
              immediates = Known t;
              blocks = Known Row_like_for_blocks.bottom
            }))
  | Value _ | Naked_float _ | Naked_int32 _ | Naked_int64 _ | Naked_nativeint _
  | Rec_info _ ->
    Misc.fatal_errorf "Type of wrong kind for [tag_immediate]: %a" print t

let tagged_immediate_alias_to ~naked_immediate : t =
  tag_immediate
    (Naked_immediate (TD.create_equals (Simple.var naked_immediate)))

let is_int_for_scrutinee ~scrutinee : t =
  Naked_immediate (TD.create (Is_int (alias_type_of K.value scrutinee)))

let get_tag_for_block ~block : t =
  Naked_immediate (TD.create (Get_tag (alias_type_of K.value block)))

let boxed_float_alias_to ~naked_float =
  box_float (Naked_float (TD.create_equals (Simple.var naked_float)))

let boxed_int32_alias_to ~naked_int32 =
  box_int32 (Naked_int32 (TD.create_equals (Simple.var naked_int32)))

let boxed_int64_alias_to ~naked_int64 =
  box_int64 (Naked_int64 (TD.create_equals (Simple.var naked_int64)))

let boxed_nativeint_alias_to ~naked_nativeint =
  box_nativeint
    (Naked_nativeint (TD.create_equals (Simple.var naked_nativeint)))

let this_immutable_string str =
  (* CR mshinwell: Use "length" not "size" for strings *)
  let size = Targetint_31_63.Imm.of_int (String.length str) in
  let string_info =
    String_info.Set.singleton
      (String_info.create ~contents:(Contents str) ~size)
  in
  Value (TD.create (String string_info))

let mutable_string ~size =
  let size = Targetint_31_63.Imm.of_int size in
  let string_info =
    String_info.Set.singleton
      (String_info.create ~contents:Unknown_or_mutable ~size)
  in
  Value (TD.create (String string_info))

let array_of_length ~length = Value (TD.create (Array { length }))

let this_rec_info (rec_info_expr : Rec_info_expr.t) =
  match rec_info_expr with
  | Var dv -> Rec_info (TD.create_equals (Simple.var dv))
  | Const _ | Succ _ | Unroll_to _ -> Rec_info (TD.create rec_info_expr)

module Descr = struct
  type t =
    | Value of head_of_kind_value TD.Descr.t Or_unknown_or_bottom.t
    | Naked_immediate of
        head_of_kind_naked_immediate TD.Descr.t Or_unknown_or_bottom.t
    | Naked_float of head_of_kind_naked_float TD.Descr.t Or_unknown_or_bottom.t
    | Naked_int32 of head_of_kind_naked_int32 TD.Descr.t Or_unknown_or_bottom.t
    | Naked_int64 of head_of_kind_naked_int64 TD.Descr.t Or_unknown_or_bottom.t
    | Naked_nativeint of
        head_of_kind_naked_nativeint TD.Descr.t Or_unknown_or_bottom.t
    | Rec_info of head_of_kind_rec_info TD.Descr.t Or_unknown_or_bottom.t
end

let descr t : Descr.t =
  match t with
  | Value ty ->
    Value
      (TD.descr ~apply_renaming_head:apply_renaming_head_of_kind_value
         ~free_names_head:free_names_head_of_kind_value ty)
  | Naked_immediate ty ->
    Naked_immediate
      (TD.descr ~apply_renaming_head:apply_renaming_head_of_kind_naked_immediate
         ~free_names_head:free_names_head_of_kind_naked_immediate ty)
  | Naked_float ty ->
    Naked_float
      (TD.descr ~apply_renaming_head:apply_renaming_head_of_kind_naked_float
         ~free_names_head:free_names_head_of_kind_naked_float ty)
  | Naked_int32 ty ->
    Naked_int32
      (TD.descr ~apply_renaming_head:apply_renaming_head_of_kind_naked_int32
         ~free_names_head:free_names_head_of_kind_naked_int32 ty)
  | Naked_int64 ty ->
    Naked_int64
      (TD.descr ~apply_renaming_head:apply_renaming_head_of_kind_naked_int64
         ~free_names_head:free_names_head_of_kind_naked_int64 ty)
  | Naked_nativeint ty ->
    Naked_nativeint
      (TD.descr ~apply_renaming_head:apply_renaming_head_of_kind_naked_nativeint
         ~free_names_head:free_names_head_of_kind_naked_nativeint ty)
  | Rec_info ty ->
    Rec_info
      (TD.descr ~apply_renaming_head:apply_renaming_head_of_kind_rec_info
         ~free_names_head:free_names_head_of_kind_rec_info ty)

let create_from_head_value head = Value (TD.create head)

let create_from_head_naked_immediate head = Naked_immediate (TD.create head)

let create_from_head_naked_float head = Naked_float (TD.create head)

let create_from_head_naked_int32 head = Naked_int32 (TD.create head)

let create_from_head_naked_int64 head = Naked_int64 (TD.create head)

let create_from_head_naked_nativeint head = Naked_nativeint (TD.create head)

let create_from_head_rec_info head = Rec_info (TD.create head)

module Head_of_kind_value = struct
  type t = head_of_kind_value

  let create_variant ~is_unique ~blocks ~immediates =
    Variant { is_unique; blocks; immediates }

  let create_boxed_float ty = Boxed_float ty

  let create_boxed_int32 ty = Boxed_int32 ty

  let create_boxed_int64 ty = Boxed_int64 ty

  let create_boxed_nativeint ty = Boxed_nativeint ty

  let create_tagged_immediate imm : t =
    Variant
      { is_unique = false;
        immediates = Known (this_naked_immediate imm);
        blocks = Known Row_like_for_blocks.bottom
      }

  let create_closures by_closure_id = Closures { by_closure_id }

  let create_string info = String info

  let create_array ~length = Array { length }
end

module Head_of_kind_naked_immediate = struct
  type t = head_of_kind_naked_immediate

  (* CR mshinwell: maybe this should return [Or_bottom.t]? *)
  let create_naked_immediates imms = Naked_immediates imms

  let create_is_int ty = Is_int ty

  let create_get_tag ty = Get_tag ty
end
