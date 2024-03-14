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

module K = Flambda_kind
module Float32 = Numeric_types.Float32_by_bit_pattern
module Float = Numeric_types.Float_by_bit_pattern
module Vec128 = Vector_types.Vec128.Bit_pattern
module Int32 = Numeric_types.Int32
module Int64 = Numeric_types.Int64
module RWC = Reg_width_const
module TD = Type_descr
open Or_bottom.Let_syntax
open Or_unknown.Let_syntax

module Block_size = struct
  include Targetint_31_63

  (** [subset t1 t2] is true if [t1] is a subset of [t2] *)
  let subset t1 t2 = Stdlib.( <= ) (compare t1 t2) 0

  (* An integer [i] represents all the values smaller than i, hence a smaller
     number is included in a bigger *)
  let union t1 t2 = Targetint_31_63.max t1 t2

  let inter t1 t2 = Targetint_31_63.min t1 t2
end

(* The grammar of Flambda types. *)
type t =
  | Value of head_of_kind_value TD.t
  | Naked_immediate of head_of_kind_naked_immediate TD.t
  | Naked_float32 of head_of_kind_naked_float32 TD.t
  | Naked_float of head_of_kind_naked_float TD.t
  | Naked_int32 of head_of_kind_naked_int32 TD.t
  | Naked_int64 of head_of_kind_naked_int64 TD.t
  | Naked_nativeint of head_of_kind_naked_nativeint TD.t
  | Naked_vec128 of head_of_kind_naked_vec128 TD.t
  | Rec_info of head_of_kind_rec_info TD.t
  | Region of head_of_kind_region TD.t

and head_of_kind_value =
  | Variant of
      { immediates : t Or_unknown.t;
        blocks : row_like_for_blocks Or_unknown.t;
        is_unique : bool
      }
  | Mutable_block of { alloc_mode : Alloc_mode.For_types.t }
  | Boxed_float32 of t * Alloc_mode.For_types.t
  | Boxed_float of t * Alloc_mode.For_types.t
  | Boxed_int32 of t * Alloc_mode.For_types.t
  | Boxed_int64 of t * Alloc_mode.For_types.t
  | Boxed_nativeint of t * Alloc_mode.For_types.t
  | Boxed_vec128 of t * Alloc_mode.For_types.t
  | Closures of
      { by_function_slot : row_like_for_closures;
        alloc_mode : Alloc_mode.For_types.t
      }
  | String of String_info.Set.t
  | Array of
      { element_kind : Flambda_kind.With_subkind.t Or_unknown_or_bottom.t;
        length : t;
        contents : array_contents Or_unknown.t;
        alloc_mode : Alloc_mode.For_types.t
      }

(* CR someday vlaviron: comparison results are encoded as naked immediates, and
   in a few cases (physical equality mostly) some values of the boolean carry
   information that we can represent in the types. Here is an actual example
   where it would be useful: *)

(* type t =
 *   | A1 of float array
 *   | A2 of int array
 *   | A3 of int array
 *   | A4 of int array
 *
 * let bar t =
 *   match t with
 *   | A3 x -> array_unsafe_get x 0 (* Not specialised currently *)
 *   | _ -> assert false *)

(* Since the match is compiled using equality on the tag and not a regular
   switch, we currently fail to restrict the type of [t] to the single [A3]
   constructor. We could solve that by adding another case like Is_int and
   Get_tag, or we could go in the other direction and make each individual
   number in the set for the Naked_immediates case carry an extension. We could
   even use that for encoding the Is_int and Get_tag constraints, although it is
   not completely clear what the impact on performance would be (we could store
   minimal extensions, carrying a shape, or we could pre-compute the full meet
   for each case and store precise extensions; the first version would be faster
   if we don't actually use the extensions, while the second version would be
   particularly useful if we switch several times on the same scrutinee. *)
and head_of_kind_naked_immediate =
  | Naked_immediates of Targetint_31_63.Set.t
  | Is_int of t
  | Get_tag of t

and head_of_kind_naked_float32 = Float32.Set.t

and head_of_kind_naked_float = Float.Set.t

and head_of_kind_naked_int32 = Int32.Set.t

and head_of_kind_naked_int64 = Int64.Set.t

and head_of_kind_naked_nativeint = Targetint_32_64.Set.t

and head_of_kind_naked_vec128 = Vec128.Set.t

and head_of_kind_rec_info = Rec_info_expr.t

and head_of_kind_region = unit

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
    other_tags : (Block_size.t, int_indexed_product) row_like_case Or_bottom.t;
    alloc_mode : Alloc_mode.For_types.t
  }

and row_like_for_closures =
  { known_closures :
      (Set_of_closures_contents.t, closures_entry) row_like_case
      Function_slot.Map.t;
    (* CR pchambart: this field is always Bottom, we should remove it *)
    other_closures :
      (Set_of_closures_contents.t, closures_entry) row_like_case Or_bottom.t
  }

and closures_entry =
  { (* CR pchambart: Forbid the Bottom case in function types (propagate to the
       whole environment *)
    function_types : function_type Or_unknown_or_bottom.t Function_slot.Map.t;
    closure_types : function_slot_indexed_product;
    value_slot_types : value_slot_indexed_product
  }

(* Products are a set of constraints: each new field reduces the concrete set.
   The empty product is top. There is no bottom. All components must be of the
   same kind except for [value_slot_indexed_product].

   { 1 => Unknown; 2 => V } is equal to { 2 => V } *)
and function_slot_indexed_product =
  { function_slot_components_by_index : t Function_slot.Map.t }

and value_slot_indexed_product =
  { value_slot_components_by_index : t Value_slot.Map.t }

and int_indexed_product =
  { fields : t array;
    kind : Flambda_kind.t
  }

and function_type =
  { code_id : Code_id.t;
    rec_info : t
        (* XXX need to understand this properly can_allocate_in_caller's_region
           : bool Or_unknown.t *)
  }

and array_contents =
  | Immutable of { fields : t array }
  | Mutable

and env_extension = { equations : t Name.Map.t } [@@unboxed]

type flambda_type = t

let row_like_is_bottom ~known ~(other : _ Or_bottom.t) ~is_empty_map_known =
  is_empty_map_known known && match other with Bottom -> true | Ok _ -> false

let rec free_names0 ~follow_value_slots t =
  let[@inline] type_descr_free_names ~free_names_head ty =
    if follow_value_slots
    then TD.free_names ~free_names_head ty
    else TD.free_names_no_cache ~free_names_head ty
  in
  match t with
  | Value ty ->
    type_descr_free_names
      ~free_names_head:(free_names_head_of_kind_value0 ~follow_value_slots)
      ty
  | Naked_immediate ty ->
    type_descr_free_names
      ~free_names_head:
        (free_names_head_of_kind_naked_immediate0 ~follow_value_slots)
      ty
  | Naked_float32 ty ->
    type_descr_free_names ~free_names_head:free_names_head_of_kind_naked_float32
      ty
  | Naked_float ty ->
    type_descr_free_names ~free_names_head:free_names_head_of_kind_naked_float
      ty
  | Naked_int32 ty ->
    type_descr_free_names ~free_names_head:free_names_head_of_kind_naked_int32
      ty
  | Naked_int64 ty ->
    type_descr_free_names ~free_names_head:free_names_head_of_kind_naked_int64
      ty
  | Naked_nativeint ty ->
    type_descr_free_names
      ~free_names_head:free_names_head_of_kind_naked_nativeint ty
  | Naked_vec128 ty ->
    type_descr_free_names ~free_names_head:free_names_head_of_kind_naked_vec128
      ty
  | Rec_info ty ->
    type_descr_free_names ~free_names_head:free_names_head_of_kind_rec_info ty
  | Region ty ->
    type_descr_free_names ~free_names_head:free_names_head_of_kind_region ty

and free_names_head_of_kind_value0 ~follow_value_slots head =
  match head with
  | Variant { blocks; immediates; is_unique = _ } ->
    Name_occurrences.union
      (Or_unknown.free_names
         (free_names_row_like_for_blocks ~follow_value_slots)
         blocks)
      (Or_unknown.free_names (free_names0 ~follow_value_slots) immediates)
  | Mutable_block { alloc_mode = _ } -> Name_occurrences.empty
  | Boxed_float32 (ty, _alloc_mode) -> free_names0 ~follow_value_slots ty
  | Boxed_float (ty, _alloc_mode) -> free_names0 ~follow_value_slots ty
  | Boxed_int32 (ty, _alloc_mode) -> free_names0 ~follow_value_slots ty
  | Boxed_int64 (ty, _alloc_mode) -> free_names0 ~follow_value_slots ty
  | Boxed_nativeint (ty, _alloc_mode) -> free_names0 ~follow_value_slots ty
  | Boxed_vec128 (ty, _alloc_mode) -> free_names0 ~follow_value_slots ty
  | Closures { by_function_slot; alloc_mode = _ } ->
    free_names_row_like_for_closures ~follow_value_slots by_function_slot
  | String _ -> Name_occurrences.empty
  | Array
      { element_kind = _;
        length;
        contents = Unknown | Known Mutable;
        alloc_mode = _
      } ->
    free_names0 ~follow_value_slots length
  | Array
      { element_kind = _;
        length;
        contents = Known (Immutable { fields });
        alloc_mode = _
      } ->
    Array.fold_left
      (fun free_names field ->
        Name_occurrences.union free_names
          (free_names0 ~follow_value_slots field))
      (free_names0 ~follow_value_slots length)
      fields

and free_names_head_of_kind_naked_immediate0 ~follow_value_slots head =
  match head with
  | Naked_immediates _ -> Name_occurrences.empty
  | Is_int ty | Get_tag ty -> free_names0 ~follow_value_slots ty

and free_names_head_of_kind_naked_float32 _ = Name_occurrences.empty

and free_names_head_of_kind_naked_float _ = Name_occurrences.empty

and free_names_head_of_kind_naked_int32 _ = Name_occurrences.empty

and free_names_head_of_kind_naked_int64 _ = Name_occurrences.empty

and free_names_head_of_kind_naked_nativeint _ = Name_occurrences.empty

and free_names_head_of_kind_naked_vec128 _ = Name_occurrences.empty

and free_names_head_of_kind_rec_info head =
  Rec_info_expr.free_names_in_types head

and free_names_head_of_kind_region () = Name_occurrences.empty

and free_names_row_like :
      'row_tag 'index 'maps_to 'known.
      free_names_index:('index -> Name_occurrences.t) ->
      free_names_maps_to:
        (follow_value_slots:bool -> 'maps_to -> Name_occurrences.t) ->
      follow_value_slots:bool ->
      known:'known ->
      other:('index, 'maps_to) row_like_case Or_bottom.t ->
      fold_known:
        (('row_tag -> ('index, 'maps_to) row_like_case -> 'acc -> 'acc) ->
        'known ->
        'acc ->
        'acc) ->
      Name_occurrences.t =
 fun ~free_names_index ~free_names_maps_to ~follow_value_slots ~known ~other
     ~fold_known ->
  let[@inline always] free_names_index index =
    match index with Known index | At_least index -> free_names_index index
  in
  let from_known =
    fold_known
      (fun _ { maps_to; env_extension; index } free_names ->
        Name_occurrences.union
          (Name_occurrences.union free_names (free_names_index index))
          (Name_occurrences.union
             (free_names_env_extension ~follow_value_slots env_extension)
             (free_names_maps_to ~follow_value_slots maps_to)))
      known Name_occurrences.empty
  in
  match other with
  | Bottom -> from_known
  | Ok { maps_to; env_extension; index } ->
    Name_occurrences.union
      (Name_occurrences.union (free_names_index index)
         (free_names_maps_to ~follow_value_slots maps_to))
      (Name_occurrences.union from_known
         (free_names_env_extension ~follow_value_slots env_extension))

and free_names_row_like_for_blocks ~follow_value_slots
    { known_tags; other_tags; alloc_mode = _ } =
  free_names_row_like
    ~free_names_index:(fun _block_size -> Name_occurrences.empty)
    ~free_names_maps_to:free_names_int_indexed_product ~follow_value_slots
    ~known:known_tags ~other:other_tags ~fold_known:Tag.Map.fold

and free_names_row_like_for_closures ~follow_value_slots
    { known_closures; other_closures } =
  free_names_row_like ~free_names_index:Set_of_closures_contents.free_names
    ~free_names_maps_to:free_names_closures_entry ~follow_value_slots
    ~known:known_closures ~other:other_closures
    ~fold_known:Function_slot.Map.fold

and free_names_closures_entry ~follow_value_slots
    { function_types; closure_types; value_slot_types } =
  let function_types_free_names =
    Function_slot.Map.fold
      (fun function_slot function_decl free_names ->
        Name_occurrences.union free_names
          (Name_occurrences.add_function_slot_in_types
             (free_names_function_type ~follow_value_slots function_decl)
             function_slot))
      function_types Name_occurrences.empty
  in
  let slots_free_names =
    if follow_value_slots
    then
      Name_occurrences.union
        (free_names_function_slot_indexed_product ~follow_value_slots
           closure_types)
        (free_names_value_slot_indexed_product ~follow_value_slots
           value_slot_types)
    else
      free_names_function_slot_indexed_product ~follow_value_slots closure_types
  in
  Name_occurrences.union function_types_free_names slots_free_names

and free_names_function_slot_indexed_product ~follow_value_slots
    { function_slot_components_by_index } =
  Function_slot.Map.fold
    (fun _ t free_names_acc ->
      Name_occurrences.union (free_names0 ~follow_value_slots t) free_names_acc)
    function_slot_components_by_index Name_occurrences.empty

and free_names_value_slot_indexed_product ~follow_value_slots
    { value_slot_components_by_index } =
  Value_slot.Map.fold
    (fun value_slot t free_names_acc ->
      Name_occurrences.add_value_slot_in_types
        (Name_occurrences.union
           (free_names0 ~follow_value_slots t)
           free_names_acc)
        value_slot)
    value_slot_components_by_index Name_occurrences.empty

and free_names_int_indexed_product ~follow_value_slots { fields; kind = _ } =
  Array.fold_left
    (fun free_names_acc t ->
      Name_occurrences.union (free_names0 ~follow_value_slots t) free_names_acc)
    Name_occurrences.empty fields

and free_names_function_type ~follow_value_slots
    (function_type : _ Or_unknown_or_bottom.t) =
  match function_type with
  | Bottom | Unknown -> Name_occurrences.empty
  | Ok { code_id; rec_info } ->
    Name_occurrences.add_code_id
      (free_names0 ~follow_value_slots rec_info)
      code_id Name_mode.normal

and free_names_env_extension ~follow_value_slots { equations } =
  Name.Map.fold
    (fun name t acc ->
      let acc =
        Name_occurrences.union acc (free_names0 ~follow_value_slots t)
      in
      Name_occurrences.add_name acc name Name_mode.in_types)
    equations Name_occurrences.empty

let free_names_except_through_value_slots t =
  free_names0 ~follow_value_slots:false t

let free_names t = free_names0 ~follow_value_slots:true t

let free_names_head_of_kind_value t =
  free_names_head_of_kind_value0 ~follow_value_slots:true t

let free_names_head_of_kind_naked_immediate t =
  free_names_head_of_kind_naked_immediate0 ~follow_value_slots:true t

let rec apply_renaming t renaming =
  if Renaming.is_identity renaming
  then t
  else
    match t with
    | Value ty ->
      let ty' =
        TD.apply_renaming ~apply_renaming_head:apply_renaming_head_of_kind_value
          ~free_names_head:free_names_head_of_kind_value ty renaming
      in
      if ty == ty' then t else Value ty'
    | Naked_immediate ty ->
      let ty' =
        TD.apply_renaming
          ~apply_renaming_head:apply_renaming_head_of_kind_naked_immediate
          ~free_names_head:free_names_head_of_kind_naked_immediate ty renaming
      in
      if ty == ty' then t else Naked_immediate ty'
    | Naked_float32 ty ->
      let ty' =
        TD.apply_renaming
          ~apply_renaming_head:apply_renaming_head_of_kind_naked_float32
          ~free_names_head:free_names_head_of_kind_naked_float32 ty renaming
      in
      if ty == ty' then t else Naked_float32 ty'
    | Naked_float ty ->
      let ty' =
        TD.apply_renaming
          ~apply_renaming_head:apply_renaming_head_of_kind_naked_float
          ~free_names_head:free_names_head_of_kind_naked_float ty renaming
      in
      if ty == ty' then t else Naked_float ty'
    | Naked_int32 ty ->
      let ty' =
        TD.apply_renaming
          ~apply_renaming_head:apply_renaming_head_of_kind_naked_int32
          ~free_names_head:free_names_head_of_kind_naked_int32 ty renaming
      in
      if ty == ty' then t else Naked_int32 ty'
    | Naked_int64 ty ->
      let ty' =
        TD.apply_renaming
          ~apply_renaming_head:apply_renaming_head_of_kind_naked_int64
          ~free_names_head:free_names_head_of_kind_naked_int64 ty renaming
      in
      if ty == ty' then t else Naked_int64 ty'
    | Naked_nativeint ty ->
      let ty' =
        TD.apply_renaming
          ~apply_renaming_head:apply_renaming_head_of_kind_naked_nativeint
          ~free_names_head:free_names_head_of_kind_naked_nativeint ty renaming
      in
      if ty == ty' then t else Naked_nativeint ty'
    | Naked_vec128 ty ->
      let ty' =
        TD.apply_renaming
          ~apply_renaming_head:apply_renaming_head_of_kind_naked_vec128
          ~free_names_head:free_names_head_of_kind_naked_vec128 ty renaming
      in
      if ty == ty' then t else Naked_vec128 ty'
    | Rec_info ty ->
      let ty' =
        TD.apply_renaming
          ~apply_renaming_head:apply_renaming_head_of_kind_rec_info
          ~free_names_head:free_names_head_of_kind_rec_info ty renaming
      in
      if ty == ty' then t else Rec_info ty'
    | Region ty ->
      let ty' =
        TD.apply_renaming
          ~apply_renaming_head:apply_renaming_head_of_kind_region
          ~free_names_head:free_names_head_of_kind_region ty renaming
      in
      if ty == ty' then t else Region ty'

and apply_renaming_head_of_kind_value head renaming =
  match head with
  | Variant { blocks; immediates; is_unique } ->
    let immediates' =
      let>+$ immediates = immediates in
      apply_renaming immediates renaming
    in
    let blocks' =
      let>+$ blocks = blocks in
      apply_renaming_row_like_for_blocks blocks renaming
    in
    if immediates == immediates' && blocks == blocks'
    then head
    else Variant { is_unique; blocks = blocks'; immediates = immediates' }
  | Mutable_block { alloc_mode = _ } -> head
  | Boxed_float32 (ty, alloc_mode) ->
    let ty' = apply_renaming ty renaming in
    if ty == ty' then head else Boxed_float32 (ty', alloc_mode)
  | Boxed_float (ty, alloc_mode) ->
    let ty' = apply_renaming ty renaming in
    if ty == ty' then head else Boxed_float (ty', alloc_mode)
  | Boxed_int32 (ty, alloc_mode) ->
    let ty' = apply_renaming ty renaming in
    if ty == ty' then head else Boxed_int32 (ty', alloc_mode)
  | Boxed_int64 (ty, alloc_mode) ->
    let ty' = apply_renaming ty renaming in
    if ty == ty' then head else Boxed_int64 (ty', alloc_mode)
  | Boxed_nativeint (ty, alloc_mode) ->
    let ty' = apply_renaming ty renaming in
    if ty == ty' then head else Boxed_nativeint (ty', alloc_mode)
  | Boxed_vec128 (ty, alloc_mode) ->
    let ty' = apply_renaming ty renaming in
    if ty == ty' then head else Boxed_vec128 (ty', alloc_mode)
  | Closures { by_function_slot; alloc_mode } ->
    let by_function_slot' =
      apply_renaming_row_like_for_closures by_function_slot renaming
    in
    if by_function_slot == by_function_slot'
    then head
    else Closures { by_function_slot = by_function_slot'; alloc_mode }
  | String _ -> head
  | Array { element_kind; length; contents = Unknown; alloc_mode } ->
    let length' = apply_renaming length renaming in
    if length == length'
    then head
    else
      Array { element_kind; length = length'; contents = Unknown; alloc_mode }
  | Array { element_kind; length; contents = Known Mutable; alloc_mode } ->
    let length' = apply_renaming length renaming in
    if length == length'
    then head
    else
      Array
        { element_kind; length = length'; contents = Known Mutable; alloc_mode }
  | Array
      { element_kind;
        length;
        contents = Known (Immutable { fields });
        alloc_mode
      } ->
    let length' = apply_renaming length renaming in
    let fields' =
      Misc.Stdlib.Array.map_sharing
        (fun field -> apply_renaming field renaming)
        fields
    in
    if length == length' && fields == fields'
    then head
    else
      Array
        { element_kind;
          length = length';
          contents = Known (Immutable { fields = fields' });
          alloc_mode
        }

and apply_renaming_head_of_kind_naked_immediate head renaming =
  match head with
  | Naked_immediates _ -> head
  | Is_int ty ->
    let ty' = apply_renaming ty renaming in
    if ty == ty' then head else Is_int ty'
  | Get_tag ty ->
    let ty' = apply_renaming ty renaming in
    if ty == ty' then head else Get_tag ty'

and apply_renaming_head_of_kind_naked_float32 head _ = head

and apply_renaming_head_of_kind_naked_float head _ = head

and apply_renaming_head_of_kind_naked_int32 head _ = head

and apply_renaming_head_of_kind_naked_int64 head _ = head

and apply_renaming_head_of_kind_naked_nativeint head _ = head

and apply_renaming_head_of_kind_naked_vec128 head _ = head

and apply_renaming_head_of_kind_rec_info head renaming =
  Rec_info_expr.apply_renaming head renaming

and apply_renaming_head_of_kind_region () _renaming = ()

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
    ({ known_tags; other_tags; alloc_mode } as row_like_for_tags) renaming =
  match
    apply_renaming_row_like
      ~apply_renaming_index:(fun block_size _ -> block_size)
      ~apply_renaming_maps_to:apply_renaming_int_indexed_product
      ~known:known_tags ~other:other_tags ~map_known:Tag.Map.map_sharing
      renaming
  with
  | None -> row_like_for_tags
  | Some (known_tags, other_tags) -> { known_tags; other_tags; alloc_mode }

and apply_renaming_row_like_for_closures
    ({ known_closures; other_closures } as row_like_for_closures) renaming =
  match
    apply_renaming_row_like
      ~apply_renaming_index:Set_of_closures_contents.apply_renaming
      ~apply_renaming_maps_to:apply_renaming_closures_entry
      ~known:known_closures ~other:other_closures
      ~map_known:Function_slot.Map.map_sharing renaming
  with
  | None -> row_like_for_closures
  | Some (known_closures, other_closures) -> { known_closures; other_closures }

and apply_renaming_closures_entry
    { function_types; closure_types; value_slot_types } renaming =
  { function_types =
      Function_slot.Map.map_sharing
        (fun function_type ->
          Or_unknown_or_bottom.map function_type ~f:(fun function_type ->
              apply_renaming_function_type function_type renaming))
        function_types;
    closure_types =
      apply_renaming_function_slot_indexed_product closure_types renaming;
    value_slot_types =
      apply_renaming_value_slot_indexed_product value_slot_types renaming
  }

and apply_renaming_function_slot_indexed_product
    { function_slot_components_by_index } renaming =
  let function_slot_components_by_index =
    Function_slot.Map.map_sharing
      (fun ty -> apply_renaming ty renaming)
      function_slot_components_by_index
  in
  { function_slot_components_by_index }

and apply_renaming_value_slot_indexed_product { value_slot_components_by_index }
    renaming =
  let value_slot_components_by_index =
    (* CR-someday mshinwell: some loss of sharing here, potentially *)
    Value_slot.Map.filter_map
      (fun value_slot ty ->
        if not (Renaming.value_slot_is_used renaming value_slot)
        then None
        else Some (apply_renaming ty renaming))
      value_slot_components_by_index
  in
  { value_slot_components_by_index }

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

let rec print ppf t =
  match t with
  | Value ty ->
    Format.fprintf ppf "@[<hov 1>(Val@ %a)@]"
      (TD.print ~print_head:print_head_of_kind_value)
      ty
  | Naked_immediate ty ->
    Format.fprintf ppf "@[<hov 1>(Naked_immediate@ %a)@]"
      (TD.print ~print_head:print_head_of_kind_naked_immediate)
      ty
  | Naked_float32 ty ->
    Format.fprintf ppf "@[<hov 1>(Naked_float32@ %a)@]"
      (TD.print ~print_head:print_head_of_kind_naked_float32)
      ty
  | Naked_float ty ->
    Format.fprintf ppf "@[<hov 1>(Naked_float@ %a)@]"
      (TD.print ~print_head:print_head_of_kind_naked_float)
      ty
  | Naked_int32 ty ->
    Format.fprintf ppf "@[<hov 1>(Naked_int32@ %a)@]"
      (TD.print ~print_head:print_head_of_kind_naked_int32)
      ty
  | Naked_int64 ty ->
    Format.fprintf ppf "@[<hov 1>(Naked_int64@ %a)@]"
      (TD.print ~print_head:print_head_of_kind_naked_int64)
      ty
  | Naked_nativeint ty ->
    Format.fprintf ppf "@[<hov 1>(Naked_nativeint@ %a)@]"
      (TD.print ~print_head:print_head_of_kind_naked_nativeint)
      ty
  | Naked_vec128 ty ->
    Format.fprintf ppf "@[<hov 1>(Naked_vec128@ %a)@]"
      (TD.print ~print_head:print_head_of_kind_naked_vec128)
      ty
  | Rec_info ty ->
    Format.fprintf ppf "@[<hov 1>(Rec_info@ %a)@]"
      (TD.print ~print_head:print_head_of_kind_rec_info)
      ty
  | Region ty ->
    Format.fprintf ppf "@[<hov 1>(Region@ %a)@]"
      (TD.print ~print_head:print_head_of_kind_region)
      ty

and print_head_of_kind_value ppf head =
  match head with
  | Variant { blocks; immediates; is_unique } ->
    (* CR-someday mshinwell: Improve so that we elide blocks and/or immediates
       when they're empty. *)
    Format.fprintf ppf
      "@[<hov 1>(Variant%s@ @[<hov 1>(blocks@ %a)@]@ @[<hov 1>(tagged_imms@ \
       %a)@])@]"
      (if is_unique then " unique" else "")
      (Or_unknown.print print_row_like_for_blocks)
      blocks (Or_unknown.print print) immediates
  | Mutable_block { alloc_mode } ->
    Format.fprintf ppf "@[<hov 1>(Mutable_block@ %a)@]"
      Alloc_mode.For_types.print alloc_mode
  | Boxed_float32 (ty, alloc_mode) ->
    Format.fprintf ppf "@[<hov 1>(Boxed_float32@ %a@ %a)@]"
      Alloc_mode.For_types.print alloc_mode print ty
  | Boxed_float (ty, alloc_mode) ->
    Format.fprintf ppf "@[<hov 1>(Boxed_float@ %a@ %a)@]"
      Alloc_mode.For_types.print alloc_mode print ty
  | Boxed_int32 (ty, alloc_mode) ->
    Format.fprintf ppf "@[<hov 1>(Boxed_int32@ %a@ %a)@]"
      Alloc_mode.For_types.print alloc_mode print ty
  | Boxed_int64 (ty, alloc_mode) ->
    Format.fprintf ppf "@[<hov 1>(Boxed_int64@ %a@ %a)@]"
      Alloc_mode.For_types.print alloc_mode print ty
  | Boxed_nativeint (ty, alloc_mode) ->
    Format.fprintf ppf "@[<hov 1>(Boxed_nativeint@ %a@ %a)@]"
      Alloc_mode.For_types.print alloc_mode print ty
  | Boxed_vec128 (ty, alloc_mode) ->
    Format.fprintf ppf "@[<hov 1>(Boxed_vec128@ %a@ %a)@]"
      Alloc_mode.For_types.print alloc_mode print ty
  | Closures { by_function_slot; alloc_mode } ->
    print_row_like_for_closures alloc_mode ppf by_function_slot
  | String str_infos ->
    Format.fprintf ppf "@[<hov 1>(Strings@ (%a))@]" String_info.Set.print
      str_infos
  | Array { element_kind; length; contents = Unknown; alloc_mode } ->
    Format.fprintf ppf
      "@[<hov 1>(Array@ (element_kind@ %a)@ (length@ %a)@ (alloc_mode@ %a))@]"
      (Or_unknown_or_bottom.print Flambda_kind.With_subkind.print)
      element_kind print length Alloc_mode.For_types.print alloc_mode
  | Array { element_kind; length; contents = Known Mutable; alloc_mode } ->
    Format.fprintf ppf
      "@[<hov 1>(Mutable_array@ (element_kind@ %a)@ (length@ %a)@ (alloc_mode@ \
       %a))@]"
      (Or_unknown_or_bottom.print Flambda_kind.With_subkind.print)
      element_kind print length Alloc_mode.For_types.print alloc_mode
  | Array
      { element_kind;
        length;
        contents = Known (Immutable { fields });
        alloc_mode
      } ->
    Format.fprintf ppf
      "@[<hov 1>(Immutable_array@ (element_kind@ %a)@ (length@ %a)@ \
       (alloc_mode@ %a)@ (fields@ (%a)))@]"
      (Or_unknown_or_bottom.print Flambda_kind.With_subkind.print)
      element_kind print length Alloc_mode.For_types.print alloc_mode
      (Format.pp_print_list ~pp_sep:Format.pp_print_space print)
      (Array.to_list fields)

and print_head_of_kind_naked_immediate ppf head =
  match head with
  | Naked_immediates is ->
    Format.fprintf ppf "@[<hov 1>(%a)@]" Targetint_31_63.Set.print is
  | Is_int ty -> Format.fprintf ppf "@[<hov 1>(Is_int@ %a)@]" print ty
  | Get_tag ty -> Format.fprintf ppf "@[<hov 1>(Get_tag@ %a)@]" print ty

and print_head_of_kind_naked_float32 ppf head =
  Format.fprintf ppf "@[(Naked_float32@ (%a))@]" Float32.Set.print head

and print_head_of_kind_naked_float ppf head =
  Format.fprintf ppf "@[(Naked_float@ (%a))@]" Float.Set.print head

and print_head_of_kind_naked_int32 ppf head =
  Format.fprintf ppf "@[(Naked_int32@ (%a))@]" Int32.Set.print head

and print_head_of_kind_naked_int64 ppf head =
  Format.fprintf ppf "@[(Naked_int64@ (%a))@]" Int64.Set.print head

and print_head_of_kind_naked_nativeint ppf head =
  Format.fprintf ppf "@[(Naked_nativeint@ (%a))@]" Targetint_32_64.Set.print
    head

and print_head_of_kind_naked_vec128 ppf head =
  Format.fprintf ppf "@[(Naked_vec128@ (%a))@]" Vec128.Set.print head

and print_head_of_kind_rec_info ppf head = Rec_info_expr.print ppf head

and print_head_of_kind_region ppf () = Format.pp_print_string ppf "Region"

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
      Alloc_mode.For_types.t ->
      Format.formatter ->
      unit =
 fun ~print_index ~print_maps_to ~print_known_map ~is_empty_map_known ~known
     ~other alloc_mode ppf ->
  let print_index ppf = function
    | Known index -> Format.fprintf ppf "(Known @[<2>%a@])" print_index index
    | At_least min_index ->
      Format.fprintf ppf "(At_least @[<2>%a@])" print_index min_index
  in
  if row_like_is_bottom ~known ~other ~is_empty_map_known
  then
    let colour = Flambda_colours.top_or_bottom_type in
    if Flambda_features.unicode ()
    then Format.fprintf ppf "%t@<1>\u{22a5}%t" colour Flambda_colours.pop
    else Format.fprintf ppf "%t_|_%t" colour Flambda_colours.pop
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
      "@[<hov 1>(@[<hov 1>(alloc_mode@ %a)@ (known@ %a)@]@ @[<hov 1>(other@ \
       %a)@])@]"
      Alloc_mode.For_types.print alloc_mode (print_known_map print) known
      (Or_bottom.print print) other

and print_row_like_for_blocks ppf { known_tags; other_tags; alloc_mode } =
  print_row_like ~print_index:Block_size.print
    ~print_maps_to:print_int_indexed_product ~print_known_map:Tag.Map.print
    ~is_empty_map_known:Tag.Map.is_empty ~known:known_tags ~other:other_tags
    alloc_mode ppf

and print_row_like_for_closures alloc_mode ppf
    { known_closures; other_closures } =
  print_row_like ~print_index:Set_of_closures_contents.print
    ~print_maps_to:print_closures_entry ~print_known_map:Function_slot.Map.print
    ~is_empty_map_known:Function_slot.Map.is_empty ~known:known_closures
    ~other:other_closures alloc_mode ppf

and print_closures_entry ppf { function_types; closure_types; value_slot_types }
    =
  Format.fprintf ppf
    "@[<hov 1>(@[<hov 1>(function_types@ %a)@]@ @[<hov 1>(closure_types@ \
     %a)@]@ @[<hov 1>(value_slot_types@ %a)@])@]"
    (Function_slot.Map.print (Or_unknown_or_bottom.print print_function_type))
    function_types print_function_slot_indexed_product closure_types
    print_value_slot_indexed_product value_slot_types

and print_function_slot_indexed_product ppf
    { function_slot_components_by_index } =
  Format.fprintf ppf
    "@[<hov 1>(@[<hov 1>(function_slot_components_by_index@ %a)@])@]"
    (Function_slot.Map.print print)
    function_slot_components_by_index

and print_value_slot_indexed_product ppf { value_slot_components_by_index } =
  Format.fprintf ppf
    "@[<hov 1>(@[<hov 1>(value_slot_components_by_index@ %a)@])@]"
    (Value_slot.Map.print print)
    value_slot_components_by_index

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

let rec ids_for_export t =
  match t with
  | Value ty ->
    TD.ids_for_export ~ids_for_export_head:ids_for_export_head_of_kind_value ty
  | Naked_immediate ty ->
    TD.ids_for_export
      ~ids_for_export_head:ids_for_export_head_of_kind_naked_immediate ty
  | Naked_float32 ty ->
    TD.ids_for_export
      ~ids_for_export_head:ids_for_export_head_of_kind_naked_float32 ty
  | Naked_float ty ->
    TD.ids_for_export
      ~ids_for_export_head:ids_for_export_head_of_kind_naked_float ty
  | Naked_int32 ty ->
    TD.ids_for_export
      ~ids_for_export_head:ids_for_export_head_of_kind_naked_int32 ty
  | Naked_int64 ty ->
    TD.ids_for_export
      ~ids_for_export_head:ids_for_export_head_of_kind_naked_int64 ty
  | Naked_nativeint ty ->
    TD.ids_for_export
      ~ids_for_export_head:ids_for_export_head_of_kind_naked_nativeint ty
  | Naked_vec128 ty ->
    TD.ids_for_export
      ~ids_for_export_head:ids_for_export_head_of_kind_naked_vec128 ty
  | Rec_info ty ->
    TD.ids_for_export ~ids_for_export_head:ids_for_export_head_of_kind_rec_info
      ty
  | Region ty ->
    TD.ids_for_export ~ids_for_export_head:ids_for_export_head_of_kind_region ty

and ids_for_export_head_of_kind_value head =
  match head with
  | Variant { blocks; immediates; is_unique = _ } ->
    Ids_for_export.union
      (Or_unknown.ids_for_export ids_for_export_row_like_for_blocks blocks)
      (Or_unknown.ids_for_export ids_for_export immediates)
  | Mutable_block { alloc_mode = _ } -> Ids_for_export.empty
  | Boxed_float (t, _alloc_mode) -> ids_for_export t
  | Boxed_float32 (t, _alloc_mode) -> ids_for_export t
  | Boxed_int32 (t, _alloc_mode) -> ids_for_export t
  | Boxed_int64 (t, _alloc_mode) -> ids_for_export t
  | Boxed_nativeint (t, _alloc_mode) -> ids_for_export t
  | Boxed_vec128 (t, _alloc_mode) -> ids_for_export t
  | Closures { by_function_slot; alloc_mode = _ } ->
    ids_for_export_row_like_for_closures by_function_slot
  | String _ -> Ids_for_export.empty
  | Array
      { element_kind = _;
        length;
        contents = Unknown | Known Mutable;
        alloc_mode = _
      } ->
    ids_for_export length
  | Array
      { element_kind = _;
        length;
        contents = Known (Immutable { fields });
        alloc_mode = _
      } ->
    Array.fold_left
      (fun ids field -> Ids_for_export.union ids (ids_for_export field))
      (ids_for_export length) fields

and ids_for_export_head_of_kind_naked_immediate head =
  match head with
  | Naked_immediates _ -> Ids_for_export.empty
  | Is_int t | Get_tag t -> ids_for_export t

and ids_for_export_head_of_kind_naked_float32 _ = Ids_for_export.empty

and ids_for_export_head_of_kind_naked_float _ = Ids_for_export.empty

and ids_for_export_head_of_kind_naked_int32 _ = Ids_for_export.empty

and ids_for_export_head_of_kind_naked_int64 _ = Ids_for_export.empty

and ids_for_export_head_of_kind_naked_vec128 _ = Ids_for_export.empty

and ids_for_export_head_of_kind_naked_nativeint _ = Ids_for_export.empty

and ids_for_export_head_of_kind_rec_info head =
  Rec_info_expr.ids_for_export head

and ids_for_export_head_of_kind_region () = Ids_for_export.empty

and ids_for_export_row_like :
      'row_tag 'index 'maps_to 'known.
      ids_for_export_maps_to:('maps_to -> Ids_for_export.t) ->
      known:'known ->
      other:('index, 'maps_to) row_like_case Or_bottom.t ->
      fold_known:
        (('row_tag -> ('index, 'maps_to) row_like_case -> 'acc -> 'acc) ->
        'known ->
        'acc ->
        'acc) ->
      Ids_for_export.t =
 fun ~ids_for_export_maps_to ~known ~other ~fold_known ->
  let from_known =
    fold_known
      (fun _tag { maps_to; env_extension; index = _ } ids ->
        Ids_for_export.union ids
          (Ids_for_export.union
             (ids_for_export_maps_to maps_to)
             (ids_for_export_env_extension env_extension)))
      known Ids_for_export.empty
  in
  match other with
  | Bottom -> from_known
  | Ok { maps_to; env_extension; index = _ } ->
    Ids_for_export.union
      (ids_for_export_maps_to maps_to)
      (Ids_for_export.union from_known
         (ids_for_export_env_extension env_extension))

and ids_for_export_row_like_for_blocks
    { known_tags; other_tags; alloc_mode = _ } =
  ids_for_export_row_like
    ~ids_for_export_maps_to:ids_for_export_int_indexed_product ~known:known_tags
    ~other:other_tags ~fold_known:Tag.Map.fold

and ids_for_export_row_like_for_closures { known_closures; other_closures } =
  ids_for_export_row_like ~ids_for_export_maps_to:ids_for_export_closures_entry
    ~known:known_closures ~other:other_closures
    ~fold_known:Function_slot.Map.fold

and ids_for_export_closures_entry
    { function_types; closure_types; value_slot_types } =
  let function_types_ids =
    Function_slot.Map.fold
      (fun _function_slot (function_type : _ Or_unknown_or_bottom.t) ids ->
        match function_type with
        | Unknown | Bottom -> ids
        | Ok function_type ->
          Ids_for_export.union ids (ids_for_export_function_type function_type))
      function_types Ids_for_export.empty
  in
  Ids_for_export.union function_types_ids
    (Ids_for_export.union
       (ids_for_export_function_slot_indexed_product closure_types)
       (ids_for_export_value_slot_indexed_product value_slot_types))

and ids_for_export_function_slot_indexed_product
    { function_slot_components_by_index } =
  Function_slot.Map.fold
    (fun _ t ids -> Ids_for_export.union ids (ids_for_export t))
    function_slot_components_by_index Ids_for_export.empty

and ids_for_export_value_slot_indexed_product { value_slot_components_by_index }
    =
  Value_slot.Map.fold
    (fun _ t ids -> Ids_for_export.union ids (ids_for_export t))
    value_slot_components_by_index Ids_for_export.empty

and ids_for_export_int_indexed_product { fields; kind = _ } =
  Array.fold_left
    (fun ids field -> Ids_for_export.union ids (ids_for_export field))
    Ids_for_export.empty fields

and ids_for_export_function_type { code_id; rec_info } =
  Ids_for_export.union
    (Ids_for_export.singleton_code_id code_id)
    (ids_for_export rec_info)

and ids_for_export_env_extension { equations } =
  Name.Map.fold
    (fun name t ids ->
      Ids_for_export.add_name (Ids_for_export.union ids (ids_for_export t)) name)
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
          coercion ty
      in
      if ty == ty' then t else Value ty'
    | Naked_immediate ty ->
      let<+ ty' =
        TD.apply_coercion
          ~apply_coercion_head:apply_coercion_head_of_kind_naked_immediate
          coercion ty
      in
      if ty == ty' then t else Naked_immediate ty'
    | Naked_float32 ty ->
      let<+ ty' =
        TD.apply_coercion
          ~apply_coercion_head:apply_coercion_head_of_kind_naked_float32
          coercion ty
      in
      if ty == ty' then t else Naked_float32 ty'
    | Naked_float ty ->
      let<+ ty' =
        TD.apply_coercion
          ~apply_coercion_head:apply_coercion_head_of_kind_naked_float coercion
          ty
      in
      if ty == ty' then t else Naked_float ty'
    | Naked_int32 ty ->
      let<+ ty' =
        TD.apply_coercion
          ~apply_coercion_head:apply_coercion_head_of_kind_naked_int32 coercion
          ty
      in
      if ty == ty' then t else Naked_int32 ty'
    | Naked_int64 ty ->
      let<+ ty' =
        TD.apply_coercion
          ~apply_coercion_head:apply_coercion_head_of_kind_naked_int64 coercion
          ty
      in
      if ty == ty' then t else Naked_int64 ty'
    | Naked_nativeint ty ->
      let<+ ty' =
        TD.apply_coercion
          ~apply_coercion_head:apply_coercion_head_of_kind_naked_nativeint
          coercion ty
      in
      if ty == ty' then t else Naked_nativeint ty'
    | Naked_vec128 ty ->
      let<+ ty' =
        TD.apply_coercion
          ~apply_coercion_head:apply_coercion_head_of_kind_naked_vec128 coercion
          ty
      in
      if ty == ty' then t else Naked_vec128 ty'
    | Rec_info ty ->
      let<+ ty' =
        TD.apply_coercion
          ~apply_coercion_head:apply_coercion_head_of_kind_rec_info coercion ty
      in
      if ty == ty' then t else Rec_info ty'
    | Region ty ->
      let<+ ty' =
        TD.apply_coercion
          ~apply_coercion_head:apply_coercion_head_of_kind_region coercion ty
      in
      if ty == ty' then t else Region ty'

and apply_coercion_head_of_kind_value head coercion : _ Or_bottom.t =
  match head with
  | Closures { by_function_slot; alloc_mode } ->
    let<+ by_function_slot' =
      apply_coercion_row_like_for_closures by_function_slot coercion
    in
    if by_function_slot == by_function_slot'
    then head
    else Closures { by_function_slot = by_function_slot'; alloc_mode }
  | Variant _ ->
    (* See the comment on [apply_coercion]. The situation for variants (sums) is
       similar to that for tuples (products): we would want a coercion for each
       branch. *)
    if Coercion.is_id coercion then Ok head else Bottom
  | Mutable_block { alloc_mode = _ } -> Ok head
  | Boxed_float _ | Boxed_float32 _ ->
    (* Even if we had coercions that would act on float constants, we would want
       to have a [Boxed_float] wrapper that would lift a float coercion to a
       value coercion. *)
    if Coercion.is_id coercion then Ok head else Bottom
  | Boxed_int32 _ | Boxed_int64 _ | Boxed_nativeint _ | Boxed_vec128 _
  | String _ ->
    (* Similarly, we don't have lifted coercions for these. *)
    if Coercion.is_id coercion then Ok head else Bottom
  | Array
      { element_kind = _;
        length = _;
        contents = Unknown | Known Mutable;
        alloc_mode = _
      } ->
    (* This one's a bit more obvious: we wouldn't want to accidentally treat a
       coercion on integers as a coercion on array lengths. *)
    if Coercion.is_id coercion then Ok head else Bottom
  | Array
      { element_kind = _;
        length = _;
        contents = Known (Immutable { fields = _ });
        alloc_mode = _
      } ->
    (* Same as the block case (in [Variant]) above. *)
    if Coercion.is_id coercion then Ok head else Bottom

and apply_coercion_head_of_kind_naked_immediate head coercion : _ Or_bottom.t =
  if Coercion.is_id coercion then Ok head else Bottom

and apply_coercion_head_of_kind_naked_float32 head coercion : _ Or_bottom.t =
  if Coercion.is_id coercion then Ok head else Bottom

and apply_coercion_head_of_kind_naked_float head coercion : _ Or_bottom.t =
  if Coercion.is_id coercion then Ok head else Bottom

and apply_coercion_head_of_kind_naked_int32 head coercion : _ Or_bottom.t =
  if Coercion.is_id coercion then Ok head else Bottom

and apply_coercion_head_of_kind_naked_int64 head coercion : _ Or_bottom.t =
  if Coercion.is_id coercion then Ok head else Bottom

and apply_coercion_head_of_kind_naked_nativeint head coercion : _ Or_bottom.t =
  if Coercion.is_id coercion then Ok head else Bottom

and apply_coercion_head_of_kind_naked_vec128 head coercion : _ Or_bottom.t =
  if Coercion.is_id coercion then Ok head else Bottom

and apply_coercion_head_of_kind_rec_info head coercion : _ Or_bottom.t =
  (* Currently no coercion has an effect on a depth variable and
     [Rec_info_expr.t] does not contain any other variety of name. *)
  if Coercion.is_id coercion then Ok head else Bottom

and apply_coercion_head_of_kind_region () _coercion : _ Or_bottom.t = Ok ()

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
      ~is_empty_map_known:Function_slot.Map.is_empty
      ~filter_map_known:Function_slot.Map.filter_map coercion
  in
  { known_closures = known; other_closures = other }

and apply_coercion_closures_entry row_tag
    { function_types; closure_types; value_slot_types } coercion : _ Or_bottom.t
    =
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
    ignore (row_tag : Function_slot.t option);
    coercion
  in
  let function_types =
    (* Somewhat hackily apply the same coercion to everything in the set of
       closures. After all, we're only adjusting recursion depth, and all
       closures in the same set have the same depth. *)
    (* CR lmaurer: Check that this is consistent with the simplifier's behavior.
       In particular, [project_function_slot] should return a closure at the
       same depth as the original closure. *)
    (* Exhaustingly, this is _entirely orthogonal_ to the issue with closures
       having row-like types. *)
    Function_slot.Map.map_sharing
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
    let<+ value_slot_types =
      apply_coercion_to_value_slot_types_in_set value_slot_types coercion
    in
    { function_types; closure_types; value_slot_types }

and apply_coercion_to_closure_types_in_set
    ({ function_slot_components_by_index } as product) coercion : _ Or_bottom.t
    =
  let found_bottom = ref false in
  let function_slot_components_by_index' =
    (* Again, just apply the same coercion to everything in the set of closures
       (see comment on [function_types] above).

       We're _also_ once again conflating the coercion applied to the
       variant-like closure type with the coercion applied to the row. If we had
       row-like coercions, we would instead construct a singleton for each
       closure (just as [product] has a singleton row-like type for each
       closure).

       CR-someday lmaurer: Fix this once we fix [apply_coercion_closures_entry]
       (presumably by adding row-like coercions). *)
    Function_slot.Map.map_sharing
      (fun t ->
        match apply_coercion t coercion with
        | Bottom ->
          found_bottom := true;
          t
        | Ok t -> t)
      function_slot_components_by_index
  in
  if !found_bottom
  then Bottom
  else if function_slot_components_by_index
          == function_slot_components_by_index'
  then Ok product
  else
    Ok
      { function_slot_components_by_index = function_slot_components_by_index' }

and apply_coercion_to_value_slot_types_in_set product _coercion : _ Or_bottom.t
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

let rec remove_unused_value_slots_and_shortcut_aliases t ~used_value_slots
    ~canonicalise =
  match t with
  | Value ty ->
    let ty' =
      TD.remove_unused_value_slots_and_shortcut_aliases ty ~used_value_slots
        ~canonicalise
        ~remove_unused_value_slots_and_shortcut_aliases_head:
          remove_unused_value_slots_and_shortcut_aliases_head_of_kind_value
    in
    if ty == ty' then t else Value ty'
  | Naked_immediate ty ->
    let ty' =
      TD.remove_unused_value_slots_and_shortcut_aliases ty ~used_value_slots
        ~canonicalise
        ~remove_unused_value_slots_and_shortcut_aliases_head:
          remove_unused_value_slots_and_shortcut_aliases_head_of_kind_naked_immediate
    in
    if ty == ty' then t else Naked_immediate ty'
  | Naked_float32 ty ->
    let ty' =
      TD.remove_unused_value_slots_and_shortcut_aliases ty ~used_value_slots
        ~canonicalise
        ~remove_unused_value_slots_and_shortcut_aliases_head:
          remove_unused_value_slots_and_shortcut_aliases_head_of_kind_naked_float32
    in
    if ty == ty' then t else Naked_float32 ty'
  | Naked_float ty ->
    let ty' =
      TD.remove_unused_value_slots_and_shortcut_aliases ty ~used_value_slots
        ~canonicalise
        ~remove_unused_value_slots_and_shortcut_aliases_head:
          remove_unused_value_slots_and_shortcut_aliases_head_of_kind_naked_float
    in
    if ty == ty' then t else Naked_float ty'
  | Naked_int32 ty ->
    let ty' =
      TD.remove_unused_value_slots_and_shortcut_aliases ty ~used_value_slots
        ~canonicalise
        ~remove_unused_value_slots_and_shortcut_aliases_head:
          remove_unused_value_slots_and_shortcut_aliases_head_of_kind_naked_int32
    in
    if ty == ty' then t else Naked_int32 ty'
  | Naked_int64 ty ->
    let ty' =
      TD.remove_unused_value_slots_and_shortcut_aliases ty ~used_value_slots
        ~canonicalise
        ~remove_unused_value_slots_and_shortcut_aliases_head:
          remove_unused_value_slots_and_shortcut_aliases_head_of_kind_naked_int64
    in
    if ty == ty' then t else Naked_int64 ty'
  | Naked_nativeint ty ->
    let ty' =
      TD.remove_unused_value_slots_and_shortcut_aliases ty ~used_value_slots
        ~canonicalise
        ~remove_unused_value_slots_and_shortcut_aliases_head:
          remove_unused_value_slots_and_shortcut_aliases_head_of_kind_naked_nativeint
    in
    if ty == ty' then t else Naked_nativeint ty'
  | Naked_vec128 ty ->
    let ty' =
      TD.remove_unused_value_slots_and_shortcut_aliases ty ~used_value_slots
        ~canonicalise
        ~remove_unused_value_slots_and_shortcut_aliases_head:
          remove_unused_value_slots_and_shortcut_aliases_head_of_kind_naked_vec128
    in
    if ty == ty' then t else Naked_vec128 ty'
  | Rec_info ty ->
    let ty' =
      TD.remove_unused_value_slots_and_shortcut_aliases ty ~used_value_slots
        ~canonicalise
        ~remove_unused_value_slots_and_shortcut_aliases_head:
          remove_unused_value_slots_and_shortcut_aliases_head_of_kind_rec_info
    in
    if ty == ty' then t else Rec_info ty'
  | Region ty ->
    let ty' =
      TD.remove_unused_value_slots_and_shortcut_aliases ty ~used_value_slots
        ~canonicalise
        ~remove_unused_value_slots_and_shortcut_aliases_head:
          remove_unused_value_slots_and_shortcut_aliases_head_of_kind_region
    in
    if ty == ty' then t else Region ty'

and remove_unused_value_slots_and_shortcut_aliases_head_of_kind_value head
    ~used_value_slots ~canonicalise =
  match head with
  | Variant { blocks; immediates; is_unique } ->
    let immediates' =
      let>+$ immediates = immediates in
      remove_unused_value_slots_and_shortcut_aliases immediates
        ~used_value_slots ~canonicalise
    in
    let blocks' =
      let>+$ blocks = blocks in
      remove_unused_value_slots_and_shortcut_aliases_row_like_for_blocks blocks
        ~used_value_slots ~canonicalise
    in
    if immediates == immediates' && blocks == blocks'
    then head
    else Variant { is_unique; blocks = blocks'; immediates = immediates' }
  | Mutable_block { alloc_mode = _ } -> head
  | Boxed_float32 (ty, alloc_mode) ->
    let ty' =
      remove_unused_value_slots_and_shortcut_aliases ty ~used_value_slots
        ~canonicalise
    in
    if ty == ty' then head else Boxed_float32 (ty', alloc_mode)
  | Boxed_float (ty, alloc_mode) ->
    let ty' =
      remove_unused_value_slots_and_shortcut_aliases ty ~used_value_slots
        ~canonicalise
    in
    if ty == ty' then head else Boxed_float (ty', alloc_mode)
  | Boxed_int32 (ty, alloc_mode) ->
    let ty' =
      remove_unused_value_slots_and_shortcut_aliases ty ~used_value_slots
        ~canonicalise
    in
    if ty == ty' then head else Boxed_int32 (ty', alloc_mode)
  | Boxed_int64 (ty, alloc_mode) ->
    let ty' =
      remove_unused_value_slots_and_shortcut_aliases ty ~used_value_slots
        ~canonicalise
    in
    if ty == ty' then head else Boxed_int64 (ty', alloc_mode)
  | Boxed_nativeint (ty, alloc_mode) ->
    let ty' =
      remove_unused_value_slots_and_shortcut_aliases ty ~used_value_slots
        ~canonicalise
    in
    if ty == ty' then head else Boxed_nativeint (ty', alloc_mode)
  | Boxed_vec128 (ty, alloc_mode) ->
    let ty' =
      remove_unused_value_slots_and_shortcut_aliases ty ~used_value_slots
        ~canonicalise
    in
    if ty == ty' then head else Boxed_vec128 (ty', alloc_mode)
  | Closures { by_function_slot; alloc_mode } ->
    let by_function_slot' =
      remove_unused_value_slots_and_shortcut_aliases_row_like_for_closures
        by_function_slot ~used_value_slots ~canonicalise
    in
    if by_function_slot == by_function_slot'
    then head
    else Closures { by_function_slot = by_function_slot'; alloc_mode }
  | String _ -> head
  | Array { element_kind; length; contents = Unknown; alloc_mode } ->
    let length' =
      remove_unused_value_slots_and_shortcut_aliases length ~used_value_slots
        ~canonicalise
    in
    if length == length'
    then head
    else
      Array { element_kind; length = length'; contents = Unknown; alloc_mode }
  | Array { element_kind; length; contents = Known Mutable; alloc_mode } ->
    let length' =
      remove_unused_value_slots_and_shortcut_aliases length ~used_value_slots
        ~canonicalise
    in
    if length == length'
    then head
    else
      Array
        { element_kind; length = length'; contents = Known Mutable; alloc_mode }
  | Array
      { element_kind;
        length;
        contents = Known (Immutable { fields });
        alloc_mode
      } ->
    let length' =
      remove_unused_value_slots_and_shortcut_aliases length ~used_value_slots
        ~canonicalise
    in
    let fields' =
      Misc.Stdlib.Array.map_sharing
        (remove_unused_value_slots_and_shortcut_aliases ~used_value_slots
           ~canonicalise)
        fields
    in
    if length == length' && fields == fields'
    then head
    else
      Array
        { element_kind;
          length = length';
          contents = Known (Immutable { fields = fields' });
          alloc_mode
        }

and remove_unused_value_slots_and_shortcut_aliases_head_of_kind_naked_immediate
    head ~used_value_slots ~canonicalise =
  match head with
  | Naked_immediates _ -> head
  | Is_int ty ->
    let ty' =
      remove_unused_value_slots_and_shortcut_aliases ty ~used_value_slots
        ~canonicalise
    in
    if ty == ty' then head else Is_int ty'
  | Get_tag ty ->
    let ty' =
      remove_unused_value_slots_and_shortcut_aliases ty ~used_value_slots
        ~canonicalise
    in
    if ty == ty' then head else Get_tag ty'

and remove_unused_value_slots_and_shortcut_aliases_head_of_kind_naked_float32
    head ~used_value_slots:_ ~canonicalise:_ =
  head

and remove_unused_value_slots_and_shortcut_aliases_head_of_kind_naked_float head
    ~used_value_slots:_ ~canonicalise:_ =
  head

and remove_unused_value_slots_and_shortcut_aliases_head_of_kind_naked_int32 head
    ~used_value_slots:_ ~canonicalise:_ =
  head

and remove_unused_value_slots_and_shortcut_aliases_head_of_kind_naked_int64 head
    ~used_value_slots:_ ~canonicalise:_ =
  head

and remove_unused_value_slots_and_shortcut_aliases_head_of_kind_naked_nativeint
    head ~used_value_slots:_ ~canonicalise:_ =
  head

and remove_unused_value_slots_and_shortcut_aliases_head_of_kind_naked_vec128
    head ~used_value_slots:_ ~canonicalise:_ =
  head

and remove_unused_value_slots_and_shortcut_aliases_head_of_kind_rec_info head
    ~used_value_slots:_ ~canonicalise:_ =
  head

and remove_unused_value_slots_and_shortcut_aliases_head_of_kind_region ()
    ~used_value_slots:_ ~canonicalise:_ =
  ()

and remove_unused_value_slots_and_shortcut_aliases_row_like :
      'index 'maps_to 'known.
      remove_unused_value_slots_and_shortcut_aliases_index:
        ('index ->
        used_value_slots:Value_slot.Set.t ->
        canonicalise:(Simple.t -> Simple.t) ->
        'index) ->
      remove_unused_value_slots_and_shortcut_aliases_maps_to:
        ('maps_to ->
        used_value_slots:Value_slot.Set.t ->
        canonicalise:(Simple.t -> Simple.t) ->
        'maps_to) ->
      known:'known ->
      other:('index, 'maps_to) row_like_case Or_bottom.t ->
      map_known:
        ((('index, 'maps_to) row_like_case -> ('index, 'maps_to) row_like_case) ->
        'known ->
        'known) ->
      used_value_slots:Value_slot.Set.t ->
      canonicalise:(Simple.t -> Simple.t) ->
      ('known * ('index, 'maps_to) row_like_case Or_bottom.t) option =
 fun ~remove_unused_value_slots_and_shortcut_aliases_index
     ~remove_unused_value_slots_and_shortcut_aliases_maps_to ~known ~other
     ~map_known ~used_value_slots ~canonicalise ->
  let[@inline always] remove_unused_value_slots_and_shortcut_aliases_index =
    function
    | Known index ->
      Known
        (remove_unused_value_slots_and_shortcut_aliases_index index
           ~used_value_slots ~canonicalise)
    | At_least index ->
      At_least
        (remove_unused_value_slots_and_shortcut_aliases_index index
           ~used_value_slots ~canonicalise)
  in
  let known' =
    map_known
      (fun { index; maps_to; env_extension } ->
        { index = remove_unused_value_slots_and_shortcut_aliases_index index;
          env_extension =
            remove_unused_value_slots_and_shortcut_aliases_env_extension
              env_extension ~used_value_slots ~canonicalise;
          maps_to =
            remove_unused_value_slots_and_shortcut_aliases_maps_to maps_to
              ~used_value_slots ~canonicalise
        })
      known
  in
  let other' : _ Or_bottom.t =
    match other with
    | Bottom -> Bottom
    | Ok { index; maps_to; env_extension } ->
      (* CR mshinwell: phys-equal tests here and elsewhere are inadequate *)
      Ok
        { index = remove_unused_value_slots_and_shortcut_aliases_index index;
          env_extension =
            remove_unused_value_slots_and_shortcut_aliases_env_extension
              env_extension ~used_value_slots ~canonicalise;
          maps_to =
            remove_unused_value_slots_and_shortcut_aliases_maps_to maps_to
              ~used_value_slots ~canonicalise
        }
  in
  if known == known' && other == other' then None else Some (known', other')

and remove_unused_value_slots_and_shortcut_aliases_row_like_for_blocks
    ({ known_tags; other_tags; alloc_mode } as row_like_for_tags)
    ~used_value_slots ~canonicalise =
  match
    remove_unused_value_slots_and_shortcut_aliases_row_like
      ~remove_unused_value_slots_and_shortcut_aliases_index:
        (fun block_size ~used_value_slots:_ ~canonicalise:_ -> block_size)
      ~remove_unused_value_slots_and_shortcut_aliases_maps_to:
        remove_unused_value_slots_and_shortcut_aliases_int_indexed_product
      ~known:known_tags ~other:other_tags ~map_known:Tag.Map.map_sharing
      ~used_value_slots ~canonicalise
  with
  | None -> row_like_for_tags
  | Some (known_tags, other_tags) -> { known_tags; other_tags; alloc_mode }

and remove_unused_value_slots_and_shortcut_aliases_row_like_for_closures
    ({ known_closures; other_closures } as row_like_for_closures)
    ~used_value_slots ~canonicalise =
  match
    remove_unused_value_slots_and_shortcut_aliases_row_like
      ~remove_unused_value_slots_and_shortcut_aliases_index:
        (fun index ~used_value_slots ~canonicalise:_ ->
        Set_of_closures_contents.remove_unused_value_slots index
          ~used_value_slots)
      ~remove_unused_value_slots_and_shortcut_aliases_maps_to:
        remove_unused_value_slots_and_shortcut_aliases_closures_entry
      ~known:known_closures ~other:other_closures
      ~map_known:Function_slot.Map.map_sharing ~used_value_slots ~canonicalise
  with
  | None -> row_like_for_closures
  | Some (known_closures, other_closures) -> { known_closures; other_closures }

and remove_unused_value_slots_and_shortcut_aliases_closures_entry
    { function_types; closure_types; value_slot_types } ~used_value_slots
    ~canonicalise =
  { function_types =
      Function_slot.Map.map_sharing
        (fun function_type ->
          Or_unknown_or_bottom.map function_type ~f:(fun function_type ->
              remove_unused_value_slots_and_shortcut_aliases_function_type
                function_type ~used_value_slots ~canonicalise))
        function_types;
    closure_types =
      remove_unused_value_slots_and_shortcut_aliases_function_slot_indexed_product
        closure_types ~used_value_slots ~canonicalise;
    value_slot_types =
      remove_unused_value_slots_and_shortcut_aliases_value_slot_indexed_product
        value_slot_types ~used_value_slots ~canonicalise
  }

and remove_unused_value_slots_and_shortcut_aliases_function_slot_indexed_product
    { function_slot_components_by_index } ~used_value_slots ~canonicalise =
  let function_slot_components_by_index =
    Function_slot.Map.map_sharing
      (fun ty ->
        remove_unused_value_slots_and_shortcut_aliases ty ~used_value_slots
          ~canonicalise)
      function_slot_components_by_index
  in
  { function_slot_components_by_index }

and remove_unused_value_slots_and_shortcut_aliases_value_slot_indexed_product
    { value_slot_components_by_index } ~used_value_slots ~canonicalise =
  let value_slot_components_by_index =
    (* CR-someday mshinwell: some loss of sharing here, potentially *)
    Value_slot.Map.filter_map
      (fun value_slot ty ->
        if (not
              (Value_slot.in_compilation_unit value_slot
                 (Compilation_unit.get_current_exn ())))
           || Value_slot.Set.mem value_slot used_value_slots
        then
          Some
            (remove_unused_value_slots_and_shortcut_aliases ty ~used_value_slots
               ~canonicalise)
        else None)
      value_slot_components_by_index
  in
  { value_slot_components_by_index }

and remove_unused_value_slots_and_shortcut_aliases_int_indexed_product
    { fields; kind } ~used_value_slots ~canonicalise =
  let fields = Array.copy fields in
  for i = 0 to Array.length fields - 1 do
    fields.(i)
      <- remove_unused_value_slots_and_shortcut_aliases fields.(i)
           ~used_value_slots ~canonicalise
  done;
  { fields; kind }

and remove_unused_value_slots_and_shortcut_aliases_function_type
    ({ code_id; rec_info } as function_type) ~used_value_slots ~canonicalise =
  let rec_info' =
    remove_unused_value_slots_and_shortcut_aliases rec_info ~used_value_slots
      ~canonicalise
  in
  if rec_info == rec_info'
  then function_type
  else { code_id; rec_info = rec_info' }

and remove_unused_value_slots_and_shortcut_aliases_env_extension
    ({ equations } as env_extension) ~used_value_slots ~canonicalise =
  let changed = ref false in
  let equations' =
    Name.Map.map_sharing
      (fun ty ->
        remove_unused_value_slots_and_shortcut_aliases ty ~used_value_slots
          ~canonicalise)
      equations
  in
  if !changed then { equations = equations' } else env_extension

let rec project_variables_out ~to_project ~expand t =
  match t with
  | Value ty ->
    let expand_with_coercion var ~coercion =
      match apply_coercion (expand var) coercion with
      | Value ty -> ty
      | ( Naked_immediate _ | Naked_float _ | Naked_float32 _ | Naked_int32 _
        | Naked_int64 _ | Naked_vec128 _ | Naked_nativeint _ | Rec_info _
        | Region _ ) as ty ->
        Misc.fatal_errorf
          "Wrong kind while expanding %a: expecting [Value], got type %a"
          Variable.print var print ty
    in
    let ty' =
      TD.project_variables_out ~free_names_head:free_names_head_of_kind_value
        ~to_project ~expand:expand_with_coercion
        ~project_head:(project_head_of_kind_value ~to_project ~expand)
        ty
    in
    if ty == ty' then t else Value ty'
  | Naked_immediate ty ->
    let expand_with_coercion var ~coercion =
      match apply_coercion (expand var) coercion with
      | Naked_immediate ty -> ty
      | ( Value _ | Naked_float _ | Naked_float32 _ | Naked_int32 _
        | Naked_int64 _ | Naked_vec128 _ | Naked_nativeint _ | Rec_info _
        | Region _ ) as ty ->
        Misc.fatal_errorf
          "Wrong kind while expanding %a: expecting [Naked_immediate], got \
           type %a"
          Variable.print var print ty
    in
    let ty' =
      TD.project_variables_out
        ~free_names_head:free_names_head_of_kind_naked_immediate ~to_project
        ~expand:expand_with_coercion
        ~project_head:(project_head_of_kind_naked_immediate ~to_project ~expand)
        ty
    in
    if ty == ty' then t else Naked_immediate ty'
  | Naked_float32 ty ->
    let expand_with_coercion var ~coercion =
      match apply_coercion (expand var) coercion with
      | Naked_float32 ty -> ty
      | ( Value _ | Naked_immediate _ | Naked_int32 _ | Naked_float _
        | Naked_int64 _ | Naked_vec128 _ | Naked_nativeint _ | Rec_info _
        | Region _ ) as ty ->
        Misc.fatal_errorf
          "Wrong kind while expanding %a: expecting [Naked_float], got type %a"
          Variable.print var print ty
    in
    let ty' =
      TD.project_variables_out
        ~free_names_head:free_names_head_of_kind_naked_float32 ~to_project
        ~expand:expand_with_coercion
        ~project_head:(project_head_of_kind_naked_float32 ~to_project ~expand)
        ty
    in
    if ty == ty' then t else Naked_float32 ty'
  | Naked_float ty ->
    let expand_with_coercion var ~coercion =
      match apply_coercion (expand var) coercion with
      | Naked_float ty -> ty
      | ( Value _ | Naked_immediate _ | Naked_int32 _ | Naked_float32 _
        | Naked_int64 _ | Naked_vec128 _ | Naked_nativeint _ | Rec_info _
        | Region _ ) as ty ->
        Misc.fatal_errorf
          "Wrong kind while expanding %a: expecting [Naked_float], got type %a"
          Variable.print var print ty
    in
    let ty' =
      TD.project_variables_out
        ~free_names_head:free_names_head_of_kind_naked_float ~to_project
        ~expand:expand_with_coercion
        ~project_head:(project_head_of_kind_naked_float ~to_project ~expand)
        ty
    in
    if ty == ty' then t else Naked_float ty'
  | Naked_int32 ty ->
    let expand_with_coercion var ~coercion =
      match apply_coercion (expand var) coercion with
      | Naked_int32 ty -> ty
      | ( Value _ | Naked_immediate _ | Naked_float _ | Naked_float32 _
        | Naked_int64 _ | Naked_vec128 _ | Naked_nativeint _ | Rec_info _
        | Region _ ) as ty ->
        Misc.fatal_errorf
          "Wrong kind while expanding %a: expecting [Naked_int32], got type %a"
          Variable.print var print ty
    in
    let ty' =
      TD.project_variables_out
        ~free_names_head:free_names_head_of_kind_naked_int32 ~to_project
        ~expand:expand_with_coercion
        ~project_head:(project_head_of_kind_naked_int32 ~to_project ~expand)
        ty
    in
    if ty == ty' then t else Naked_int32 ty'
  | Naked_int64 ty ->
    let expand_with_coercion var ~coercion =
      match apply_coercion (expand var) coercion with
      | Naked_int64 ty -> ty
      | ( Value _ | Naked_immediate _ | Naked_float _ | Naked_float32 _
        | Naked_int32 _ | Naked_vec128 _ | Naked_nativeint _ | Rec_info _
        | Region _ ) as ty ->
        Misc.fatal_errorf
          "Wrong kind while expanding %a: expecting [Naked_int64], got type %a"
          Variable.print var print ty
    in
    let ty' =
      TD.project_variables_out
        ~free_names_head:free_names_head_of_kind_naked_int64 ~to_project
        ~expand:expand_with_coercion
        ~project_head:(project_head_of_kind_naked_int64 ~to_project ~expand)
        ty
    in
    if ty == ty' then t else Naked_int64 ty'
  | Naked_nativeint ty ->
    let expand_with_coercion var ~coercion =
      match apply_coercion (expand var) coercion with
      | Naked_nativeint ty -> ty
      | ( Value _ | Naked_immediate _ | Naked_float _ | Naked_float32 _
        | Naked_int32 _ | Naked_vec128 _ | Naked_int64 _ | Rec_info _ | Region _
          ) as ty ->
        Misc.fatal_errorf
          "Wrong kind while expanding %a: expecting [Naked_nativeint], got \
           type %a"
          Variable.print var print ty
    in
    let ty' =
      TD.project_variables_out
        ~free_names_head:free_names_head_of_kind_naked_nativeint ~to_project
        ~expand:expand_with_coercion
        ~project_head:(project_head_of_kind_naked_nativeint ~to_project ~expand)
        ty
    in
    if ty == ty' then t else Naked_nativeint ty'
  | Naked_vec128 ty ->
    let expand_with_coercion var ~coercion =
      match apply_coercion (expand var) coercion with
      | Naked_vec128 ty -> ty
      | ( Value _ | Naked_immediate _ | Naked_float _ | Naked_float32 _
        | Naked_int32 _ | Naked_nativeint _ | Naked_int64 _ | Rec_info _
        | Region _ ) as ty ->
        Misc.fatal_errorf
          "Wrong kind while expanding %a: expecting [Naked_vec128], got type %a"
          Variable.print var print ty
    in
    let ty' =
      TD.project_variables_out
        ~free_names_head:free_names_head_of_kind_naked_vec128 ~to_project
        ~expand:expand_with_coercion
        ~project_head:(project_head_of_kind_naked_vec128 ~to_project ~expand)
        ty
    in
    if ty == ty' then t else Naked_vec128 ty'
  | Rec_info ty ->
    let expand_with_coercion var ~coercion =
      match apply_coercion (expand var) coercion with
      | Rec_info ty -> ty
      | ( Value _ | Naked_immediate _ | Naked_float _ | Naked_float32 _
        | Naked_int32 _ | Naked_vec128 _ | Naked_int64 _ | Naked_nativeint _
        | Region _ ) as ty ->
        Misc.fatal_errorf
          "Wrong kind while expanding %a: expecting [Rec_info], got type %a"
          Variable.print var print ty
    in
    let ty' =
      TD.project_variables_out ~free_names_head:free_names_head_of_kind_rec_info
        ~to_project ~expand:expand_with_coercion
        ~project_head:(project_head_of_kind_rec_info ~to_project ~expand)
        ty
    in
    if ty == ty' then t else Rec_info ty'
  | Region ty ->
    let expand_with_coercion var ~coercion =
      match apply_coercion (expand var) coercion with
      | Region ty -> ty
      | ( Value _ | Naked_immediate _ | Naked_float _ | Naked_float32 _
        | Naked_int32 _ | Naked_vec128 _ | Naked_int64 _ | Naked_nativeint _
        | Rec_info _ ) as ty ->
        Misc.fatal_errorf
          "Wrong kind while expanding %a: expecting [Region], got type %a"
          Variable.print var print ty
    in
    let ty' =
      TD.project_variables_out ~free_names_head:free_names_head_of_kind_region
        ~to_project ~expand:expand_with_coercion
        ~project_head:(project_head_of_kind_region ~to_project ~expand)
        ty
    in
    if ty == ty' then t else Region ty'

and project_head_of_kind_value ~to_project ~expand head =
  match head with
  | Variant { blocks; immediates; is_unique } ->
    let immediates' =
      let>+$ immediates = immediates in
      project_variables_out ~to_project ~expand immediates
    in
    let blocks' =
      let>+$ blocks = blocks in
      project_row_like_for_blocks ~to_project ~expand blocks
    in
    if immediates == immediates' && blocks == blocks'
    then head
    else Variant { is_unique; blocks = blocks'; immediates = immediates' }
  | Mutable_block _ -> head
  | Boxed_float32 (ty, alloc_mode) ->
    let ty' = project_variables_out ~to_project ~expand ty in
    if ty == ty' then head else Boxed_float32 (ty', alloc_mode)
  | Boxed_float (ty, alloc_mode) ->
    let ty' = project_variables_out ~to_project ~expand ty in
    if ty == ty' then head else Boxed_float (ty', alloc_mode)
  | Boxed_int32 (ty, alloc_mode) ->
    let ty' = project_variables_out ~to_project ~expand ty in
    if ty == ty' then head else Boxed_int32 (ty', alloc_mode)
  | Boxed_int64 (ty, alloc_mode) ->
    let ty' = project_variables_out ~to_project ~expand ty in
    if ty == ty' then head else Boxed_int64 (ty', alloc_mode)
  | Boxed_nativeint (ty, alloc_mode) ->
    let ty' = project_variables_out ~to_project ~expand ty in
    if ty == ty' then head else Boxed_nativeint (ty', alloc_mode)
  | Boxed_vec128 (ty, alloc_mode) ->
    let ty' = project_variables_out ~to_project ~expand ty in
    if ty == ty' then head else Boxed_vec128 (ty', alloc_mode)
  | Closures { by_function_slot; alloc_mode } ->
    let by_function_slot' =
      project_row_like_for_closures ~to_project ~expand by_function_slot
    in
    if by_function_slot == by_function_slot'
    then head
    else Closures { by_function_slot = by_function_slot'; alloc_mode }
  | String _ -> head
  | Array { element_kind; length; contents = Unknown; alloc_mode } ->
    let length' = project_variables_out ~to_project ~expand length in
    if length == length'
    then head
    else
      Array { element_kind; length = length'; contents = Unknown; alloc_mode }
  | Array { element_kind; length; contents = Known Mutable; alloc_mode } ->
    let length' = project_variables_out ~to_project ~expand length in
    if length == length'
    then head
    else
      Array
        { element_kind; length = length'; contents = Known Mutable; alloc_mode }
  | Array
      { element_kind;
        length;
        contents = Known (Immutable { fields });
        alloc_mode
      } ->
    let length' = project_variables_out ~to_project ~expand length in
    let fields' =
      Misc.Stdlib.Array.map_sharing
        (project_variables_out ~to_project ~expand)
        fields
    in
    if length == length' && fields == fields'
    then head
    else
      Array
        { element_kind;
          length = length';
          contents = Known (Immutable { fields = fields' });
          alloc_mode
        }

and project_head_of_kind_naked_immediate ~to_project ~expand head =
  match head with
  | Naked_immediates _ -> head
  | Is_int ty ->
    let ty' = project_variables_out ~to_project ~expand ty in
    if ty == ty' then head else Is_int ty'
  | Get_tag ty ->
    let ty' = project_variables_out ~to_project ~expand ty in
    if ty == ty' then head else Get_tag ty'

and project_head_of_kind_naked_float32 ~to_project:_ ~expand:_ head = head

and project_head_of_kind_naked_float ~to_project:_ ~expand:_ head = head

and project_head_of_kind_naked_int32 ~to_project:_ ~expand:_ head = head

and project_head_of_kind_naked_int64 ~to_project:_ ~expand:_ head = head

and project_head_of_kind_naked_nativeint ~to_project:_ ~expand:_ head = head

and project_head_of_kind_naked_vec128 ~to_project:_ ~expand:_ head = head

and project_head_of_kind_rec_info ~to_project ~expand:_ head =
  match (head : head_of_kind_rec_info) with
  | Const _ | Succ _ | Unroll_to _ -> head
  | Var var ->
    if not (Variable.Set.mem var to_project)
    then head
    else Misc.fatal_error "Project of depth variables is not implemented"

and project_head_of_kind_region ~to_project:_ ~expand:_ () = ()

and project_row_like_for_blocks ~to_project ~expand
    ({ known_tags; other_tags; alloc_mode } as blocks) =
  let known_tags' =
    Tag.Map.map_sharing
      (fun ({ index; maps_to; env_extension } as case) ->
        let env_extension' =
          project_env_extension ~to_project ~expand env_extension
        in
        let maps_to' =
          project_int_indexed_product ~to_project ~expand maps_to
        in
        if env_extension == env_extension' && maps_to == maps_to'
        then case
        else { index; env_extension = env_extension'; maps_to = maps_to' })
      known_tags
  in
  let other_tags' : _ Or_bottom.t =
    match other_tags with
    | Bottom -> Bottom
    | Ok { index; maps_to; env_extension } ->
      let env_extension' =
        project_env_extension ~to_project ~expand env_extension
      in
      let maps_to' = project_int_indexed_product ~to_project ~expand maps_to in
      if env_extension == env_extension' && maps_to == maps_to'
      then other_tags
      else Ok { index; env_extension = env_extension'; maps_to = maps_to' }
  in
  if known_tags == known_tags' && other_tags == other_tags'
  then blocks
  else { known_tags = known_tags'; other_tags = other_tags'; alloc_mode }

and project_row_like_for_closures ~to_project ~expand
    ({ known_closures; other_closures } as closures) =
  let known_closures' =
    Function_slot.Map.map_sharing
      (fun ({ index; maps_to; env_extension } as case) ->
        let env_extension' =
          project_env_extension ~to_project ~expand env_extension
        in
        let maps_to' = project_closures_entry ~to_project ~expand maps_to in
        if env_extension == env_extension' && maps_to == maps_to'
        then case
        else { index; env_extension = env_extension'; maps_to = maps_to' })
      known_closures
  in
  let other_closures' : _ Or_bottom.t =
    match other_closures with
    | Bottom -> Bottom
    | Ok { index; maps_to; env_extension } ->
      let env_extension' =
        project_env_extension ~to_project ~expand env_extension
      in
      let maps_to' = project_closures_entry ~to_project ~expand maps_to in
      if env_extension == env_extension' && maps_to == maps_to'
      then other_closures
      else Ok { index; env_extension = env_extension'; maps_to = maps_to' }
  in
  if known_closures == known_closures' && other_closures == other_closures'
  then closures
  else { known_closures = known_closures'; other_closures = other_closures' }

and project_closures_entry ~to_project ~expand
    ({ function_types; closure_types; value_slot_types } as closures_entry) =
  let function_types' =
    Function_slot.Map.map_sharing
      (fun function_type ->
        Or_unknown_or_bottom.map_sharing function_type ~f:(fun function_type ->
            project_function_type ~to_project ~expand function_type))
      function_types
  in
  let closure_types' =
    project_function_slot_indexed_product ~to_project ~expand closure_types
  in
  let value_slot_types' =
    project_value_slot_indexed_product ~to_project ~expand value_slot_types
  in
  if function_types == function_types'
     && closure_types == closure_types'
     && value_slot_types == value_slot_types'
  then closures_entry
  else
    { function_types = function_types';
      closure_types = closure_types';
      value_slot_types = value_slot_types'
    }

and project_function_slot_indexed_product ~to_project ~expand
    ({ function_slot_components_by_index } as product) =
  let function_slot_components_by_index' =
    Function_slot.Map.map_sharing
      (project_variables_out ~to_project ~expand)
      function_slot_components_by_index
  in
  if function_slot_components_by_index == function_slot_components_by_index'
  then product
  else
    { function_slot_components_by_index = function_slot_components_by_index' }

and project_value_slot_indexed_product ~to_project ~expand
    ({ value_slot_components_by_index } as product) =
  let value_slot_components_by_index' =
    Value_slot.Map.map_sharing
      (project_variables_out ~to_project ~expand)
      value_slot_components_by_index
  in
  if value_slot_components_by_index == value_slot_components_by_index'
  then product
  else { value_slot_components_by_index = value_slot_components_by_index' }

and project_int_indexed_product ~to_project ~expand
    ({ fields; kind } as product) =
  let changed = ref false in
  let fields' = Array.copy fields in
  for i = 0 to Array.length fields - 1 do
    let field = fields.(i) in
    let field' = project_variables_out ~to_project ~expand field in
    if field != field'
    then (
      changed := true;
      fields'.(i) <- field')
  done;
  if !changed then { fields = fields'; kind } else product

and project_function_type ~to_project ~expand
    ({ code_id; rec_info } as function_type) =
  let rec_info' = project_variables_out ~to_project ~expand rec_info in
  if rec_info == rec_info'
  then function_type
  else { code_id; rec_info = rec_info' }

and project_env_extension ~to_project ~expand ({ equations } as env_extension) =
  let changed = ref false in
  let equations' =
    Name.Map.fold
      (fun name ty acc ->
        let keep_equation () =
          let ty' = project_variables_out ~to_project ~expand ty in
          if ty != ty' then changed := true;
          Name.Map.add name ty' acc
        in
        Name.pattern_match name
          ~symbol:(fun _ -> keep_equation ())
          ~var:(fun var ->
            if Variable.Set.mem var to_project
            then (
              changed := true;
              acc)
            else keep_equation ()))
      equations Name.Map.empty
  in
  if !changed then { equations = equations' } else env_extension

let kind t =
  match t with
  | Value _ -> K.value
  | Naked_immediate _ -> K.naked_immediate
  | Naked_float32 _ -> K.naked_float32
  | Naked_float _ -> K.naked_float
  | Naked_int32 _ -> K.naked_int32
  | Naked_int64 _ -> K.naked_int64
  | Naked_nativeint _ -> K.naked_nativeint
  | Naked_vec128 _ -> K.naked_vec128
  | Rec_info _ -> K.rec_info
  | Region _ -> K.region

let create_variant ~is_unique ~(immediates : _ Or_unknown.t) ~blocks =
  (match immediates with
  | Unknown -> ()
  | Known immediates ->
    if not (K.equal (kind immediates) K.naked_immediate)
    then
      Misc.fatal_errorf
        "Cannot create [immediates] with type that is not of kind \
         [Naked_immediate]:@ %a"
        print immediates);
  Value (TD.create (Variant { immediates; blocks; is_unique }))

let mutable_block alloc_mode = Value (TD.create (Mutable_block { alloc_mode }))

let create_closures alloc_mode by_function_slot =
  Value (TD.create (Closures { by_function_slot; alloc_mode }))

module Function_type = struct
  type t = function_type

  let create code_id ~rec_info = { code_id; rec_info }

  let code_id t = t.code_id

  let rec_info t = t.rec_info
end

module Closures_entry = struct
  type t = closures_entry

  let create ~function_types ~closure_types ~value_slot_types =
    { function_types; closure_types; value_slot_types }

  let find_function_type t function_slot : _ Or_unknown_or_bottom.t =
    match Function_slot.Map.find function_slot t.function_types with
    | exception Not_found -> Bottom
    | func_decl -> func_decl

  let value_slot_types { value_slot_types; _ } =
    value_slot_types.value_slot_components_by_index
end

module Product = struct
  module Function_slot_indexed = struct
    type t = function_slot_indexed_product

    let create function_slot_components_by_index =
      let function_slot_components_by_index =
        Function_slot.Map.map
          (fun ty ->
            if not (K.equal (kind ty) K.value)
            then
              Misc.fatal_errorf
                "Function-slot-indexed products can only hold types of kind \
                 [Value]:@ %a"
                (Function_slot.Map.print print)
                function_slot_components_by_index
            else ty)
          function_slot_components_by_index
      in
      { function_slot_components_by_index }

    let top = { function_slot_components_by_index = Function_slot.Map.empty }

    let width t =
      Targetint_31_63.of_int
        (Function_slot.Map.cardinal t.function_slot_components_by_index)
  end

  module Value_slot_indexed = struct
    type t = value_slot_indexed_product

    let create value_slot_components_by_index =
      { value_slot_components_by_index }

    let top = { value_slot_components_by_index = Value_slot.Map.empty }

    let width t =
      Targetint_31_63.of_int
        (Value_slot.Map.cardinal t.value_slot_components_by_index)
  end

  module Int_indexed = struct
    type t = int_indexed_product

    let field_kind t = t.kind

    let create_from_list kind tys = { kind; fields = Array.of_list tys }

    let create_from_array kind fields = { kind; fields }

    let create_top kind = { kind; fields = [||] }

    let width t = Targetint_31_63.of_int (Array.length t.fields)

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

  let bottom =
    { known_tags = Tag.Map.empty;
      other_tags = Bottom;
      alloc_mode = Alloc_mode.For_types.unknown ()
    }

  let is_bottom { known_tags; other_tags; alloc_mode = _ } =
    Tag.Map.is_empty known_tags
    && match other_tags with Bottom -> true | Ok _ -> false

  let all_tags { known_tags; other_tags; alloc_mode = _ } :
      Tag.Set.t Or_unknown.t =
    match other_tags with
    | Ok _ -> Unknown
    | Bottom -> Known (Tag.Map.keys known_tags)

  let create_exactly tag index maps_to alloc_mode =
    { known_tags =
        Tag.Map.singleton tag
          { maps_to;
            index = Known index;
            env_extension = { equations = Name.Map.empty }
          };
      other_tags = Bottom;
      alloc_mode
    }

  let create_at_least tag index maps_to alloc_mode =
    { known_tags =
        Tag.Map.singleton tag
          { maps_to;
            index = At_least index;
            env_extension = { equations = Name.Map.empty }
          };
      other_tags = Bottom;
      alloc_mode
    }

  let create_at_least_unknown_tag index maps_to alloc_mode =
    { known_tags = Tag.Map.empty;
      other_tags =
        Ok
          { maps_to;
            index = At_least index;
            env_extension = { equations = Name.Map.empty }
          };
      alloc_mode
    }

  let check_field_tys ~field_kind ~field_tys =
    let field_kind' =
      List.map kind field_tys |> Flambda_kind.Set.of_list
      |> Flambda_kind.Set.get_singleton
    in
    if Flambda_features.check_invariants ()
    then
      match field_kind' with
      | None ->
        if List.length field_tys <> 0
        then Misc.fatal_error "[field_tys] must all be of the same kind"
      | Some field_kind' ->
        if not (Flambda_kind.equal field_kind field_kind')
        then
          Misc.fatal_errorf "Declared field kind %a doesn't match [field_tys]"
            Flambda_kind.print field_kind

  let create ~(field_kind : Flambda_kind.t) ~field_tys
      (open_or_closed : open_or_closed) alloc_mode =
    check_field_tys ~field_kind ~field_tys;
    let tag : _ Or_unknown.t =
      let tag : _ Or_unknown.t =
        match open_or_closed with
        | Open (Known tag) -> Known tag
        | Open Unknown -> Unknown
        | Closed tag -> Known tag
      in
      match tag with
      | Unknown -> (
        match field_kind with
        | Value -> Unknown
        | Naked_number Naked_float -> Known Tag.double_array_tag
        | Naked_number Naked_float32
        | Naked_number Naked_immediate
        | Naked_number Naked_int32
        | Naked_number Naked_int64
        | Naked_number Naked_nativeint
        | Naked_number Naked_vec128
        | Region | Rec_info ->
          Misc.fatal_errorf "Bad kind %a for fields" Flambda_kind.print
            field_kind)
      | Known tag -> (
        match field_kind with
        | Value -> (
          match Tag.Scannable.of_tag tag with
          | Some _ -> Known tag
          | None ->
            Misc.fatal_error
              "Blocks full of [Value]s must have a tag less than [No_scan_tag]")
        | Naked_number Naked_float ->
          if not (Tag.equal tag Tag.double_array_tag)
          then
            Misc.fatal_error
              "Blocks full of naked floats must have tag [Tag.double_array_tag]";
          Known tag
        | Naked_number Naked_float32
        | Naked_number Naked_immediate
        | Naked_number Naked_int32
        | Naked_number Naked_int64
        | Naked_number Naked_nativeint
        | Naked_number Naked_vec128
        | Region | Rec_info ->
          Misc.fatal_errorf "Bad kind %a for fields" Flambda_kind.print
            field_kind)
    in
    let product = { kind = field_kind; fields = Array.of_list field_tys } in
    let size = Targetint_31_63.of_int (List.length field_tys) in
    match open_or_closed with
    | Open _ -> (
      match tag with
      | Known tag -> create_at_least tag size product alloc_mode
      | Unknown -> create_at_least_unknown_tag size product alloc_mode)
    | Closed _ -> (
      match tag with
      | Known tag -> create_exactly tag size product alloc_mode
      | Unknown -> assert false)
  (* see above *)

  let create_blocks_with_these_tags ~field_kind tags alloc_mode =
    let maps_to = Product.Int_indexed.create_top field_kind in
    let case =
      { maps_to;
        index = At_least Targetint_31_63.zero;
        env_extension = { equations = Name.Map.empty }
      }
    in
    { known_tags = Tag.Map.of_set (fun _ -> case) tags;
      other_tags = Bottom;
      alloc_mode
    }

  let create_exactly_multiple ~field_tys_by_tag alloc_mode =
    let known_tags =
      Tag.Map.map
        (fun field_tys ->
          let field_kind =
            match field_tys with
            | [] -> Flambda_kind.value
            | field_ty :: _ -> kind field_ty
          in
          check_field_tys ~field_kind ~field_tys;
          let maps_to =
            { kind = field_kind; fields = Array.of_list field_tys }
          in
          let size = Targetint_31_63.of_int (List.length field_tys) in
          { maps_to;
            index = Known size;
            env_extension = { equations = Name.Map.empty }
          })
        field_tys_by_tag
    in
    { known_tags; other_tags = Bottom; alloc_mode }

  let create_raw ~known_tags ~other_tags ~alloc_mode =
    (* CR-someday mshinwell: add invariant check? *)
    { known_tags; other_tags; alloc_mode }

  let all_tags_and_indexes { known_tags; other_tags; alloc_mode = _ } :
      _ Or_unknown.t =
    match other_tags with
    | Ok _ -> Unknown
    | Bottom -> Known (Tag.Map.map (fun case -> case.index) known_tags)

  let all_tags_and_sizes t : Targetint_31_63.t Tag.Map.t Or_unknown.t =
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

  let get_singleton { known_tags; other_tags; alloc_mode } =
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
        | Known index -> Some ((tag, index), maps_to, alloc_mode)))

  let project_int_indexed_product { fields; kind = _ } index : _ Or_unknown.t =
    if Array.length fields <= index then Unknown else Known fields.(index)

  let get_field t index : _ Or_unknown_or_bottom.t =
    match get_singleton t with
    | None -> Unknown
    | Some ((_tag, size), maps_to, _alloc_mode) -> (
      if Targetint_31_63.( <= ) size index
      then Bottom
      else
        match
          project_int_indexed_product maps_to (Targetint_31_63.to_int index)
        with
        | Unknown -> Unknown
        | Known res -> Ok res)
end

module Row_like_for_closures = struct
  type t = row_like_for_closures

  let create_exactly (function_slot : Function_slot.t)
      (contents : Set_of_closures_contents.t) (closures_entry : closures_entry)
      =
    let known_closures =
      Function_slot.Map.singleton function_slot
        { index = Known contents;
          maps_to = closures_entry;
          env_extension = { equations = Name.Map.empty }
        }
    in
    { known_closures; other_closures = Bottom }

  let create_at_least (function_slot : Function_slot.t)
      (contents : Set_of_closures_contents.t) (closures_entry : closures_entry)
      =
    let known_closures =
      Function_slot.Map.singleton function_slot
        { index = At_least contents;
          maps_to = closures_entry;
          env_extension = { equations = Name.Map.empty }
        }
    in
    { known_closures; other_closures = Bottom }

  let create_raw ~known_closures ~other_closures =
    (* CR-someday mshinwell: add invariant check? *)
    { known_closures; other_closures }

  let get_singleton { known_closures; other_closures } =
    match other_closures with
    | Ok _ -> None
    | Bottom -> (
      match Function_slot.Map.get_singleton known_closures with
      | None -> None
      | Some (tag, { maps_to; index; env_extension = _ }) -> (
        (* If this is a singleton all the information from the env_extension is
           already part of the environment *)
        match index with
        | At_least _ -> None
        | Known index -> Some ((tag, index), maps_to)))

  let get_closure t function_slot : _ Or_unknown.t =
    match get_singleton t with
    | None -> Unknown
    | Some ((_tag, index), maps_to) ->
      if not
           (Function_slot.Set.mem function_slot
              (Set_of_closures_contents.closures index))
      then Unknown
      else
        let closure_ty =
          try
            Function_slot.Map.find function_slot
              maps_to.closure_types.function_slot_components_by_index
          with Not_found ->
            Misc.fatal_errorf
              "Function slot %a is bound in index but not in maps_to@.Index:@ \
               %a@.Maps_to:@ %a"
              Function_slot.print function_slot Set_of_closures_contents.print
              index print_closures_entry maps_to
        in
        Known closure_ty

  let get_env_var t env_var : _ Or_unknown.t =
    match get_singleton t with
    | None -> Unknown
    | Some ((_tag, index), maps_to) ->
      if not
           (Value_slot.Set.mem env_var
              (Set_of_closures_contents.value_slots index))
      then Unknown
      else
        let env_var_ty =
          try
            Value_slot.Map.find env_var
              maps_to.value_slot_types.value_slot_components_by_index
          with Not_found ->
            Misc.fatal_errorf
              "Environment variable %a is bound in index but not in \
               maps_to@.Index:@ %a@.Maps_to:@ %a"
              Value_slot.print env_var Set_of_closures_contents.print index
              print_closures_entry maps_to
        in
        Known env_var_ty
end

module Env_extension = struct
  type t = env_extension

  let empty = { equations = Name.Map.empty }

  let create ~equations = { equations }

  let ids_for_export = ids_for_export_env_extension

  let apply_renaming = apply_renaming_env_extension

  let free_names = free_names_env_extension ~follow_value_slots:true

  let print = print_env_extension

  let to_map t = t.equations
end

let get_alias_exn t =
  match t with
  | Value ty -> TD.get_alias_exn ty
  | Naked_immediate ty -> TD.get_alias_exn ty
  | Naked_float32 ty -> TD.get_alias_exn ty
  | Naked_float ty -> TD.get_alias_exn ty
  | Naked_int32 ty -> TD.get_alias_exn ty
  | Naked_int64 ty -> TD.get_alias_exn ty
  | Naked_nativeint ty -> TD.get_alias_exn ty
  | Naked_vec128 ty -> TD.get_alias_exn ty
  | Rec_info ty -> TD.get_alias_exn ty
  | Region ty -> TD.get_alias_exn ty

let get_alias_opt t =
  match get_alias_exn t with s -> Some s | exception Not_found -> None

let is_obviously_bottom t =
  match t with
  | Value ty -> TD.is_obviously_bottom ty
  | Naked_immediate ty -> TD.is_obviously_bottom ty
  | Naked_float32 ty -> TD.is_obviously_bottom ty
  | Naked_float ty -> TD.is_obviously_bottom ty
  | Naked_int32 ty -> TD.is_obviously_bottom ty
  | Naked_int64 ty -> TD.is_obviously_bottom ty
  | Naked_nativeint ty -> TD.is_obviously_bottom ty
  | Naked_vec128 ty -> TD.is_obviously_bottom ty
  | Rec_info ty -> TD.is_obviously_bottom ty
  | Region ty -> TD.is_obviously_bottom ty

let is_obviously_unknown t =
  match t with
  | Value ty -> TD.is_obviously_unknown ty
  | Naked_immediate ty -> TD.is_obviously_unknown ty
  | Naked_float32 ty -> TD.is_obviously_unknown ty
  | Naked_float ty -> TD.is_obviously_unknown ty
  | Naked_int32 ty -> TD.is_obviously_unknown ty
  | Naked_int64 ty -> TD.is_obviously_unknown ty
  | Naked_nativeint ty -> TD.is_obviously_unknown ty
  | Naked_vec128 ty -> TD.is_obviously_unknown ty
  | Rec_info ty -> TD.is_obviously_unknown ty
  | Region ty -> TD.is_obviously_unknown ty

let alias_type_of (kind : K.t) name : t =
  match kind with
  | Value -> Value (TD.create_equals name)
  | Naked_number Naked_immediate -> Naked_immediate (TD.create_equals name)
  | Naked_number Naked_float32 -> Naked_float32 (TD.create_equals name)
  | Naked_number Naked_float -> Naked_float (TD.create_equals name)
  | Naked_number Naked_int32 -> Naked_int32 (TD.create_equals name)
  | Naked_number Naked_int64 -> Naked_int64 (TD.create_equals name)
  | Naked_number Naked_nativeint -> Naked_nativeint (TD.create_equals name)
  | Naked_number Naked_vec128 -> Naked_vec128 (TD.create_equals name)
  | Rec_info -> Rec_info (TD.create_equals name)
  | Region -> Region (TD.create_equals name)

let bottom_value = Value TD.bottom

let bottom_naked_immediate = Naked_immediate TD.bottom

let bottom_naked_float32 = Naked_float32 TD.bottom

let bottom_naked_float = Naked_float TD.bottom

let bottom_naked_int32 = Naked_int32 TD.bottom

let bottom_naked_int64 = Naked_int64 TD.bottom

let bottom_naked_nativeint = Naked_nativeint TD.bottom

let bottom_naked_vec128 = Naked_vec128 TD.bottom

let bottom_rec_info = Rec_info TD.bottom

let bottom_region = Region TD.bottom

let any_value = Value TD.unknown

let any_naked_immediate = Naked_immediate TD.unknown

let any_naked_float32 = Naked_float32 TD.unknown

let any_naked_float = Naked_float TD.unknown

let any_naked_int32 = Naked_int32 TD.unknown

let any_naked_int64 = Naked_int64 TD.unknown

let any_naked_nativeint = Naked_nativeint TD.unknown

let any_naked_vec128 = Naked_vec128 TD.unknown

let any_region = Region TD.unknown

let any_rec_info = Rec_info TD.unknown

let this_naked_immediate i : t =
  Naked_immediate (TD.create_equals (Simple.const (RWC.naked_immediate i)))

let this_naked_float32 f : t =
  Naked_float32 (TD.create_equals (Simple.const (RWC.naked_float32 f)))

let this_naked_float f : t =
  Naked_float (TD.create_equals (Simple.const (RWC.naked_float f)))

let this_naked_int32 i : t =
  Naked_int32 (TD.create_equals (Simple.const (RWC.naked_int32 i)))

let this_naked_int64 i : t =
  Naked_int64 (TD.create_equals (Simple.const (RWC.naked_int64 i)))

let this_naked_nativeint i : t =
  Naked_nativeint (TD.create_equals (Simple.const (RWC.naked_nativeint i)))

let this_naked_vec128 i : t =
  Naked_vec128 (TD.create_equals (Simple.const (RWC.naked_vec128 i)))

let these_naked_immediates is =
  match Targetint_31_63.Set.get_singleton is with
  | Some i -> this_naked_immediate i
  | _ ->
    if Targetint_31_63.Set.is_empty is
    then bottom_naked_immediate
    else Naked_immediate (TD.create (Naked_immediates is))

let these_naked_float32s fs =
  match Float32.Set.get_singleton fs with
  | Some f -> this_naked_float32 f
  | _ ->
    if Float32.Set.is_empty fs
    then bottom_naked_float32
    else Naked_float32 (TD.create fs)

let these_naked_floats fs =
  match Float.Set.get_singleton fs with
  | Some f -> this_naked_float f
  | _ ->
    if Float.Set.is_empty fs
    then bottom_naked_float
    else Naked_float (TD.create fs)

let these_naked_int32s is =
  match Int32.Set.get_singleton is with
  | Some i -> this_naked_int32 i
  | _ ->
    if Int32.Set.is_empty is
    then bottom_naked_int32
    else Naked_int32 (TD.create is)

let these_naked_int64s is =
  match Int64.Set.get_singleton is with
  | Some i -> this_naked_int64 i
  | _ ->
    if Int64.Set.is_empty is
    then bottom_naked_int64
    else Naked_int64 (TD.create is)

let these_naked_nativeints is =
  match Targetint_32_64.Set.get_singleton is with
  | Some i -> this_naked_nativeint i
  | _ ->
    if Targetint_32_64.Set.is_empty is
    then bottom_naked_nativeint
    else Naked_nativeint (TD.create is)

let these_naked_vec128s vs =
  match Vector_types.Vec128.Bit_pattern.Set.get_singleton vs with
  | Some v -> this_naked_vec128 v
  | _ ->
    if Vector_types.Vec128.Bit_pattern.Set.is_empty vs
    then bottom_naked_vec128
    else Naked_vec128 (TD.create vs)

let box_float32 (t : t) alloc_mode : t =
  match t with
  | Naked_float32 _ -> Value (TD.create (Boxed_float32 (t, alloc_mode)))
  | Value _ | Naked_immediate _ | Naked_int32 _ | Naked_float _ | Naked_int64 _
  | Naked_vec128 _ | Naked_nativeint _ | Rec_info _ | Region _ ->
    Misc.fatal_errorf "Type of wrong kind for [box_float32]: %a" print t

let box_float (t : t) alloc_mode : t =
  match t with
  | Naked_float _ -> Value (TD.create (Boxed_float (t, alloc_mode)))
  | Value _ | Naked_immediate _ | Naked_int32 _ | Naked_float32 _
  | Naked_int64 _ | Naked_vec128 _ | Naked_nativeint _ | Rec_info _ | Region _
    ->
    Misc.fatal_errorf "Type of wrong kind for [box_float]: %a" print t

let box_int32 (t : t) alloc_mode : t =
  match t with
  | Naked_int32 _ -> Value (TD.create (Boxed_int32 (t, alloc_mode)))
  | Value _ | Naked_immediate _ | Naked_float _ | Naked_float32 _
  | Naked_int64 _ | Naked_vec128 _ | Naked_nativeint _ | Rec_info _ | Region _
    ->
    Misc.fatal_errorf "Type of wrong kind for [box_int32]: %a" print t

let box_int64 (t : t) alloc_mode : t =
  match t with
  | Naked_int64 _ -> Value (TD.create (Boxed_int64 (t, alloc_mode)))
  | Value _ | Naked_immediate _ | Naked_float _ | Naked_float32 _
  | Naked_int32 _ | Naked_vec128 _ | Naked_nativeint _ | Rec_info _ | Region _
    ->
    Misc.fatal_errorf "Type of wrong kind for [box_int64]: %a" print t

let box_nativeint (t : t) alloc_mode : t =
  match t with
  | Naked_nativeint _ -> Value (TD.create (Boxed_nativeint (t, alloc_mode)))
  | Value _ | Naked_immediate _ | Naked_float _ | Naked_float32 _
  | Naked_int32 _ | Naked_int64 _ | Naked_vec128 _ | Rec_info _ | Region _ ->
    Misc.fatal_errorf "Type of wrong kind for [box_nativeint]: %a" print t

let box_vec128 (t : t) alloc_mode : t =
  match t with
  | Naked_vec128 _ -> Value (TD.create (Boxed_vec128 (t, alloc_mode)))
  | Value _ | Naked_immediate _ | Naked_float _ | Naked_float32 _
  | Naked_int32 _ | Naked_int64 _ | Naked_nativeint _ | Rec_info _ | Region _ ->
    Misc.fatal_errorf "Type of wrong kind for [box_vec128]: %a" print t

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
  | Value _ | Naked_float _ | Naked_float32 _ | Naked_int32 _ | Naked_int64 _
  | Naked_nativeint _ | Naked_vec128 _ | Rec_info _ | Region _ ->
    Misc.fatal_errorf "Type of wrong kind for [tag_immediate]: %a" print t

let tagged_immediate_alias_to ~naked_immediate : t =
  tag_immediate
    (Naked_immediate (TD.create_equals (Simple.var naked_immediate)))

let is_int_for_scrutinee ~scrutinee : t =
  Naked_immediate (TD.create (Is_int (alias_type_of K.value scrutinee)))

let get_tag_for_block ~block : t =
  Naked_immediate (TD.create (Get_tag (alias_type_of K.value block)))

let boxed_float32_alias_to ~naked_float32 =
  box_float (Naked_float32 (TD.create_equals (Simple.var naked_float32)))

let boxed_float_alias_to ~naked_float =
  box_float (Naked_float (TD.create_equals (Simple.var naked_float)))

let boxed_int32_alias_to ~naked_int32 =
  box_int32 (Naked_int32 (TD.create_equals (Simple.var naked_int32)))

let boxed_int64_alias_to ~naked_int64 =
  box_int64 (Naked_int64 (TD.create_equals (Simple.var naked_int64)))

let boxed_nativeint_alias_to ~naked_nativeint =
  box_nativeint
    (Naked_nativeint (TD.create_equals (Simple.var naked_nativeint)))

let boxed_vec128_alias_to ~naked_vec128 =
  box_vec128 (Naked_vec128 (TD.create_equals (Simple.var naked_vec128)))

let this_immutable_string str =
  let size = Targetint_31_63.of_int (String.length str) in
  let string_info =
    String_info.Set.singleton
      (String_info.create ~contents:(Contents str) ~size)
  in
  Value (TD.create (String string_info))

let mutable_string ~size =
  let size = Targetint_31_63.of_int size in
  let string_info =
    String_info.Set.singleton
      (String_info.create ~contents:Unknown_or_mutable ~size)
  in
  Value (TD.create (String string_info))

let array_of_length ~element_kind ~length alloc_mode =
  Value
    (TD.create (Array { element_kind; length; contents = Unknown; alloc_mode }))

let mutable_array ~element_kind ~length alloc_mode =
  Value
    (TD.create
       (Array { element_kind; length; contents = Known Mutable; alloc_mode }))

let immutable_array ~element_kind ~fields alloc_mode =
  Value
    (TD.create
       (Array
          { element_kind;
            length =
              this_tagged_immediate
                (Targetint_31_63.of_int (List.length fields));
            contents = Known (Immutable { fields = Array.of_list fields });
            alloc_mode
          }))

let this_rec_info (rec_info_expr : Rec_info_expr.t) =
  match rec_info_expr with
  | Var dv -> Rec_info (TD.create_equals (Simple.var dv))
  | Const _ | Succ _ | Unroll_to _ -> Rec_info (TD.create rec_info_expr)

module Descr = struct
  type t =
    | Value of head_of_kind_value TD.Descr.t Or_unknown_or_bottom.t
    | Naked_immediate of
        head_of_kind_naked_immediate TD.Descr.t Or_unknown_or_bottom.t
    | Naked_float32 of
        head_of_kind_naked_float32 TD.Descr.t Or_unknown_or_bottom.t
    | Naked_float of head_of_kind_naked_float TD.Descr.t Or_unknown_or_bottom.t
    | Naked_int32 of head_of_kind_naked_int32 TD.Descr.t Or_unknown_or_bottom.t
    | Naked_int64 of head_of_kind_naked_int64 TD.Descr.t Or_unknown_or_bottom.t
    | Naked_nativeint of
        head_of_kind_naked_nativeint TD.Descr.t Or_unknown_or_bottom.t
    | Naked_vec128 of
        head_of_kind_naked_vec128 TD.Descr.t Or_unknown_or_bottom.t
    | Rec_info of head_of_kind_rec_info TD.Descr.t Or_unknown_or_bottom.t
    | Region of head_of_kind_region TD.Descr.t Or_unknown_or_bottom.t
end

let descr t : Descr.t =
  match t with
  | Value ty -> Value (TD.descr ty)
  | Naked_immediate ty -> Naked_immediate (TD.descr ty)
  | Naked_float32 ty -> Naked_float32 (TD.descr ty)
  | Naked_float ty -> Naked_float (TD.descr ty)
  | Naked_int32 ty -> Naked_int32 (TD.descr ty)
  | Naked_int64 ty -> Naked_int64 (TD.descr ty)
  | Naked_nativeint ty -> Naked_nativeint (TD.descr ty)
  | Naked_vec128 ty -> Naked_vec128 (TD.descr ty)
  | Rec_info ty -> Rec_info (TD.descr ty)
  | Region ty -> Region (TD.descr ty)

let create_from_head_value head = Value (TD.create head)

let create_from_head_naked_immediate head = Naked_immediate (TD.create head)

let create_from_head_naked_float32 head = Naked_float32 (TD.create head)

let create_from_head_naked_float head = Naked_float (TD.create head)

let create_from_head_naked_int32 head = Naked_int32 (TD.create head)

let create_from_head_naked_int64 head = Naked_int64 (TD.create head)

let create_from_head_naked_nativeint head = Naked_nativeint (TD.create head)

let create_from_head_naked_vec128 head = Naked_vec128 (TD.create head)

let create_from_head_rec_info head = Rec_info (TD.create head)

let create_from_head_region head = Region (TD.create head)

module Head_of_kind_value = struct
  type t = head_of_kind_value

  let create_variant ~is_unique ~blocks ~immediates =
    Variant { is_unique; blocks; immediates }

  let create_mutable_block alloc_mode = Mutable_block { alloc_mode }

  let create_boxed_float32 ty alloc_mode = Boxed_float32 (ty, alloc_mode)

  let create_boxed_float ty alloc_mode = Boxed_float (ty, alloc_mode)

  let create_boxed_int32 ty alloc_mode = Boxed_int32 (ty, alloc_mode)

  let create_boxed_int64 ty alloc_mode = Boxed_int64 (ty, alloc_mode)

  let create_boxed_nativeint ty alloc_mode = Boxed_nativeint (ty, alloc_mode)

  let create_boxed_vec128 ty alloc_mode = Boxed_vec128 (ty, alloc_mode)

  let create_tagged_immediate imm : t =
    Variant
      { is_unique = false;
        immediates = Known (this_naked_immediate imm);
        blocks = Known Row_like_for_blocks.bottom
      }

  let create_closures by_function_slot alloc_mode =
    Closures { by_function_slot; alloc_mode }

  let create_string info = String info

  let create_array_with_contents ~element_kind ~length contents alloc_mode =
    Array { element_kind; length; contents; alloc_mode }
end

module type Head_of_kind_naked_number_intf = sig
  type t

  type n

  type n_set

  val create : n -> t

  val create_set : n_set -> t Or_bottom.t

  val create_non_empty_set : n_set -> t

  val union : t -> t -> t

  val inter : t -> t -> t Or_bottom.t
end

module Head_of_kind_naked_immediate = struct
  type t = head_of_kind_naked_immediate

  let create_naked_immediate imm =
    Naked_immediates (Targetint_31_63.Set.singleton imm)

  let create_naked_immediates imms : _ Or_bottom.t =
    if Targetint_31_63.Set.is_empty imms
    then Bottom
    else Ok (Naked_immediates imms)

  let create_naked_immediates_non_empty imms =
    if Targetint_31_63.Set.is_empty imms
    then
      Misc.fatal_error
        "Head_of_kind_naked_immediates.create_naked_immediates_non_empty";
    Naked_immediates imms

  let create_is_int ty = Is_int ty

  let create_get_tag ty = Get_tag ty
end

module Make_head_of_kind_naked_number (N : Container_types.S) = struct
  type t = N.Set.t

  type n = N.t

  type n_set = N.Set.t

  let create i = N.Set.singleton i

  let create_set is : _ Or_bottom.t =
    if N.Set.is_empty is then Bottom else Ok is

  let create_non_empty_set is =
    if N.Set.is_empty is
    then Misc.fatal_error "Make_head_of_kind_naked_number.create_non_empty_set";
    is

  let union = N.Set.union

  let inter t1 t2 : _ Or_bottom.t =
    let t = N.Set.inter t1 t2 in
    if N.Set.is_empty t then Bottom else Ok t
end

module Head_of_kind_naked_float32 = Make_head_of_kind_naked_number (Float32)
module Head_of_kind_naked_float = Make_head_of_kind_naked_number (Float)
module Head_of_kind_naked_int32 = Make_head_of_kind_naked_number (Int32)
module Head_of_kind_naked_int64 = Make_head_of_kind_naked_number (Int64)
module Head_of_kind_naked_nativeint =
  Make_head_of_kind_naked_number (Targetint_32_64)
module Head_of_kind_naked_vec128 =
  Make_head_of_kind_naked_number (Vector_types.Vec128.Bit_pattern)

let rec recover_some_aliases t =
  match t with
  | Value ty -> (
    match TD.descr ty with
    | Unknown | Bottom
    | Ok (Equals _)
    | Ok
        (No_alias
          ( Mutable_block _ | Boxed_float _ | Boxed_float32 _ | Boxed_int32 _
          | Boxed_int64 _ | Boxed_vec128 _ | Boxed_nativeint _ | String _
          | Closures _ | Array _ )) ->
      t
    | Ok (No_alias (Variant { immediates; blocks; is_unique = _ })) -> (
      match blocks with
      | Unknown -> t
      | Known blocks -> (
        if not (Row_like_for_blocks.is_bottom blocks)
        then t
        else
          match immediates with
          | Unknown -> t
          | Known immediates -> (
            let t' = recover_some_aliases immediates in
            match t' with
            | Naked_immediate ty -> (
              match TD.descr ty with
              | Ok (Equals alias) ->
                Simple.pattern_match' alias
                  ~var:(fun _ ~coercion:_ -> t)
                  ~symbol:(fun _ ~coercion:_ -> t)
                  ~const:(fun const ->
                    match Reg_width_const.descr const with
                    | Naked_immediate i -> this_tagged_immediate i
                    | Tagged_immediate _ | Naked_float _ | Naked_float32 _
                    | Naked_int32 _ | Naked_int64 _ | Naked_nativeint _
                    | Naked_vec128 _ ->
                      Misc.fatal_errorf
                        "Immediates case returned wrong kind of constant:@ %a"
                        Reg_width_const.print const)
              | Unknown | Bottom | Ok (No_alias _) -> t)
            | Value _ | Naked_float _ | Naked_float32 _ | Naked_int32 _
            | Naked_int64 _ | Naked_vec128 _ | Naked_nativeint _ | Rec_info _
            | Region _ ->
              Misc.fatal_errorf "Immediates case returned wrong kind:@ %a" print
                t' ()))))
  | Naked_immediate ty -> (
    match TD.descr ty with
    | Unknown | Bottom | Ok (Equals _) | Ok (No_alias (Is_int _ | Get_tag _)) ->
      t
    | Ok (No_alias (Naked_immediates is)) -> (
      match Targetint_31_63.Set.get_singleton is with
      | Some i -> this_naked_immediate i
      | None -> t))
  | Naked_float32 ty -> (
    match TD.descr ty with
    | Unknown | Bottom | Ok (Equals _) -> t
    | Ok (No_alias fs) -> (
      match Float32.Set.get_singleton fs with
      | Some f -> this_naked_float32 f
      | None -> t))
  | Naked_float ty -> (
    match TD.descr ty with
    | Unknown | Bottom | Ok (Equals _) -> t
    | Ok (No_alias fs) -> (
      match Float.Set.get_singleton fs with
      | Some f -> this_naked_float f
      | None -> t))
  | Naked_int32 ty -> (
    match TD.descr ty with
    | Unknown | Bottom | Ok (Equals _) -> t
    | Ok (No_alias is) -> (
      match Int32.Set.get_singleton is with
      | Some f -> this_naked_int32 f
      | None -> t))
  | Naked_int64 ty -> (
    match TD.descr ty with
    | Unknown | Bottom | Ok (Equals _) -> t
    | Ok (No_alias is) -> (
      match Int64.Set.get_singleton is with
      | Some f -> this_naked_int64 f
      | None -> t))
  | Naked_nativeint ty -> (
    match TD.descr ty with
    | Unknown | Bottom | Ok (Equals _) -> t
    | Ok (No_alias is) -> (
      match Targetint_32_64.Set.get_singleton is with
      | Some f -> this_naked_nativeint f
      | None -> t))
  | Naked_vec128 ty -> (
    match TD.descr ty with
    | Unknown | Bottom | Ok (Equals _) -> t
    | Ok (No_alias is) -> (
      match Vec128.Set.get_singleton is with
      | Some f -> this_naked_vec128 f
      | None -> t))
  | Rec_info _ | Region _ -> t
