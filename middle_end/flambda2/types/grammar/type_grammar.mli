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

(** The grammar of Flambda types plus the basic creation functions upon them
    that do not require an environment. *)

module Block_size : sig
  type t

  val equal : t -> t -> bool

  val subset : t -> t -> bool

  val union : t -> t -> t

  val inter : t -> t -> t
end

type t = private
  | Value of head_of_kind_value Type_descr.t
  | Naked_immediate of head_of_kind_naked_immediate Type_descr.t
  | Naked_float32 of head_of_kind_naked_float32 Type_descr.t
  | Naked_float of head_of_kind_naked_float Type_descr.t
  | Naked_int32 of head_of_kind_naked_int32 Type_descr.t
  | Naked_int64 of head_of_kind_naked_int64 Type_descr.t
  | Naked_nativeint of head_of_kind_naked_nativeint Type_descr.t
  | Naked_vec128 of head_of_kind_naked_vec128 Type_descr.t
  | Rec_info of head_of_kind_rec_info Type_descr.t
  | Region of head_of_kind_region Type_descr.t

and head_of_kind_value = private
  | Variant of
      { immediates : t Or_unknown.t;
        blocks : row_like_for_blocks Or_unknown.t;
        is_unique : bool
      }
  (* CR mshinwell: It would be better to track per-field mutability. *)
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
        (* CR mshinwell: add support for tracking unboxed product arrays *)
        length : t;
        contents : array_contents Or_unknown.t;
        alloc_mode : Alloc_mode.For_types.t
      }

and head_of_kind_naked_immediate = private
  | Naked_immediates of Targetint_31_63.Set.t
  | Is_int of t  (** For variants only *)
  | Get_tag of t  (** For variants only *)

(** Invariant: the float/integer sets for naked float, int32, int64 and
    nativeint heads are non-empty. (Empty sets are represented as an overall
    bottom type.) *)

and head_of_kind_naked_float32 = private
  Numeric_types.Float32_by_bit_pattern.Set.t

and head_of_kind_naked_float = private Numeric_types.Float_by_bit_pattern.Set.t

and head_of_kind_naked_int32 = private Numeric_types.Int32.Set.t

and head_of_kind_naked_int64 = private Numeric_types.Int64.Set.t

and head_of_kind_naked_nativeint = private Targetint_32_64.Set.t

and head_of_kind_naked_vec128 = private Vector_types.Vec128.Bit_pattern.Set.t

and head_of_kind_rec_info = Rec_info_expr.t

and head_of_kind_region = unit

and 'lattice row_like_index_domain = private
  | Known of 'lattice
  | At_least of 'lattice

and ('lattice, 'shape) row_like_index = private
  { domain : 'lattice row_like_index_domain;
    shape : 'shape
  }

and ('lattice, 'shape, 'maps_to) row_like_case = private
  { maps_to : 'maps_to;
    index : ('lattice, 'shape) row_like_index;
    env_extension : env_extension
  }

and row_like_block_case =
  (Block_size.t, Flambda_kind.Block_shape.t, t array) row_like_case

and row_like_for_blocks = private
  { known_tags : row_like_block_case Or_unknown.t Tag.Map.t;
    other_tags : row_like_block_case Or_bottom.t;
    alloc_mode : Alloc_mode.For_types.t
  }

and row_like_for_closures = private
  { known_closures :
      (Set_of_closures_contents.t, unit, closures_entry) row_like_case
      Function_slot.Map.t;
    other_closures :
      (Set_of_closures_contents.t, unit, closures_entry) row_like_case
      Or_bottom.t
  }

and closures_entry = private
  { function_types : function_type Or_unknown_or_bottom.t Function_slot.Map.t;
    closure_types : function_slot_indexed_product;
    value_slot_types : value_slot_indexed_product
  }

and function_slot_indexed_product = private
  { function_slot_components_by_index : t Function_slot.Map.t }

and value_slot_indexed_product = private
  { value_slot_components_by_index : t Value_slot.Map.t }

and function_type = private
  { code_id : Code_id.t;
    rec_info : t
  }

and array_contents =
  | Immutable of { fields : t array }
  | Mutable

and env_extension = private { equations : t Name.Map.t } [@@unboxed]

type flambda_type = t

val print : Format.formatter -> t -> unit

(** [free_names] returns *all* value slots occurring in the given type
    regardless of where in the type such variables occur. *)
include Contains_names.S with type t := t

val free_names_except_through_value_slots : t -> Name_occurrences.t

include Contains_ids.S with type t := t

val remove_unused_value_slots_and_shortcut_aliases :
  t ->
  used_value_slots:Value_slot.Set.t ->
  canonicalise:(Simple.t -> Simple.t) ->
  t

val project_variables_out :
  to_project:Variable.Set.t -> expand:(Variable.t -> t) -> t -> t

val kind : t -> Flambda_kind.t

val alias_type_of : Flambda_kind.t -> Simple.t -> t

val apply_coercion : t -> Coercion.t -> t

val get_alias_exn : t -> Simple.t

val get_alias_opt : t -> Simple.t option

val is_obviously_bottom : t -> bool

val is_obviously_unknown : t -> bool

val bottom_value : t

val bottom_naked_immediate : t

val bottom_naked_float32 : t

val bottom_naked_float : t

val bottom_naked_int32 : t

val bottom_naked_int64 : t

val bottom_naked_nativeint : t

val bottom_naked_vec128 : t

val bottom_rec_info : t

val bottom_region : t

val any_value : t

val any_naked_immediate : t

val any_naked_float32 : t

val any_naked_float : t

val any_naked_int32 : t

val any_naked_int64 : t

val any_naked_nativeint : t

val any_naked_vec128 : t

val any_region : t

val any_rec_info : t

val this_tagged_immediate : Targetint_31_63.t -> t

val this_rec_info : Rec_info_expr.t -> t

val this_naked_immediate : Targetint_31_63.t -> t

val this_naked_float32 : Numeric_types.Float32_by_bit_pattern.t -> t

val this_naked_float : Numeric_types.Float_by_bit_pattern.t -> t

val this_naked_int32 : Numeric_types.Int32.t -> t

val this_naked_int64 : Numeric_types.Int64.t -> t

val this_naked_nativeint : Targetint_32_64.t -> t

val this_naked_vec128 : Vector_types.Vec128.Bit_pattern.t -> t

val these_naked_immediates : Targetint_31_63.Set.t -> t

val these_naked_float32s : Numeric_types.Float32_by_bit_pattern.Set.t -> t

val these_naked_floats : Numeric_types.Float_by_bit_pattern.Set.t -> t

val these_naked_int32s : Numeric_types.Int32.Set.t -> t

val these_naked_int64s : Numeric_types.Int64.Set.t -> t

val these_naked_nativeints : Targetint_32_64.Set.t -> t

val these_naked_vec128s : Vector_types.Vec128.Bit_pattern.Set.t -> t

val boxed_float32_alias_to :
  naked_float32:Variable.t -> Alloc_mode.For_types.t -> t

val boxed_float_alias_to : naked_float:Variable.t -> Alloc_mode.For_types.t -> t

val boxed_int32_alias_to : naked_int32:Variable.t -> Alloc_mode.For_types.t -> t

val boxed_int64_alias_to : naked_int64:Variable.t -> Alloc_mode.For_types.t -> t

val boxed_nativeint_alias_to :
  naked_nativeint:Variable.t -> Alloc_mode.For_types.t -> t

val boxed_vec128_alias_to :
  naked_vec128:Variable.t -> Alloc_mode.For_types.t -> t

val box_float32 : t -> Alloc_mode.For_types.t -> t

val box_float : t -> Alloc_mode.For_types.t -> t

val box_int32 : t -> Alloc_mode.For_types.t -> t

val box_int64 : t -> Alloc_mode.For_types.t -> t

val box_nativeint : t -> Alloc_mode.For_types.t -> t

val box_vec128 : t -> Alloc_mode.For_types.t -> t

val tagged_immediate_alias_to : naked_immediate:Variable.t -> t

val tag_immediate : t -> t

val is_int_for_scrutinee : scrutinee:Simple.t -> t

val get_tag_for_block : block:Simple.t -> t

val create_variant :
  is_unique:bool ->
  immediates:t Or_unknown.t ->
  blocks:row_like_for_blocks Or_unknown.t ->
  t

val mutable_block : Alloc_mode.For_types.t -> t

val create_closures : Alloc_mode.For_types.t -> row_like_for_closures -> t

(** Note this assumes the allocation mode is [Heap] *)
val this_immutable_string : string -> t

val mutable_string : size:int -> t

val array_of_length :
  element_kind:Flambda_kind.With_subkind.t Or_unknown_or_bottom.t ->
  length:t ->
  Alloc_mode.For_types.t ->
  t

val mutable_array :
  element_kind:Flambda_kind.With_subkind.t Or_unknown_or_bottom.t ->
  length:t ->
  Alloc_mode.For_types.t ->
  t

val immutable_array :
  element_kind:Flambda_kind.With_subkind.t Or_unknown_or_bottom.t ->
  fields:t list ->
  Alloc_mode.For_types.t ->
  t

module Product : sig
  module Function_slot_indexed : sig
    type t = function_slot_indexed_product

    val top : t

    val create : flambda_type Function_slot.Map.t -> t

    val width : t -> Targetint_31_63.t
  end

  module Value_slot_indexed : sig
    type t = value_slot_indexed_product

    val top : t

    val create : flambda_type Value_slot.Map.t -> t

    val width : t -> Targetint_31_63.t
  end

  module Int_indexed : sig
    type t = flambda_type array

    val create_top : unit -> t

    val create_from_list : flambda_type list -> t

    val create_from_array : flambda_type array -> t

    val width : t -> Targetint_31_63.t

    val components : t -> flambda_type list
  end
end

module Function_type : sig
  type t = function_type

  val create : Code_id.t -> rec_info:flambda_type -> t

  val code_id : t -> Code_id.t

  val rec_info : t -> flambda_type
end

module Closures_entry : sig
  type t = closures_entry

  val create :
    function_types:Function_type.t Or_unknown_or_bottom.t Function_slot.Map.t ->
    closure_types:Product.Function_slot_indexed.t ->
    value_slot_types:Product.Value_slot_indexed.t ->
    t

  val find_function_type :
    t -> Function_slot.t -> Function_type.t Or_unknown_or_bottom.t

  val value_slot_types : t -> flambda_type Value_slot.Map.t
end

module Row_like_index : sig
  type ('lattice, 'shape) t = ('lattice, 'shape) row_like_index

  val create :
    domain:'lattice row_like_index_domain ->
    shape:'shape ->
    ('lattice, 'shape) t
end

module Row_like_index_domain : sig
  type 'lattice t = 'lattice row_like_index_domain

  val known : 'lattice -> 'lattice t

  val at_least : 'lattice -> 'lattice t
end

module Row_like_case : sig
  type ('lattice, 'shape, 'maps_to) t =
    ('lattice, 'shape, 'maps_to) row_like_case

  val create :
    maps_to:'maps_to ->
    index:('lattice, 'shape) row_like_index ->
    env_extension:env_extension ->
    ('lattice, 'shape, 'maps_to) row_like_case
end

module Row_like_for_blocks : sig
  type t = row_like_for_blocks

  val bottom : t

  type open_or_closed =
    | Open of Tag.t Or_unknown.t
    | Closed of Tag.t

  val create :
    shape:Flambda_kind.Block_shape.t ->
    field_tys:flambda_type list ->
    open_or_closed ->
    Alloc_mode.For_types.t ->
    t

  val create_blocks_with_these_tags :
    Flambda_kind.Block_shape.t Or_unknown.t Tag.Map.t ->
    Alloc_mode.For_types.t ->
    t

  val create_exactly_multiple :
    shape_and_field_tys_by_tag:
      (Flambda_kind.Block_shape.t * flambda_type list) Tag.Map.t ->
    Alloc_mode.For_types.t ->
    t

  val create_raw :
    known_tags:row_like_block_case Or_unknown.t Tag.Map.t ->
    other_tags:row_like_block_case Or_bottom.t ->
    alloc_mode:Alloc_mode.For_types.t ->
    t

  val all_tags : t -> Tag.Set.t Or_unknown.t

  val all_tags_and_sizes :
    t -> (Targetint_31_63.t * Flambda_kind.Block_shape.t) Tag.Map.t Or_unknown.t

  val get_singleton :
    t ->
    (Tag.t
    * Flambda_kind.Block_shape.t
    * Targetint_31_63.t
    * Product.Int_indexed.t
    * Alloc_mode.For_types.t)
    option

  (** Get the nth field of the block if it is unambiguous.

      This can be done precisely using Type_grammar.meet_shape, but this meet
      can be expensive. This function allows to give a precise answer quickly in
      the common case where the block type is known exactly (for example, it is
      the result of a previous record or module allocation).

      This will return Unknown if:

      - The block type represents a disjunction (several possible tags)

      - The tag or size is not exactly known

      - The nth field exists, is unique, but has Unknown type

      This will return Bottom if there is no nth field (the read is invalid, and
      will produce bottom)

      The handling of those cases could be improved:

      - In the case of disjunctions, if all possible nth fields point to the
      same type, this type could be returned directly.

      - When the tag or size is not known but there is a unique possible value,
      it could be returned anyway

      - There could be a distinction between the first three cases (where we
      expect that doing the actual meet could give us a better result) and the
      last case where we already know what the result of the meet will be. *)
  val get_field : t -> Targetint_31_63.t -> flambda_type Or_unknown_or_bottom.t

  val is_bottom : t -> bool

  (** The [Maps_to] value which [meet] returns contains the join of all
      [Maps_to] values in the range of the row-like structure after the meet
      operation has been completed. *)
end

module Row_like_for_closures : sig
  type t = row_like_for_closures

  val create_exactly :
    Function_slot.t -> Set_of_closures_contents.t -> Closures_entry.t -> t

  val create_at_least :
    Function_slot.t -> Set_of_closures_contents.t -> Closures_entry.t -> t

  val create_raw :
    known_closures:
      (Set_of_closures_contents.t, unit, closures_entry) row_like_case
      Function_slot.Map.t ->
    other_closures:
      (Set_of_closures_contents.t, unit, closures_entry) row_like_case
      Or_bottom.t ->
    t

  val get_singleton :
    t ->
    ((Function_slot.t * Set_of_closures_contents.t) * Closures_entry.t) option

  (** Same as For_blocks.get_field: attempt to find the type associated to the
      given environment variable without an expensive meet. *)
  val get_env_var : t -> Value_slot.t -> flambda_type Or_unknown.t

  (** Similar to [get_env_var] but for closures within the set. *)
  val get_closure : t -> Function_slot.t -> flambda_type Or_unknown.t
end

module Env_extension : sig
  type t = env_extension

  val empty : t

  val create : equations:flambda_type Name.Map.t -> t

  include Contains_ids.S with type t := t

  include Contains_names.S with type t := t

  val print : Format.formatter -> t -> unit

  val to_map : t -> flambda_type Name.Map.t
end

module Descr : sig
  type t = private
    | Value of head_of_kind_value Type_descr.Descr.t Or_unknown_or_bottom.t
    | Naked_immediate of
        head_of_kind_naked_immediate Type_descr.Descr.t Or_unknown_or_bottom.t
    | Naked_float32 of
        head_of_kind_naked_float32 Type_descr.Descr.t Or_unknown_or_bottom.t
    | Naked_float of
        head_of_kind_naked_float Type_descr.Descr.t Or_unknown_or_bottom.t
    | Naked_int32 of
        head_of_kind_naked_int32 Type_descr.Descr.t Or_unknown_or_bottom.t
    | Naked_int64 of
        head_of_kind_naked_int64 Type_descr.Descr.t Or_unknown_or_bottom.t
    | Naked_nativeint of
        head_of_kind_naked_nativeint Type_descr.Descr.t Or_unknown_or_bottom.t
    | Naked_vec128 of
        head_of_kind_naked_vec128 Type_descr.Descr.t Or_unknown_or_bottom.t
    | Rec_info of
        head_of_kind_rec_info Type_descr.Descr.t Or_unknown_or_bottom.t
    | Region of head_of_kind_region Type_descr.Descr.t Or_unknown_or_bottom.t
end

val descr : t -> Descr.t

val create_from_head_value : head_of_kind_value -> t

val create_from_head_naked_immediate : head_of_kind_naked_immediate -> t

val create_from_head_naked_float32 : head_of_kind_naked_float32 -> t

val create_from_head_naked_float : head_of_kind_naked_float -> t

val create_from_head_naked_int32 : head_of_kind_naked_int32 -> t

val create_from_head_naked_int64 : head_of_kind_naked_int64 -> t

val create_from_head_naked_nativeint : head_of_kind_naked_nativeint -> t

val create_from_head_naked_vec128 : head_of_kind_naked_vec128 -> t

val create_from_head_rec_info : head_of_kind_rec_info -> t

val create_from_head_region : head_of_kind_region -> t

val apply_coercion_head_of_kind_value :
  head_of_kind_value -> Coercion.t -> head_of_kind_value Or_bottom.t

val apply_coercion_head_of_kind_naked_immediate :
  head_of_kind_naked_immediate ->
  Coercion.t ->
  head_of_kind_naked_immediate Or_bottom.t

val apply_coercion_head_of_kind_naked_float32 :
  head_of_kind_naked_float32 ->
  Coercion.t ->
  head_of_kind_naked_float32 Or_bottom.t

val apply_coercion_head_of_kind_naked_float :
  head_of_kind_naked_float -> Coercion.t -> head_of_kind_naked_float Or_bottom.t

val apply_coercion_head_of_kind_naked_int32 :
  head_of_kind_naked_int32 -> Coercion.t -> head_of_kind_naked_int32 Or_bottom.t

val apply_coercion_head_of_kind_naked_int64 :
  head_of_kind_naked_int64 -> Coercion.t -> head_of_kind_naked_int64 Or_bottom.t

val apply_coercion_head_of_kind_naked_nativeint :
  head_of_kind_naked_nativeint ->
  Coercion.t ->
  head_of_kind_naked_nativeint Or_bottom.t

val apply_coercion_head_of_kind_naked_vec128 :
  head_of_kind_naked_vec128 ->
  Coercion.t ->
  head_of_kind_naked_vec128 Or_bottom.t

val apply_coercion_head_of_kind_rec_info :
  head_of_kind_rec_info -> Coercion.t -> head_of_kind_rec_info Or_bottom.t

val apply_coercion_head_of_kind_region :
  head_of_kind_region -> Coercion.t -> head_of_kind_region Or_bottom.t

module Head_of_kind_value : sig
  type t = head_of_kind_value

  val create_variant :
    is_unique:bool ->
    blocks:Row_like_for_blocks.t Or_unknown.t ->
    immediates:flambda_type Or_unknown.t ->
    t

  val create_mutable_block : Alloc_mode.For_types.t -> t

  (* CR-someday mshinwell: these alloc mode params should probably be
     labelled *)
  val create_boxed_float32 : flambda_type -> Alloc_mode.For_types.t -> t

  val create_boxed_float : flambda_type -> Alloc_mode.For_types.t -> t

  val create_boxed_int32 : flambda_type -> Alloc_mode.For_types.t -> t

  val create_boxed_int64 : flambda_type -> Alloc_mode.For_types.t -> t

  val create_boxed_nativeint : flambda_type -> Alloc_mode.For_types.t -> t

  val create_boxed_vec128 : flambda_type -> Alloc_mode.For_types.t -> t

  val create_tagged_immediate : Targetint_31_63.t -> t

  val create_closures : Row_like_for_closures.t -> Alloc_mode.For_types.t -> t

  val create_string : String_info.Set.t -> t

  val create_array_with_contents :
    element_kind:Flambda_kind.With_subkind.t Or_unknown_or_bottom.t ->
    length:flambda_type ->
    array_contents Or_unknown.t ->
    Alloc_mode.For_types.t ->
    t
end

module Head_of_kind_naked_immediate : sig
  type t = head_of_kind_naked_immediate

  val create_naked_immediate : Targetint_31_63.t -> t

  val create_naked_immediates : Targetint_31_63.Set.t -> t Or_bottom.t

  val create_naked_immediates_non_empty : Targetint_31_63.Set.t -> t

  val create_is_int : flambda_type -> t

  val create_get_tag : flambda_type -> t
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

module Head_of_kind_naked_float32 :
  Head_of_kind_naked_number_intf
    with type t = head_of_kind_naked_float32
    with type n = Numeric_types.Float32_by_bit_pattern.t
    with type n_set = Numeric_types.Float32_by_bit_pattern.Set.t

module Head_of_kind_naked_float :
  Head_of_kind_naked_number_intf
    with type t = head_of_kind_naked_float
    with type n = Numeric_types.Float_by_bit_pattern.t
    with type n_set = Numeric_types.Float_by_bit_pattern.Set.t

module Head_of_kind_naked_int32 :
  Head_of_kind_naked_number_intf
    with type t = head_of_kind_naked_int32
    with type n = Numeric_types.Int32.t
    with type n_set = Numeric_types.Int32.Set.t

module Head_of_kind_naked_int64 :
  Head_of_kind_naked_number_intf
    with type t = head_of_kind_naked_int64
    with type n = Numeric_types.Int64.t
    with type n_set = Numeric_types.Int64.Set.t

module Head_of_kind_naked_nativeint :
  Head_of_kind_naked_number_intf
    with type t = head_of_kind_naked_nativeint
    with type n = Targetint_32_64.t
    with type n_set = Targetint_32_64.Set.t

module Head_of_kind_naked_vec128 :
  Head_of_kind_naked_number_intf
    with type t = head_of_kind_naked_vec128
    with type n = Vector_types.Vec128.Bit_pattern.t
    with type n_set = Vector_types.Vec128.Bit_pattern.Set.t

val recover_some_aliases : t -> t
