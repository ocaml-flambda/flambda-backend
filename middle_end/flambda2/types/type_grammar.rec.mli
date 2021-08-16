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

(** The definition of types together with their constructors and operations
    upon them. *)

type t = private
  | Value of Type_of_kind_value.t
  | Naked_immediate of Type_of_kind_naked_immediate.t
  | Naked_float of Type_of_kind_naked_float.t
  | Naked_int32 of Type_of_kind_naked_int32.t
  | Naked_int64 of Type_of_kind_naked_int64.t
  | Naked_nativeint of Type_of_kind_naked_nativeint.t
  | Rec_info of Type_of_kind_rec_info.t

val print : Format.formatter -> t -> unit

val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit

include Contains_names.S with type t := t

include Contains_ids.S with type t := t

val kind : t -> Flambda_kind.t

val alias_type_of : Flambda_kind.t -> Simple.t -> t

val apply_coercion : t -> Coercion.t -> t Or_bottom.t

val eviscerate : t -> Typing_env.t -> t

val get_alias_exn : t -> Simple.t

val is_obviously_bottom : t -> bool
val is_obviously_unknown : t -> bool

val bottom : Flambda_kind.t -> t
val bottom_like : t -> t

val unknown : Flambda_kind.t -> t
val unknown_like : t -> t

val any_value : unit -> t

val any_tagged_immediate : unit -> t
val any_tagged_bool : unit -> t

val any_boxed_float : unit -> t
val any_boxed_int32 : unit -> t
val any_boxed_int64 : unit -> t
val any_boxed_nativeint : unit -> t

val any_naked_immediate : unit -> t
val any_naked_bool : unit -> t
val any_naked_float : unit -> t
val any_naked_int32 : unit -> t
val any_naked_int64 : unit -> t
val any_naked_nativeint : unit -> t

val any_rec_info : unit -> t

val this_tagged_immediate : Targetint_31_63.t -> t
val this_boxed_float : Numeric_types.Float_by_bit_pattern.t -> t
val this_boxed_int32 : Int32.t -> t
val this_boxed_int64 : Int64.t -> t
val this_boxed_nativeint : Targetint_32_64.t -> t

val these_tagged_immediates : Targetint_31_63.Set.t -> t
val these_naked_immediates : Targetint_31_63.Set.t -> t
val these_boxed_floats : Numeric_types.Float_by_bit_pattern.Set.t -> t
val these_boxed_int32s : Int32.Set.t -> t
val these_boxed_int64s : Int64.Set.t -> t
val these_boxed_nativeints : Targetint_32_64.Set.t -> t

val this_rec_info : Rec_info_expr.t -> t

val this_naked_immediate : Targetint_31_63.t -> t
val this_naked_float : Numeric_types.Float_by_bit_pattern.t -> t
val this_naked_int32 : Int32.t -> t
val this_naked_int64 : Int64.t -> t
val this_naked_nativeint : Targetint_32_64.t -> t

val this_tagged_immediate_without_alias : Targetint_31_63.t -> t
val this_naked_immediate_without_alias : Targetint_31_63.t -> t
val this_naked_float_without_alias : Numeric_types.Float_by_bit_pattern.t -> t
val this_naked_int32_without_alias : Int32.t -> t
val this_naked_int64_without_alias : Int64.t -> t
val this_naked_nativeint_without_alias : Targetint_32_64.t -> t

val these_naked_floats : Numeric_types.Float_by_bit_pattern.Set.t -> t
val these_naked_int32s : Int32.Set.t -> t
val these_naked_int64s : Int64.Set.t -> t
val these_naked_nativeints : Targetint_32_64.Set.t -> t

val boxed_float_alias_to : naked_float:Variable.t -> t
val boxed_int32_alias_to : naked_int32:Variable.t -> t
val boxed_int64_alias_to : naked_int64:Variable.t -> t
val boxed_nativeint_alias_to : naked_nativeint:Variable.t -> t

val box_float : t -> t
val box_int32 : t -> t
val box_int64 : t -> t
val box_nativeint : t -> t

val tagged_immediate_alias_to : naked_immediate:Variable.t -> t
val tag_immediate : t -> t

val any_block : unit -> t

val is_int_for_scrutinee : scrutinee:Simple.t -> t
val get_tag_for_block : block:Simple.t -> t

(** Create a shape of blocks, for use as a meet constraint.
    Special tags are approximated to Unknown, as there is no good way
    to represent their shapes exactly.
    In particular, creating a Variant shape for a special tag is unsound,
    as it forbids the other special shapes that could apply. *)
val blocks_with_these_tags : Tag.Set.t -> t Or_unknown.t

val immutable_block
   : is_unique:bool
  -> Tag.t
  -> field_kind:Flambda_kind.t
  -> fields:t list
  -> t

val immutable_block_with_size_at_least
   : tag:Tag.t Or_unknown.t
  -> n:Targetint_31_63.Imm.t
  -> field_kind:Flambda_kind.t
  -> field_n_minus_one:Variable.t
  -> t

val variant
   : const_ctors:t
  -> non_const_ctors:t list Tag.Scannable.Map.t
  -> t

val open_variant_from_const_ctors_type : const_ctors:t -> t

val open_variant_from_non_const_ctor_with_size_at_least
   : n:Targetint_31_63.Imm.t
  -> field_n_minus_one:Variable.t
  -> t

val this_immutable_string : string -> t

val mutable_string : size:int -> t

val type_for_const : Reg_width_const.t -> t
val kind_for_const : Reg_width_const.t -> Flambda_kind.t

val create_function_declaration
   : code:Flambda.Code.t
  -> rec_info:t
  -> Function_declaration_type.t * Function_decl_inlining_decision.t

val create_non_inlinable_function_declaration
   : code_id:Code_id.t
  -> Function_declaration_type.t

val exactly_this_closure
   : Closure_id.t
  -> all_function_decls_in_set:Function_declaration_type.t Closure_id.Map.t
  -> all_closures_in_set:t Closure_id.Map.t
  -> all_closure_vars_in_set:t Var_within_closure.Map.t
  -> t

val at_least_the_closures_with_ids
   : this_closure:Closure_id.t
  -> Simple.t Closure_id.Map.t
  -> t

val closure_with_at_least_this_closure_var
   : this_closure:Closure_id.t
  -> Var_within_closure.t
  -> closure_element_var:Variable.t
  -> t

val closure_with_at_least_these_closure_vars
   : this_closure:Closure_id.t
  -> Variable.t Var_within_closure.Map.t
  -> t

val array_of_length : length:t -> t

val make_suitable_for_environment
   : t
  -> Typing_env.t
  -> suitable_for:Typing_env.t
  -> bind_to:Name.t
  -> Typing_env_extension.With_extra_variables.t

val expand_head : t -> Typing_env.t -> Resolved_type.t

(** Greatest lower bound of two types. *)
val meet : Meet_env.t -> t -> t -> (t * Typing_env_extension.t) Or_bottom.t

(** Least upper bound of two types. *)
val join
   : ?bound_name:Name.t
  -> Join_env.t
  -> t
  -> t
  -> t Or_unknown.t

(* Checks that the equation is not directly recursive (x : =x)
   (Depends on [Flambda_features.invariant_checks ()]) *)
val check_equation : Name.t -> t -> unit
