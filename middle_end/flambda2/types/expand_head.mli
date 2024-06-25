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

(** Functions involving the expansion of any [Alias] type at the outermost level
    of a type. *)

module Expanded_type : sig
  type t

  val create_value : Type_grammar.head_of_kind_value -> t

  val create_naked_immediate : Type_grammar.head_of_kind_naked_immediate -> t

  val create_naked_float32 : Type_grammar.head_of_kind_naked_float32 -> t

  val create_naked_float : Type_grammar.head_of_kind_naked_float -> t

  val create_naked_int32 : Type_grammar.head_of_kind_naked_int32 -> t

  val create_naked_int64 : Type_grammar.head_of_kind_naked_int64 -> t

  val create_naked_nativeint : Type_grammar.head_of_kind_naked_nativeint -> t

  val create_naked_vec128 : Type_grammar.head_of_kind_naked_vec128 -> t

  val create_rec_info : Type_grammar.head_of_kind_rec_info -> t

  val create_region : Type_grammar.head_of_kind_region -> t

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
    | Naked_float32 of Type_grammar.head_of_kind_naked_float32
    | Naked_float of Type_grammar.head_of_kind_naked_float
    | Naked_int32 of Type_grammar.head_of_kind_naked_int32
    | Naked_int64 of Type_grammar.head_of_kind_naked_int64
    | Naked_nativeint of Type_grammar.head_of_kind_naked_nativeint
    | Naked_vec128 of Type_grammar.head_of_kind_naked_vec128
    | Rec_info of Type_grammar.head_of_kind_rec_info
    | Region of Type_grammar.head_of_kind_region

  val descr : t -> descr Or_unknown_or_bottom.t

  type descr_oub = private
    | Value of Type_grammar.head_of_kind_value Or_unknown_or_bottom.t
    | Naked_immediate of
        Type_grammar.head_of_kind_naked_immediate Or_unknown_or_bottom.t
    | Naked_float32 of
        Type_grammar.head_of_kind_naked_float32 Or_unknown_or_bottom.t
    | Naked_float of
        Type_grammar.head_of_kind_naked_float Or_unknown_or_bottom.t
    | Naked_int32 of
        Type_grammar.head_of_kind_naked_int32 Or_unknown_or_bottom.t
    | Naked_int64 of
        Type_grammar.head_of_kind_naked_int64 Or_unknown_or_bottom.t
    | Naked_nativeint of
        Type_grammar.head_of_kind_naked_nativeint Or_unknown_or_bottom.t
    | Naked_vec128 of
        Type_grammar.head_of_kind_naked_vec128 Or_unknown_or_bottom.t
    | Rec_info of Type_grammar.head_of_kind_rec_info Or_unknown_or_bottom.t
    | Region of Type_grammar.head_of_kind_region Or_unknown_or_bottom.t

  val descr_oub : t -> descr_oub
end

val expand_head : Typing_env.t -> Type_grammar.t -> Expanded_type.t

val expand_head0 :
  Typing_env.t ->
  Type_grammar.t ->
  known_canonical_simple_at_in_types_mode:Simple.t option ->
  Expanded_type.t

val is_bottom : Typing_env.t -> Type_grammar.t -> bool

val is_unknown : Typing_env.t -> Type_grammar.t -> bool

type to_erase =
  | Everything_not_in of Typing_env.t
  | All_variables_except of Variable.Set.t

(** This function doesn't descend past an alias type specifying a symbol: the
    assumption is that such types are already valid in the target environment.
    (This applies no matter what the setting of [to_erase]).

    The returned extension doesn't include equations involving the "bind-to"
    names but associated to other names. As an example, if one of the "bind-to"
    names is the result of a projection from a symbol, and the type of the
    corresponding symbol field is not an alias, then the relation will be lost.

    If any of the [Name.t]s provided as the "bind-to" names occur already in the
    supplied environment then the types provided as input to this function will
    be used instead of the types in such environment. (This situation does not
    usually occur but does arise when this function is called during function
    result type computation.) *)
val make_suitable_for_environment :
  Typing_env.t ->
  to_erase ->
  (Name.t * Type_grammar.t) list ->
  (* these [Name.t] values are called the "bind-to" names *)
  Typing_env_extension.With_extra_variables.t
