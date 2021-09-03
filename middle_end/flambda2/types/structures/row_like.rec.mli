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

[@@@ocaml.warning "+a-4-30-40-41-42"]

(* It would be nicer to have the following elsewhere other than with the
   generic definition of the row-like structure, but unfortunately
   restrictions on the evaluation of recursive modules prevent this, as the
   functor applications produce "undefined recursive module" errors. *)

(* CR mshinwell: tidy up! *)
(* CR gbury: Consider using Tag.Scannable.t instead of Tag.t in
             For_blocks, and use another module to specifically handle
             record of floats *)
module For_blocks : sig
  type t

  val create_bottom : unit -> t

  type open_or_closed = Open of Tag.t Or_unknown.t | Closed of Tag.t

  val create
     : field_kind:Flambda_kind.t
    -> field_tys:Type_grammar.t list
    -> open_or_closed
    -> t

  val create_blocks_with_these_tags : field_kind:Flambda_kind.t -> Tag.Set.t -> t

  val create_exactly_multiple
     : field_tys_by_tag:Type_grammar.t list Tag.Map.t
    -> t

  val all_tags : t -> Tag.Set.t Or_unknown.t

  val all_tags_and_sizes : t -> Targetint_31_63.Imm.t Tag.Map.t Or_unknown.t

  val get_singleton : t -> (Tag_and_size.t * Product.Int_indexed.t) option

  (** Get the nth field of the block if it is unambiguous.

      This can be done precisely using Type_grammar.meet_shape, but this meet
      can be expensive. This function allows to give a precise answer quickly
      in the common case where the block type is known exactly (for example,
      it is the result of a previous record or module allocation).

      This will return Unknown if:

      - There is no nth field (the read is invalid, and will produce bottom)

      - The block type represents a disjunction (several possible tags)

      - The tag or size is not exactly known

      - The nth field exists, is unique, but has Unknown type

      The handling of those cases could be improved:

      - When there is no valid field, Bottom could be returned instead

      - In the case of disjunctions, if all possible nth fields point to the
      same type, this type could be returned directly.

      - When the tag or size is not known but there is a unique possible value,
      it could be returned anyway

      - There could be a distinction between the first three cases (where we
      expect that doing the actual meet could give us a better result) and the
      last case where we already know what the result of the meet will be.
  *)
  val get_field : t -> Targetint_31_63.t -> Type_grammar.t Or_unknown_or_bottom.t

  val get_variant_field :
    t -> Tag.t -> Targetint_31_63.t -> Type_grammar.t Or_unknown_or_bottom.t

  val is_bottom : t -> bool

  (** The [Maps_to] value which [meet] returns contains the join of all
      [Maps_to] values in the range of the row-like structure after the meet
      operation has been completed. *)
  include Type_structure_intf.S
    with type t := t
    with type flambda_type := Type_grammar.t
    with type typing_env := Typing_env.t
    with type meet_env := Meet_env.t
    with type join_env := Join_env.t
    with type typing_env_extension := Typing_env_extension.t
end

module For_closures_entry_by_set_of_closures_contents : sig
  type t

   val create_exactly
     : Closure_id.t
    -> Set_of_closures_contents.t
    -> Closures_entry.t
    -> t

   val create_at_least
     : Closure_id.t
    -> Set_of_closures_contents.t
    -> Closures_entry.t
    -> t

  val get_singleton
     : t
    -> ((Closure_id.t * Set_of_closures_contents.t) * Closures_entry.t) option

  (** Same as For_blocks.get_field: attempt to find the type associated to
      the given environment variable without an expensive meet. *)
  val get_env_var : t -> Var_within_closure.t -> Type_grammar.t Or_unknown.t

  val map_function_decl_types
     : t
    -> f:(Function_declaration_type.t -> Function_declaration_type.t Or_bottom.t)
    -> t Or_bottom.t

  val map_closure_types
     : t
    -> f:(Type_grammar.t -> Type_grammar.t Or_bottom.t)
    -> t Or_bottom.t

  include Type_structure_intf.S
    with type t := t
    with type flambda_type := Type_grammar.t
    with type meet_env := Meet_env.t
    with type join_env := Join_env.t
    with type typing_env := Typing_env.t
    with type typing_env_extension := Typing_env_extension.t
end
