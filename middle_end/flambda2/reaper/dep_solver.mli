(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Nathanaëlle Courant, Pierre Chambart, OCamlPro               *)
(*                                                                        *)
(*   Copyright 2024 OCamlPro SAS                                          *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type result

val pp_result : Format.formatter -> result -> unit

val fixpoint : Global_flow_graph.graph -> result

type 'a unboxed_fields =
  | Not_unboxed of 'a
  | Unboxed of 'a unboxed_fields Global_flow_graph.Field.Map.t

val print_unboxed_fields :
  (Format.formatter -> 'a -> unit) ->
  Format.formatter ->
  'a unboxed_fields ->
  unit

type unboxed = Variable.t unboxed_fields Global_flow_graph.Field.Map.t

type changed_representation =
  | Block_representation of
      (int * Flambda_primitive.Block_access_kind.t) unboxed_fields
      Global_flow_graph.Field.Map.t
      * int
  | Closure_representation of
      Value_slot.t unboxed_fields Global_flow_graph.Field.Map.t
      * Function_slot.t Function_slot.Map.t
      * Function_slot.t

val map_unboxed_fields : ('a -> 'b) -> 'a unboxed_fields -> 'b unboxed_fields

val get_unboxed_fields : result -> Code_id_or_name.t -> unboxed option

val get_changed_representation :
  result -> Code_id_or_name.t -> changed_representation option

val has_use : result -> Code_id_or_name.t -> bool

val has_source : result -> Code_id_or_name.t -> bool

val field_used :
  result -> Code_id_or_name.t -> Global_flow_graph.Field.t -> bool

val cofield_has_use :
  result -> Code_id_or_name.t -> Global_flow_graph.CoField.t -> bool

val not_local_field_has_source :
  result -> Code_id_or_name.t -> Global_flow_graph.Field.t -> bool

(** Color of node when producing the graph as a .dot *)
val print_color : result -> Code_id_or_name.t -> string

(** [rewrite_kind_with_subkind result var kind_with_subkind]
    For [kind_with_subkind] the kind associated to variable [var],
    removes the subkinds on the parts that are not used in [result]. *)
val rewrite_kind_with_subkind :
  result -> Name.t -> Flambda_kind.With_subkind.t -> Flambda_kind.With_subkind.t
(* CR pchambart: rename to remove_unused_part_of_subkind or something like
   that *)

val cannot_change_calling_convention : result -> Code_id.t -> bool

val code_id_actually_called : result -> Name.t -> (Code_id.t * int) option
