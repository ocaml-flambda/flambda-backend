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

type closure_entry_point =
  | Indirect_code_pointer
  | Direct_code_pointer

module Field : sig
  type return_kind =
    | Normal of int
    | Exn

  type t =
    | Block of int * Flambda_kind.t (* nth field of a block *)
    | Value_slot of Value_slot.t
    | Function_slot of Function_slot.t
    | Code_of_closure (* code_id in a set of closurse *)
    | Is_int (* value checked for [Is_int] *)
    | Get_tag (* tag of the value is read *)
    | Apply of closure_entry_point * return_kind
    | Code_id_of_call_witness of int
  (* Returns of functions: either exn path or nth value for normal returns *)

  val equal : t -> t -> bool

  val print : Format.formatter -> t -> unit

  val kind : t -> Flambda_kind.t

  module Map : Container_types.Map with type key = t

  val encode : t -> int

  val decode : int -> t
end

module FieldC : Datalog.Column.S with type t = int

module CoField : sig
  type t = Param of closure_entry_point * int

  val equal : t -> t -> bool

  val print : Format.formatter -> t -> unit

  module Map : Container_types.Map with type key = t

  val encode : t -> int

  val decode : int -> t
end

module CoFieldC : Datalog.Column.S with type t = int

type graph

val to_datalog : graph -> Datalog.database

type 'a rel0 = [> `Atom of Datalog.atom] as 'a

type ('a, 'b) rel1 = 'a Datalog.Term.t -> 'b rel0

type ('a, 'b, 'c) rel2 = 'a Datalog.Term.t -> ('b, 'c) rel1

type ('a, 'b, 'c, 'd) rel3 = 'a Datalog.Term.t -> ('b, 'c, 'd) rel2

val alias_rel : (Code_id_or_name.t, Code_id_or_name.t, _) rel2

val use_rel : (Code_id_or_name.t, Code_id_or_name.t, _) rel2

val accessor_rel : (Code_id_or_name.t, int, Code_id_or_name.t, _) rel3

val constructor_rel : (Code_id_or_name.t, int, Code_id_or_name.t, _) rel3

val coaccessor_rel : (Code_id_or_name.t, int, Code_id_or_name.t, _) rel3

val coconstructor_rel : (Code_id_or_name.t, int, Code_id_or_name.t, _) rel3

val propagate_rel :
  (Code_id_or_name.t, Code_id_or_name.t, Code_id_or_name.t, _) rel3

val any_usage_pred : (Code_id_or_name.t, _) rel1

val create : unit -> graph

val add_opaque_let_dependency :
  graph -> to_:Bound_pattern.t -> from:Name_occurrences.t -> unit

val add_alias : graph -> to_:Code_id_or_name.t -> from:Code_id_or_name.t -> unit

val add_use_dep :
  graph -> to_:Code_id_or_name.t -> from:Code_id_or_name.t -> unit

val add_use : graph -> Code_id_or_name.t -> unit

val add_propagate_dep :
  graph ->
  if_used:Code_id_or_name.t ->
  to_:Code_id_or_name.t ->
  from:Code_id_or_name.t ->
  unit

val add_constructor_dep :
  graph -> base:Code_id_or_name.t -> Field.t -> from:Code_id_or_name.t -> unit

val add_accessor_dep :
  graph -> to_:Code_id_or_name.t -> Field.t -> base:Code_id_or_name.t -> unit

val add_coaccessor_dep :
  graph -> to_:Code_id_or_name.t -> CoField.t -> base:Code_id_or_name.t -> unit

val add_coconstructor_dep :
  graph -> base:Code_id_or_name.t -> CoField.t -> from:Code_id_or_name.t -> unit

val print_iter_edges :
  print_edge:(Code_id_or_name.t * Code_id_or_name.t * string -> unit) ->
  graph ->
  unit
