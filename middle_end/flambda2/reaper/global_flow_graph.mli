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

module Field : sig
  type return_kind =
    | Normal of int
    | Exn

  type closure_entry_point =
    | Indirect_code_pointer
    | Direct_code_pointer

  type t =
    | Block of int (* nth field of a block *)
    | Value_slot of Value_slot.t
    | Function_slot of Function_slot.t
    | Code_of_closure (* code_id in a set of closurse *)
    | Is_int (* value checked for [Is_int] *)
    | Get_tag (* tag of the value is read *)
    | Apply of closure_entry_point * return_kind
  (* Returns of functions: either exn path or nth value for normal returns *)

  val equal : t -> t -> bool

  val print : Format.formatter -> t -> unit

  module Map : Container_types.Map with type key = t

  val encode : t -> int

  val decode : int -> t
end

module Dep : sig
  type t =
    | Alias of { target : Name.t }
    | Use of { target : Code_id_or_name.t }
    (* If source is not bottom, then target is fully used (top) *)
    | Accessor of
        { target : Name.t;
          relation : Field.t
        }
    (* The source is obtained from the target by accessing this relation *)
    | Constructor of
        { target : Code_id_or_name.t;
          relation : Field.t
        }
    (* The source is obtained from the target by building a value with this
       relation

       Note: in general there are multiple such dependencies with the same
       source, since a block has multiple fields for instance *)
    | Alias_if_def of
        { target : Name.t;
          if_defined : Code_id.t
        }
    (* If [if_defined] is not bottom, then this is equivalent to an alias to
       [target] *)
    | Propagate of
        { target : Name.t;
          source : Name.t
        }
  (* If the source this not bottom, then [source] is an alias to [target]
     (counterpart of [Alias_if_def], always generated in pairs) *)

  val print : Format.formatter -> t -> unit

  module Set : Container_types.Set with type elt = t
end

module FieldC : Datalog.Column.S with type t = int

type graph

val name_to_dep : graph -> (Code_id_or_name.t, Dep.Set.t) Hashtbl.t

val used : graph -> (Code_id_or_name.t, unit) Hashtbl.t

val to_datalog : graph -> Datalog.database

type 'a rel0 = [> `Atom of Datalog.atom] as 'a

type ('a, 'b) rel1 = 'a Datalog.Term.t -> 'b rel0

type ('a, 'b, 'c) rel2 = 'a Datalog.Term.t -> ('b, 'c) rel1

type ('a, 'b, 'c, 'd) rel3 = 'a Datalog.Term.t -> ('b, 'c, 'd) rel2

val alias_rel : (Code_id_or_name.t, Code_id_or_name.t, _) rel2

val use_rel : (Code_id_or_name.t, Code_id_or_name.t, _) rel2

val accessor_rel : (Code_id_or_name.t, int, Code_id_or_name.t, _) rel3

val constructor_rel : (Code_id_or_name.t, int, Code_id_or_name.t, _) rel3

val propagate_rel :
  (Code_id_or_name.t, Code_id_or_name.t, Code_id_or_name.t, _) rel3

val used_pred : (Code_id_or_name.t, _) rel1

val used_fields_top_rel : (Code_id_or_name.t, int, _) rel2

val used_fields_rel : (Code_id_or_name.t, int, Code_id_or_name.t, _) rel3

val pp_used_graph : Format.formatter -> graph -> unit

val create : unit -> graph

val add_opaque_let_dependency :
  graph -> to_:Bound_pattern.t -> from:Name_occurrences.t -> unit

val add_alias : graph -> to_:Code_id_or_name.t -> from:Name.t -> unit

val add_use_dep :
  graph -> to_:Code_id_or_name.t -> from:Code_id_or_name.t -> unit

val add_use : graph -> Code_id_or_name.t -> unit

val add_propagate_dep :
  graph -> if_used:Code_id.t -> to_:Name.t -> from:Name.t -> unit

val add_constructor_dep :
  graph -> base:Code_id_or_name.t -> Field.t -> from:Code_id_or_name.t -> unit

val add_accessor_dep :
  graph -> to_:Code_id_or_name.t -> Field.t -> base:Name.t -> unit
