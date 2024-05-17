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

val debug_print : bool

module Field : sig
  type return_kind =
    | Normal of int
    | Exn

  type t =
    | Block of int
    | Value_slot of Value_slot.t
    | Function_slot of Function_slot.t
    | Code_of_closure
    | Is_int
    | Get_tag
    | Apply of return_kind

  val equal : t -> t -> bool

  val print : Format.formatter -> t -> unit

  module Map : Container_types.Map with type key = t
end

module Dep : sig
  type t =
    | Alias of Name.t
    | Use of Code_id_or_name.t
    | Contains of Code_id_or_name.t
    | Field of Field.t * Name.t
    | Block of Field.t * Code_id_or_name.t
    | Alias_if_def of Name.t * Code_id.t
    | Propagate of Name.t * Name.t

  val print : Format.formatter -> t -> unit

  module Set : Container_types.Set with type elt = t
end

type graph =
  { name_to_dep : (Code_id_or_name.t, Dep.Set.t) Hashtbl.t;
    used : (Code_id_or_name.t, unit) Hashtbl.t
  }

val pp_used_graph : Format.formatter -> graph -> unit

val create : unit -> graph

val inserts : ('a, Dep.Set.t) Hashtbl.t -> 'a -> Dep.Set.t -> unit

val add_opaque_let_dependency :
  graph -> Bound_pattern.t -> Name_occurrences.t -> unit

val add_let_field : graph -> Bound_pattern.t -> Field.t -> Name.t -> unit

val add_dep : graph -> Code_id_or_name.t -> Dep.t -> unit

val add_deps : graph -> Code_id_or_name.t -> Dep.Set.t -> unit

val add_let_dep : graph -> Bound_pattern.t -> Dep.t -> unit

val add_cont_dep : graph -> Int_ids.Variable.t -> Name.t -> unit

val add_func_param : graph -> param:Int_ids.Variable.t -> arg:Name.t -> unit

val add_use : graph -> Code_id_or_name.t -> unit

val add_called : graph -> Code_id.t -> unit
