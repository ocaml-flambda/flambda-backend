(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Kosaraju's algorithm for strongly connected components.

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

module type Id = sig
  type t

  module Set : sig
    type elt = t

    type t

    val empty : t

    val add : elt -> t -> t

    val elements : t -> elt list

    val iter : (elt -> unit) -> t -> unit

    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  end

  module Map : sig
    type key = t

    type 'a t

    val empty : _ t

    val add : key -> 'a -> 'a t -> 'a t

    val cardinal : _ t -> int

    val bindings : 'a t -> (key * 'a) list

    val find : key -> 'a t -> 'a

    val iter : (key -> 'a -> unit) -> 'a t -> unit

    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

    val mem : key -> 'a t -> bool
  end

  val print : Format.formatter -> t -> unit
end

module type S = sig
  module Id : Id

  type directed_graph = Id.Set.t Id.Map.t
  (** If (a -> set) belongs to the map, it means that there are edges
      from [a] to every element of [set].  It is assumed that no edge
      points to a vertex not represented in the map. *)

  type component =
    | Has_loop of Id.t list
    | No_loop of Id.t

  val connected_components_sorted_from_roots_to_leaf
     : directed_graph
    -> component array

  val component_graph : directed_graph -> (component * int list) array
end

module Make (Id : Id) : S with module Id := Id
