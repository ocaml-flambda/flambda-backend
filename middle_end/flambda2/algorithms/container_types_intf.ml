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

module type Thing_no_hash = sig
  type t

  include Map.OrderedType with type t := t

  val print : Format.formatter -> t -> unit
end

module type Thing = sig
  type t

  include Hashtbl.HashedType with type t := t

  include Map.OrderedType with type t := t

  val print : Format.formatter -> t -> unit
end

module type Set = sig
  type elt

  type t

  val empty : t

  val is_empty : t -> bool

  val mem : elt -> t -> bool

  val add : elt -> t -> t

  val singleton : elt -> t

  val remove : elt -> t -> t

  val union : t -> t -> t

  val inter : t -> t -> t

  val disjoint : t -> t -> bool

  val diff : t -> t -> t

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val subset : t -> t -> bool

  val iter : (elt -> unit) -> t -> unit

  val map : (elt -> elt) -> t -> t

  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a

  val for_all : (elt -> bool) -> t -> bool

  val exists : (elt -> bool) -> t -> bool

  val filter : (elt -> bool) -> t -> t

  val filter_map : (elt -> elt option) -> t -> t

  val partition : (elt -> bool) -> t -> t * t

  val cardinal : t -> int

  val elements : t -> elt list

  val min_elt : t -> elt

  val min_elt_opt : t -> elt option

  val max_elt : t -> elt

  val max_elt_opt : t -> elt option

  val choose : t -> elt

  val choose_opt : t -> elt option

  val split : elt -> t -> t * bool * t

  val find : elt -> t -> elt

  val of_list : elt list -> t

  val to_seq : t -> elt Seq.t

  val print : Format.formatter -> t -> unit

  val to_string : t -> string

  val union_list : t list -> t

  val get_singleton : t -> elt option
end

module type Map = sig
  type key

  module Set : Set with type elt = key

  type +!'a t

  val empty : 'a t

  val is_empty : 'a t -> bool

  val mem : key -> 'a t -> bool

  val add : key -> 'a -> 'a t -> 'a t

  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t

  val singleton : key -> 'a -> 'a t

  val remove : key -> 'a t -> 'a t

  val merge :
    (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t

  (* CR lmaurer: It's mentioned in [Stdlib.Map], but we really should be rid of
     the option type here. Surely anything that doesn't always return [Some] is
     niche enough that it can use [merge] (and we can generalize [merge] to
     cover it efficiently). *)
  val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t

  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

  val iter : (key -> 'a -> unit) -> 'a t -> unit

  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val for_all : (key -> 'a -> bool) -> 'a t -> bool

  val exists : (key -> 'a -> bool) -> 'a t -> bool

  val filter : (key -> 'a -> bool) -> 'a t -> 'a t

  val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t

  val filter_map_sharing : (key -> 'a -> 'a option) -> 'a t -> 'a t

  val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t

  val cardinal : 'a t -> int

  val bindings : 'a t -> (key * 'a) list

  val min_binding : 'a t -> key * 'a

  val min_binding_opt : 'a t -> (key * 'a) option

  val max_binding : 'a t -> key * 'a

  val max_binding_opt : 'a t -> (key * 'a) option

  val choose : 'a t -> key * 'a

  val choose_opt : 'a t -> (key * 'a) option

  val split : key -> 'a t -> 'a t * 'a option * 'a t

  val find : key -> 'a t -> 'a

  val find_opt : key -> 'a t -> 'a option

  val map : ('a -> 'b) -> 'a t -> 'b t

  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t

  val print_debug :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

  val of_list : (key * 'a) list -> 'a t

  val disjoint_union :
    ?eq:('a -> 'a -> bool) ->
    ?print:(Format.formatter -> 'a -> unit) ->
    'a t ->
    'a t ->
    'a t

  val map_keys : (key -> key) -> 'a t -> 'a t

  val keys : 'a t -> Set.t

  val data : 'a t -> 'a list

  val of_set : (key -> 'a) -> Set.t -> 'a t

  val print :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

  val diff_domains : 'a t -> 'a t -> 'a t

  val inter : (key -> 'a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

  val inter_domain_is_non_empty : 'a t -> 'a t -> bool

  val get_singleton : 'a t -> (key * 'a) option

  val replace : key -> ('a -> 'a) -> 'a t -> 'a t

  val map_sharing : ('a -> 'a) -> 'a t -> 'a t
end

module type S = sig
  type t

  module T : Thing with type t = t

  include Thing with type t := T.t

  module Set : Set with type elt = t

  module Map : Map with type key = t and module Set = Set
end

module type Set_plus_stdlib = sig
  include Set

  include Stdlib.Set.S with type elt := elt and type t := t
end

module type Map_plus_stdlib = sig
  include Map

  include Stdlib.Map.S with type key := key and type 'a t := 'a t
end

module type S_plus_stdlib = sig
  type t

  module T : Thing with type t = t

  include Thing with type t := T.t

  module Set : Set_plus_stdlib with type elt = t

  module Map : Map_plus_stdlib with type key = t and module Set = Set
end

module type Map_plus_iterator = sig
  include Map

  type 'a iterator

  val iterator : 'a t -> 'a iterator

  val current : 'a iterator -> (key * 'a) option

  val advance : 'a iterator -> 'a iterator

  val seek : 'a iterator -> key -> 'a iterator

  val to_seq : 'a t -> (key * 'a) Seq.t
end

module type S_plus_iterator = sig
  type t

  module T : Thing with type t = t

  include Thing with type t := T.t

  module Set : Set with type elt = t

  module Map : Map_plus_iterator with type key = t and module Set = Set
end
