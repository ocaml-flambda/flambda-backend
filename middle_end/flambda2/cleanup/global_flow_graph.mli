val debug_print : bool

module Field : sig
  type t =
    | Block of int
    | Value_slot of Value_slot.t
    | Function_slot of Function_slot.t
    | Code_of_closure

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

type fun_graph =
  { name_to_dep : (Code_id_or_name.t, Dep.Set.t) Hashtbl.t;
    used : (Code_id_or_name.t, unit) Hashtbl.t
  }

type graph =
  { toplevel_graph : fun_graph;
    function_graphs : (Code_id.t, fun_graph) Hashtbl.t
  }

val pp_used_fun_graph : Format.formatter -> fun_graph -> unit

val pp_used : Format.formatter -> graph -> unit

val create : unit -> fun_graph

val inserts : ('a, Dep.Set.t) Hashtbl.t -> 'a -> Dep.Set.t -> unit

val add_opaque_let_dependency :
  fun_graph -> Bound_pattern.t -> Name_occurrences.t -> unit

val add_let_field : fun_graph -> Bound_pattern.t -> Field.t -> Name.t -> unit

val add_dep : fun_graph -> Code_id_or_name.t -> Dep.t -> unit

val add_deps : fun_graph -> Code_id_or_name.t -> Dep.Set.t -> unit

val add_let_dep : fun_graph -> Bound_pattern.t -> Dep.t -> unit

val add_cont_dep : fun_graph -> Int_ids.Variable.t -> Name.t -> unit

val add_func_param : fun_graph -> param:Int_ids.Variable.t -> arg:Name.t -> unit

val add_use : fun_graph -> Code_id_or_name.t -> unit

val add_called : fun_graph -> Code_id.t -> unit
