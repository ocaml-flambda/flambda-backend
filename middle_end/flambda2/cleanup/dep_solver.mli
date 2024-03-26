type field = Cleanup_deps.field

module Field :
  sig
    module Map : Container_types.Map with type key = field
  end

type elt =
  | Top
  | Fields of { depth : int; fields : elt Field.Map.t }
  | Bottom

type graph = Cleanup_deps.graph
type result = (Code_id_or_name.t, elt) Hashtbl.t

val pp_result : Format.formatter -> result -> unit
val fixpoint : graph -> result
