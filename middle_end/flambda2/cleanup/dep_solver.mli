type elt =
  | Top
  | Fields of
      { depth : int;
        fields : elt Global_flow_graph.Field.Map.t
      }
  | Bottom

type result = (Code_id_or_name.t, elt) Hashtbl.t

val pp_result : Format.formatter -> result -> unit

val fixpoint : Global_flow_graph.graph -> result
