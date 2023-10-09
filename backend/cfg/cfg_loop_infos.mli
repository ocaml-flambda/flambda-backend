[@@@ocaml.warning "+a-4-30-40-41-42"]

module Edge : sig
  type t =
    { src : Label.t;
      dst : Label.t
    }

  val compare : t -> t -> int
end

module EdgeMap : Map.S with type key = Edge.t

val compute_back_edges : Cfg.t -> Cfg_dominators.t -> Edge.t list

type loop = Label.Set.t
(* Blocks in a loop; if a node is part of several/nested loops, it will appear
   in several sets. *)

val compute_loop_of_back_edge : Cfg.t -> Edge.t -> loop
(* Assumes the passed edge is a back edge. *)

type loops = loop EdgeMap.t
(* Map from back edge to loop. *)

val compute_loops_of_back_edges : Cfg.t -> Edge.t list -> loops
(* Assumes the passed edges are back edges. *)

type header_map = loop list Label.Map.t
(* Map from loop header to loops. *)

val compute_header_map : loops -> header_map

type loop_depths = int Label.Map.t
(* Maps labels to the number of nested loops it is part of. *)

val compute_loop_depths : Cfg.t -> header_map -> loop_depths

type t = private
  { back_edges : Edge.t list;
    loops : loops;
    header_map : header_map;
    loop_depths : loop_depths
  }

val build : Cfg.t -> Cfg_dominators.t -> t
