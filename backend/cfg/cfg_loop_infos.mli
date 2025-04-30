[@@@ocaml.warning "+a-40-41-42"]

val compute_back_edges : Cfg.t -> Cfg_dominators.t -> Cfg_edge.Set.t

type loop = Label.Set.t
(* Blocks in a loop; if a node is part of several/nested loops, it will appear
   in several sets. *)

val compute_loop_of_back_edge : Cfg.t -> Cfg_edge.t -> loop
(* Assumes the passed edge is a back edge. *)

type loops = loop Cfg_edge.Map.t
(* Map from back edge to loop. *)

val compute_loops_of_back_edges : Cfg.t -> Cfg_edge.Set.t -> loops
(* Assumes the passed edges are back edges. *)

type header_map = loop list Label.Map.t
(* Map from loop header to loops. *)

val compute_header_map : loops -> header_map

type loop_depths = int Label.Map.t
(* Maps labels to the number of nested loops it is part of. *)

val compute_loop_depths : Cfg.t -> header_map -> loop_depths

type t = private
  { back_edges : Cfg_edge.Set.t;
    loops : loops;
    header_map : header_map;
    loop_depths : loop_depths
  }

val build : Cfg.t -> Cfg_dominators.t -> t
