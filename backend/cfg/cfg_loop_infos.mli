[@@@ocaml.warning "+a-4-30-40-41-42"]

type dominators = Label.Set.t Label.Map.t

val compute_dominators : Cfg.t -> dominators

val is_dominating : dominators -> Label.t -> Label.t -> bool
(* [is_dominating doms x y] is [true] iff [x] is dominating [y] according to
   [dominators]. *)

val is_strictly_dominating : dominators -> Label.t -> Label.t -> bool
(* [is_strictly_dominating doms x y] is [true] iff [x] is strictly dominating
   [y] according to [dominators]. *)

module Edge : sig
  type t =
    { src : Label.t;
      dst : Label.t
    }

  val compare : t -> t -> int
end

module EdgeMap : Map.S with type key = Edge.t

val compute_back_edges : Cfg.t -> dominators -> Edge.t list

val compute_loop_of_back_edge : Cfg.t -> Edge.t -> Label.Set.t
(* Assumes the passed edge is a back edge. *)

type loops = Label.Set.t EdgeMap.t

val compute_loops_of_back_edges : Cfg.t -> Edge.t list -> loops
(* Assumes the passed edges are back edges. *)

type loop_depths = int Label.Map.t

val compute_loop_depths : Cfg.t -> loops -> loop_depths

type t =
  { dominators : dominators;
    back_edges : Edge.t list;
    loops : loops;
    loop_depths : loop_depths
  }

val build : Cfg.t -> t
