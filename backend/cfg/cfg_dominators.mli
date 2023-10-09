[@@@ocaml.warning "+a-4-30-40-41-42"]

(* Dominator-related utility functions. *)

type dominator_tree = private
  { label : Label.t;
    children : dominator_tree list
  }

type t

val build : Cfg.t -> t
(* Computes all dominator-related information, in particular immediate
   dominators, dominance frontiers, and dominator tree for the passed CFG. *)

val is_dominating : t -> Label.t -> Label.t -> bool
(* [is_dominating doms x y] is [true] iff [x] is dominating [y] according to
   [doms]. That is, all paths from the entry node to [y] go through [x]. All
   edges, regular and exceptional are treated the same way. *)

val is_strictly_dominating : t -> Label.t -> Label.t -> bool
(* [is_strictly_dominating doms x y] is [true] iff [x] is strictly dominating
   [y] according to [doms]. That is, [is_dominating doms x y = true] and [x] is
   not equal [y]. All edges, regular and exceptional are treated the same way.*)

val find_dominance_frontier : t -> Label.t -> Label.Set.t
(* [find_dominance_frontier doms label] returns the dominance frontier for
   [label] according to [doms]. The definition we use is the following: "the
   dominance frontier of a node n is the set of all nodes m such that n
   dominates a predecessor of m, but does not strictly dominate m itself". *)

val dominator_tree : t -> dominator_tree

val iter_breadth_dominator_tree : t -> f:(Label.t -> unit) -> unit
(* [iter_breadth_dominator_tree doms ~f] iterates over the dominator tree from
   [doms] in a breadth-first manner, applying [f] to visited nodes. *)
