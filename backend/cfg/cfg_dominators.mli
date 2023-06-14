[@@@ocaml.warning "+a-4-30-40-41-42"]

(* Dominator-related utility functions. *)

type dominators = Label.Set.t Label.Map.t

val compute_dominators : Cfg.t -> dominators

val is_dominating : dominators -> Label.t -> Label.t -> bool
(* [is_dominating doms x y] is [true] iff [x] is dominating [y] according to
   [dominators]. That is, all paths from the entry node to [y] go through [x].
   All edges, regular and exceptional are treated the same way. *)

val is_strictly_dominating : dominators -> Label.t -> Label.t -> bool
(* [is_strictly_dominating doms x y] is [true] iff [x] is strictly dominating
   [y] according to [dominators]. That is, [is_dominating x y = true] and [x] is
   not equal [y]. All edges, regular and exceptional are treated the same way.*)

type immediate_dominators = Label.t Label.Map.t

val compute_immediate_dominators : Cfg.t -> dominators -> immediate_dominators
(* [compute_immediate_dominators cfg doms] returns a map from labels to their
   immediate dominator, using the following definition: "the immediate dominator
   a node n is the unique node that strictly dominates n but does not strictly
   dominate any other node that strictly dominates n". By definition, the
   returned map has an binding for each node of [cfg], except the entry
   point. *)

type dominance_frontiers = Label.Set.t Label.Map.t

val compute_dominance_frontiers :
  Cfg.t -> dominators -> immediate_dominators -> dominance_frontiers
(* [compute_dominance_frontiers cfg doms idoms] returns a map from labels to
   their frontiers as sets of labels. The definition we use is the following:
   "the dominance frontier of a node n is the set of all nodes m such that n
   dominates a predecessor of m, but does not dominate m itself". *)

type dominator_tree =
  { label : Label.t;
    children : dominator_tree list
  }

val iter_breadth_dominator_tree : dominator_tree -> f:(Label.t -> unit) -> unit
(* [iter_breadth_dominator_tree tree ~f] iterates over [tree] in a breadth-first
   manner, applying [f] to visited nodes. *)

val compute_dominator_tree : Cfg.t -> immediate_dominators -> dominator_tree

type t =
  { dominators : dominators;
    immediate_dominators : immediate_dominators;
    dominance_frontiers : dominance_frontiers;
    dominator_tree : dominator_tree
  }

val build : Cfg.t -> t
