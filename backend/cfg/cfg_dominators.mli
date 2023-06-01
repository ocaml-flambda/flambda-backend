[@@@ocaml.warning "+a-4-30-40-41-42"]

(* Dominator-related utility functions. *)

type dominators = Label.Set.t Label.Map.t

val compute_dominators : Cfg.t -> dominators

val is_dominating : dominators -> Label.t -> Label.t -> bool
(* [is_dominating doms x y] is [true] iff [x] is dominating [y] according to
   [dominators]. All edges, regular and exceptional are treated the same way. *)

val is_strictly_dominating : dominators -> Label.t -> Label.t -> bool
(* [is_strictly_dominating doms x y] is [true] iff [x] is strictly dominating
   [y] according to [dominators]. All edges, regular and exceptional are treated
   the same way.*)

type immediate_dominators = Label.t Label.Map.t

val compute_immediate_dominators : Cfg.t -> dominators -> immediate_dominators

type dominance_frontiers = Label.Set.t Label.Map.t

val compute_dominance_frontiers :
  Cfg.t -> dominators -> immediate_dominators -> dominance_frontiers

type dominator_tree =
  { label : Label.t;
    children : dominator_tree list
  }

val iter_breadth_dominator_tree : dominator_tree -> f:(Label.t -> unit) -> unit
(* [iter_breadth_dominator_tree tree ~f] iterates over [true] in a breadth-first
   manner, applying [f] to visited nodes. *)

val compute_dominator_tree : Cfg.t -> immediate_dominators -> dominator_tree

type t =
  { dominators : dominators;
    immediate_dominators : immediate_dominators;
    dominance_frontiers : dominance_frontiers;
    dominator_tree : dominator_tree
  }

val build : Cfg.t -> t
