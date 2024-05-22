[@@@ocaml.warning "+a-4-30-40-41-42"]

(** Dominator-related utility functions. *)

type dominator_tree = private
  { label : Label.t;
    children : dominator_tree list
  }

type t

(** Computes all dominator-related information, in particular immediate
   dominators, dominance frontiers, and dominator forest for the passed CFG. *)
val build : Cfg.t -> t

(** [is_dominating doms x y] is [true] iff [x] is dominating [y] according to
   [doms]. That is, all paths from the entry node to [y] go through [x]. All
   edges, regular and exceptional are treated the same way. *)
val is_dominating : t -> Label.t -> Label.t -> bool

(** [is_strictly_dominating doms x y] is [true] iff [x] is strictly dominating
   [y] according to [doms]. That is, [is_dominating doms x y = true] and [x] is
   not equal [y]. All edges, regular and exceptional are treated the same way.*)
val is_strictly_dominating : t -> Label.t -> Label.t -> bool

(** [find_dominance_frontier doms label] returns the dominance frontier for
   [label] according to [doms]. The definition we use is the following: "the
   dominance frontier of a node n is the set of all nodes m such that n
   dominates a predecessor of m, but does not strictly dominate m itself". *)
val find_dominance_frontier : t -> Label.t -> Label.Set.t

(** Returns all dominator trees. Typically one of these will correspond to the
   entry point of the program and the remainder to dead code. *)
val dominator_forest : t -> dominator_tree list

(** Returns the unique dominator tree rooted at the entry point of the program
   (thus ignoring any isolated trees that correspond to dead code). *)
val dominator_tree_for_entry_point : t -> dominator_tree

(** [iter_breadth_dominator_forest doms ~f] iterates over the dominator forest
   from [doms] in a breadth-first manner (iterating over a whole tree of the
   forest before moving to the next tree), applying [f] to visited nodes. *)
val iter_breadth_dominator_forest : t -> f:(Label.t -> unit) -> unit
