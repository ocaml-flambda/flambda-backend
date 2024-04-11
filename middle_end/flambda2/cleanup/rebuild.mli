val rebuild :
  Flambda_kind.t Name.Map.t ->
  Dep_solver.result ->
  Traverse.rev_expr ->
  Flambda.Expr.t * Name_occurrences.t * Code.t Code_id.Map.t * Slot_offsets.t
