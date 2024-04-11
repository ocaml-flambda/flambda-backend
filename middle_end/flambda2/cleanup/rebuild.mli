type result = {
  body : Flambda.Expr.t;
  free_names : Name_occurrences.t;
  all_code : Code.t Code_id.Map.t;
  slot_offsets : Slot_offsets.t;
}

val rebuild :
  Flambda_kind.t Name.Map.t ->
  Dep_solver.result ->
  Rev_expr.t ->
  result
