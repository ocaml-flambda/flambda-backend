type atom =
  | Value
  | Value_int
  | Unboxed_float
  | Unboxed_int of Lambda.boxed_integer

val fold_left_layout :
  ('acc -> Clambda.ulambda -> atom -> 'acc) -> 'acc -> Clambda.ulambda ->
  Clambda_primitives.layout -> 'acc

type decomposition =
  | Atom of { offset : int; layout : atom }
  | Product of decomposition array

val decompose_free_vars :
  base_offset:int ->
  free_vars:('a * Clambda_primitives.layout) list ->
  ('a * decomposition) list
