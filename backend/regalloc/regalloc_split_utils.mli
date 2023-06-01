[@@@ocaml.warning "+a-4-30-40-41-42"]

open Regalloc_utils

val split_live_ranges : bool Lazy.t

val split_debug : bool

val split_verbose : bool Lazy.t

val split_invariants : bool Lazy.t

val log :
  indent:int -> ?no_eol:unit -> ('a, Format.formatter, unit) format -> 'a

val log_dominance_frontier :
  indent:int -> Cfg_dominators.dominance_frontiers -> unit

val log_dominator_tree : indent:int -> Cfg_dominators.dominator_tree -> unit

val log_substitution : indent:int -> Substitution.t -> unit

val log_substitutions : indent:int -> Substitution.map -> unit

val log_stack_subst : indent:int -> Substitution.t -> unit

val is_unknown : Reg.t -> bool

val filter_unknown : Reg.Set.t -> Reg.Set.t

val fold_blocks :
  Cfg_with_liveness.t ->
  f:(Label.t -> Cfg.basic_block -> 'a -> 'a) ->
  init:'a ->
  'a

val get_block_exn : Cfg_with_liveness.t -> Label.t -> Cfg.basic_block

val live_at_block_beginning : Cfg_with_liveness.t -> Label.t -> Reg.Set.t

type destruction_kind =
  | Destruction_on_all_paths
  | Destruction_only_on_exceptional_path

val destruction_point_at_end : Cfg.basic_block -> destruction_kind option
