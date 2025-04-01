[@@@ocaml.warning "+a-30-40-41-42"]

open Regalloc_utils

val split_live_ranges : bool Lazy.t

val split_debug : bool

val split_invariants : bool Lazy.t

val indent : unit -> unit

val dedent : unit -> unit

val log : ?no_eol:unit -> ('a, Format.formatter, unit) format -> 'a

val log_dominance_frontier : Cfg.t -> Cfg_dominators.t -> unit

val log_dominator_tree : Cfg_dominators.dominator_tree -> unit

val log_dominator_forest : Cfg_dominators.dominator_tree list -> unit

val log_substitution : Substitution.t -> unit

val log_substitutions : Substitution.map -> unit

val log_stack_subst : Substitution.t -> unit

val live_at_block_beginning : Cfg_with_infos.t -> Label.t -> Reg.Set.t

type destruction_kind =
  | Destruction_on_all_paths
  | Destruction_only_on_exceptional_path

val equal_destruction_kind : destruction_kind -> destruction_kind -> bool

val destruction_point_at_end : Cfg.basic_block -> destruction_kind option
