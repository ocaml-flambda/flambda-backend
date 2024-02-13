[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Regalloc_utils

let split_debug = false

let split_live_ranges : bool Lazy.t = lazy true

let split_more_destruction_points : bool Lazy.t =
  bool_of_param "SPLIT_MORE_DESTR_POINTS"

let bool_of_param param_name =
  bool_of_param ~guard:(split_debug, "split_debug") param_name

let split_verbose : bool Lazy.t = bool_of_param "SPLIT_VERBOSE"

let split_invariants : bool Lazy.t = bool_of_param "SPLIT_INVARIANTS"

let log_function =
  lazy (make_log_function ~verbose:(Lazy.force split_verbose) ~label:"split")

let log :
    type a.
    indent:int -> ?no_eol:unit -> (a, Format.formatter, unit) format -> a =
 fun ~indent ?no_eol fmt -> (Lazy.force log_function).log ~indent ?no_eol fmt

let log_dominance_frontier : indent:int -> Cfg.t -> Cfg_dominators.t -> unit =
 fun ~indent cfg doms ->
  log ~indent "dominance frontier:";
  Cfg.iter_blocks cfg ~f:(fun label _block ->
      let frontier = Cfg_dominators.find_dominance_frontier doms label in
      log ~indent:(indent + 1) "block %d" label;
      Label.Set.iter
        (fun frontier_label ->
          log ~indent:(indent + 1) "block %d" frontier_label)
        frontier)

let log_dominator_tree : indent:int -> Cfg_dominators.dominator_tree -> unit =
 fun ~indent dom_tree ->
  let rec ldt ~indent tree =
    log ~indent ". %d" tree.Cfg_dominators.label;
    List.iter tree.Cfg_dominators.children ~f:(fun child ->
        ldt ~indent:(succ indent) child)
  in
  ldt ~indent dom_tree

let log_dominator_forest :
    indent:int -> Cfg_dominators.dominator_tree list -> unit =
 fun ~indent dom_forest ->
  List.iter dom_forest ~f:(fun dom_tree -> log_dominator_tree ~indent dom_tree)

let log_substitution : indent:int -> Substitution.t -> unit =
 fun ~indent subst ->
  Reg.Tbl.iter
    (fun old_reg new_reg ->
      log ~indent "%a -> %a" Printmach.reg old_reg Printmach.reg new_reg)
    subst

let log_substitutions : indent:int -> Substitution.map -> unit =
 fun ~indent substs ->
  log ~indent "substitutions:";
  Label.Tbl.iter
    (fun label (subst : Substitution.t) ->
      log ~indent:(indent + 1) "subst for block %d" label;
      log_substitution ~indent:(indent + 2) subst)
    substs

let log_stack_subst : indent:int -> Substitution.t -> unit =
 fun ~indent stack_subst ->
  log ~indent "stack substitution:";
  log_substitution ~indent:(indent + 1) stack_subst

let filter_unknown : Reg.Set.t -> Reg.Set.t =
 fun regset -> Reg.Set.filter Reg.is_unknown regset

let live_at_block_beginning : Cfg_with_infos.t -> Label.t -> Reg.Set.t =
 fun cfg_with_infos label ->
  let block = Cfg_with_infos.get_block_exn cfg_with_infos label in
  let first_id = Cfg.first_instruction_id block in
  match Cfg_with_infos.liveness_find_opt cfg_with_infos first_id with
  | None -> fatal "liveness information missing for instruction %d" first_id
  | Some { Cfg_liveness.before; across = _ } -> filter_unknown before

type destruction_kind =
  | Destruction_on_all_paths
  | Destruction_only_on_exceptional_path

let destruction_point_at_end : Cfg.basic_block -> destruction_kind option =
 fun block ->
  let more_destruction_points = Lazy.force split_more_destruction_points in
  if Proc.is_destruction_point ~more_destruction_points block.terminator.desc
  then Some Destruction_on_all_paths
  else if Option.is_none block.exn
  then None
  else (
    assert (Cfg.can_raise_terminator block.terminator.desc);
    if Label.Set.is_empty (Cfg.successor_labels block ~normal:true ~exn:false)
    then Some Destruction_on_all_paths
    else Some Destruction_only_on_exceptional_path)
