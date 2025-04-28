[@@@ocaml.warning "+a-30-40-41-42"]

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]
open! Regalloc_utils
module Substitution = Regalloc_substitution

let split_live_ranges : bool Lazy.t =
  bool_of_param ~default:true "SPLIT_LIVE_RANGES"

let split_more_destruction_points : bool Lazy.t =
  bool_of_param "SPLIT_MORE_DESTR_POINTS"

let log_function = lazy (make_log_function ~label:"split")

let indent () = (Lazy.force log_function).indent ()

let dedent () = (Lazy.force log_function).dedent ()

let log : type a. ?no_eol:unit -> (a, Format.formatter, unit) format -> a =
 fun ?no_eol fmt -> (Lazy.force log_function).log ?no_eol fmt

let log_dominance_frontier : Cfg.t -> Cfg_dominators.t -> unit =
 fun cfg doms ->
  log "dominance frontier:";
  indent ();
  Cfg.iter_blocks cfg ~f:(fun label _block ->
      let frontier = Cfg_dominators.find_dominance_frontier doms label in
      log "block %a" Label.format label;
      Label.Set.iter
        (fun frontier_label -> log "block %a" Label.format frontier_label)
        frontier);
  dedent ()

let log_dominator_tree : Cfg_dominators.dominator_tree -> unit =
 fun dom_tree ->
  let rec ldt tree =
    log ". %a" Label.format tree.Cfg_dominators.label;
    indent ();
    List.iter tree.Cfg_dominators.children ~f:(fun child -> ldt child);
    dedent ()
  in
  ldt dom_tree

let log_dominator_forest : Cfg_dominators.dominator_tree list -> unit =
 fun dom_forest ->
  List.iter dom_forest ~f:(fun dom_tree -> log_dominator_tree dom_tree)

let log_substitution : Substitution.t -> unit =
 fun subst ->
  Reg.Tbl.iter
    (fun old_reg new_reg ->
      log "%a -> %a" Printreg.reg old_reg Printreg.reg new_reg)
    subst

let log_substitutions : Substitution.map -> unit =
 fun substs ->
  log "substitutions:";
  Label.Tbl.iter
    (fun label (subst : Substitution.t) ->
      indent ();
      log "subst for block %a" Label.format label;
      indent ();
      log_substitution subst;
      dedent ();
      dedent ())
    substs

let log_stack_subst : Substitution.t -> unit =
 fun stack_subst ->
  log "stack substitution:";
  indent ();
  log_substitution stack_subst;
  dedent ()

let filter_unknown : Reg.Set.t -> Reg.Set.t =
 fun regset -> Reg.Set.filter Reg.is_unknown regset

let live_at_block_beginning : Cfg_with_infos.t -> Label.t -> Reg.Set.t =
 fun cfg_with_infos label ->
  let block = Cfg_with_infos.get_block_exn cfg_with_infos label in
  let first_id = Cfg.first_instruction_id block in
  match Cfg_with_infos.liveness_find_opt cfg_with_infos first_id with
  | None ->
    fatal "liveness information missing for instruction %a" InstructionId.format
      first_id
  | Some { Cfg_liveness.before; across = _ } -> filter_unknown before

type destruction_kind =
  | Destruction_on_all_paths
  | Destruction_only_on_exceptional_path

let equal_destruction_kind left right =
  match left, right with
  | Destruction_on_all_paths, Destruction_on_all_paths
  | Destruction_only_on_exceptional_path, Destruction_only_on_exceptional_path
    ->
    true
  | (Destruction_on_all_paths | Destruction_only_on_exceptional_path), _ ->
    false

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
