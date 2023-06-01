[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Regalloc_utils

let split_debug = false

let split_live_ranges : bool Lazy.t = bool_of_param "SPLIT_LIVE_RANGES"

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

let log_dominance_frontier :
    indent:int -> Cfg_dominators.dominance_frontiers -> unit =
 fun ~indent dom_front ->
  log ~indent "dominance frontier:";
  Label.Map.iter
    (fun label labels ->
      log ~indent:(indent + 1) "block %d" label;
      Label.Set.iter
        (fun label -> log ~indent:(indent + 1) "block %d" label)
        labels)
    dom_front

let log_dominator_tree : indent:int -> Cfg_dominators.dominator_tree -> unit =
 fun ~indent dom_tree ->
  let rec ldt ~indent tree =
    log ~indent ". %d" tree.Cfg_dominators.label;
    List.iter tree.Cfg_dominators.children ~f:(fun child ->
        ldt ~indent:(succ indent) child)
  in
  ldt ~indent dom_tree

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

let is_unknown : Reg.t -> bool =
 fun reg ->
  match reg.Reg.loc with
  | Unknown -> true
  | Reg _ | Stack (Local _ | Incoming _ | Outgoing _ | Domainstate _) -> false

let filter_unknown : Reg.Set.t -> Reg.Set.t =
 fun regset -> Reg.Set.filter is_unknown regset

let fold_blocks :
    Cfg_with_liveness.t ->
    f:(Label.t -> Cfg.basic_block -> 'a -> 'a) ->
    init:'a ->
    'a =
 fun cfg_with_liveness ~f ~init ->
  Cfg.fold_blocks (Cfg_with_liveness.cfg cfg_with_liveness) ~f ~init

let get_block_exn : Cfg_with_liveness.t -> Label.t -> Cfg.basic_block =
 fun cfg_with_liveness label ->
  Cfg.get_block_exn (Cfg_with_liveness.cfg cfg_with_liveness) label

let live_at_block_beginning : Cfg_with_liveness.t -> Label.t -> Reg.Set.t =
 fun cfg_with_liveness label ->
  let block = get_block_exn cfg_with_liveness label in
  let first_id = Cfg.first_instruction_id block in
  match Cfg_with_liveness.liveness_find_opt cfg_with_liveness first_id with
  | None -> fatal "liveness information missing for instruction %d" first_id
  | Some { Cfg_liveness.before; across = _ } -> filter_unknown before

type destruction_kind =
  | Destruction_on_all_paths
  | Destruction_only_on_exceptional_path

let destruction_point_at_end : Cfg.basic_block -> destruction_kind option =
 fun block ->
  match block.terminator.desc, block.exn with
  | Raise _, Some _ -> Some Destruction_on_all_paths
  | _, None ->
    if Proc.is_destruction_point block.terminator.desc
    then Some Destruction_on_all_paths
    else None
  | _, Some _ ->
    if Proc.is_destruction_point block.terminator.desc
    then Some Destruction_on_all_paths
    else Some Destruction_only_on_exceptional_path
