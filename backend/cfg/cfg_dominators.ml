[@@@ocaml.warning "+a-4-30-40-41-42"]

(* CR-soon xclerc for xclerc: the implementations (and data structure choices)
   in this module are rather naive and should be optimized. *)

module List = ListLabels

let fatal = Misc.fatal_errorf

let debug = false

type dominators = Label.Set.t Label.Map.t

let compute_dominators (cfg : Cfg.t) =
  let all_labels =
    Cfg.fold_blocks cfg ~init:Label.Set.empty ~f:(fun label _block acc ->
        Label.Set.add label acc)
  in
  let init =
    Cfg.fold_blocks cfg ~init:Label.Map.empty ~f:(fun label _block acc ->
        Label.Map.add label
          (if Label.equal label cfg.entry_label
          then Label.Set.singleton cfg.entry_label
          else all_labels)
          acc)
  in
  let rec loop curr =
    let get_curr label =
      match Label.Map.find_opt label curr with
      | None ->
        fatal "Cfg_dominators.compute_dominators: unknown label %d" label
      | Some set -> set
    in
    let dominators, changed =
      Label.Set.fold
        (fun label (dominators, changed) ->
          let new_value =
            if Label.equal label cfg.entry_label
            then Label.Set.singleton cfg.entry_label
            else
              let predecessor_labels =
                Cfg.predecessor_labels (Cfg.get_block_exn cfg label)
              in
              match predecessor_labels with
              | [] -> Label.Set.singleton label
              | hd :: tl ->
                Label.Set.add label
                  (List.fold_left tl ~init:(get_curr hd) ~f:(fun acc label ->
                       Label.Set.inter acc (get_curr label)))
          in
          ( Label.Map.add label new_value dominators,
            changed || not (Label.Set.equal new_value (get_curr label)) ))
        all_labels (Label.Map.empty, false)
    in
    if changed then loop dominators else dominators
  in
  loop init

let is_dominating dominators left right =
  match Label.Map.find_opt right dominators with
  | None -> fatal "Cfg_dominators.is_dominating: unknown label %d" right
  | Some set -> Label.Set.mem left set

let is_strictly_dominating dominators left right =
  (not (Label.equal left right)) && is_dominating dominators left right

type immediate_dominators = Label.t Label.Map.t

let invariant_immediate_dominators :
    Cfg.t -> dominators -> immediate_dominators -> unit =
 fun cfg dominators immediate_dominators ->
  (* Check that `immediate_dominators` has the same keys as `dominator`, except
     the entry point. *)
  Label.Map.iter
    (fun key _ ->
      if not
           (Label.equal key cfg.entry_label
           || Label.Map.mem key immediate_dominators)
      then
        fatal "Cfg_dominators.invariant_immediate_dominators: missing label %d"
          key)
    dominators;
  (* Check that (i) the immediate dominator is a strict dominator, and (ii)
     there is no other intermediate strict dominator. *)
  Label.Map.iter
    (fun n idom_n ->
      if not (is_strictly_dominating dominators idom_n n)
      then
        fatal
          "Cfg_dominators.invariant_immediate_dominators: the immediate \
           dominator of %d, %d, is not strictly dominating it"
          n idom_n;
      Label.Map.iter
        (fun m _ ->
          if is_strictly_dominating dominators idom_n m
             && is_strictly_dominating dominators m n
          then
            fatal
              "Cfg_dominators.invariant_immediate_dominators: there is a \
               strict dominator, %d, between %d and its immediate dominator, \
               %d"
              m n idom_n)
        dominators)
    immediate_dominators

let compute_immediate_dominators : Cfg.t -> dominators -> immediate_dominators =
 fun cfg dominator_map ->
  let immediate_dominators =
    Label.Map.filter_map
      (fun label dominators ->
        let strict_dominators = Label.Set.remove label dominators in
        match Label.Set.choose_opt strict_dominators with
        | None ->
          if not (Label.equal label cfg.entry_label)
          then
            fatal
              "Cfg_dominators.compute_immediate_dominators: label %d has no \
               immediate dominator but is not the entry point (%d)"
              label cfg.entry_label
          else None
        | Some strict_dominator ->
          let immediate_dominator =
            Label.Set.fold
              (fun other_dominator immediate_dominator ->
                if is_strictly_dominating dominator_map immediate_dominator
                     other_dominator
                then other_dominator
                else immediate_dominator)
              strict_dominators strict_dominator
          in
          Some immediate_dominator)
      dominator_map
  in
  if debug
  then invariant_immediate_dominators cfg dominator_map immediate_dominators;
  immediate_dominators

type dominance_frontiers = Label.Set.t Label.Map.t

let invariant_dominance_frontiers :
    Cfg.t -> dominators -> dominance_frontiers -> unit =
 fun cfg dominators dominance_frontiers ->
  Label.Map.iter
    (fun label frontier_labels ->
      Label.Set.iter
        (fun frontier_label ->
          if is_strictly_dominating dominators label frontier_label
          then
            fatal
              "Cfg_dominators.invariant_dominance_frontiers: %d is strictly \
               dominating %d"
              label frontier_label;
          let block = Cfg.get_block_exn cfg frontier_label in
          let dominates_a_predecessor =
            Label.Set.exists
              (fun predecessor -> is_dominating dominators label predecessor)
              block.predecessors
          in
          if not dominates_a_predecessor
          then
            fatal
              "Cfg_dominators.invariant_dominance_frontiers: %d does not \
               dominate any predecessor of %d"
              label frontier_label)
        frontier_labels)
    dominance_frontiers

(* CR-soon xclerc for xclerc: `dominators` is passed only for the invariants. *)
let compute_dominance_frontiers :
    Cfg.t -> dominators -> immediate_dominators -> dominance_frontiers =
 fun cfg dominators immediate_dominators ->
  let idom l =
    match Label.Map.find_opt l immediate_dominators with
    | None ->
      fatal
        "Cfg_dominators.compute_dominance_frontiers: no immediate dominator \
         for %d"
        l
    | Some idom -> idom
  in
  let dominance_frontiers =
    ref
      (Cfg.fold_blocks cfg ~init:Label.Map.empty ~f:(fun label _block acc ->
           Label.Map.add label Label.Set.empty acc))
  in
  Cfg.iter_blocks cfg ~f:(fun label block ->
      let num_predecessors = Label.Set.cardinal block.predecessors in
      if num_predecessors >= 2
      then
        let idom_predecessor = idom label in
        Label.Set.iter
          (fun predecessor ->
            let curr = ref predecessor in
            while not (Label.equal !curr idom_predecessor) do
              dominance_frontiers
                := Label.Map.update !curr
                     (function
                       | None ->
                         fatal
                           "Cfg_dominators.compute_dominance_frontiers: \
                            frontier for %d has not been initialized"
                           !curr
                       | Some df -> Some (Label.Set.add label df))
                     !dominance_frontiers;
              curr := idom !curr
            done)
          block.predecessors);
  if debug
  then invariant_dominance_frontiers cfg dominators !dominance_frontiers;
  !dominance_frontiers

type dominator_tree =
  { label : Label.t;
    children : dominator_tree list
  }

let iter_breadth_dominator_tree : dominator_tree -> f:(Label.t -> unit) -> unit
    =
 fun dominator_tree ~f ->
  let queue = Queue.create () in
  Queue.add dominator_tree queue;
  while not (Queue.is_empty queue) do
    let dom_tree = Queue.take queue in
    f dom_tree.label;
    List.iter dom_tree.children ~f:(fun child -> Queue.add child queue)
  done

let invariant_dominator_tree :
    Cfg.t -> immediate_dominators -> dominator_tree -> unit =
 fun cfg immediate_dominators dominator_tree ->
  let seen_by_iter = ref Label.Set.empty in
  iter_breadth_dominator_tree dominator_tree ~f:(fun label ->
      seen_by_iter := Label.Set.add label !seen_by_iter);
  let seen_by_cfg =
    Cfg.fold_blocks cfg ~init:Label.Set.empty
      ~f:(fun label _block seen_by_cfg -> Label.Set.add label seen_by_cfg)
  in
  if not (Label.Set.equal !seen_by_iter seen_by_cfg)
  then
    fatal
      "Cfg_dominators.invariant_dominator_tree: iterator did not see all blocks";
  let rec check_parent ~parent tree =
    let immediate_dominator : Label.t option =
      Label.Map.find_opt tree.label immediate_dominators
    in
    if not (Option.equal Label.equal immediate_dominator parent)
    then
      fatal
        "Cfg_dominators.invariant_dominator_tree: unexpected parent (%s) for \
         label %d"
        (Option.fold ~none:"none" ~some:string_of_int parent)
        tree.label;
    List.iter tree.children ~f:(fun child ->
        check_parent ~parent:(Some tree.label) child)
  in
  check_parent ~parent:None dominator_tree

let compute_dominator_tree : Cfg.t -> immediate_dominators -> dominator_tree =
 fun cfg immediate_dominators ->
  let rec children_of parent =
    Label.Map.fold
      (fun label (immediate_dominator : Label.t) acc ->
        if Label.equal parent immediate_dominator
        then { label; children = children_of label } :: acc
        else acc)
      immediate_dominators []
  in
  let res =
    { label = cfg.entry_label; children = children_of cfg.entry_label }
  in
  if debug then invariant_dominator_tree cfg immediate_dominators res;
  res

type t =
  { dominators : dominators;
    immediate_dominators : immediate_dominators;
    dominance_frontiers : dominance_frontiers;
    dominator_tree : dominator_tree
  }

let build : Cfg.t -> t =
 fun cfg ->
  let dominators = compute_dominators cfg in
  let immediate_dominators = compute_immediate_dominators cfg dominators in
  let dominance_frontiers =
    compute_dominance_frontiers cfg dominators immediate_dominators
  in
  let dominator_tree = compute_dominator_tree cfg immediate_dominators in
  { dominators; immediate_dominators; dominance_frontiers; dominator_tree }
