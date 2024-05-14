[@@@ocaml.warning "+a-4-30-40-41-42"]

(** The implementation below is based on the following article:
    A Simple, Fast Dominance Algorithm
    Keith D. Cooper, Timothy J. Harvey, and Ken Kennedy *)

module List = ListLabels

let fatal = Misc.fatal_errorf

let debug = false

type doms = Label.t Label.Tbl.t

type dominance_frontiers = Label.Set.t Label.Tbl.t

type dominator_tree =
  { label : Label.t;
    children : dominator_tree list
  }

type t =
  { entry_label : Label.t;
    doms : doms;
    dominance_frontiers : dominance_frontiers;
    dominator_forest : dominator_tree list
  }

let rec is_dominating (doms : doms) left right =
  if Label.equal left right
  then true
  else
    let parent = Label.Tbl.find doms right in
    if Label.equal right parent then false else is_dominating doms left parent

let is_strictly_dominating (doms : doms) left right =
  (not (Label.equal left right)) && is_dominating doms left right

let invariant_doms : Cfg.t -> doms -> unit =
 fun cfg doms ->
  (* Check that `doms` has the same keys as the CFG. *)
  if Label.Tbl.length cfg.blocks <> Label.Tbl.length doms
  then fatal "Cfg_dominators.invariant_doms: invalid doms length";
  Label.Tbl.iter
    (fun key _ ->
      if not (Label.Tbl.mem doms key)
      then fatal "Cfg_dominators.invariant_doms: missing label %d" key)
    cfg.blocks;
  (* Check that (i) the immediate dominator is a strict dominator, and (ii)
     there is no other intermediate strict dominator - except for the entry
     block and unreachable blocks, which all have (by convention) themselves as
     immediate dominators. *)
  Label.Tbl.iter
    (fun n idom_n ->
      if not (Label.equal n idom_n)
      then (
        if not (is_strictly_dominating doms idom_n n)
        then
          fatal
            "Cfg_dominators.invariant_doms: the immediate dominator of %d, %d, \
             is not strictly dominating it"
            n idom_n;
        Label.Tbl.iter
          (fun m _ ->
            if is_strictly_dominating doms idom_n m
               && is_strictly_dominating doms m n
            then
              fatal
                "Cfg_dominators.invariant_doms: there is a strict dominator, \
                 %d, between %d and its immediate dominator, %d"
                m n idom_n)
          doms))
    doms

module DFSUtils : sig
  type component
  (* Identifier of a "component", which is the set of blocks reachable from a
     given (pseudo-)entry (i.e. a block with no predecessors). *)

  val equal_component : component -> component -> bool

  type components
  (* Map from block labels to component identifier. *)

  val find_component : components -> Label.t -> component
  (* [find_component components label] return the component identifier for
     [label] in [components] - cannot raise. *)

  val iter_blocks : Cfg.t -> f:(Cfg.basic_block -> unit) -> components
  (* [iter_blocks cfg ~f] iterates over the blocks of [cfg] in depth-first
     order, calling [f] on each node. In order to visit all blocks, even the
     ones actually dead, it first iterates from the entry point of [cfg], and
     then does the same from each pseudo-entry. The entry, and then each
     pseudo-entry, defines a component (set of blocks reachable from that
     origin), and the returned value is a (total) map from block labels to
     components. *)
end = struct
  type component = int

  let equal_component = Int.equal

  type components = component Label.Tbl.t

  let find_component components label =
    match Label.Tbl.find_opt components label with
    | None -> Misc.fatal_errorf "no component identifier for label %d" label
    | Some component -> component

  let iter_blocks_from :
      Cfg.t ->
      from:Label.t ->
      f:(Cfg.basic_block -> unit) ->
      component:component ->
      components:components ->
      unit =
   fun cfg ~from ~f ~component ~components ->
    let rec iter (label : Label.t) : unit =
      if not (Label.Tbl.mem components label)
      then (
        Label.Tbl.replace components label component;
        let block = Cfg.get_block_exn cfg label in
        Label.Set.iter
          (fun succ_label -> iter succ_label)
          (Cfg.successor_labels ~normal:true ~exn:true block);
        f block)
    in
    iter from

  exception Found of Label.t

  let find_pseudo_entry : Cfg.t -> components:components -> Label.t =
   fun cfg ~components ->
    try
      Cfg.iter_blocks cfg ~f:(fun label block ->
          if (not (Label.Tbl.mem components label))
             && Label.Set.is_empty block.predecessors
          then raise (Found label));
      Misc.fatal_error "did not find a block with no predecessors"
    with Found label -> label

  let rec iter_blocks_pseudo_entries :
      Cfg.t ->
      f:(Cfg.basic_block -> unit) ->
      component:component ->
      components:components ->
      unit =
   fun cfg ~f ~component ~components ->
    if Label.Tbl.length components < Label.Tbl.length cfg.blocks
    then (
      let from = find_pseudo_entry cfg ~components in
      iter_blocks_from cfg ~from ~f ~component ~components;
      iter_blocks_pseudo_entries cfg ~f ~component:(succ component) ~components)

  let iter_blocks : Cfg.t -> f:(Cfg.basic_block -> unit) -> components =
   fun cfg ~f ->
    let components = Label.Tbl.create (Label.Tbl.length cfg.blocks) in
    iter_blocks_from cfg ~from:cfg.entry_label ~f ~component:0 ~components;
    iter_blocks_pseudo_entries cfg ~f ~component:1 ~components;
    components
end

type post_order_stack = Cfg.basic_block Stack.t

let build_reverse_post_order : Cfg.t -> post_order_stack * DFSUtils.components =
 fun cfg ->
  let stack : Cfg.basic_block Stack.t = Stack.create () in
  let components =
    DFSUtils.iter_blocks cfg ~f:(fun block -> Stack.push block stack)
  in
  stack, components

let iter_stack : post_order_stack -> f:(Cfg.basic_block -> unit) -> unit =
 fun post_order_stack ~f ->
  let stack = Stack.copy post_order_stack in
  while not (Stack.is_empty stack) do
    let block : Cfg.basic_block = Stack.pop stack in
    f block
  done

type order = int Label.Tbl.t

let build_order : Cfg.t -> post_order_stack * order * DFSUtils.components =
 fun cfg ->
  let order = Label.Tbl.create (Label.Tbl.length cfg.blocks) in
  let stack, components = build_reverse_post_order cfg in
  iter_stack stack ~f:(fun (block : Cfg.basic_block) ->
      let label = block.start in
      Label.Tbl.replace order label (Label.Tbl.length order));
  stack, order, components

(* See Figure 3 in the cited article. The only difference is the comparison,
   which is reversed because of the way we distribute the idenfier when we build
   `post_order`. Both labels are in the same component. *)
let intersect : doms -> order -> Label.t -> Label.t -> Label.t =
 fun doms post_order b1 b2 ->
  let finger1 = ref b1 in
  let finger2 = ref b2 in
  let before_in_post_order (left : Label.t) (right : Label.t) =
    Label.Tbl.find post_order left > Label.Tbl.find post_order right
  in
  while not (Label.equal !finger1 !finger2) do
    while before_in_post_order !finger1 !finger2 do
      match Label.Tbl.find_opt doms !finger1 with
      | None -> assert false
      | Some f ->
        assert (not (Label.equal f !finger1));
        finger1 := f
    done;
    while before_in_post_order !finger2 !finger1 do
      match Label.Tbl.find_opt doms !finger2 with
      | None -> assert false
      | Some f ->
        assert (not (Label.equal f !finger2));
        finger2 := f
    done
  done;
  !finger1

(* See Figure 3 in the cited article. There are two differences:

   - a minor one: "Undefined", which is encoded here as a missing key;

   - a major one: we perform the computation "per-component", in order to handle
   dead code. *)
let compute_doms : Cfg.t -> doms =
 fun cfg ->
  let doms = Label.Tbl.create (Label.Tbl.length cfg.blocks) in
  Cfg.iter_blocks cfg ~f:(fun label block ->
      if Label.Set.is_empty block.predecessors
      then Label.Tbl.replace doms label label);
  (match Label.Tbl.find_opt doms cfg.entry_label with
  | None -> assert false
  | Some label -> assert (Label.equal label cfg.entry_label));
  let stack, order, components = build_order cfg in
  let changed = ref true in
  while !changed do
    changed := false;
    iter_stack stack ~f:(fun (block : Cfg.basic_block) ->
        let label = block.start in
        let same =
          match Label.Tbl.find_opt doms label with
          | None -> false
          | Some label' -> Label.equal label label'
        in
        if not same
        then (
          let label_component = DFSUtils.find_component components label in
          let new_idom = ref None in
          let predecessor_labels = Cfg.predecessor_labels block in
          List.iter predecessor_labels ~f:(fun predecessor_label ->
              let predecessor_component =
                DFSUtils.find_component components predecessor_label
              in
              if DFSUtils.equal_component label_component predecessor_component
              then
                match Label.Tbl.find_opt doms predecessor_label with
                | None -> ()
                | Some _ -> (
                  match !new_idom with
                  | None -> new_idom := Some predecessor_label
                  | Some new_idom_pred ->
                    new_idom
                      := Some
                           (intersect doms order predecessor_label new_idom_pred)
                  ));
          let new_idom =
            match !new_idom with
            | None -> Misc.fatal_errorf "no new idom for label %d" label
            | Some new_idom -> new_idom
          in
          match Label.Tbl.find_opt doms label with
          | None ->
            Label.Tbl.replace doms label new_idom;
            changed := true
          | Some dom_label ->
            if not (Label.equal dom_label new_idom)
            then (
              Label.Tbl.replace doms label new_idom;
              changed := true)))
  done;
  if debug then invariant_doms cfg doms;
  doms

let invariant_dominance_frontiers : Cfg.t -> doms -> dominance_frontiers -> unit
    =
 fun cfg doms dominance_frontiers ->
  Label.Tbl.iter
    (fun label frontier_labels ->
      Label.Set.iter
        (fun frontier_label ->
          if is_strictly_dominating doms label frontier_label
          then
            fatal
              "Cfg_dominators.invariant_dominance_frontiers: %d is strictly \
               dominating %d"
              label frontier_label;
          let block = Cfg.get_block_exn cfg frontier_label in
          let dominates_a_predecessor =
            Label.Set.exists
              (fun predecessor -> is_dominating doms label predecessor)
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

(* See Figure 5 in the cited article, the only difference is that because of
   dead code, there is a second condition to the while loop: the loop stops if
   it reaches a block which is its own immediate dominator (this means we have
   reached a "dead" entry point). *)
let compute_dominance_frontiers (cfg : Cfg.t) (doms : doms) :
    dominance_frontiers =
  let res = Label.Tbl.create (Label.Tbl.length doms) in
  Label.Tbl.iter
    (fun label _idom -> Label.Tbl.replace res label Label.Set.empty)
    doms;
  Label.Tbl.iter
    (fun label idom ->
      let block = Cfg.get_block_exn cfg label in
      let predecessor_labels = Cfg.predecessor_labels block in
      match predecessor_labels with
      | [] | [_] -> ()
      | _ :: _ :: _ ->
        List.iter predecessor_labels ~f:(fun predecessor_label ->
            let runner = ref predecessor_label in
            let continue = ref true in
            while (not (Label.equal !runner idom)) && !continue do
              let curr =
                match Label.Tbl.find_opt res !runner with
                | None -> Label.Set.empty
                | Some set -> set
              in
              Label.Tbl.replace res !runner (Label.Set.add label curr);
              let next_runner = Label.Tbl.find doms !runner in
              if Label.equal !runner next_runner
              then continue := false
              else runner := next_runner
            done))
    doms;
  if debug then invariant_dominance_frontiers cfg doms res;
  res

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

let invariant_dominator_forest : Cfg.t -> doms -> dominator_tree list -> unit =
 fun cfg doms dominator_forest ->
  let seen_by_iter = ref Label.Set.empty in
  List.iter dominator_forest ~f:(fun dominator_tree ->
      iter_breadth_dominator_tree dominator_tree ~f:(fun label ->
          seen_by_iter := Label.Set.add label !seen_by_iter));
  let seen_by_cfg =
    Cfg.fold_blocks cfg ~init:Label.Set.empty
      ~f:(fun label _block seen_by_cfg -> Label.Set.add label seen_by_cfg)
  in
  if not (Label.Set.equal !seen_by_iter seen_by_cfg)
  then
    fatal
      "Cfg_dominators.invariant_dominator_forest: iterator did not see all \
       blocks";
  let rec check_parent ~parent tree =
    let immediate_dominator : Label.t option =
      match Label.Tbl.find_opt doms tree.label with
      | None -> None
      | Some idom -> if Label.equal idom tree.label then None else Some idom
    in
    if not (Option.equal Label.equal immediate_dominator parent)
    then
      fatal
        "Cfg_dominators.invariant_dominator_forest: unexpected parent (%s) for \
         label %d"
        (Option.fold ~none:"none" ~some:string_of_int parent)
        tree.label;
    List.iter tree.children ~f:(fun child ->
        check_parent ~parent:(Some tree.label) child)
  in
  List.iter dominator_forest ~f:(fun dominator_tree ->
      check_parent ~parent:None dominator_tree)

let compute_dominator_forest_naive : Cfg.t -> doms -> dominator_tree list =
 fun cfg doms ->
  let rec children_of parent =
    Label.Tbl.fold
      (fun label (immediate_dominator : Label.t) acc ->
        if Label.equal parent immediate_dominator
           && not (Label.equal label immediate_dominator)
        then { label; children = children_of label } :: acc
        else acc)
      doms []
  in
  let res =
    Cfg.fold_blocks cfg ~init:[] ~f:(fun label _block acc ->
        match Label.Tbl.find_opt doms label with
        | None -> assert false
        | Some immediate_dominator ->
          if Label.equal label immediate_dominator
          then { label; children = children_of label } :: acc
          else acc)
  in
  if debug then invariant_dominator_forest cfg doms res;
  res

let compute_dominator_forest_opt : Cfg.t -> doms -> dominator_tree list =
 fun cfg doms ->
  let roots = ref [] in
  let children = Label.Tbl.create (Label.Tbl.length cfg.blocks) in
  let rec build_tree (label : Label.t) : dominator_tree =
    let children_labels =
      match Label.Tbl.find_opt children label with
      | None -> []
      | Some labels -> labels
    in
    { label; children = List.rev_map children_labels ~f:build_tree }
  in
  Cfg.iter_blocks cfg ~f:(fun label _block ->
      match Label.Tbl.find_opt doms label with
      | None -> assert false
      | Some immediate_dominator ->
        if Label.equal label immediate_dominator
        then roots := label :: !roots
        else
          let current =
            match Label.Tbl.find_opt children immediate_dominator with
            | None -> []
            | Some children -> children
          in
          Label.Tbl.replace children immediate_dominator (label :: current));
  let res = List.rev_map !roots ~f:build_tree in
  if debug then invariant_dominator_forest cfg doms res;
  res

let sort_forest : dominator_tree list -> dominator_tree list =
 fun trees ->
  let compare_label left right = Label.compare left.label right.label in
  List.sort ~cmp:compare_label trees

let rec equal_tree : dominator_tree -> dominator_tree -> bool =
 fun left right ->
  Label.equal left.label right.label
  && equal_forest left.children right.children

and equal_forest : dominator_tree list -> dominator_tree list -> bool =
 fun left right ->
  match sort_forest left, sort_forest right with
  | [], [] -> true
  | hd_left :: tl_left, hd_right :: tl_right ->
    equal_tree hd_left hd_right && equal_forest tl_left tl_right
  | [], _ :: _ | _ :: _, [] -> false

let build : Cfg.t -> t =
 fun cfg ->
  let doms = compute_doms cfg in
  let dominance_frontiers = compute_dominance_frontiers cfg doms in
  let dominator_forest_naive = compute_dominator_forest_naive cfg doms in
  let dominator_forest_opt = compute_dominator_forest_opt cfg doms in
  assert (equal_forest dominator_forest_naive dominator_forest_opt);
  { entry_label = cfg.entry_label;
    doms;
    dominance_frontiers;
    dominator_forest = dominator_forest_opt
  }

let is_dominating t left right = is_dominating t.doms left right

let is_strictly_dominating t left right =
  is_strictly_dominating t.doms left right

let find_dominance_frontier t label =
  match Label.Tbl.find_opt t.dominance_frontiers label with
  | Some frontier -> frontier
  | None ->
    fatal "Cfg_dominators.find_dominance_frontier: no frontier for label %d"
      label

let dominator_forest t = t.dominator_forest

let dominator_tree_for_entry_point t =
  match
    List.find_opt t.dominator_forest ~f:(fun tree ->
        Label.equal tree.label t.entry_label)
  with
  | None ->
    fatal
      "Cfg_dominators.dominator_tree_for_entry_point: no tree for entry point \
       (label %d)"
      t.entry_label
  | Some tree -> tree

let iter_breadth_dominator_forest t ~f =
  List.iter t.dominator_forest ~f:(fun dominator_tree ->
      iter_breadth_dominator_tree dominator_tree ~f)
