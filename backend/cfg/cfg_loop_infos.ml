[@@@ocaml.warning "+a-4-30-40-41-42"]

module List = ListLabels

let fatal = Misc.fatal_errorf

let debug = false

type dominators = Label.Set.t Label.Map.t

let compute_dominators (cfg : Cfg.t) =
  (* CR-soon xclerc for xclerc: this is a naive implementation, that could
     clearly be optimized and/or rely on the dataflow framework. *)
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
        fatal "Cfg_loop_infos.compute_dominators: unknown label %d" label
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
  | None -> fatal "Cfg_loop_infos.is_dominating: unknown label %d" right
  | Some set -> Label.Set.mem left set

let is_strictly_dominating dominators left right =
  (not (Label.equal left right)) && is_dominating dominators left right

module Edge = struct
  type t =
    { src : Label.t;
      dst : Label.t
    }

  let compare { src = left_src; dst = left_dst }
      { src = right_src; dst = right_dst } =
    match Label.compare left_src right_src with
    | 0 -> Label.compare left_dst right_dst
    | c -> c
end

module EdgeMap : Map.S with type key = Edge.t = Map.Make (Edge)

(* CR-soon xclerc for xclerc: consider deduplicating. *)
let compute_back_edges cfg dominators =
  Cfg.fold_blocks cfg ~init:[] ~f:(fun src_label src_block acc ->
      let dst_labels =
        (* CR-soon xclerc for xclerc: probably safe to pass `~exn:false`. *)
        Cfg.successor_labels ~normal:true ~exn:true src_block
      in
      Label.Set.fold
        (fun dst_label acc ->
          let is_back_edge = is_dominating dominators dst_label src_label in
          if is_back_edge
          then { Edge.src = src_label; dst = dst_label } :: acc
          else acc)
        dst_labels acc)

type loop = Label.Set.t

let compute_loop_of_back_edge cfg { Edge.src; dst } =
  let rec visit stack acc =
    match stack with
    | [] -> acc
    | hd :: tl ->
      let block = Cfg.get_block_exn cfg hd in
      let predecessor_labels = Cfg.predecessor_labels block in
      let stack, acc =
        List.fold_left predecessor_labels ~init:(tl, acc)
          ~f:(fun (stack, acc) predecessor_label ->
            if not (Label.Set.mem predecessor_label acc)
            then predecessor_label :: stack, Label.Set.add predecessor_label acc
            else stack, acc)
      in
      visit stack acc
  in
  visit [src] (Label.Set.add src (Label.Set.singleton dst))

type loops = loop EdgeMap.t

let compute_loops_of_back_edges cfg back_edges =
  List.fold_left back_edges ~init:EdgeMap.empty ~f:(fun acc edge ->
      EdgeMap.add edge (compute_loop_of_back_edge cfg edge) acc)

type loop_depths = int Label.Map.t

let compute_loop_depths cfg (loops : loops) =
  let init =
    Cfg.fold_blocks cfg ~init:Label.Map.empty ~f:(fun label _block acc ->
        Label.Map.add label 0 acc)
  in
  EdgeMap.fold
    (fun _edge labels acc ->
      Label.Set.fold
        (fun label acc ->
          Label.Map.update label
            (function
              | None ->
                fatal "Cfg_loop_infos.compute_loop_depths: unknown label %d"
                  label
              | Some depth -> Some (succ depth))
            acc)
        labels acc)
    loops init

type t =
  { dominators : dominators;
    back_edges : Edge.t list;
    loops : loops;
    loop_depths : loop_depths
  }

let build cfg =
  let dominators = compute_dominators cfg in
  let back_edges = compute_back_edges cfg dominators in
  let loops = compute_loops_of_back_edges cfg back_edges in
  let loop_depths = compute_loop_depths cfg loops in
  if debug
  then (
    Format.eprintf "*** Cfg_loop_infos.build for %S\n" cfg.Cfg.fun_name;
    Label.Map.iter
      (fun label dominators ->
        Format.eprintf "dominators for %d:\n" label;
        Label.Set.iter (Format.eprintf "- %d\n") dominators)
      dominators;
    Format.eprintf "back edges:\n";
    List.iter back_edges ~f:(fun { Edge.src; dst } ->
        Format.eprintf "- %d -> %d\n" src dst);
    EdgeMap.iter
      (fun { Edge.src; dst } labels ->
        Format.eprintf "loop for back edge %d -> %d:\n" src dst;
        Label.Set.iter (Format.eprintf "- %d:\n") labels)
      loops;
    Label.Map.iter (Format.eprintf "loop depth for %d is %d\n") loop_depths);
  { dominators; back_edges; loops; loop_depths }
