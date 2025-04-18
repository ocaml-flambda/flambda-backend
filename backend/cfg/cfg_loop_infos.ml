[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]
module List = ListLabels

let fatal = Misc.fatal_errorf

let debug = false

let compute_back_edges cfg dominators =
  Cfg.fold_blocks cfg ~init:Cfg_edge.Set.empty
    ~f:(fun src_label src_block acc ->
      let dst_labels =
        (* CR-soon xclerc for xclerc: probably safe to pass `~exn:false`. *)
        Cfg.successor_labels ~normal:true ~exn:true src_block
      in
      Label.Set.fold
        (fun dst_label acc ->
          let is_back_edge =
            Cfg_dominators.is_dominating dominators dst_label src_label
          in
          if is_back_edge
          then
            Cfg_edge.Set.add { Cfg_edge.src = src_label; dst = dst_label } acc
          else acc)
        dst_labels acc)

type loop = Label.Set.t

let compute_loop_of_back_edge cfg { Cfg_edge.src; dst } =
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

type loops = loop Cfg_edge.Map.t

let compute_loops_of_back_edges cfg back_edges =
  Cfg_edge.Set.fold
    (fun edge acc ->
      Cfg_edge.Map.add edge (compute_loop_of_back_edge cfg edge) acc)
    back_edges Cfg_edge.Map.empty

type header_map = loop list Label.Map.t

let compute_header_map loops =
  let compare_loop_by_cardinal left right =
    Int.compare (Label.Set.cardinal left) (Label.Set.cardinal right)
  in
  Cfg_edge.Map.fold
    (fun { Cfg_edge.src = _; dst = header } labels acc ->
      Label.Map.update header
        (function
          | None -> Some [labels]
          | Some l -> Some (List.merge ~cmp:compare_loop_by_cardinal [labels] l))
        acc)
    loops Label.Map.empty

type loop_depths = int Label.Map.t

(* Merge loops when not properly nested. *)
let merge_loops l =
  let rec fl ~curr ~acc l =
    match l with
    | [] -> List.rev (curr :: acc)
    | hd :: tl ->
      if Label.Set.subset curr hd
      then fl ~curr:hd ~acc:(curr :: acc) tl
      else fl ~curr:(Label.Set.union curr hd) ~acc tl
  in
  match l with [] -> [] | hd :: tl -> fl ~curr:hd ~acc:[] tl

let compute_loop_depths cfg header_map =
  let incr_label map label =
    Label.Map.update label
      (function
        | None ->
          fatal "Cfg_loop_infos.compute_loop_depths: unknown label %a"
            Label.format label
        | Some depth -> Some (succ depth))
      map
  in
  let incr_loop map loop =
    Label.Set.fold (fun label map -> incr_label map label) loop map
  in
  let init =
    Cfg.fold_blocks cfg ~init:Label.Map.empty ~f:(fun label _block acc ->
        Label.Map.add label 0 acc)
  in
  Label.Map.fold
    (fun _header loop_list acc ->
      List.fold_left loop_list ~init:acc ~f:incr_loop)
    (Label.Map.map merge_loops header_map)
    init

type t =
  { back_edges : Cfg_edge.Set.t;
    loops : loops;
    header_map : header_map;
    loop_depths : loop_depths
  }

let build : Cfg.t -> Cfg_dominators.t -> t =
 fun cfg doms ->
  let back_edges = compute_back_edges cfg doms in
  let loops = compute_loops_of_back_edges cfg back_edges in
  let header_map = compute_header_map loops in
  let loop_depths = compute_loop_depths cfg header_map in
  if debug
  then (
    Format.eprintf "*** Cfg_loop_infos.build for %S\n" cfg.Cfg.fun_name;
    Format.eprintf "back edges:\n";
    Cfg_edge.Set.iter
      (fun { Cfg_edge.src; dst } ->
        Format.eprintf "- %a -> %a\n" Label.format src Label.format dst)
      back_edges;
    Cfg_edge.Map.iter
      (fun { Cfg_edge.src; dst } labels ->
        Format.eprintf "loop for back edge %a -> %a:\n" Label.format src
          Label.format dst;
        Label.Set.iter (Format.eprintf "- %a:\n" Label.format) labels)
      loops;
    Label.Map.iter
      (Format.eprintf "loop depth for %a is %d\n" Label.format)
      loop_depths);
  { back_edges; loops; header_map; loop_depths }
