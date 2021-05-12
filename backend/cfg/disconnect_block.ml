[@@@ocaml.warning "+a-30-40-41-42"]

module C = Cfg
module CL = Cfg_with_layout

let update_predecessor's_terminators (cfg : C.t) ~pred_block
    ~being_disconnected ~target_label =
  let replace_label l =
    if Label.equal l being_disconnected then target_label else l
  in
  C.replace_successor_labels cfg ~normal:true ~exn:false pred_block
    ~f:replace_label

let disconnect cfg_with_layout label =
  let cfg = CL.cfg cfg_with_layout in
  let block = C.get_block_exn cfg label in
  if block.is_trap_handler then
    (* CR-someday gyorsh: if trap handlers can be eliminated, remove this
       label from block.exn of other blocks. *)
    Misc.fatal_error "Removing trap handler blocks is not supported";
  let successors = C.successor_labels ~normal:true ~exn:true cfg block in
  let has_predecessors = not (Label.Set.is_empty block.predecessors) in
  let n = Label.Set.cardinal successors in
  let has_more_than_one_successor = n > 1 in
  if !C.verbose then Printf.printf "Disconnect %d in %s\n" label cfg.fun_name;
  if has_more_than_one_successor && has_predecessors then
    (* CR-someday xclerc: it feels like this condition is really tied to the
     * current features of the tool. *)
    Misc.fatal_errorf
      "Cannot disconnect block %a: it has more than one successor and at \
       least one predecessor"
      Label.print label;
  (* Update successor blocks. *)
  Label.Set.iter
    (fun succ ->
      let succ_block = C.get_block_exn cfg succ in
      assert (Label.Set.mem label succ_block.predecessors);
      succ_block.predecessors <-
        Label.Set.union
          (Label.Set.remove label succ_block.predecessors)
          block.predecessors)
    successors;
  (* Update predecessor blocks. *)
  if n = 1 then
    let target_label = Label.Set.min_elt successors in
    Label.Set.iter
      (fun pred_label ->
        let pred_block = Label.Tbl.find cfg.blocks pred_label in
        assert (not (Label.Set.mem label pred_block.exns));
        update_predecessor's_terminators cfg ~pred_block
          ~being_disconnected:label ~target_label)
      block.predecessors
  else assert (Label.Set.is_empty block.predecessors);
  CL.remove_block cfg_with_layout label
