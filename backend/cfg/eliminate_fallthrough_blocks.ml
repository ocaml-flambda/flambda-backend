[@@@ocaml.warning "+a-30-40-41-42"]

module C = Cfg
module CL = Cfg_with_layout

let is_fallthrough_block cfg_with_layout (block : C.basic_block) =
  let cfg = CL.cfg cfg_with_layout in
  if
    Label.equal cfg.entry_label block.start
    || block.is_trap_handler
    || List.length block.body > 0
    || block.can_raise
    (* We need to check for can_raise here, because of Tailcall to Self that
       can raise and has a single successor. *)
  then None
  else
    let successors = C.successor_labels ~normal:true ~exn:false cfg block in
    if Label.Set.cardinal successors = 1 then
      let target_label = Label.Set.min_elt successors in
      if Label.equal target_label block.start then None (* self-loop *)
      else Some target_label
    else None

(* CR-someday mshinwell: The logic below looks similar in structure to
   [Eliminate_dead_blocks]. I think it would be worth trying to factor that
   out (into a functor) -- this would presumably form the starting point for
   a generic traversal / rewrite mechanism in the future. *)

let rec disconnect_fallthrough_blocks cfg_with_layout =
  let cfg = CL.cfg cfg_with_layout in
  let found =
    Label.Tbl.fold
      (fun label block found ->
        match is_fallthrough_block cfg_with_layout block with
        | None -> found
        | Some target_label ->
            if !C.verbose then
              Printf.printf "block at %d has single successor %d\n" label
                target_label;
            Disconnect_block.disconnect cfg_with_layout label;
            label :: found)
      cfg.blocks []
  in
  let len = List.length found in
  if len > 0 then (
    if !C.verbose then
      Printf.printf "%s: disconnected fallthrough blocks: %d\n" cfg.fun_name
        len;
    disconnect_fallthrough_blocks cfg_with_layout )

let run cfg_with_layout =
  let cfg = CL.cfg cfg_with_layout in

  (* Find and disconnect fallthrough blocks (i.e., blocks with empty body and
     a single successor) by rerouting their predecessors to point directly to
     their successors. It can create a new fallthrough block, for example: if
     we have edges { A -> B, B -> C , A -> C }, with A empty, and B being the
     only fallthrough block. If we eliminate B, then A becomes fallthrough.
     As such we iterate until fixpoint.

     Disconnected fallthrough nodes can be eliminate by dead block
     elimination pass.

     Termination is guaranteed because every step eliminates a node. The
     order matters for performance but not for the final result. *)
  (* CR-someday xclerc: I am not positive it should be changed, but we
   * should not need a fix point here: `is_fallthrough_block` could
   * return the next non-fallthrough block rather than the next block. *)
  let len = Label.Tbl.length cfg.blocks in
  if !C.verbose then CL.save_as_dot cfg_with_layout "before_elim_ft";
  disconnect_fallthrough_blocks cfg_with_layout;
  Eliminate_dead_blocks.run cfg_with_layout;
  let new_len = Label.Tbl.length cfg.blocks in
  if !C.verbose then (
    Printf.printf "%s: eliminated %d block that were dead or fallthrough.\n"
      cfg.fun_name (len - new_len);
    CL.save_as_dot cfg_with_layout "after_elim_ft" )
