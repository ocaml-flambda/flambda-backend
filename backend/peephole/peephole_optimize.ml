[@@@ocaml.warning "+a-29-40-41-42"]

module DLL = Flambda_backend_utils.Doubly_linked_list
module R = Peephole_rules

(* We currently don't check that the peephole optimizer terminates. In the case
   that the peephole optimization does not terminate we limit the number of
   steps to be linear with respect to the block's body (i.e.
   O(block_body_length) with a small constant). *)
let termination_cond_const = 5

(* Here cell is an iterator of the doubly linked list data structure that
   encapsulates the body's instructions. *)
let rec optimize_body steps_until_termination cell =
  if steps_until_termination > 0
  then
    match R.apply cell with
    | None -> (
      match DLL.next cell with
      | None -> ()
      | Some next_cell -> optimize_body (steps_until_termination - 1) next_cell)
    | Some continuation_cell ->
      optimize_body (steps_until_termination - 1) continuation_cell

(* Apply peephole optimization for the body of each block of the CFG*)
let peephole_optimize_cfg cfg_with_layout =
  if !Flambda_backend_flags.cfg_peephole_optimize
  then
    Cfg.iter_blocks (Cfg_with_layout.cfg cfg_with_layout)
      ~f:(fun (_ : int) block ->
        Option.iter
          (optimize_body (termination_cond_const * DLL.length block.body))
          (DLL.hd_cell block.body));
  cfg_with_layout
