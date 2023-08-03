[@@@ocaml.warning "+a-29-40-41-42"]

module DLL = Peephole_utils.DLL
module R = Peephole_rules

(* Here cell is an iterator of the doubly linked list data structure that
   encapsulates the body's instructions. *)
let rec optimize_body cell =
  match R.apply cell with
  | None -> (
    match DLL.next cell with
    | None -> ()
    | Some next_cell -> optimize_body next_cell)
  | Some continuation_cell -> optimize_body continuation_cell

(* Apply peephole optimization for the body of each block of the CFG*)
let peephole_optimize_cfg cfg_with_layout =
  if !Flambda_backend_flags.cfg_peephole_optimize
  then
    Cfg.iter_blocks (Cfg_with_layout.cfg cfg_with_layout)
      ~f:(fun (_ : int) block ->
        Option.iter optimize_body (DLL.hd_cell block.body));
  cfg_with_layout
