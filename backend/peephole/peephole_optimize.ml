[@@@ocaml.warning "+a-29-40-41-42"]

module DLL = Flambda_backend_utils.Doubly_linked_list
module IntCsv = Peephole_utils.IntCsv
module U = Peephole_utils
module R = Peephole_rules
module G = Peephole_generated

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
    match G.apply cell with
    | None -> (
      match R.apply cell with
      | None -> (
        match DLL.next cell with
        | None -> ()
        | Some next_cell ->
          optimize_body (steps_until_termination - 1) next_cell)
      | Some continuation_cell ->
        optimize_body (steps_until_termination - 1) continuation_cell)
    | Some continuation_cell ->
      optimize_body (steps_until_termination - 1) continuation_cell

let set_csv () =
  if Option.is_none !U.csv_singleton
  then (
    let new_csv =
      IntCsv.create (G.generated_rule_names @ R.handbuilt_rule_names)
    in
    if Option.is_some !Flambda_backend_flags.cfg_peephole_optimize_track
    then
      Stdlib.at_exit (fun () ->
          (* the csv filename is a hex string deterministically generated from
             the command line arguments. *)
          IntCsv.print new_csv
            (Option.get !Flambda_backend_flags.cfg_peephole_optimize_track
            ^ (Array.to_list Sys.argv |> String.concat "" |> Digest.string
             |> Digest.to_hex)
            ^ ".csv"));
    U.csv_singleton := Some new_csv)

(* Apply peephole optimization for the body of each block of the CFG*)
let peephole_optimize_cfg cfg_with_layout =
  if !Flambda_backend_flags.cfg_peephole_optimize
  then (
    let fun_name = (Cfg_with_layout.cfg cfg_with_layout).fun_name in
    if Option.is_some !Flambda_backend_flags.cfg_peephole_optimize_track
    then (
      set_csv ();
      IntCsv.add_empty_row (U.get_csv ()) fun_name);
    Cfg.iter_blocks (Cfg_with_layout.cfg cfg_with_layout)
      ~f:(fun (_ : int) block ->
        Option.iter
          (optimize_body (termination_cond_const * DLL.length block.body))
          (DLL.hd_cell block.body)));
  cfg_with_layout
