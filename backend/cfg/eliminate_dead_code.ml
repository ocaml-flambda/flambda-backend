module Domain = struct
  type t =
    | Reachable
    | Unreachable

  let bot = Unreachable

  let less_equal left right =
    match left, right with
    | Reachable, Reachable -> true
    | Reachable, Unreachable -> false
    | Unreachable, Reachable -> true
    | Unreachable, Unreachable -> true

  let join left right =
    match left, right with
    | Reachable, (Reachable | Unreachable) | Unreachable, Reachable -> Reachable
    | Unreachable, Unreachable -> Unreachable
end

module Transfer = struct
  type domain = Domain.t

  type image =
    { normal : domain;
      exceptional : domain
    }

  let basic value _ = value

  let terminator value _ = { normal = value; exceptional = value }
end

module Dataflow = Cfg_dataflow.Forward (Domain) (Transfer)

let run_dead_block : Cfg_with_layout.t -> unit =
 fun cfg_with_layout ->
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  match Dataflow.run cfg ~init:Reachable () with
  | Result.Error _ ->
    Misc.fatal_error
      "Dataflow.run_dead_code: forward analysis did not reach a fix-point"
  | Result.Ok map ->
    let unreachable_labels =
      Label.Tbl.fold
        (fun label value acc ->
          match value with
          | Domain.Reachable -> acc
          | Domain.Unreachable -> Label.Set.add label acc)
        map Label.Set.empty
    in
    Label.Set.iter
      (fun label ->
        let block = Cfg.get_block_exn cfg label in
        block.predecessors <- Label.Set.empty;
        Label.Set.iter
          (fun succ_label ->
            let succ_block = Cfg.get_block_exn cfg succ_label in
            succ_block.predecessors
              <- Label.Set.remove label succ_block.predecessors)
          (Cfg.successor_labels ~normal:true ~exn:true block);
        block.terminator <- { block.terminator with desc = Cfg_intf.S.Never };
        block.exn <- None)
      unreachable_labels;
    Cfg_with_layout.remove_blocks cfg_with_layout unreachable_labels;
    (* CR xclerc for xclerc: temporary. *)
    Eliminate_dead_blocks.run cfg_with_layout
