(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2019-2021 Jane Street Group LLC                                  *
 *                                                                                *
 * Permission is hereby granted, free of charge, to any person obtaining a copy   *
 * of this software and associated documentation files (the "Software"), to deal  *
 * in the Software without restriction, including without limitation the rights   *
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      *
 * copies of the Software, and to permit persons to whom the Software is          *
 * furnished to do so, subject to the following conditions:                       *
 *                                                                                *
 * The above copyright notice and this permission notice shall be included in all *
 * copies or substantial portions of the Software.                                *
 *                                                                                *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    *
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  *
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  *
 * SOFTWARE.                                                                      *
 *                                                                                *
 **********************************************************************************)
[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare
module C = Cfg
module CL = Cfg_with_layout
module DLL = Oxcaml_utils.Doubly_linked_list

module Eliminate_dead_code : sig
  val run : Cfg_with_layout.t -> Label.Set.t
end = struct
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
      | Reachable, (Reachable | Unreachable) | Unreachable, Reachable ->
        Reachable
      | Unreachable, Unreachable -> Unreachable
  end

  module Transfer = struct
    type domain = Domain.t

    type context = unit

    type image =
      { normal : domain;
        exceptional : domain
      }

    let basic value _ _ = value

    let terminator value _ _ = { normal = value; exceptional = value }
  end

  module Dataflow = Cfg_dataflow.Forward (Domain) (Transfer)

  let run : Cfg_with_layout.t -> Label.Set.t =
   fun cfg_with_layout ->
    let cfg = Cfg_with_layout.cfg cfg_with_layout in
    let handlers_are_entry_points =
      not !Oxcaml_flags.cfg_eliminate_dead_trap_handlers
    in
    match Dataflow.run cfg ~init:Reachable ~handlers_are_entry_points () with
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
      unreachable_labels
end

module Eliminate_fallthrough_blocks : sig
  val run : Cfg_with_layout.t -> Label.Set.t
end = struct
  let is_fallthrough_block cfg_with_layout (block : C.basic_block) =
    let cfg = CL.cfg cfg_with_layout in
    if Label.equal cfg.entry_label block.start
       || block.is_trap_handler
       || (not (DLL.is_empty block.body))
       || (not (C.is_pure_terminator block.terminator.desc))
       || C.can_raise_terminator block.terminator.desc
    then None
    else
      let successors = C.successor_labels ~normal:true ~exn:false block in
      if Label.Set.cardinal successors = 1
      then
        let target_label = Label.Set.min_elt successors in
        if Label.equal target_label block.start
        then None (* self-loop *)
        else Some target_label
      else None

  let rec disconnect_fallthrough_blocks cfg_with_layout dead_labels =
    let cfg = CL.cfg cfg_with_layout in
    let found =
      Label.Tbl.fold
        (fun label (block : C.basic_block) found ->
          match is_fallthrough_block cfg_with_layout block with
          | None -> found
          | Some target_label ->
            if !C.verbose
            then
              Printf.printf "block at %s has single successor %s\n"
                (Label.to_string label)
                (Label.to_string target_label);
            (* Update successor block. *)
            let succ_block = C.get_block_exn cfg target_label in
            assert (Label.Set.mem label succ_block.predecessors);
            succ_block.predecessors
              <- Label.Set.union
                   (Label.Set.remove label succ_block.predecessors)
                   block.predecessors;
            (* Update predecessor blocks. *)
            Label.Set.iter
              (fun pred_label ->
                let pred_block = Label.Tbl.find cfg.blocks pred_label in
                C.replace_successor_labels cfg ~normal:true ~exn:false
                  pred_block ~f:(fun l ->
                    if Label.equal l label then target_label else l))
              block.predecessors;
            (* Update terminator. *)
            block.terminator
              <- { block.terminator with desc = Cfg_intf.S.Never };
            assert (Option.is_none block.exn);
            Label.Set.add label found)
        cfg.blocks Label.Set.empty
    in
    if not (Label.Set.is_empty found)
    then (
      if !C.verbose
      then
        Printf.printf "%s: disconnected fallthrough blocks: %d\n" cfg.fun_name
          (Label.Set.cardinal found);
      let dead_labels = Label.Set.union found dead_labels in
      disconnect_fallthrough_blocks cfg_with_layout dead_labels)
    else dead_labels

  let run cfg_with_layout =
    let cfg = CL.cfg cfg_with_layout in
    (* Find and disconnect fallthrough blocks (i.e., blocks with empty body and
       a single successor) by rerouting their predecessors to point directly to
       their successors. It can create a new fallthrough block, for example: if
       we have edges { A -> B, B -> C , A -> C }, with A empty, and B being the
       only fallthrough block. If we eliminate B, then A becomes fallthrough. As
       such we iterate until fixpoint.

       Disconnected fallthrough nodes can be eliminate by dead block elimination
       pass.

       Termination is guaranteed because every step eliminates a node. The order
       matters for performance but not for the final result. *)
    (* CR-someday xclerc: I am not positive it should be changed, but we
     * should not need a fix point here: `is_fallthrough_block` could
     * return the next non-fallthrough block rather than the next block. *)
    if !C.verbose then CL.save_as_dot cfg_with_layout "before_elim_ft";
    let dead_labels =
      disconnect_fallthrough_blocks cfg_with_layout Label.Set.empty
    in
    if !C.verbose
    then (
      Printf.printf "%s: eliminated %d block that were dead or fallthrough.\n"
        cfg.fun_name
        (Label.Set.cardinal dead_labels);
      CL.save_as_dot cfg_with_layout "after_elim_ft");
    dead_labels
end

module Merge_straightline_blocks : sig
  val run : Cfg_with_layout.t -> Label.Set.t
end = struct
  (* Two blocks `b1` and `b2` can be merged if:
   * - `b1` is not the entry block;
   * - `b1` has only one non-exceptional successor, `b2`;
   * - `b1` cannot raise;
   * - `b2` has only one predecessor, `b1`;
   * - `b1` and `b2` are distinct blocks;
   * - the terminator of `b1` is not a tailcall to self.
   *
   *  When the condition is met, `b1` is modified as follows:
   *  - its body is set to the concatenation of `b1.body` and `b2.body`;
   *  - its terminator becomes the terminator of `b2`;
   *  - its `can_raise` and `exn` fields are set to these of `b2` (`b1` cannot raise);
   *  - (its other fields are left unchanged);
   *  and `b2` is modified as follows:
   *  - its prececessors are set to empty;
   *  - (its other fields are left unchanged).
   *
   *  As a consequence, `b2` becomes dead and is removed.
   *  This pass does remove any other dead blocks.
   *)

  (* CR gyorsh: with the new requirement on b1 (that it cannot raise) this pass
     is even closer to eliminate_fallthrough_blocks. The only difference I think
     is that b1's body need not be empty here. *)
  let rec merge_blocks (removed : Label.Set.t)
      (cfg_with_layout : Cfg_with_layout.t) : Label.Set.t =
    let cfg = Cfg_with_layout.cfg cfg_with_layout in
    let new_removed =
      Label.Tbl.fold
        (fun b1_label (b1_block : Cfg.basic_block) acc ->
          let b1_successors =
            Cfg.successor_labels ~normal:true ~exn:false b1_block
          in
          match Label.Set.cardinal b1_successors with
          | 1 ->
            let b2_label = Label.Set.choose b1_successors in
            let b2_block = Label.Tbl.find cfg.blocks b2_label in
            let b2_predecessors = Cfg.predecessor_labels b2_block in
            if (not (Label.equal b1_label cfg.entry_label))
               && (not (Label.equal b1_label b2_label))
               && List.compare_length_with b2_predecessors 1 = 0
               && Cfg.is_pure_terminator b1_block.terminator.desc
               && not b1_block.can_raise
            then (
              assert (Label.equal b1_label (List.hd b2_predecessors));
              (* modify b1 *)
              DLL.transfer ~to_:b1_block.body ~from:b2_block.body ();
              b1_block.terminator <- b2_block.terminator;
              b1_block.exn <- b2_block.exn;
              b1_block.can_raise <- b2_block.can_raise;
              b1_block.cold <- b1_block.cold || b2_block.cold;
              (* modify b2 *)
              b2_block.predecessors <- Label.Set.empty;
              Label.Set.iter
                (fun (b2_succ_label : Label.t) ->
                  let b2_succ_block = Cfg.get_block_exn cfg b2_succ_label in
                  b2_succ_block.predecessors
                    <- Label.Set.add b1_label
                         (Label.Set.remove b2_label b2_succ_block.predecessors))
                (Cfg.successor_labels ~normal:true ~exn:true b2_block);
              b2_block.terminator
                <- { b2_block.terminator with desc = Cfg_intf.S.Never };
              b2_block.exn <- None;
              Label.Set.add b2_label acc)
            else acc
          | _ -> acc)
        cfg.blocks Label.Set.empty
    in
    if not (Label.Set.is_empty new_removed)
    then merge_blocks (Label.Set.union new_removed removed) cfg_with_layout
    else removed

  let run (cfg_with_layout : Cfg_with_layout.t) : Label.Set.t =
    merge_blocks Label.Set.empty cfg_with_layout
end

let run cfg_with_layout =
  let dead_labels = ref Label.Set.empty in
  let acc s = dead_labels := Label.Set.union s !dead_labels in
  Eliminate_fallthrough_blocks.run cfg_with_layout |> acc;
  Merge_straightline_blocks.run cfg_with_layout |> acc;
  Eliminate_dead_code.run cfg_with_layout |> acc;
  (* CR gyorsh: do we need remove_blocks here or only at the end? *)
  Cfg_with_layout.remove_blocks cfg_with_layout !dead_labels;
  dead_labels := Label.Set.empty;
  (* [Simplify_terminator] should happen *after* [Merge_straightline_blocks] and
     [Eliminate_fallthrough_blocks] because merging blocks creates more
     opportunities for terminator simplification. *)
  Simplify_terminator.run (Cfg_with_layout.cfg cfg_with_layout);
  Eliminate_dead_code.run cfg_with_layout |> acc;
  Cfg_with_layout.remove_blocks cfg_with_layout !dead_labels;
  cfg_with_layout
