(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2021 Jane Street Group LLC                                       *
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
open! Int_replace_polymorphic_compare

[@@@ocaml.warning "+a-40-41-42"]

module CL = Cfg_with_layout
module DLL = Flambda_backend_utils.Doubly_linked_list

type t =
  { mutable result : bool;
    ppf : Format.formatter;
    fun_name : string;
    cfg : Cfg.t;
    mutable tailrec_entry_label : Label.t option
  }

let report t =
  t.result <- true;
  Format.fprintf t.ppf "Cfg invariant failed in %s: " t.fun_name;
  Format.fprintf t.ppf

let print_layout ppf layout =
  Format.(
    pp_print_list ~pp_sep:pp_print_space Label.print ppf (layout |> DLL.to_list))

let check_layout t layout =
  (* [layout] is not empty and entry of cfg is the first node in the layout. *)
  (match DLL.hd layout with
  | None -> report t "Empty layout"
  | Some hd ->
    if not (Label.equal hd t.cfg.entry_label)
    then
      report t "Cfg entry node %a is not the first node in the layout @.%a@."
        Label.print t.cfg.entry_label print_layout layout);
  (* No duplicates in layout *)
  let labels =
    DLL.fold_left
      ~f:(fun acc l ->
        if Label.Set.mem l acc
        then report t "Duplicate label %a in layout@." Label.print l;
        Label.Set.add l acc)
      ~init:Label.Set.empty layout
  in
  let num_labels = Label.Set.cardinal labels in
  if DLL.length layout > num_labels
  then report t "Layout contains duplicates:@.%a@." print_layout layout;
  (* The set of nodes in the layout is the same as the set of nodes in the
     cfg. *)
  let num_nodes = Label.Tbl.length t.cfg.blocks in
  if num_nodes > num_labels
  then (
    report t "Not all cfg nodes are in the layout:@.%a" print_layout layout;
    Label.Tbl.iter
      (fun label _ ->
        if not (Label.Set.mem label labels)
        then
          report t "Cfg node label %a is missing from layout" Label.print label)
      t.cfg.blocks);
  if num_nodes < num_labels
  then
    report t "Not all labels in the layout have cfg nodes:@.%a" print_layout
      layout;
  ();
  DLL.iter
    ~f:(fun label ->
      match Cfg.get_block t.cfg label with
      | Some b ->
        if not (Label.equal b.start label)
        then
          report t "Block labelled %a is associated with label %a" Label.print
            b.start Label.print label
      | None ->
        report t "Node not found for label %a in layout" Label.print label)
    layout

let check_tailrec_position t =
  (* tailrec entry point is either the entry block or the only successor of the
     entry block. *)
  match t.tailrec_entry_label with
  | None -> ()
  | Some tailrec_label ->
    if not (Label.equal tailrec_label t.cfg.entry_label)
    then
      let entry_block = Cfg.get_block_exn t.cfg t.cfg.entry_label in
      let successors =
        Cfg.successor_labels ~normal:true ~exn:false entry_block
      in
      if not
           (Label.Set.cardinal successors = 1
           && Label.equal tailrec_label (Label.Set.min_elt successors))
      then
        report t
          "Expected tailrec block %a to be the entry block or the only \
           successor of the entry block but entry block the \
           followingsuccessors:@.%a@."
          Label.print tailrec_label Label.Set.print successors

let check_tailrec t _label block =
  (* check all Tailrec Self agree on the successor label *)
  match block.Cfg.terminator.desc with
  | Tailcall_self { destination } -> (
    match t.tailrec_entry_label with
    | None -> t.tailrec_entry_label <- Some destination
    | Some l ->
      if not (Label.equal l destination)
      then
        report t "Two self-tailcall terminators with different labels: %a %a"
          Label.print l Label.print destination)
  | Call _ | Prim _ | Tailcall_func _ | Never | Always _ | Parity_test _
  | Truth_test _ | Float_test _ | Int_test _ | Switch _ | Return | Raise _
  | Call_no_return _ ->
    ()

let check_can_raise t label (block : Cfg.basic_block) =
  (* Check that block's [can_raise] field agrees with terminator instruction.
     Only the terminator can raise, other instructions in the block (basic
     instructions) do not raise. *)
  let terminator_can_raise = Cfg.can_raise_terminator block.terminator.desc in
  if not (Bool.equal block.can_raise terminator_can_raise)
  then
    report t
      "Block %s: block.can_raise is %B which does not match can_raise of its \
       terminator"
      (Label.to_string label) block.can_raise;
  (* a block can have an exn successor only if the block can raise *)
  if Option.is_some block.exn && not terminator_can_raise
  then
    report t
      "Block %s has an exceptional successor but the terminator cannot raise."
      (Label.to_string label)

let check_stack_offset t label (block : Cfg.basic_block) =
  (* [stack_offset] cannot be propagated on exceptional edges because of stack
     allocated arguments. If a terminator raises, all stack allocated arguments
     are freed up to the top of trap stack, and the number of freed slots may be
     different for different predecessors of a given trap handler block. *)
  (if not block.is_trap_handler
  then
    let stack_offset = block.stack_offset in
    List.iter
      (fun predecessor ->
        let pred_block = Cfg.get_block_exn t.cfg predecessor in
        let pred_terminator_stack_offset = pred_block.terminator.stack_offset in
        if not (Int.equal stack_offset pred_terminator_stack_offset)
        then
          report t
            "Wrong stack offset: block %s in predecessors of block %s, stack \
             offset of the terminator of %s is %d, stack offset of block %s is \
             %d, expected stack offset of terminator is %d \
             (block.is_trap_handler=%b).\n"
            (Label.to_string predecessor)
            (Label.to_string label)
            (Label.to_string predecessor)
            pred_terminator_stack_offset (Label.to_string label)
            block.stack_offset stack_offset block.is_trap_handler)
      (Cfg.predecessor_labels block));
  let terminator_stack_offset = block.terminator.stack_offset in
  Label.Set.iter
    (fun successor ->
      let succ_block = Cfg.get_block_exn t.cfg successor in
      if not (Int.equal terminator_stack_offset succ_block.stack_offset)
      then
        report t
          "Wrong stack offset: block %s in normal successors of block %s, \
           stack offset of the terminator of %s is %d, stack offset of block \
           %s is %d.\n"
          (Label.to_string successor)
          (Label.to_string label) (Label.to_string label)
          terminator_stack_offset
          (Label.to_string successor)
          succ_block.stack_offset)
    (Cfg.successor_labels ~normal:true ~exn:false block);
  let stack_offset_after_body =
    DLL.fold_left block.body ~init:block.stack_offset
      ~f:(fun cur_stack_offset (basic : Cfg.basic Cfg.instruction) ->
        if not (Int.equal cur_stack_offset basic.stack_offset)
        then
          report t
            "Wrong stack offset in block %s: the offset of [(id:%a) %a] \
             instruction is %d, but expected %d.\n"
            (Label.to_string label) InstructionId.print basic.id Cfg.dump_basic
            basic.desc basic.stack_offset cur_stack_offset;
        match basic.desc with
        | Pushtrap { lbl_handler } ->
          let handler_block = Cfg.get_block_exn t.cfg lbl_handler in
          if not (Int.equal cur_stack_offset handler_block.stack_offset)
          then
            report t
              "Wrong stack offset in block %s: the offset of [(id:%a) %a] \
               instruction is %d, the offset of block %s is %d.\n"
              (Label.to_string label) InstructionId.print basic.id
              Cfg.dump_basic basic.desc cur_stack_offset
              (Label.to_string lbl_handler)
              handler_block.stack_offset;
          cur_stack_offset + Proc.trap_size_in_bytes
        | Poptrap { lbl_handler = _ } ->
          let new_stack_offset = cur_stack_offset - Proc.trap_size_in_bytes in
          if Int.compare new_stack_offset 0 < 0
          then
            report t
              "Negative stack offset in block %s: the offset after [(id:%a) \
               %a] instruction is %d\n"
              (Label.to_string label) InstructionId.print basic.id
              Cfg.dump_basic basic.desc new_stack_offset;
          new_stack_offset
        | Op (Stackoffset n) ->
          let new_stack_offset = cur_stack_offset + n in
          if Int.compare new_stack_offset 0 < 0
          then
            report t
              "Negative stack offset in block %s: the offset after [(id:%a) \
               %a] instruction is %d\n"
              (Label.to_string label) InstructionId.print basic.id
              Cfg.dump_basic basic.desc new_stack_offset;
          new_stack_offset
        | Op
            ( Move | Spill | Reload | Const_int _ | Const_float _
            | Const_float32 _ | Const_symbol _ | Const_vec128 _ | Load _
            | Store _ | Intop _ | Intop_imm _ | Intop_atomic _ | Floatop _
            | Csel _ | Static_cast _ | Reinterpret_cast _ | Probe_is_enabled _
            | Opaque | Begin_region | End_region | Specific _
            | Name_for_debugger _ | Dls_get | Poll | Alloc _ )
        | Reloadretaddr | Prologue ->
          cur_stack_offset
        | Stack_check _ ->
          Misc.fatal_error
            "Cfg_invariant.check_stack_offset: unexpected stack check")
  in
  if not (Int.equal stack_offset_after_body terminator_stack_offset)
  then
    report t
      "Wrong stack offset in block %s: stack offset after body is %d, stack \
       offset of the terminator is %d.\n"
      (Label.to_string label) stack_offset_after_body terminator_stack_offset;
  ()

let check_block t label (block : Cfg.basic_block) =
  check_tailrec t label block;
  check_can_raise t label block;
  (* exn and normal successors are disjoint *)
  let exn = Cfg.successor_labels ~normal:false ~exn:true block in
  let normal = Cfg.successor_labels ~normal:true ~exn:false block in
  if not (Label.Set.disjoint exn normal)
  then
    report t "exn and normal successors of %s are not disjoint"
      (Label.to_string label);
  let successors = Label.Set.union exn normal in
  (* successors and predecessors agree *)
  Label.Set.iter
    (fun successor ->
      let succ_block = Cfg.get_block_exn t.cfg successor in
      if not
           (List.exists (Label.equal label) (Cfg.predecessor_labels succ_block))
      then
        report t "%s in successors(%s) but %s is not in predecessors(%s)"
          (Label.to_string successor)
          (Label.to_string label) (Label.to_string label)
          (Label.to_string successor))
    successors;
  List.iter
    (fun predecessor ->
      let pred_block = Cfg.get_block_exn t.cfg predecessor in
      (* trap handler block is reachable through exceptional edges only. *)
      let exn = Cfg.successor_labels ~normal:false ~exn:true pred_block in
      let normal = Cfg.successor_labels ~normal:true ~exn:false pred_block in
      let check_edge ~must ~must_not =
        if Label.Set.mem label must_not
        then
          report t "Unexpected edge from %s to block %s"
            (Label.to_string predecessor)
            (Label.to_string label);
        if not (Label.Set.mem label must)
        then
          report t "%s in predecessors of %s but %s not in successors of %s"
            (Label.to_string predecessor)
            (Label.to_string label) (Label.to_string label)
            (Label.to_string predecessor)
      in
      if block.is_trap_handler
      then check_edge ~must:exn ~must_not:normal
      else check_edge ~must:normal ~must_not:exn)
    (Cfg.predecessor_labels block);
  (* [stack_offset] consistent across edges and calculated correctly within
     blocks *)
  check_stack_offset t label block;
  ()

let run ppf cfg_with_layout =
  let cfg = CL.cfg cfg_with_layout in
  let layout = CL.layout cfg_with_layout in
  let t =
    { result = false;
      ppf;
      fun_name = cfg.fun_name;
      cfg;
      tailrec_entry_label = None
    }
  in
  check_layout t layout;
  Cfg.iter_blocks ~f:(check_block t) cfg;
  check_tailrec_position t;
  t.result
