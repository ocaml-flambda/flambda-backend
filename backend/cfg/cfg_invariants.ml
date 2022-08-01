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
[@@@ocaml.warning "+a-30-40-41-42"]

module CL = Cfg_with_layout

type t =
  { mutable result : bool;
    ppf : Format.formatter;
    fun_name : string;
    cfg : Cfg.t;
    mutable tailrec_entry_label : Label.t option
  }

let report t =
  t.result <- false;
  Format.fprintf t.ppf "Cfg invariant failed in %s: " t.fun_name;
  Format.fprintf t.ppf

let print_layout ppf layout =
  Format.(pp_print_list ~pp_sep:pp_print_space pp_print_int ppf layout)

let check_layout t layout =
  (* [layout] is not empty and entry of cfg is the first node in the layout. *)
  (match layout with
  | [] -> report t "Empty layout"
  | hd :: _ ->
    if not (Label.equal hd t.cfg.entry_label)
    then
      report t "Cfg entry node %d is not the first first in the layout @.%a@."
        t.cfg.entry_label print_layout layout);
  (* No duplicates in layout *)
  let labels =
    List.fold_left
      (fun acc l ->
        if Label.Set.mem l acc then report t "Duplicate label %d in layout@." l;
        Label.Set.add l acc)
      Label.Set.empty layout
  in
  let num_labels = Label.Set.cardinal labels in
  if List.compare_length_with layout num_labels > 0
  then report t "Layout contains duplicates:@.%a@." print_layout layout;
  (* The set of nodes in the layout is the same as the set of nodes in the
     cfg. *)
  let num_nodes = Label.Tbl.length t.cfg.blocks in
  if num_nodes > num_labels
  then begin
    report t "Not all cfg nodes are in the layout:@.%a" print_layout layout;
    Label.Tbl.iter
      (fun label _ ->
        if not (Label.Set.mem label labels)
        then report t "Cfg node label %d is missing from layout" label)
      t.cfg.blocks
  end;
  if num_nodes < num_labels
  then
    report t "Not all labels in the layout have cfg nodes:@.%a" print_layout
      layout;
  ();
  List.iter
    (fun label ->
      match Cfg.get_block t.cfg label with
      | Some _ -> ()
      | None -> report t "Node not found for label %d in layout" label)
    layout

let check_tailrec t _label block =
  (* check all Tailrec Self agree on the successor label *)
  match block.Cfg.terminator.desc with
  | Tailcall (Self { destination }) -> (
    match t.tailrec_entry_label with
    | None -> t.tailrec_entry_label <- Some destination
    | Some l ->
      if not (Label.equal l destination)
      then
        report t "Two self-tailcall terminators with different labels: %d %d" l
          destination)
  | Tailcall (Func _)
  | Never | Always _ | Parity_test _ | Truth_test _ | Float_test _ | Int_test _
  | Switch _ | Return | Raise _ | Call_no_return _ ->
    ()

let check_can_raise t label (block : Cfg.basic_block) =
  (* only the last instruction can raise, and block's can_raise field agrees
     with can_raise of instructions in the block. *)
  let computed_can_raise_block =
    try Cfg.can_raise_block block
    with Cfg.Malformed_cfg reason ->
      report t "%s" reason;
      true
  in
  if not (block.can_raise = computed_can_raise_block)
  then
    report t
      "Block %d: block.can_raise is %b which does not match can_raise of its \
       instructions"
      label block.can_raise

let check_block t label (block : Cfg.basic_block) =
  (* exn and normal successors are disjoint *)
  let exn = Cfg.successor_labels ~normal:false ~exn:true block in
  let normal = Cfg.successor_labels ~normal:true ~exn:false block in
  if not (Label.Set.disjoint exn normal)
  then report t "exn and normal successors of %d are not disjoint" label;
  let successors = Label.Set.union exn normal in
  (* successors and predecessors agree *)
  Label.Set.iter
    (fun successor ->
      let succ_block = Cfg.get_block_exn t.cfg successor in
      if not
           (List.exists (Label.equal label) (Cfg.predecessor_labels succ_block))
      then
        report t "%d in successors(%d) but %d is not in predecessors(%d)"
          successor label label successor)
    successors;
  List.iter
    (fun predecessor ->
      let pred_block = Cfg.get_block_exn t.cfg predecessor in
      (* trap handler block is reachable through exceptional edges only. *)
      let exn = Cfg.successor_labels ~normal:false ~exn:true pred_block in
      let normal = Cfg.successor_labels ~normal:true ~exn:false pred_block in
      let check_edge ~must ~must_not =
        if Label.Set.mem label must_not
        then report t "Unexpected edge from %d to block %d" predecessor label;
        if not (Label.Set.mem label must)
        then
          report t "%d in predecessors of %d but %d not in successors of %d"
            predecessor label label predecessor
      in
      if block.is_trap_handler
      then check_edge ~must:exn ~must_not:normal
      else check_edge ~must:normal ~must_not:exn)
    (Cfg.predecessor_labels block);
  (* CR gyorsh: check stack_offset consistent across edges and calculated
     correctly within blocks *)
  check_tailrec t label block;
  check_can_raise t label block;
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
  t.result
