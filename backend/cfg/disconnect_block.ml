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
[@@@ocaml.warning "+a-30-40-41-42"]

module C = Cfg
module CL = Cfg_with_layout

let debug = false

let update_predecessor's_terminators (cfg : C.t) ~pred_block ~being_disconnected
    ~target_label =
  let replace_label l =
    if Label.equal l being_disconnected then target_label else l
  in
  C.replace_successor_labels cfg ~normal:true ~exn:false pred_block
    ~f:replace_label

let disconnect cfg_with_layout label =
  let cfg = CL.cfg cfg_with_layout in
  let block = C.get_block_exn cfg label in
  if block.is_trap_handler
  then
    (* CR-someday gyorsh: if trap handlers can be eliminated, remove this label
       from block.exn of other blocks. *)
    Misc.fatal_error "Removing trap handler blocks is not supported";
  let successors = C.successor_labels ~normal:true ~exn:false block in
  let has_predecessors = not (Label.Set.is_empty block.predecessors) in
  let n = Label.Set.cardinal successors in
  let has_more_than_one_successor = n > 1 in
  if !C.verbose then Printf.printf "Disconnect %d in %s\n" label cfg.fun_name;
  if has_more_than_one_successor && has_predecessors
  then
    (* CR-someday xclerc: it feels like this condition is really tied to the
     * current features of the tool. *)
    Misc.fatal_errorf
      "Cannot disconnect block %a: it has more than one successor and at least \
       one predecessor"
      Label.print label;
  (* Update successor blocks. *)
  Label.Set.iter
    (fun succ ->
      let succ_block = C.get_block_exn cfg succ in
      if debug then assert (Label.Set.mem label succ_block.predecessors);
      succ_block.predecessors
        <- Label.Set.union
             (Label.Set.remove label succ_block.predecessors)
             block.predecessors)
    successors;
  Label.Set.iter
    (fun succ ->
      let succ_block = C.get_block_exn cfg succ in
      if debug then assert (Label.Set.mem label succ_block.predecessors);
      succ_block.predecessors <- Label.Set.remove label succ_block.predecessors)
    (C.successor_labels ~normal:false ~exn:true block);
  (* Update predecessor blocks. *)
  if n = 1
  then
    let target_label = Label.Set.min_elt successors in
    Label.Set.iter
      (fun pred_label ->
        let pred_block = Label.Tbl.find cfg.blocks pred_label in
        if debug
        then
          Option.iter
            (fun pred_block_exn ->
              assert (not (Label.equal label pred_block_exn)))
            pred_block.exn;
        update_predecessor's_terminators cfg ~pred_block
          ~being_disconnected:label ~target_label)
      block.predecessors
  else if debug
  then assert (Label.Set.is_empty block.predecessors)
  else ();
  CL.remove_block cfg_with_layout label
