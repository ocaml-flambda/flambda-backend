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

(* Two blocks `b1` and `b2` can be merged if:
   - `b1` is not the entry block;
   - `b1` has only one non-exceptional successor, `b2`;
   - `b2` has only one predecessor, `b1`;
   - `b1` and `b2` are distinct blocks;
   - the terminator of `b1` is not a tailcall to self.

   When the condition is met, `b1` is modified as follows:
   - its body is set to the concatenation of `b1.body` and `b2.body`;
   - its terminator becomes the terminator of `b2`;
   - its `exns`, `can_raise`, and `can_raise_interproc` fields  are set to the "union"
     of the respective fields in `b1` and `b2`;
   - (its other fields are left unchanged);
   and `b2` is modified as follows:
   - its prececessors are set to empty;
   - (its other fields are left unchanged).

   As a consequence, `b2` becomes dead.

   Note: by taking the "union" of the `exns`, `can_raise`, and `can_raise_interproc`
   fields, we are losing a bit of precision as to which handler can be reached from
   which block. Such a loss of precision may affect e.g. a liveness analysis. This
   does however not affect the semantics of the code, because at runtime the handler
   is chosen according to the `Pushtrap` and `Poptrap` instructions executed so far
   (as opposed to the information encoded in the graph). *)

let rec merge_blocks (modified : bool) (cfg_with_layout : Cfg_with_layout.t) : bool =
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  let merged =
    Label.Tbl.fold
      (fun b1_label (b1_block : Cfg.basic_block) merged ->
         let b1_successors = Cfg.successor_labels ~normal:true ~exn:false b1_block in
         match Label.Set.cardinal b1_successors with
         | 1 ->
           let b2_label = Label.Set.choose b1_successors in
           let b2_block = Label.Tbl.find cfg.blocks b2_label in
           let b2_predecessors = Cfg.predecessor_labels b2_block in
           if not (Label.equal b1_label cfg.entry_label)
           && not (Label.equal b1_label b2_label)
           && (List.compare_length_with b2_predecessors 1 = 0)
           && (match b1_block.terminator.desc with
               | Tailcall (Self _) -> false
               | Never | Always _ | Parity_test _ | Truth_test _ | Float_test _
               | Int_test _ | Switch _ | Return | Raise _ | Tailcall (Func _)
               | Call_no_return _ -> true) then begin
             assert (Label.equal b1_label (List.hd b2_predecessors));
             (* modify b1 *)
             b1_block.body <- b1_block.body @ b2_block.body;
             b1_block.terminator <- b2_block.terminator;
             b1_block.exns <- Label.Set.union b1_block.exns b2_block.exns;
             b1_block.can_raise <- b1_block.can_raise || b2_block.can_raise;
             (* modify b2 *)
             b2_block.predecessors <- Label.Set.empty;
             Label.Set.iter
               (fun (b2_succ_label : Label.t) ->
                  let b2_succ_block = Cfg.get_block_exn cfg b2_succ_label in
                  b2_succ_block.predecessors <-
                    Label.Set.add
                      b1_label
                      (Label.Set.remove b2_label b2_succ_block.predecessors))
               (Cfg.successor_labels ~normal:true ~exn:true b2_block);
             b2_block.terminator <- { b2_block.terminator with desc = Cfg_intf.S.Never; };
             b2_block.exns <- Label.Set.empty;
             true
           end else
             merged
         | _ ->
           merged)
      cfg.blocks
      false
  in
  if merged then
    merge_blocks true cfg_with_layout
  else
    modified

let run (cfg_with_layout : Cfg_with_layout.t) : unit =
  let modified = merge_blocks false cfg_with_layout in
  if modified then
    Eliminate_dead_blocks.run cfg_with_layout
