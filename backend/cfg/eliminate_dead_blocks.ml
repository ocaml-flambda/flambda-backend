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

(* CR-someday gyorsh: Eliminate dead cycles. *)
let block_is_dead cfg_with_layout (block : C.basic_block) =
  let cfg = CL.cfg cfg_with_layout in
  Label.Set.is_empty block.predecessors
  (* CR-someday gyorsh: Predecessors already contains all live handlers. Remove
     is_trap_handler check when CFG is updated to use trap stacks instead of
     pushtrap/poptrap instructions in CFG. *)
  && (not block.is_trap_handler)
  && not (Label.equal cfg.entry_label block.start)

(* CR-someday xclerc: not to say the implementation should change any time soon,
   but since it was mentioned the other day: with support for generic data flow
   analysis, a trivial analysis would identify all live/dead blocks in one go
   and the function below would no longer be recursive. This would also
   eliminate the dead cycles mentioned above. *)
let rec eliminate_dead_blocks cfg_with_layout =
  let cfg = CL.cfg cfg_with_layout in
  if CL.preserve_orig_labels cfg_with_layout
  then
    Misc.fatal_error
      "Won't eliminate dead blocks when [preserve_orig_labels] is set";
  let found_dead =
    Label.Tbl.fold
      (fun label block found ->
        if block_is_dead cfg_with_layout block then label :: found else found)
      cfg.blocks []
  in
  if List.compare_length_with found_dead 0 > 0
  then (
    List.iter (Disconnect_block.disconnect cfg_with_layout) found_dead;
    if !C.verbose
    then (
      (* CR xclerc for xclerc: temporary. *)
      let _ = assert false in
      Printf.printf "Found and eliminated %d dead blocks in function %s.\n"
        (List.length found_dead) cfg.fun_name;
      Printf.printf "Eliminated blocks are:";
      List.iter (Printf.printf "\n%d") found_dead;
      Printf.printf "\n");
    (* Termination: the number of remaining blocks is strictly smaller in each
       recursive call. *)
    eliminate_dead_blocks cfg_with_layout)
  else
    (* check that no blocks are left that are marked as dead *)
    C.iter_blocks cfg ~f:(fun label block ->
        if block.dead
        then
          Misc.fatal_errorf "Block %d in %s marked as dead but not eliminated\n"
            label cfg.fun_name)

let run cfg_with_layout = eliminate_dead_blocks cfg_with_layout
