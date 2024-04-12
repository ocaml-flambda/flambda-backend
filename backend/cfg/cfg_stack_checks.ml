(******************************************************************************
 *                             flambda-backend                                *
 *                  Xavier Clerc and Mark Shinwell, Jane Street               *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2024 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

[@@@ocaml.warning "+4"]

open! Int_replace_polymorphic_compare
module DLL = Flambda_backend_utils.Doubly_linked_list

let is_nontail_call : Cfg.terminator -> bool =
 fun term_desc ->
  (* CR-soon xclerc for xclerc: reconsider whether this predicate is generic and
     well-defined enough to be moved to `Cfg` once the transitive checks are
     implemented. *)
  match term_desc with
  | Call_no_return _ | Call _ -> true
  | Never | Always _ | Parity_test _ | Truth_test _ | Float_test _ | Int_test _
  | Switch _ | Return | Raise _ | Tailcall_self _ | Tailcall_func _ | Prim _ ->
    false
  | Specific_can_raise _ ->
    (* Specific operations cannot raise, and hence cannot call OCaml functions;
       for the purpose of this check it is thus fine to return `false` even
       though a specific operation may call some C code. *)
    false

(* Returns the stack check info, and the max of seen instruction ids. *)
let block_preproc_stack_check_result :
    Cfg.basic_block ->
    frame_size:int ->
    Emitaux.preproc_stack_check_result * int =
 fun block ~frame_size ->
  let contains_nontail_calls =
    (* XCR mshinwell: move to a method in Cfg somewhere?

       xclerc: I have extracted the match to a dedicated function, but I don't
       think the predicate is generic enough to be moved to `Cfg`. *)
    is_nontail_call block.terminator.desc
  in
  let max_frame_size, max_instr_id =
    DLL.fold_left block.body
      ~init:(block.terminator.stack_offset, block.terminator.id)
      ~f:(fun (max_stack_frame, max_instr_id) (instr : _ Cfg.instruction) ->
        ( Int.max max_stack_frame instr.stack_offset,
          Int.max max_instr_id instr.id ))
  in
  let max_frame_size = max_frame_size + frame_size in
  { max_frame_size; contains_nontail_calls }, max_instr_id

type cfg_info =
  { max_frame_size : int;
    blocks_needing_stack_checks : Label.Set.t;
    max_instr_id : int
  }

let build_cfg_info : Cfg.t -> cfg_info =
 fun cfg ->
  let frame_required =
    Proc.frame_required ~fun_contains_calls:cfg.fun_contains_calls
      ~fun_num_stack_slots:cfg.fun_num_stack_slots
  in
  let frame_size =
    Stack_check.frame_size ~stack_offset:0 ~frame_required
      ~num_stack_slots:cfg.fun_num_stack_slots
  in
  let init =
    { max_frame_size = 0;
      blocks_needing_stack_checks = Label.Set.empty;
      max_instr_id = 0
    }
  in
  Cfg.fold_blocks cfg ~init
    ~f:(fun
         label
         block
         { max_frame_size; blocks_needing_stack_checks; max_instr_id }
       ->
      let preproc_stack_check_result, max_instr_id_block =
        block_preproc_stack_check_result block ~frame_size
      in
      let block_needs_stack_checks =
        preproc_stack_check_result.contains_nontail_calls
        || preproc_stack_check_result.max_frame_size
           >= Stack_check.stack_threshold_size
      in
      let max_frame_size =
        Int.max max_frame_size preproc_stack_check_result.max_frame_size
      in
      let blocks_needing_stack_checks =
        if block_needs_stack_checks
        then Label.Set.add label blocks_needing_stack_checks
        else blocks_needing_stack_checks
      in
      let max_instr_id = Int.max max_instr_id max_instr_id_block in
      { max_frame_size; blocks_needing_stack_checks; max_instr_id })

(* Populates `num_checks` with the number of blocks needing a stack check in the
   subtree whose root is the associated label, and returns that value. *)
let rec num_checks_tree :
    Cfg_dominators.dominator_tree ->
    blocks_needing_stack_checks:Label.Set.t ->
    num_checks:int Label.Tbl.t ->
    int =
 fun tree ~blocks_needing_stack_checks ~num_checks ->
  let num_for_root : int =
    if Label.Set.mem tree.label blocks_needing_stack_checks then 1 else 0
  in
  let num_for_children : int =
    List.fold_left
      (fun acc child ->
        acc + num_checks_tree child ~blocks_needing_stack_checks ~num_checks)
      0 tree.children
  in
  let res = num_for_root + num_for_children in
  Label.Tbl.replace num_checks tree.label res;
  res

(* Determines which block should have the stack check. `num_checks` contains for
   each label the number of blocks needing the check in the subtree; `to_cover`
   is the number of blocks needing the check at the entry point. We recursively
   choose the child whose `num_checks` is equal to `to_cover`, if it exists. *)
let rec find_stack_check_block :
    Cfg_dominators.dominator_tree ->
    to_cover:int ->
    num_checks:int Label.Tbl.t ->
    loop_infos:Cfg_loop_infos.t Lazy.t ->
    Label.t =
 fun tree ~to_cover ~num_checks ~loop_infos ->
  assert (to_cover = Label.Tbl.find num_checks tree.label);
  (* Either:
   *   to_cover = num_checks_child0 + ... + num_checks_childN
   * or (in the case where the current root block needs a stack check):
   *   to_cover = 1 + num_checks_child0 + ... + num_checks_childN
   * This allows us to work out whether to put a stack check before the
   * current root block, or before exactly one of the children.
   *)
  let candidates =
    List.filter
      (fun (child : Cfg_dominators.dominator_tree) ->
        to_cover = Label.Tbl.find num_checks child.label)
      tree.children
  in
  match candidates with
  | [] -> tree.label
  | [candidate] ->
    (* Never push a stack check into a loop. *)
    let candidate_is_in_a_loop =
      Label.Map.find candidate.label (Lazy.force loop_infos).loop_depths > 0
    in
    if candidate_is_in_a_loop
    then tree.label
    else find_stack_check_block candidate ~to_cover ~num_checks ~loop_infos
  | _ :: _ :: _ ->
    Misc.fatal_errorf
      "More than one child has num_checks = %d (= to_cover), maybe a bug in \
       num_checks_tree"
      to_cover

let insert_stack_checks (cfg : Cfg.t) ~max_frame_size
    ~blocks_needing_stack_checks ~max_instr_id =
  (* CR-soon xclerc for xclerc: use the dominators and loop infos from
     Cfg_with_infos (at least on some paths). *)
  let doms = Cfg_dominators.build cfg in
  let loop_infos = lazy (Cfg_loop_infos.build cfg doms) in
  (* note: the other entries in the forest are dead code *)
  let tree = Cfg_dominators.dominator_tree_for_entry_point doms in
  let num_checks = Label.Tbl.create (Label.Tbl.length cfg.blocks) in
  let num_checks_root =
    num_checks_tree tree ~blocks_needing_stack_checks ~num_checks
  in
  match num_checks_root with
  | 0 -> ()
  | to_cover ->
    let label = find_stack_check_block tree ~to_cover ~num_checks ~loop_infos in
    let block = Cfg.get_block_exn cfg label in
    let stack_offset = Cfg.first_instruction_stack_offset block in
    let check : Cfg.basic Cfg.instruction =
      (* CR xclerc for xclerc: double check `available_before` and
         `available_across`.

         mshinwell: having these as None should be fine, so long as this is run
         before the forthcoming Cfg_available_regs (which it probably should
         be)?

         xclerc: (keeping the comment, and the explicit values below until all
         of that is implemented.) *)
      Cfg.make_instruction ()
        ~desc:(Cfg.Stack_check { max_frame_size_bytes = max_frame_size })
        ~stack_offset ~id:(succ max_instr_id) ~available_before:None
        ~available_across:None
    in
    DLL.add_begin block.body check

(* CR-someday xclerc for xclerc: we may want to duplicate the check in some
   cases, rather than simply pushing it down. *)
let cfg (cfg_with_layout : Cfg_with_layout.t) =
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  let { max_frame_size; blocks_needing_stack_checks; max_instr_id } =
    build_cfg_info cfg
  in
  if not (Label.Set.is_empty blocks_needing_stack_checks)
  then
    insert_stack_checks cfg ~max_frame_size ~blocks_needing_stack_checks
      ~max_instr_id;
  cfg_with_layout
