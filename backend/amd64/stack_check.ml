(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

module DLL = Flambda_backend_utils.Doubly_linked_list

let stack_threshold_size = Config.stack_threshold * 8 (* bytes *)

let fp = Config.with_frame_pointers

(* includes return address *)
let frame_size :
    stack_offset:int -> frame_required:bool -> num_stack_slots:int array -> int
    =
 fun ~stack_offset ~frame_required ~num_stack_slots ->
  if frame_required
  then (
    if num_stack_slots.(2) > 0 then Arch.assert_simd_enabled ();
    let sz =
      stack_offset + 8
      + (8 * num_stack_slots.(0))
      + (8 * num_stack_slots.(1))
      + (16 * num_stack_slots.(2))
      + if fp then 8 else 0
    in
    Misc.align sz 16)
  else stack_offset + 8

let linear : Linear.fundecl -> Linear.fundecl =
 fun fundecl ->
  match Config.runtime5 with
  | false -> fundecl
  | true ->
    let frame_size =
      frame_size ~stack_offset:0 ~frame_required:fundecl.fun_frame_required
        ~num_stack_slots:fundecl.fun_num_stack_slots
    in
    let { Emitaux.max_frame_size; contains_nontail_calls } =
      Emitaux.preproc_stack_check ~fun_body:fundecl.fun_body ~frame_size
        ~trap_size:16
    in
    let insert_stack_check =
      contains_nontail_calls || max_frame_size >= stack_threshold_size
    in
    if insert_stack_check
    then
      let fun_body =
        Linear.instr_cons
          (Lstackcheck { max_frame_size_bytes = max_frame_size })
          [||] [||] ~available_before:fundecl.fun_body.available_before
          ~available_across:fundecl.fun_body.available_across fundecl.fun_body
      in
      { fundecl with fun_body }
    else fundecl

(* Returns the stack check infos, and the max of seen instruction ids. *)
let block_preproc_stack_check_result :
    Cfg.basic_block ->
    frame_size:int ->
    Emitaux.preproc_stack_check_result * int =
 fun block ~frame_size ->
  let contains_nontail_calls =
    match block.terminator.desc with
    | Call_no_return _ | Call _ -> true
    | Never | Always _ | Parity_test _ | Truth_test _ | Float_test _
    | Int_test _ | Switch _ | Return | Raise _ | Tailcall_self _
    | Tailcall_func _ | Prim _ | Specific_can_raise _ ->
      false
  in
  let max_frame_size, max_instr_id =
    DLL.fold_left block.body
      ~init:(block.terminator.stack_offset, block.terminator.id)
      ~f:(fun (max_stack_frame, max_instr_id) (instr : _ Cfg.instruction) ->
        ( Int.max max_stack_frame (instr.stack_offset + frame_size),
          Int.max max_instr_id instr.id ))
  in
  { max_frame_size; contains_nontail_calls }, max_instr_id

type cfg_infos =
  { max_frame_size : int;
    blocks_needing_stack_checks : Label.Set.t;
    max_instr_id : int
  }

let build_cfg_infos : Cfg.t -> cfg_infos =
 fun cfg ->
  let frame_required =
    Proc.frame_required ~fun_contains_calls:cfg.fun_contains_calls
      ~fun_num_stack_slots:cfg.fun_num_stack_slots
  in
  let frame_size =
    frame_size ~stack_offset:0 ~frame_required
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
        || preproc_stack_check_result.max_frame_size >= stack_threshold_size
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
   choose the child whose `num_checks` is equal to `to_cover`, if it exists.
   There cannot be two such children since the `num_checks` value for the
   current node is know to be `>=` the sum of the values for the children. *)
let rec find_stack_check_block :
    Cfg_dominators.dominator_tree ->
    to_cover:int ->
    num_checks:int Label.Tbl.t ->
    loop_infos:Cfg_loop_infos.t Lazy.t ->
    Label.t =
 fun tree ~to_cover ~num_checks ~loop_infos ->
  assert (to_cover = Label.Tbl.find num_checks tree.label);
  let candidates =
    List.filter
      (fun (child : Cfg_dominators.dominator_tree) ->
        Label.Tbl.find num_checks child.label = to_cover)
      tree.children
  in
  match candidates with
  | [] -> tree.label
  | [candidate] ->
    let candidate_is_in_a_loop =
      Label.Map.find candidate.label (Lazy.force loop_infos).loop_depths > 0
    in
    if candidate_is_in_a_loop
    then tree.label
    else find_stack_check_block candidate ~to_cover ~num_checks ~loop_infos
  | _ -> assert false

(* CR-someday xclerc for xclerc: we may want to duplicate the check in some
   cases, rather than simply pushing it down. *)
let cfg : Cfg_with_layout.t -> Cfg_with_layout.t =
 fun cfg_with_layout ->
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  let { max_frame_size; blocks_needing_stack_checks; max_instr_id } =
    build_cfg_infos cfg
  in
  (if not (Label.Set.is_empty blocks_needing_stack_checks)
  then
    (* CR-soon xclerc for xclerc: use the dominators and loop infos from
       Cfg_with_infos (at least on some paths). *)
    let doms = Cfg_dominators.build cfg in
    let loop_infos = lazy (Cfg_loop_infos.build cfg doms) in
    List.iter
      (fun (tree : Cfg_dominators.dominator_tree) ->
        (* note: the other entries in the forest are dead code *)
        if Label.equal tree.label cfg.entry_label
        then
          let num_checks = Label.Tbl.create (Label.Tbl.length cfg.blocks) in
          let num_checks_root =
            num_checks_tree tree ~blocks_needing_stack_checks ~num_checks
          in
          match num_checks_root with
          | 0 -> ()
          | to_cover ->
            let label =
              find_stack_check_block tree ~to_cover ~num_checks ~loop_infos
            in
            let block = Cfg.get_block_exn cfg label in
            let stack_offset =
              match DLL.hd block.body with
              | None -> block.terminator.stack_offset
              | Some instr -> instr.stack_offset
            in
            let check : Cfg.basic Cfg.instruction =
              { desc = Stack_check { max_frame_size_bytes = max_frame_size };
                arg = [||];
                res = [||];
                dbg = Debuginfo.none;
                fdo = Fdo_info.none;
                live = Reg.Set.empty;
                stack_offset;
                id = succ max_instr_id;
                irc_work_list = Unknown_list;
                ls_order = 0;
                (* CR xclerc for xclerc: double check `available_before` and
                   `available_across`. *)
                available_before = None;
                available_across = None
              }
            in
            DLL.add_begin block.body check)
      (Cfg_dominators.dominator_forest doms));
  cfg_with_layout
