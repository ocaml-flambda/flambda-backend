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
module Dll = Flambda_backend_utils.Doubly_linked_list

(* Convert simple [Switch] to branches. *)
let simplify_switch (block : C.basic_block) labels =
  let len = Array.length labels in
  if len < 1
  then Misc.fatal_error "Malformed terminator: switch with empty arms";
  (* Count continuous repeated occurrences of labels *)
  let labels_with_counts =
    Array.fold_right
      (fun l acc ->
        if List.compare_length_with acc 3 > 0
        then acc
        else
          match acc with
          | [] -> [l, 1]
          | (hd, n) :: tl ->
            if Label.equal hd l then (hd, n + 1) :: tl else (l, 1) :: acc)
      labels []
  in
  match labels_with_counts with
  | [(l, _)] ->
    (* All labels are the same and equal to l *)
    block.terminator <- { block.terminator with desc = Always l }
  | [(l0, n); (ln, k)] ->
    assert (Label.equal labels.(0) l0);
    assert (Label.equal labels.(n) ln);
    assert (len = n + k);
    let desc =
      C.Int_test { is_signed = false; imm = Some n; lt = l0; eq = ln; gt = ln }
    in
    block.terminator <- { block.terminator with desc }
  | [(l0, m); (l1, 1); (l2, _)] when Label.equal l0 l2 ->
    let desc =
      C.Int_test { is_signed = false; imm = Some m; lt = l0; eq = l1; gt = l0 }
    in
    block.terminator <- { block.terminator with desc }
  | [(l0, 1); (l1, 1); (l2, n)] ->
    assert (Label.equal labels.(0) l0);
    assert (Label.equal labels.(1) l1);
    assert (Label.equal labels.(2) l2);
    assert (len = n + 2);
    let desc =
      C.Int_test { is_signed = false; imm = Some 1; lt = l0; eq = l1; gt = l2 }
    in
    block.terminator <- { block.terminator with desc }
  | _ -> ()

(* Compute the destination of a terminator, knowing that [reg] is equal to
   [const], returning [None] if the destination is not statically known. *)
let evaluate_terminator ~(reg : Reg.t) ~(const : nativeint)
    (term : Cfg.terminator Cfg.instruction) : Label.t option =
  let same_reg ~arg_idx =
    arg_idx >= 0
    && arg_idx < Array.length term.arg
    && Reg.same reg (Array.unsafe_get term.arg arg_idx)
  in
  match term.desc with
  | Parity_test { ifso; ifnot } ->
    if same_reg ~arg_idx:0
    then if Nativeint.logand const 1n = 0n then Some ifso else Some ifnot
    else None
  | Truth_test { ifso; ifnot } ->
    if same_reg ~arg_idx:0
    then if const <> 0n then Some ifso else Some ifnot
    else None
  | Int_test { lt; eq; gt; is_signed; imm } -> (
    match imm with
    | None -> None
    | Some const' ->
      if same_reg ~arg_idx:0
      then
        let const' = Nativeint.of_int const' in
        let result =
          if is_signed
          then Nativeint.compare const const'
          else Nativeint.unsigned_compare const const'
        in
        if result < 0 then Some lt else if result > 0 then Some gt else Some eq
      else None)
  | Switch labels ->
    if same_reg ~arg_idx:0 && const <= Nativeint.of_int Int.max_int
    then
      let idx = Nativeint.to_int const in
      if idx >= 0 && idx < Array.length labels
      then Some (Array.unsafe_get labels idx)
      else None
    else None
  | Never -> assert false
  | Always _ | Float_test _ | Return | Raise _ | Tailcall_self _
  | Tailcall_func _ | Call_no_return _ | Call _ | Prim _ | Specific_can_raise _
    ->
    None

(* CR-someday gyorsh: merge (Lbranch | Lcondbranch | Lcondbranch3)+ into a
   single terminator when the argments are the same. Enables reordering of
   branch instructions and save cmp instructions. The main problem is that it
   involves boolean combination of conditionals of type Mach.test that can arise
   from a sequence of branches. When all conditions in the combination are
   integer comparisons, we can simplify them into a single condition, but it
   doesn't work for Ieventest and Ioddtest (which come from the primitive "is
   integer"). The advantage is that it will enable us to reorder branch
   instructions to avoid generating jmp to fallthrough location in the new
   order. Also, for linear to cfg and back will be harder to generate exactly
   the same layout. Also, how do we map execution counts about branches onto
   this terminator? *)
let block (cfg : C.t) (block : C.basic_block) : bool =
  match block.terminator.desc with
  | Always successor_label -> (
    match[@ocaml.warning "-4"] Dll.last block.body with
    | None -> false
    | Some { desc = Op (Const_int const); res = [| reg |]; _ } ->
      (* If we have an Iconst_int instruction at the end of the block followed
         by a jump to an empty block whose terminator is a condition over the
         Iconst_value, then we can evaluate the condition at compile-time and
         short-circuit the empty block. *)
      let successor_block = C.get_block_exn cfg successor_label in
      if Dll.is_empty successor_block.body
      then (
        let new_successor =
          evaluate_terminator ~reg ~const successor_block.terminator
        in
        match new_successor with
        | None -> false
        | Some succ ->
          block.terminator <- { block.terminator with desc = Always succ };
          true)
      else false
    | Some _ -> false)
  | Never ->
    Misc.fatal_errorf "Cannot simplify terminator: Never (in block %d)"
      block.start
  | Parity_test _ | Truth_test _ | Int_test _ | Float_test _ ->
    let labels = C.successor_labels ~normal:true ~exn:false block in
    (if Label.Set.cardinal labels = 1
    then
      let l = Label.Set.min_elt labels in
      block.terminator <- { block.terminator with desc = Always l });
    false
  | Switch labels ->
    simplify_switch block labels;
    false
  | Raise _ | Return | Tailcall_self _ | Tailcall_func _ | Call_no_return _
  | Call _ | Prim _ | Specific_can_raise _ ->
    false

let run cfg =
  let registration_needed =
    C.fold_blocks cfg ~init:false ~f:(fun _ b registration_needed ->
        let shortcircuit = block cfg b in
        registration_needed || shortcircuit)
  in
  if registration_needed then Cfg.register_predecessors_for_all_blocks cfg
