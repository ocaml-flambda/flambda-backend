module DLL = Flambda_backend_utils.Doubly_linked_list
open! Int_replace_polymorphic_compare

let are_equal_regs (reg1 : Reg.t) (reg2 : Reg.t) =
  Reg.same_loc reg1 reg2 && Cmm.equal_machtype_component reg1.typ reg2.typ

(* CR-soon gtulba-lecu: Delete this when implementing auto-generated rules. *)
let go_back_const = 1

let rec prev_at_most steps cell =
  (* Convention: must try to go back at least one element *)
  assert (steps > 0);
  match DLL.prev cell with
  | Some prev_cell ->
    if steps = 1 then prev_cell else prev_at_most (steps - 1) prev_cell
  | None -> cell

let rec get_cells' (cell : Cfg.basic Cfg.instruction DLL.cell option) size lst =
  match cell with
  | Some cell -> (
    match size with
    | 0 -> List.rev lst
    | size -> get_cells' (DLL.next cell) (size - 1) (cell :: lst))
  | None -> List.rev lst

let get_cells cell size =
  assert (size > 0);
  get_cells' (DLL.next cell) (size - 1) [cell]

let assert_within_range integer_operation imm =
  if not (Arch.is_immediate_for_intop integer_operation imm)
  then
    Misc.fatal_errorf "Peephole: unexpected immediate %d for operation %s" imm
      (Simple_operation.string_of_integer_operation integer_operation)

let[@inline] op_immediates integer_operation imm1 imm2 no_overflow op =
  assert_within_range integer_operation imm1;
  assert_within_range integer_operation imm1;
  let res = op imm1 imm2 in
  if no_overflow imm1 imm2 && Arch.is_immediate_for_intop integer_operation res
  then Some (integer_operation, res)
  else None

let add_immediates integer_operation imm1 imm2 =
  op_immediates integer_operation imm1 imm2 Misc.no_overflow_add ( + )

let sub_immediates integer_operation imm1 imm2 =
  op_immediates integer_operation imm1 imm2 Misc.no_overflow_sub ( - )

let mul_immediates integer_operation imm1 imm2 =
  op_immediates integer_operation imm1 imm2 Misc.no_overflow_mul ( * )

let never_overflow _ _ = true

let bitwise_immediates integer_operation imm1 imm2 op =
  (* Bitwise operations on immediates within range cannot produce immediates
     outside of range. Bitwise operations do not need overflow check. *)
  match op_immediates integer_operation imm1 imm2 never_overflow op with
  | None ->
    Misc.fatal_errorf
      "Peephole: cannot rewrite immediates for %s: combining %d %d = %d"
      (Simple_operation.string_of_integer_operation integer_operation)
      imm1 imm2 (op imm1 imm2)
  | Some _ as res -> res
