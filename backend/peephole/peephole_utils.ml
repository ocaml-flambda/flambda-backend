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

(* CR-soon gyorsh: This functor is also instantiated in
   [Asmgen.compile_fundecl]. Find a shared place to put it, instead of
   instantiating twice. May require restructuring the backend to avoid
   dependency cycles. *)
module Cfg_selection = Cfg_selectgen.Make (Cfg_selection)

let is_immediate_for_intop op n = Cfg_selection.is_immediate op n

let assert_within_range integer_operation imm =
  if not (is_immediate_for_intop integer_operation imm)
  then
    Misc.fatal_errorf "Peephole: unexpected immediate %d for operation %s" imm
      (Operation.string_of_integer_operation integer_operation)

let[@inline] op_immediates integer_operation imm1 imm2 no_overflow op =
  (* [no_overflow imm1 imm2] operation may assume that each of the immediates on
     its own is within bounds. *)
  assert_within_range integer_operation imm1;
  assert_within_range integer_operation imm2;
  let res = op imm1 imm2 in
  if no_overflow imm1 imm2 && is_immediate_for_intop integer_operation res
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
  op_immediates integer_operation imm1 imm2 never_overflow op
