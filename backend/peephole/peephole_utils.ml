module DLL = Flambda_backend_utils.Doubly_linked_list

(* CR-someday gtulba-lecu: make sure that this comparison is correct and
   sufficent. Take into consideration using Proc.regs_are_volatile in the
   future. As we only support amd64 and Proc.regs_are_volatile is always false
   in amd64 this is not necessary for now. See backend/cfg/cfg_deadcode.ml for
   more details.*)
let are_equal_regs (reg1 : Reg.t) (reg2 : Reg.t) =
  Reg.same_loc reg1 reg2 && reg1.typ = reg2.typ

(* CR-soon gtulba-lecu: Delete this when imeplementing auto-generated rules. *)
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

let is_bitwise_op (op : Mach.integer_operation) =
  match op with
  | Mach.Iand | Ior | Ixor | Ilsl | Ilsr | Iasr -> true
  | _ -> false
  [@@ocaml.warning "-4"]

let bitwise_shift_assert (imm1 : int) (imm2 : int) =
  if imm1 < 0 || imm1 > Sys.int_size || imm2 < 0 || imm2 > Sys.int_size
  then assert false
  [@@inline]

(* CR-someday gtulba-lecu: This is architecture specific and should be moved in
   a different part of the compiler that is specific to the amd64 architecture.
   This is fine for now as we only support amd64. *)
let amd64_imm32_within_bounds imm1 imm2 op =
  let imm = op imm1 imm2 in
  Int32.to_int Int32.min_int <= imm && imm <= Int32.to_int Int32.max_int
  [@@inline]
