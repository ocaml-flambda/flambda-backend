[@@@ocaml.warning "+a-29-40-41-42"]

module DLL = Flambda_backend_utils.Doubly_linked_list

module type IntCell = sig
  include Csv_data.Cell

  val update : t -> t
end

module IntCell : IntCell = struct
  type t = int

  let to_string = Int.to_string

  let empty () = 0

  let update t = t + 1
end

module IntCsv = Csv_data.Make (IntCell)

let csv_singleton = ref Option.None

let get_csv () = Option.get !csv_singleton

let update_csv str =
  let csv = get_csv () in
  match IntCsv.rows csv with
  | [] ->
    (* there should always be at least a row in the csv when updating *)
    assert false
  | rows_hd :: rows_tl ->
    IntCsv.set_rows csv (IntCsv.update_row rows_hd str IntCell.update :: rows_tl)

(* CR-someday gtulba-lecu: make sure that this comparison is correct and
   sufficent. Take into consideration using Proc.regs_are_volatile in the
   future. As we only support amd64 and Proc.regs_are_volatile is always false
   in amd64 this is not necessary for now. See backend/cfg/cfg_deadcode.ml for
   more details.*)
let are_equal_regs (reg1 : Reg.t) (reg2 : Reg.t) =
  Reg.same_loc reg1 reg2 && reg1.typ = reg2.typ

(* convenient Doubly_linked_list manipulation functions *)
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
    | 0 -> Some (List.rev lst)
    | size -> get_cells' (DLL.next cell) (size - 1) (cell :: lst))
  | None -> None

let get_cells cell size =
  assert (size > 0);
  get_cells' (DLL.next cell) (size - 1) [cell]

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

let amd64_imm32_within_bounds_assert_if_false imm1 imm2 op =
  if amd64_imm32_within_bounds imm1 imm2 op then true else assert false
  [@@inline]
