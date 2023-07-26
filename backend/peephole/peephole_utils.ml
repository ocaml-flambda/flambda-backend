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

let set_csv () =
  if Option.is_none !csv_singleton
  then (
    let new_csv =
      IntCsv.create
        ["remove_useless_mov"; "fold_intop_imm_bitwise"; "fold_intop_imm_arith"]
    in
    if Option.is_some !Flambda_backend_flags.cfg_peephole_optimize_track
    then
      Stdlib.at_exit (fun () ->
          (* the csv filename is a hex string deterministically generated from
             the command line arguments. *)
          IntCsv.print new_csv
            (Option.get !Flambda_backend_flags.cfg_peephole_optimize_track
            ^ (Array.to_list Sys.argv |> String.concat "" |> Digest.string
             |> Digest.to_hex)
            ^ ".csv"));
    csv_singleton := Some new_csv)

let get_csv () = Option.get !csv_singleton

let update_csv str =
  let csv = get_csv () in
  match IntCsv.rows csv with
  | [] ->
    (* there should always be at least a row in the csv when updating *)
    assert false
  | rows_hd :: rows_tl ->
    IntCsv.set_rows csv (IntCsv.update_row rows_hd str IntCell.update :: rows_tl)

(* CR gtulba-lecu for gtulba-lecu: make sure that this comparison is correct and
   sufficent. *)
let are_equal_regs (reg1 : Reg.t) (reg2 : Reg.t) =
  Reg.same_loc reg1 reg2 && reg1.typ = reg2.typ

(* CR gtulba-lecu for gtulba-lecu: It would be nice to compute this based on the
   rules, but since the layout of this code will probably change it's fine for
   now. *)
let go_back_const = 1

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
      | 0 -> List.rev lst
      | size -> get_cells' (DLL.next cell) (size - 1) (cell :: lst))
  | None -> List.rev lst

let get_cells cell size =
  assert (size > 0);
  get_cells' (DLL.next cell) (size - 1) [cell]

type rule = Cfg.basic Cfg.instruction DLL.cell -> Cfg.basic Cfg.instruction DLL.cell option
