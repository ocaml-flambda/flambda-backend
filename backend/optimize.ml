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

module IntCsv = Csv_data.Make(IntCell)

let csv = ref Option.None

let set_csv () =
  if Option.is_none !csv then begin 
  let new_csv = IntCsv.create ["remove_useless_mov"; "fold_intop_imm"] in
  if !Flambda_backend_flags.cfg_peephole_optimize_track then
  Stdlib.at_exit (fun () -> print_endline (IntCsv.to_string new_csv));
  csv := Some new_csv
  end

(* Logical condition for simplifying the following case:

  {v
    mov x, y
    mov y, x
  v}   

  In this case, the second instruction should be removed
*)
let remove_useless_mov row_opt (fst:Cfg.basic Cfg.instruction DLL.cell) (snd:Cfg.basic Cfg.instruction DLL.cell) =
  let fst_val = DLL.value fst in
  let snd_val = DLL.value snd in
  match fst_val.desc, snd_val.desc with
  | Op Move, Op Move -> 
    let fst_src, fst_dst = fst_val.arg.(0), fst_val.res.(0) in
    let snd_src, snd_dst = snd_val.arg.(0), snd_val.res.(0) in
    let is_same_loc = Reg.same_loc fst_src snd_dst && Reg.same_loc fst_dst snd_src in
    let is_same_reg_class = Regalloc_utils.same_reg_class fst_src snd_dst && Regalloc_utils.same_reg_class fst_dst snd_src in
    if is_same_loc && is_same_reg_class then begin
      DLL.delete_curr snd;
      row_opt := Option.map (fun row -> IntCsv.update_row row "remove_useless_mov" IntCell.update) !row_opt;
      true
    end else
      false
  | _ -> false

(* Logical condition for simplifying the following case:

  {v
    <op 1> const1, r
    <op 2> const2, r
  v}   

  to:

  {v
    <op 1> const1 <op 2> const2, r
  v}   

  Where <op 1> and <op 2> can be any two binary operators that are associative and const1
  and const2 are immediate values.
*)
(* CR-soon gtulba-lecu for gtulba-lecu: implement the rest of Intop_imm cases*)
let fold_intop_imm row_opt (fst:Cfg.basic Cfg.instruction DLL.cell) (snd:Cfg.basic Cfg.instruction DLL.cell) = 
  match (DLL.value fst).desc, (DLL.value snd).desc with
  | Op (Intop_imm (Ior, imm1)), Op (Intop_imm (Ior, imm2)) -> 
    DLL.insert_before fst {(DLL.value fst) with desc = Cfg.Op (Intop_imm (Ior, imm1 lor imm2))}; 
    DLL.delete_curr fst;
    DLL.delete_curr snd;
    row_opt := Option.map (fun row -> IntCsv.update_row row "fold_intop_imm" IntCell.update) !row_opt;
    true
  | _ -> false

(* Helper function for optimize_body. Here cell is an iterator of the doubly linked list
 data structure that encapsulates the body's instructions. *)
let rec optimize_body' row_opt cell =
  let prev_cell_opt = DLL.prev cell in
  let next_cell_opt = DLL.next cell in
  match prev_cell_opt with
  | None -> 
    begin match next_cell_opt with
    | None -> ()
    | Some next_cell -> optimize_body' row_opt next_cell
    end
  | Some prev_cell ->
    begin 
      if not (remove_useless_mov row_opt prev_cell cell) then
      if not (fold_intop_imm row_opt prev_cell cell) then ();
      match next_cell_opt with
      | None -> ()
      | Some next_cell -> optimize_body' row_opt next_cell
    end

let optimize_body row_opt (body: Cfg.basic_instruction_list) =
  match DLL.hd_cell body with
  | Some cell -> begin
      optimize_body' row_opt cell;
      if Option.is_some !row_opt then IntCsv.add_row (Option.get !csv) (Option.get !row_opt)
    end
  | None -> ()
;;

(* Apply peephole optimization for the body of each block of the CFG*)
let peephole_optimize_cfg cfg_with_layout =
  set_csv ();
  let fun_name = (Cfg_with_layout.cfg cfg_with_layout).fun_name in
  let row_opt = if !Flambda_backend_flags.cfg_peephole_optimize_track then ref (Some (IntCsv.empty_row fun_name (IntCsv.column_names (Option.get !csv)))) else ref None in
  Label.Tbl.iter 
    (fun (_:Label.t) (block:Cfg.basic_block) -> optimize_body row_opt block.body) 
    (Cfg_with_layout.cfg cfg_with_layout).blocks;
  cfg_with_layout
;;