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

let csv_singleton = ref Option.None

let set_csv () =
  if Option.is_none !csv_singleton then begin 
  let new_csv = IntCsv.create ["remove_useless_mov"; "fold_intop_imm"] in
  if !Flambda_backend_flags.cfg_peephole_optimize_track then
  Stdlib.at_exit (fun () -> IntCsv.print new_csv "opt_hits.csv");
  csv_singleton := Some new_csv
  end

let get_csv () = Option.get !csv_singleton

let cmp_regs reg1 reg2 =
  Reg.same_loc reg1 reg2 && Regalloc_utils.same_reg_class reg1 reg2

let update_csv str =
  let csv = get_csv () in
  match IntCsv.rows csv with
  | [] -> 
    (* there should always be at least a row in the csv when updating *)
    assert false
  | rows_hd :: rows_tl ->
    IntCsv.set_rows csv ((IntCsv.update_row rows_hd str IntCell.update) :: rows_tl)

(* Logical condition for simplifying the following case:
  {v
    mov x, y
    mov y, x
  v}   

  In this case, the second instruction should be removed
*)
let remove_useless_mov (fst:Cfg.basic Cfg.instruction DLL.cell) (snd:Cfg.basic Cfg.instruction DLL.cell) =
  let fst_val = DLL.value fst in
  let snd_val = DLL.value snd in
  match fst_val.desc, snd_val.desc with
  | Op Move, Op Move -> 
    let fst_src, fst_dst = fst_val.arg.(0), fst_val.res.(0) in
    let snd_src, snd_dst = snd_val.arg.(0), snd_val.res.(0) in
    if cmp_regs fst_src snd_dst && cmp_regs fst_dst snd_src then begin
      DLL.delete_curr snd;
      if !Flambda_backend_flags.cfg_peephole_optimize_track then 
        update_csv "remove_useless_mov";
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
let fold_intop_imm (fst:Cfg.basic Cfg.instruction DLL.cell) (snd:Cfg.basic Cfg.instruction DLL.cell) =
  let fst_val = DLL.value fst in
  let snd_val = DLL.value snd in
  if cmp_regs fst_val.arg.(0) snd_val.arg.(0) then begin
    match fst_val.desc, snd_val.desc with
    | Op (Intop_imm (Ior, imm1)), Op (Intop_imm (Ior, imm2)) -> 
      DLL.insert_before fst {fst_val with desc = Cfg.Op (Intop_imm (Ior, imm1 lor imm2))}; 
      DLL.delete_curr fst;
      DLL.delete_curr snd;
      if !Flambda_backend_flags.cfg_peephole_optimize_track then 
        update_csv "fold_intop_imm";
      true
    | _ -> false
  end else false

(* Helper function for optimize_body. Here cell is an iterator of the doubly linked list
 data structure that encapsulates the body's instructions. *)
let rec optimize_body' cell =
  let prev_cell_opt = DLL.prev cell in
  let next_cell_opt = DLL.next cell in
  match prev_cell_opt with
  | None -> 
    begin match next_cell_opt with
    | None -> ()
    | Some next_cell -> optimize_body' next_cell
    end
  | Some prev_cell ->
    begin 
      if not (remove_useless_mov prev_cell cell) then
      if not (fold_intop_imm prev_cell cell) then ();
      match next_cell_opt with
      | None -> ()
      | Some next_cell -> optimize_body' next_cell
    end

let optimize_body (body: Cfg.basic_instruction_list) =
  match DLL.hd_cell body with
  | Some cell -> begin
      optimize_body' cell;
    end
  | None -> ()
;;

(* Apply peephole optimization for the body of each block of the CFG*)
let peephole_optimize_cfg cfg_with_layout =
  set_csv ();
  let fun_name = (Cfg_with_layout.cfg cfg_with_layout).fun_name in
  if !Flambda_backend_flags.cfg_peephole_optimize_track then 
    IntCsv.add_empty_row (get_csv ()) fun_name;
  Label.Tbl.iter 
    (fun (_:Label.t) (block:Cfg.basic_block) -> optimize_body block.body) 
    (Cfg_with_layout.cfg cfg_with_layout).blocks;
  cfg_with_layout
;;