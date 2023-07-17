[@@@ocaml.warning "+a-29-40-41-42-4"]

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

let random_hex_string () =
  let random_value = Random.int (1 lsl 24) in
  Printf.sprintf "%06x" random_value

let set_csv () =
  if Option.is_none !csv_singleton
  then (
    let new_csv = IntCsv.create ["remove_useless_mov"; "fold_intop_imm"] in
    if !Flambda_backend_flags.cfg_peephole_optimize_track
    then Stdlib.at_exit (fun () -> 
      Random.self_init (); 
      (* Generate a random 6-hexdigit string for the name of the csv file *)
      IntCsv.print new_csv (random_hex_string () ^ ".csv"));
    csv_singleton := Some new_csv)

let get_csv () = Option.get !csv_singleton

(* CR gtulba-lecu for gtulba-lecu: make sure that this comparison *)
let are_equal_regs reg1 reg2 =
  Reg.same_loc reg1 reg2 && Regalloc_utils.same_reg_class reg1 reg2

let update_csv str =
  let csv = get_csv () in
  match IntCsv.rows csv with
  | [] ->
    (* there should always be at least a row in the csv when updating *)
    assert false
  | rows_hd :: rows_tl ->
    IntCsv.set_rows csv (IntCsv.update_row rows_hd str IntCell.update :: rows_tl)

(* Logical condition for simplifying the following case: 
  {v 
    mov x, y 
    mov y, x 
  v}

   In this case, the second instruction should be removed *)

let remove_useless_mov (fst : Cfg.basic Cfg.instruction DLL.cell)
    (snd : Cfg.basic Cfg.instruction DLL.cell) =
  let fst_val = DLL.value fst in
  let snd_val = DLL.value snd in
  match fst_val.desc with
  | Op Move | Op Spill | Op Reload -> (
    let fst_src, fst_dst = fst_val.arg.(0), fst_val.res.(0) in
    match snd_val.desc with
    | Op Move | Op Spill | Op Reload ->
      let snd_src, snd_dst = snd_val.arg.(0), snd_val.res.(0) in
      if are_equal_regs fst_src snd_dst && are_equal_regs fst_dst snd_src
      then (
        if !Flambda_backend_flags.cfg_peephole_optimize_track
        then update_csv "remove_useless_mov";
        DLL.delete_curr snd;
        Some fst)
      else None
    | _ -> None)
  | _ -> None

(* Logical condition for simplifying the following case: 
  {v 
    <op 1> const1, r 
    <op 2> const2, r 
  v}

  to: 
  {v 
    <op 1> (const1 <op 2> const2), r 
  v}

   Where <op 1> and <op 2> can be any two binary operators that are associative
   and const1 and const2 are immediate values. *)

(* CR-soon gtulba-lecu for gtulba-lecu: implement the rest of Intop_imm cases 
   (only Imulh left to optimize if possible)
    
    it may be the case that after Iadd, Isub folding the result is 0, treat this case.
*)
let are_compatible op1 op2 imm1 imm2 =
  match (op1: Mach.integer_operation), (op2: Mach.integer_operation) with
  | Mach.Iand, Mach.Iand -> Some (Mach.Iand, imm1 land imm2)
  | Ior, Ior -> Some (Mach.Ior, imm1 lor imm2)
  | Ixor, Ixor -> Some (Mach.Ixor, imm1 lxor imm2)
  | Ilsl, Ilsl -> Some (Mach.Ilsl, min (imm1 + imm2) Sys.int_size)
  | Ilsr, Ilsr -> Some (Mach.Ilsr, min (imm1 + imm2) Sys.int_size)
  | Iasr, Iasr -> Some (Mach.Iasr, min (imm1 + imm2) Sys.int_size)
  | Iadd, Iadd -> 
    if Misc.no_overflow_add imm1 imm2 then
      Some (Mach.Iadd, imm1 + imm2)
    else None
  | Iadd, Isub -> 
    if imm1 >= imm2 then
      Some (Mach.Iadd, imm1 - imm2)
    else Some (Mach.Isub, imm2 - imm1)
  | Isub, Isub -> 
    if Misc.no_overflow_add imm1 imm2 then
      Some (Mach.Iadd, imm1 + imm2)
    else None
  | Isub, Iadd -> 
    if imm1 >= imm2 then
      Some (Mach.Isub, imm1 - imm2)
    else Some (Mach.Iadd, imm2 - imm1)
  | Imul, Imul -> 
    if Misc.no_overflow_mul imm1 imm2 then
      Some (Mach.Imul, imm1 * imm2)
    else None    
  | Idiv, Idiv -> 
    if Misc.no_overflow_mul imm1 imm2 then
      Some (Mach.Idiv, imm1 * imm2)
    else None
  | Imod, Imod ->
    if (imm1 mod imm2) = 0 then
      Some (Mach.Imod, imm2)
    else None
  | _ -> None

let fold_intop_imm (fst : Cfg.basic Cfg.instruction DLL.cell)
    (snd : Cfg.basic Cfg.instruction DLL.cell) =
  let fst_val = DLL.value fst in
  let snd_val = DLL.value snd in
  if are_equal_regs fst_val.arg.(0) snd_val.arg.(0)
  then
    match fst_val.desc, snd_val.desc with
    | Op (Intop_imm (op1, imm1)), Op (Intop_imm (op2, imm2)) ->
      (match are_compatible op1 op2 imm1 imm2 with
      | Some (op, imm) -> 
        (if !Flambda_backend_flags.cfg_peephole_optimize_track
        then update_csv "fold_intop_imm";
        DLL.insert_before fst
          { fst_val with desc = Cfg.Op (Intop_imm (op, imm)) };
        let new_cell = DLL.prev fst in
        DLL.delete_curr fst;
        DLL.delete_curr snd;
        new_cell)
      | _ -> None)
    | _ -> None
  else None

(* CR gtulba-lecu for xclerc: I am thinking of having this functionality in the
   dll. It feels useful, what do you think? *)
let rec prev_at_most cell steps =
  (* Convention: must try to go back at least one element *)
  assert (steps > 0);
  match DLL.prev cell with
  | Some prev_cell ->
    if steps = 1 then prev_cell else prev_at_most prev_cell (steps - 1)
  | None -> cell

(* Helper function for optimize_body. Here cell is an iterator of the doubly
   linked list data structure that encapsulates the body's instructions. *)
(* CR-soon gtulba-lecu for gtulba-lecu: change the `2` constant to the biggest
   arity (in terms of asm instructions) of the pattern matches. *)
let rec optimize_body' cell =
  let prev_cell_opt = DLL.prev cell in
  let next_cell_opt = DLL.next cell in
  let go_back_const = 2 in
  match prev_cell_opt with
  | None -> (
    match next_cell_opt with
    | None -> ()
    | Some next_cell -> optimize_body' next_cell)
  | Some prev_cell -> (
    let new_cell_opt = remove_useless_mov prev_cell cell in
    match new_cell_opt with
    | Some new_cell -> prev_at_most new_cell go_back_const |> optimize_body'
    | None -> (
      let new_cell_opt = fold_intop_imm prev_cell cell in
      match new_cell_opt with
      | Some new_cell -> prev_at_most new_cell go_back_const |> optimize_body'
      | None -> (
        match next_cell_opt with
        | None -> ()
        | Some next_cell -> optimize_body' next_cell)))

let optimize_body (body : Cfg.basic_instruction_list) =
  match DLL.hd_cell body with Some cell -> optimize_body' cell | None -> ()

(* Apply peephole optimization for the body of each block of the CFG*)
let peephole_optimize_cfg cfg_with_layout =
  set_csv ();
  let fun_name = (Cfg_with_layout.cfg cfg_with_layout).fun_name in
  if !Flambda_backend_flags.cfg_peephole_optimize_track
  then IntCsv.add_empty_row (get_csv ()) fun_name;
  Label.Tbl.iter
    (fun (_ : Label.t) (block : Cfg.basic_block) -> optimize_body block.body)
    (Cfg_with_layout.cfg cfg_with_layout).blocks;
  cfg_with_layout
