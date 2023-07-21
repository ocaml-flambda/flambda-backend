(* CR-someday: see whether the `-4` can be dropped. *)
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
let are_equal_regs (reg1 : Reg.t ) (reg2 : Reg.t) =
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

(** Logical condition for simplifying the following case: 
  {| 
    mov x, y 
    mov y, x 
  |}

   In this case, the second instruction should be removed *)

let remove_useless_mov (cell : Cfg.basic Cfg.instruction DLL.cell) =
  match get_cells cell 2 with
  | [fst; snd] -> (
    let fst_val = DLL.value fst in
    let snd_val = DLL.value snd in
    match fst_val.desc with
    | Op (Move | Spill | Reload) -> (
      let fst_src, fst_dst = fst_val.arg.(0), fst_val.res.(0) in
      match snd_val.desc with
      | Op (Move | Spill | Reload) ->
        let snd_src, snd_dst = snd_val.arg.(0), snd_val.res.(0) in
        if are_equal_regs fst_src snd_dst && are_equal_regs fst_dst snd_src
        then (
          if Option.is_some !Flambda_backend_flags.cfg_peephole_optimize_track
          then update_csv "remove_useless_mov";
          DLL.delete_curr snd;
          Some (prev_at_most go_back_const fst))
        else None
      | _ -> None)
    | _ -> None)
  | _ -> None

let is_bitwise_op (op : Mach.integer_operation) =
  match op with
  | Mach.Iand | Ior | Ixor | Ilsl | Ilsr | Iasr -> true
  | _ -> false

(** Logical condition for simplifying the following case: 
  {| 
    <op 1> const1, r 
    <op 2> const2, r 
  |}

  to: 
  {|
    <op 1> (const1 <op 2> const2), r 
  |}

   Where <op 1> and <op 2> can be any two binary operators that are associative
   and const1 and const2 are immediate values. *)

(* CR-soon gtulba-lecu for gtulba-lecu: implement the rest of Intop_imm cases
   (only Imulh left to optimize if possible)

   it may be the case that after Iadd, Isub folding the result is 0, treat this
   case. *)
let are_compatible op1 op2 imm1 imm2 =
  match (op1 : Mach.integer_operation), (op2 : Mach.integer_operation) with
  | Mach.Iand, Mach.Iand -> Some (Mach.Iand, imm1 land imm2)
  | Ior, Ior -> Some (Mach.Ior, imm1 lor imm2)
  | Ixor, Ixor -> Some (Mach.Ixor, imm1 lxor imm2)
  (* For the following three cases we have the issue that in some situations,
     one or both immediate values could be out of bounds, but the result might
     be within bounds (e.g. imm1 = -4 and imm2 = 65, their sum being 61), in
     that case we still do the folding. This should not happen at all since the
     immediate values should always be within the bounds [0, Sys.int_size]. *)
  | Ilsl, Ilsl ->
    if Misc.no_overflow_add imm1 imm2 && imm1 + imm2 <= Sys.int_size
    then Some (Mach.Ilsl, imm1 + imm2)
    else None
  | Ilsr, Ilsr ->
    if Misc.no_overflow_add imm1 imm2 && imm1 + imm2 <= Sys.int_size
    then Some (Mach.Ilsr, imm1 + imm2)
    else None
  | Iasr, Iasr ->
    if Misc.no_overflow_add imm1 imm2 && imm1 + imm2 <= Sys.int_size
    then Some (Mach.Iasr, imm1 + imm2)
    else None
  | Iadd, Iadd ->
    if Misc.no_overflow_add imm1 imm2
    then Some (Mach.Iadd, imm1 + imm2)
    else None
  | Iadd, Isub ->
    if imm1 >= imm2
    then
      if Misc.no_overflow_sub imm1 imm2
      then Some (Mach.Iadd, imm1 - imm2)
      else None
    else if Misc.no_overflow_sub imm2 imm1
    then Some (Mach.Isub, imm2 - imm1)
    else None
  | Isub, Isub ->
    if Misc.no_overflow_add imm1 imm2
    then Some (Mach.Isub, imm1 + imm2)
    else None
  | Isub, Iadd ->
    if imm1 >= imm2
    then
      if Misc.no_overflow_sub imm1 imm2
      then Some (Mach.Isub, imm1 - imm2)
      else None
    else if Misc.no_overflow_sub imm2 imm1
    then Some (Mach.Iadd, imm2 - imm1)
    else None
  | Ilsl, Imul ->
    if imm1 >= 0 && imm1 < (Sys.int_size - 1) && Misc.no_overflow_mul (1 lsl imm1) imm2
    then Some (Mach.Imul, (1 lsl imm1) * imm2)
    else None
  | Imul, Ilsl ->
    if imm2 >= 0 && imm2 < (Sys.int_size - 1) && Misc.no_overflow_mul imm1 (1 lsl imm2)
    then Some (Mach.Imul, imm1 * (1 lsl imm2))
    else None
  | Imul, Imul ->
    if Misc.no_overflow_mul imm1 imm2
    then Some (Mach.Imul, imm1 * imm2)
    else None
  (* temporarily commented out | Idiv, Idiv -> if Misc.no_overflow_mul imm1 imm2
     then Some (Mach.Idiv, imm1 * imm2) else None *)
  (* The integer modulo imm2 group is a subgroup of the integer modulo imm1 iff
     imm2 divides imm1

     This is because the operations in the groups are addition modulo n and m
     respectively. If n divides m, then every result of the operation (addition)
     in the n group will also be a legal result in the m group, which is
     essentially the definition of a subgroup. If n does not divide m, there
     will be some results in the n group that are not acceptable in the m
     group. *)
  (* temporarily commented out | Imod, Imod -> if imm1 mod imm2 = 0 then Some
     (Mach.Imod, imm2) else None *)
  | _ -> None

let fold_intop_imm (cell : Cfg.basic Cfg.instruction DLL.cell) =
  match get_cells cell 2 with
  | [fst; snd] ->
    let fst_val = DLL.value fst in
    let snd_val = DLL.value snd in
    (* The following check does the following: 1. Ensures that both instructions
       use the same source register; 2. Ensures that both instructions output
       the result to the source register, this is redundant for amd64 since
       there are no instructions that invalidate this condition. *)
    if Array.length fst_val.arg = 1
       && Array.length snd_val.arg = 1
       && Array.length fst_val.res = 1
       && Array.length snd_val.res = 1
       && are_equal_regs fst_val.arg.(0) snd_val.arg.(0)
       && are_equal_regs fst_val.arg.(0) fst_val.res.(0)
       && are_equal_regs snd_val.arg.(0) snd_val.res.(0)
    then
      match fst_val.desc, snd_val.desc with
      | Op (Intop_imm (op1, imm1)), Op (Intop_imm (op2, imm2)) -> (
        match are_compatible op1 op2 imm1 imm2 with
        | Some (op, imm) ->
          if Option.is_some !Flambda_backend_flags.cfg_peephole_optimize_track
          then
            update_csv
              (if is_bitwise_op op
              then "fold_intop_imm_bitwise"
              else "fold_intop_imm_arith");
          let new_cell =
            DLL.insert_and_return_before fst
              { fst_val with desc = Cfg.Op (Intop_imm (op, imm)) }
          in
          DLL.delete_curr fst;
          DLL.delete_curr snd;
          Some ((prev_at_most go_back_const) new_cell)
        | _ -> None)
      | _ -> None
    else None
  | _ -> None

let optimizations = [remove_useless_mov; fold_intop_imm]

(* Helper function for optimize_body. Here cell is an iterator of the doubly
   linked list data structure that encapsulates the body's instructions. *)
let rec optimize_body' cell made_optimizations =
  match List.find_map (fun opt_func -> opt_func cell) optimizations with
  | None -> (
    match DLL.next cell with
    | None -> made_optimizations
    | Some next_cell -> optimize_body' next_cell made_optimizations)
  | Some continuation_cell -> optimize_body' continuation_cell true

let optimize_body (body : Cfg.basic_instruction_list) =
  match DLL.hd_cell body with
  | Some cell -> optimize_body' cell false
  | None -> false

(* Apply peephole optimization for the body of each block of the CFG*)
let peephole_optimize_cfg cfg_with_layout =
  let fun_name = (Cfg_with_layout.cfg cfg_with_layout).fun_name in
  if Option.is_some !Flambda_backend_flags.cfg_peephole_optimize_track
  then (
    set_csv();
    IntCsv.add_empty_row (get_csv ()) fun_name;);
  let made_optimizations =
    Label.Tbl.fold
      (fun (_ : Label.t) (block : Cfg.basic_block) (made_optimizations : bool) ->
        made_optimizations || optimize_body block.body)
      (Cfg_with_layout.cfg cfg_with_layout).blocks false
  in
  cfg_with_layout, made_optimizations
