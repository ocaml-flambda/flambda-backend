module DLL = Flambda_backend_utils.Doubly_linked_list

(* Logical condition for simplifying the following case:

  {v
    mov x, y
    mov y, x
  v}   

  In this case, the second instruction should be removed
*)
let is_useless_mov (fst:Cfg.basic Cfg.instruction) (snd:Cfg.basic Cfg.instruction) =
  match fst.desc, snd.desc with
  | Op Move, Op Move -> 
    (let fst_src, fst_dst = fst.arg.(0), fst.res.(0) in
    let snd_src, snd_dst = snd.arg.(0), snd.res.(0) in
    let is_same_loc = Reg.same_loc fst_src snd_dst && Reg.same_loc fst_dst snd_src in
    let is_same_reg_class = Regalloc_utils.same_reg_class fst_src snd_dst && Regalloc_utils.same_reg_class fst_dst snd_src in
    is_same_loc && is_same_reg_class)
  | _ -> false 

(* Helper function for optimize_body. Here cell is an iterator of the doubly linked list
 data structure that encapsulates the body's instructions. *)
let rec optimize_body' cell =
  let prev_cell_opt = DLL.prev cell in
  let next_cell_opt = DLL.next cell in
  match prev_cell_opt with
  | None -> 
    (match next_cell_opt with
    | None -> ()
    | Some next_cell -> optimize_body' next_cell)
  | Some prev_cell ->
    ((if is_useless_mov (DLL.value prev_cell) (DLL.value cell) then
      DLL.delete_curr cell
    else ());
    match next_cell_opt with
    | None -> ()
    | Some next_cell -> optimize_body' next_cell)

let optimize_body (body: Cfg.basic_instruction_list) =
  match DLL.hd_cell body with
  | Some cell -> optimize_body' cell
  | None -> ()
;;

(* Apply peephole optimization for the body of each block of the CFG*)
let peephole_optimize_cfg cfg_with_layout =
  Label.Tbl.iter 
    (fun (_:Label.t) (block:Cfg.basic_block) -> optimize_body block.body) 
    (Cfg_with_layout.cfg cfg_with_layout).blocks;
  cfg_with_layout
;;