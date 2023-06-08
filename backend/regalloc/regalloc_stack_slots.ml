[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Regalloc_utils

type slot = int

type t =
  { stack_slots : int Reg.Tbl.t;
    num_stack_slots : int array
  }

let[@inline] make () =
  let stack_slots = Reg.Tbl.create 128 in
  let num_stack_slots = Array.make Proc.num_register_classes 0 in
  { stack_slots; num_stack_slots }

let[@inline] get_and_incr t ~reg_class =
  let res = t.num_stack_slots.(reg_class) in
  t.num_stack_slots.(reg_class) <- succ res;
  res

let[@inline] get_or_create t reg =
  match Reg.Tbl.find_opt t.stack_slots reg with
  | Some slot -> slot
  | None ->
    let res = get_and_incr t ~reg_class:(Proc.register_class reg) in
    Reg.Tbl.replace t.stack_slots reg res;
    res

let[@inline] get_or_fatal t reg =
  match Reg.Tbl.find_opt t.stack_slots reg with
  | None -> fatal "register %a has no associated slot" Printmach.reg reg
  | Some slot -> slot

let[@inline] use_same_slot_or_fatal t reg ~existing =
  match Reg.Tbl.find_opt t.stack_slots existing with
  | None -> fatal "register %a has no associated slot" Printmach.reg existing
  | Some slot -> Reg.Tbl.replace t.stack_slots reg slot

let[@inline] update_cfg_with_layout t cfg_with_layout =
  let fun_num_stack_slots =
    (Cfg_with_layout.cfg cfg_with_layout).fun_num_stack_slots
  in
  for reg_class = 0 to pred Proc.num_register_classes do
    fun_num_stack_slots.(reg_class) <- t.num_stack_slots.(reg_class)
  done
