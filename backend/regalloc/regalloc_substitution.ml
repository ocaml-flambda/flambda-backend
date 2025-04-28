[@@@ocaml.warning "+a-30-40-41-42"]

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]
module DLL = Flambda_backend_utils.Doubly_linked_list

type t = Reg.t Reg.Tbl.t

let apply_reg : t -> Reg.t -> Reg.t =
 fun subst old_reg ->
  match Reg.Tbl.find_opt subst old_reg with
  | None -> old_reg
  | Some new_reg -> new_reg

let apply_array_in_place : t -> Reg.t array -> unit =
 fun subst arr ->
  for i = 0 to pred (Array.length arr) do
    let old_reg = Array.unsafe_get arr i in
    match Reg.Tbl.find_opt subst old_reg with
    | None -> ()
    | Some new_reg -> Array.unsafe_set arr i new_reg
  done

let apply_array : t -> Reg.t array -> Reg.t array =
 fun subst arr ->
  let res = Array.copy arr in
  apply_array_in_place subst res;
  res

let apply_set : t -> Reg.Set.t -> Reg.Set.t =
 fun subst set -> Reg.Set.map (fun reg -> apply_reg subst reg) set

(* CR mshinwell: Apply substitution to [Iname_for_debugger] registers. *)
let apply_instruction_in_place : t -> _ Cfg.instruction -> unit =
 fun subst instr ->
  apply_array_in_place subst instr.arg;
  apply_array_in_place subst instr.res

let apply_block_in_place : t -> Cfg.basic_block -> unit =
 fun subst block ->
  DLL.iter block.body ~f:(fun instr -> apply_instruction_in_place subst instr);
  apply_instruction_in_place subst block.terminator

type map = t Label.Tbl.t

let for_label : map -> Label.t -> t =
 fun map label ->
  match Label.Tbl.find_opt map label with
  | None -> Reg.Tbl.create 0
  | Some subst -> subst

let apply_cfg_in_place : map -> Cfg.t -> unit =
 fun map cfg ->
  Cfg.iter_blocks cfg ~f:(fun label block ->
      match Label.Tbl.find_opt map label with
      | None -> ()
      | Some subst -> apply_block_in_place subst block)
