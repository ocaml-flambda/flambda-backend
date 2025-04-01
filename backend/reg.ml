(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Cmm

type irc_work_list =
  | Unknown_list
  | Precolored
  | Initial
  | Simplify
  | Freeze
  | Spill
  | Spilled
  | Coalesced
  | Colored
  | Select_stack

let equal_irc_work_list left right =
  match left, right with
  | Unknown_list, Unknown_list
  | Precolored, Precolored
  | Initial, Initial
  | Simplify, Simplify
  | Freeze, Freeze
  | Spill, Spill
  | Spilled, Spilled
  | Coalesced, Coalesced
  | Colored, Colored
  | Select_stack, Select_stack -> true
  | (Unknown_list
  | Precolored
  | Initial
  | Simplify
  | Freeze
  | Spill
  | Spilled
  | Coalesced
  | Colored
  | Select_stack), _ -> false

let string_of_irc_work_list = function
  | Unknown_list -> "unknown_list"
  | Precolored -> "precolored"
  | Initial -> "initial"
  | Simplify -> "simplify"
  | Freeze -> "freeze"
  | Spill -> "spill"
  | Spilled -> "spilled"
  | Coalesced -> "coalesced"
  | Colored -> "colored"
  | Select_stack -> "select_stack"

module V = Backend_var

module Name = struct
  type t =
    | Anon
    | Var of V.t

  let to_string = function
    | Anon -> "anon"
    | Var var -> V.name var

  let with_prefix ~prefix = function
    | Anon -> Anon
    | Var var -> Var (V.create_local (prefix ^ "-" ^ V.name var))
end

type reg =
  { name: Name.t;                         (* Name *)
    stamp: int;                           (* Unique stamp *)
    preassigned: bool;                    (* Pinned to a specific location *)
    mutable loc: location;                (* Current location *)
    mutable irc_work_list: irc_work_list; (* Current work list (IRC only) *)
    mutable irc_color : int option;       (* Current color (IRC only) *)
    mutable irc_alias : t option;         (* Current alias (IRC only) *)
    mutable spill: bool;                  (* "true" to force stack allocation  *)
    mutable interf: t list;               (* Other regs live simultaneously *)
    mutable degree: int;                  (* Number of other regs live sim. *)
    mutable spill_cost: int; }            (* Estimate of spilling cost *)

and t =
  { typ: Cmm.machtype_component;          (* Type of contents *)
    reg : reg; }

and location =
    Unknown
  | Reg of int
  | Stack of stack_location

and stack_location =
    Local of int
  | Incoming of int
  | Outgoing of int
  | Domainstate of int

let dummy_reg =
  { name = Name.Anon; stamp = 0; preassigned = false; loc = Unknown;
    irc_work_list = Unknown_list; irc_color = None; irc_alias = None;
    spill = false; interf = []; degree = 0; spill_cost = 0;
  }

let dummy = { typ = Int; reg = dummy_reg }

let currstamp = ref 0
let all_relocatable_regs = ref ([] : t list)

let create_gen ~name ~typ ~loc =
  let preassigned =
    match loc with
    | Reg _ | Stack _ -> true
    | Unknown -> false
  in
  let reg = { name; stamp = !currstamp; preassigned; loc;
            irc_work_list = Unknown_list; irc_color = None; irc_alias = None;
            spill = false; interf = []; degree = 0;
            spill_cost = 0; } in
  let t = { typ; reg } in
  if not preassigned then all_relocatable_regs := t :: !all_relocatable_regs;
  incr currstamp;
  t

let create typ = create_gen ~name:Name.Anon ~typ ~loc:Unknown

let create_with_typ r = create_gen ~name:Name.Anon ~typ:r.typ ~loc:Unknown

let create_with_typ_and_name ?prefix_if_var r =
  let name =
    match prefix_if_var with
    | Some prefix -> Name.with_prefix r.reg.name ~prefix
    | None -> r.reg.name
  in
  create_gen ~name ~typ:r.typ ~loc:Unknown

let create_at_location typ loc = create_gen ~name:Name.Anon ~typ ~loc

let createv_gen ~name ~typs =
  let n = Array.length typs in
  let rv = Array.make n dummy in
  for i = 0 to n-1 do rv.(i) <- create_gen ~name ~typ:typs.(i) ~loc:Unknown done;
  rv

let createv typs = createv_gen ~name:Name.Anon ~typs

let createv_with_id ~id typs = createv_gen ~name:(Name.Var id) ~typs

let createv_with_typs rs = createv_gen ~name:Name.Anon ~typs:(Array.map (fun r -> r.typ) rs)

let createv_with_typs_and_id ~id rs = createv_gen ~name:(Name.Var id) ~typs:(Array.map (fun r -> r.typ) rs)

let typv rv =
  Array.map (fun r -> r.typ) rv

let print t =
  let prefix =
    if t.reg.preassigned then "pin:"
    else if t.reg.spill then "spill:"
    else ""
  in
  prefix ^ Name.to_string t.reg.name

let is_preassigned t = t.reg.preassigned

let is_unknown t =
  match t.reg.loc with
  | Unknown -> true
  | Reg _ | Stack (Local _ | Incoming _ | Outgoing _ | Domainstate _) -> false

let first_virtual_reg_stamp = ref (-1)

let is_stack t =
  match t.reg.loc with
  | Stack _ -> true
  | Reg _ | Unknown -> false

let is_reg t =
  match t.reg.loc with
  | Reg _ -> true
  | Stack _ | Unknown -> false

let restart () =
  (* When restart is called for the first time, the current
     stamp reflects all hard pseudo-registers that have been allocated by Proc,
     so remember it and use it as the base stamp for allocating soft pseudo-registers *)
  if !first_virtual_reg_stamp = -1 then begin
    first_virtual_reg_stamp := !currstamp;
    assert (!all_relocatable_regs = []) (* Only preassigned regs created before now *)
  end;
  currstamp := !first_virtual_reg_stamp;
  all_relocatable_regs := []

let reinit_reg r =
  r.reg.loc <- Unknown;
  r.reg.irc_work_list <- Unknown_list;
  r.reg.irc_color <- None;
  r.reg.irc_alias <- None;
  r.reg.interf <- [];
  r.reg.degree <- 0;
  (* Preserve the very high spill costs introduced by the reloading pass *)
  if r.reg.spill_cost >= 100000
  then r.reg.spill_cost <- 100000
  else r.reg.spill_cost <- 0

let reinit_relocatable_regs () = List.iter reinit_reg !all_relocatable_regs
let all_relocatable_regs () = !all_relocatable_regs
let num_registers () = !currstamp

let compare r1 r2 =
  let s = r1.reg.stamp - r2.reg.stamp in
  if s <> 0 then s else Stdlib.compare r1.typ r2.typ

let same r1 r2 = compare r1 r2 = 0

module RegOrder = struct
  type nonrec t = t
  let compare = compare
end

module Set = Set.Make(RegOrder)
module Map = Map.Make(RegOrder)
module Tbl = Hashtbl.Make (struct
    type nonrec t = t
    let equal = same
    let hash r = r.reg.stamp
  end)

let add_set_array s v =
  match Array.length v with
    0 -> s
  | 1 -> Set.add v.(0) s
  | n -> let rec add_all i =
           if i >= n then s else Set.add v.(i) (add_all(i+1))
         in add_all 0

let diff_set_array s v =
  match Array.length v with
    0 -> s
  | 1 -> Set.remove v.(0) s
  | n -> let rec remove_all i =
           if i >= n then s else Set.remove v.(i) (remove_all(i+1))
         in remove_all 0

let inter_set_array s v =
  match Array.length v with
    0 -> Set.empty
  | 1 -> if Set.mem v.(0) s
         then Set.add v.(0) Set.empty
         else Set.empty
  | n -> let rec inter_all i =
           if i >= n then Set.empty
           else if Set.mem v.(i) s then Set.add v.(i) (inter_all(i+1))
           else inter_all(i+1)
         in inter_all 0

let disjoint_set_array s v =
  match Array.length v with
    0 -> true
  | 1 -> not (Set.mem v.(0) s)
  | n -> let rec disjoint_all i =
           if i >= n then true
           else if Set.mem v.(i) s then false
           else disjoint_all (i+1)
         in disjoint_all 0

let set_of_array v =
  match Array.length v with
    0 -> Set.empty
  | 1 -> Set.add v.(0) Set.empty
  | n -> let rec add_all i =
           if i >= n then Set.empty else Set.add v.(i) (add_all(i+1))
         in add_all 0

let set_has_collisions s =
  let phys_regs = Hashtbl.create (Int.min (Set.cardinal s) 32) in
  Set.fold (fun r acc ->
    match r.reg.loc with
    | Reg id ->
      if Hashtbl.mem phys_regs id then true
      else (Hashtbl.add phys_regs id (); acc)
    | Unknown | Stack _ -> acc) s false

let equal_stack_location left right =
  match left, right with
  | Local left, Local right -> Int.equal left right
  | Incoming left, Incoming right -> Int.equal left right
  | Outgoing left, Outgoing right -> Int.equal left right
  | Domainstate left, Domainstate right -> Int.equal left right
  | Local _, (Incoming _ | Outgoing _ | Domainstate _)
  | Incoming _, (Local _ | Outgoing _ | Domainstate _)
  | Outgoing _, (Local _ | Incoming _ | Domainstate _)
  | Domainstate _, (Local _ | Incoming _ | Outgoing _)->
    false

let equal_location left right =
  match left, right with
  | Unknown, Unknown -> true
  | Reg left, Reg right -> Int.equal left right
  | Stack left, Stack right -> equal_stack_location left right
  | Unknown, (Reg _ | Stack _)
  | Reg _, (Unknown | Stack _)
  | Stack _, (Unknown | Reg _) ->
    false

let same_loc left right =
  (* CR-soon azewierzejew: This should also compare [reg_class] for [Stack
     (Local _)]. That's complicated because [reg_class] is definied in [Proc]
     which relies on [Reg]. *)
  equal_location left.reg.loc right.reg.loc

