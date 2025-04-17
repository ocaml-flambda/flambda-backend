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

open! Int_replace_polymorphic_compare
open Cmm


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

type t =
  { name: Name.t;
    stamp: int;
    typ: Cmm.machtype_component;
    preassigned: bool;
    mutable loc: location; }

and location =
    Unknown
  | Reg of int
  | Stack of stack_location

and stack_location =
    Local of int
  | Incoming of int
  | Outgoing of int
  | Domainstate of int

type reg = t

let dummy =
  { name = Name.Anon; stamp = 0; typ = Int; preassigned = false; loc = Unknown; }

let currstamp = ref 0
let all_relocatable_regs = ref ([] : t list)

let create_gen ~name ~typ ~loc =
  let preassigned =
    match loc with
    | Reg _ | Stack _ -> true
    | Unknown -> false
  in
  let r = { name; stamp = !currstamp; typ; preassigned; loc } in
  if not preassigned then all_relocatable_regs := r :: !all_relocatable_regs;
  incr currstamp;
  r

let create typ = create_gen ~name:Name.Anon ~typ ~loc:Unknown

let create_with_typ r = create_gen ~name:Name.Anon ~typ:r.typ ~loc:Unknown

let create_with_typ_and_name ?prefix_if_var r =
  let name =
    match prefix_if_var with
    | Some prefix -> Name.with_prefix r.name ~prefix
    | None -> r.name
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

let is_preassigned t = t.preassigned

let is_unknown t =
  match t.loc with
  | Unknown -> true
  | Reg _ | Stack (Local _ | Incoming _ | Outgoing _ | Domainstate _) -> false

let first_virtual_reg_stamp = ref (-1)

let is_stack t =
  match t.loc with
  | Stack _ -> true
  | Reg _ | Unknown -> false

let is_reg t =
  match t.loc with
  | Reg _ -> true
  | Stack _ | Unknown -> false

let clear_relocatable_regs () =
  (* When restart is called for the first time, the current
     stamp reflects all hard pseudo-registers that have been allocated by Proc,
     so remember it and use it as the base stamp for allocating soft pseudo-registers *)
  if !first_virtual_reg_stamp = -1 then begin
    first_virtual_reg_stamp := !currstamp;
    (* Only hard regs created before now *)
    assert (Misc.Stdlib.List.is_empty !all_relocatable_regs)
  end;
  currstamp := !first_virtual_reg_stamp;
  all_relocatable_regs := []

let reinit_reg r =
  r.loc <- Unknown

let reinit_relocatable_regs () = List.iter reinit_reg !all_relocatable_regs
let all_relocatable_regs () = !all_relocatable_regs

let compare r1 r2 =
  let c = Int.compare r1.stamp r2.stamp in
  if c <> 0 then c
  else Cmm.compare_machtype_component r1.typ r2.typ

let same r1 r2 =
  r1.stamp = r2.stamp && Cmm.equal_machtype_component r1.typ r2.typ

module RegOrder = struct
  type t = reg
  let equal = same
  let compare = compare
  let hash r = r.stamp
end

module Set = Set.Make (RegOrder)
module Map = Map.Make (RegOrder)
module Tbl = Hashtbl.Make (RegOrder)

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
    match r.loc with
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
  equal_location left.loc right.loc
