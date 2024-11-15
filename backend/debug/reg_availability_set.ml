(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module RD = Reg_with_debug_info
module V = Backend_var

type t =
  | Ok of RD.Set.t
  | Unreachable

let of_list rds = Ok (RD.Set.of_list rds)

(* CR mshinwell: The implementations below probably aren't really adequate, but
   will suffice for now. We should aim to improve them soon *)

let union t1 t2 =
  (* Since [RD.Set]'s comparison function just looks at the [Reg.t] values, this
     will arbitrarily pick between debug info values in the case where [t1] and
     [t2] both contain the same [Reg.t]. That seems ok, at least for now. *)
  match t1, t2 with
  | Ok avail1, Ok avail2 -> Ok (RD.Set.union avail1 avail2)
  | Unreachable, _ | _, Unreachable -> Unreachable

(* This is intersection on the [Reg.t] values with the additional semantics that
   conflicting debug info values are erased (see comment below). *)
let inter t1 t2 =
  match t1, t2 with
  | Unreachable, _ -> t2
  | _, Unreachable -> t1
  | Ok avail1, Ok avail2 ->
    let result =
      RD.Set.fold
        (fun reg1 result ->
          match RD.Set.find_reg_exn avail2 (RD.reg reg1) with
          | exception Not_found -> result
          | reg2 ->
            let debug_info1 = RD.debug_info reg1 in
            let debug_info2 = RD.debug_info reg2 in
            let debug_info =
              match debug_info1, debug_info2 with
              | None, None -> None
              (* Example for this next case: the value of a mutable variable x
                 is copied into another variable y; then there is a conditional
                 where on one branch x is assigned and on the other branch it is
                 not. This means that on the former branch we have forgotten
                 about y holding the value of x; but we have not on the latter.
                 At the join point we must have forgotten the information. *)
              | None, Some _ | Some _, None -> None
              | Some debug_info1, Some debug_info2 ->
                if RD.Debug_info.compare debug_info1 debug_info2 = 0
                then Some debug_info1
                else None
            in
            let reg =
              RD.create_with_debug_info ~reg:(RD.reg reg1) ~debug_info
            in
            RD.Set.add reg result)
        avail1 RD.Set.empty
    in
    Ok result

(* This ignores the debug info values completely. *)
let diff t1 t2 =
  match t1, t2 with
  | Unreachable, (Ok _ | Unreachable) -> Unreachable
  | Ok avail1, Ok avail2 -> Ok (RD.Set.diff avail1 avail2)
  | Ok _, Unreachable -> Ok RD.Set.empty

let fold f t init =
  match t with
  | Unreachable -> init
  | Ok availability -> RD.Set.fold f availability init

let canonicalise availability =
  match availability with
  | Unreachable -> Unreachable
  | Ok availability ->
    let regs_by_ident = V.Tbl.create 42 in
    RD.Set.iter
      (fun reg ->
        match RD.debug_info reg with
        | None -> ()
        | Some debug_info -> (
          let name = RD.Debug_info.holds_value_of debug_info in
          if not (V.is_global_or_predef name)
          then
            match V.Tbl.find regs_by_ident name with
            | exception Not_found -> V.Tbl.add regs_by_ident name reg
            | (reg' : RD.t) -> (
              (* We prefer registers that are assigned to the stack since they
                 probably give longer available ranges (less likely to be
                 clobbered). *)
              match RD.location reg, RD.location reg' with
              | Reg _, Stack _
              | Reg _, Reg _
              | Stack _, Stack _
              | _, Unknown
              | Unknown, _ ->
                ()
              | Stack _, Reg _ ->
                V.Tbl.remove regs_by_ident name;
                V.Tbl.add regs_by_ident name reg)))
      availability;
    let result =
      V.Tbl.fold
        (fun _ident reg availability -> RD.Set.add reg availability)
        regs_by_ident RD.Set.empty
    in
    Ok result

(* This ignores the debug info values. *)
let equal t1 t2 =
  match t1, t2 with
  | Unreachable, Unreachable -> true
  | Unreachable, Ok _ | Ok _, Unreachable -> false
  | Ok regs1, Ok regs2 -> RD.Set.equal regs1 regs2

let subset t1 t2 =
  match t1, t2 with
  | Unreachable, Unreachable -> true
  | Unreachable, Ok _ -> false
  | Ok _, Unreachable -> false
  | Ok regs1, Ok regs2 -> RD.Set.subset regs1 regs2

let print ~print_reg ppf = function
  | Unreachable -> Format.fprintf ppf "<unreachable>"
  | Ok availability ->
    Format.fprintf ppf "{%a}"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
         (Reg_with_debug_info.print ~print_reg))
      (RD.Set.elements availability)
