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

(* Pretty-printing of registers *)

[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]
open Format
open! Reg

let loc ?(wrap_out = fun ppf f -> f ppf) ~unknown ppf loc typ =
  match loc with
  | Unknown -> unknown ppf
  | Reg r ->
    wrap_out ppf (fun ppf -> fprintf ppf "%s" (Reg_class.register_name typ r))
  | Stack (Local s) ->
    wrap_out ppf (fun ppf ->
        fprintf ppf "s[%s:%i]" (Stack_class.tag (Stack_class.of_machtype typ)) s)
  | Stack (Incoming s) -> wrap_out ppf (fun ppf -> fprintf ppf "par[%i]" s)
  | Stack (Outgoing s) -> wrap_out ppf (fun ppf -> fprintf ppf "arg[%i]" s)
  | Stack (Domainstate s) -> wrap_out ppf (fun ppf -> fprintf ppf "ds[%i]" s)

let reg ppf r =
  fprintf ppf "%s%s:%s"
    (if r.preassigned then "pin:" else "")
    (Reg.Name.to_string r.name)
    (match (r.typ : Cmm.machtype_component) with
    | Val -> "V"
    | Addr -> "A"
    | Int -> "I"
    | Float -> "F"
    | Vec128 -> "X"
    | Vec256 -> "Y"
    | Vec512 -> "Z"
    | Valx2 -> "VV"
    | Float32 -> "S");
  fprintf ppf "/%i" r.stamp;
  loc
    ~wrap_out:(fun ppf f -> fprintf ppf "[%t]" f)
    ~unknown:(fun _ -> ())
    ppf r.loc r.typ

let regs' ?(print_reg = reg) ppf v =
  let reg = print_reg in
  match Array.length v with
  | 0 -> ()
  | 1 -> reg ppf v.(0)
  | n ->
    reg ppf v.(0);
    for i = 1 to n - 1 do
      fprintf ppf " %a" reg v.(i)
    done

let regs ppf v = regs' ppf v

let reglist ppf l = Format.pp_print_list ~pp_sep:pp_print_space reg ppf l

let regset ppf s =
  let first = ref true in
  Set.iter
    (fun r ->
      if !first
      then (
        first := false;
        fprintf ppf "%a" reg r)
      else fprintf ppf "@ %a" reg r)
    s

let regsetaddr' ?(print_reg = reg) ppf s =
  let reg = print_reg in
  let first = ref true in
  Set.iter
    (fun r ->
      if !first
      then (
        first := false;
        fprintf ppf "%a" reg r)
      else fprintf ppf "@ %a" reg r;
      match r.typ with
      | Val -> fprintf ppf "*"
      | Addr -> fprintf ppf "!"
      | Int | Float | Vec128 | Vec256 | Vec512 | Float32 | Valx2 -> ())
    s

let regsetaddr ppf s = regsetaddr' ppf s
