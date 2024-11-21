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

[@@@ocaml.warning "+a-4-9-40-41-42"]

open Format
open! Reg

let loc ?(wrap_out = fun ppf f -> f ppf) ~unknown ppf loc typ =
  match loc with
  | Unknown -> unknown ppf
  | Reg r ->
    wrap_out ppf (fun ppf -> fprintf ppf "%s" (Proc.register_name typ r))
  | Stack (Local s) ->
    wrap_out ppf (fun ppf ->
        fprintf ppf "s[%s:%i]"
          (Proc.stack_class_tag (Proc.stack_slot_class typ))
          s)
  | Stack (Incoming s) -> wrap_out ppf (fun ppf -> fprintf ppf "par[%i]" s)
  | Stack (Outgoing s) -> wrap_out ppf (fun ppf -> fprintf ppf "arg[%i]" s)
  | Stack (Domainstate s) -> wrap_out ppf (fun ppf -> fprintf ppf "ds[%i]" s)

let reg ppf r =
  if not (anonymous r) then fprintf ppf "%s:" (name r);
  fprintf ppf "%s"
    (match (r.typ : Cmm.machtype_component) with
    | Val -> "V"
    | Addr -> "A"
    | Int -> "I"
    | Float -> "F"
    | Vec128 -> "X"
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
      | _ -> ())
    s

let regsetaddr ppf s = regsetaddr' ppf s
