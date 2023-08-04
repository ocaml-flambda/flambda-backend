(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*            Mark Shinwell and Thomas Refis, Jane Street Europe          *)
(*                                                                        *)
(*   Copyright 2013--2023 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Asm_targets

(* CR mshinwell: remove this for the new scheme *)
let base_type_die_name_for_var compilation_unit var
    (is_parameter : Is_parameter.t) =
  let var_name = Backend_var.name var in
  assert (
    try
      ignore (String.index var_name ' ');
      false
    with Not_found -> true);
  let stamp = Backend_var.stamp var in
  let is_parameter =
    match is_parameter with
    | Local -> ""
    | Parameter { index } -> Printf.sprintf " %d" index
  in
  Printf.sprintf "__ocaml %s %s %d%s"
    (Compilation_unit.full_path_as_string compilation_unit)
    var_name stamp is_parameter
