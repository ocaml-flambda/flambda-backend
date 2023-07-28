(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2023 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module L = Linear

type result =
  { fundecl : Dwarf_concrete_instances.fundecl;
    available_ranges_vars : Available_ranges_vars.t
  }

let passes_for_fundecl (fundecl : L.fundecl) =
  let available_ranges_vars, fundecl =
    Profile.record "debug_available_ranges_vars"
      (fun fundecl -> Available_ranges_vars.create fundecl)
      ~accumulate:true fundecl
  in
  available_ranges_vars, fundecl

let passes_for_fundecl (fundecl : L.fundecl) ~fun_end_label =
  let available_ranges_vars, linear_fundecl =
    if !Clflags.debug && not !Dwarf_flags.restrict_to_upstream_dwarf
    then passes_for_fundecl fundecl
    else Available_ranges_vars.empty, fundecl
  in
  let fundecl : Dwarf_concrete_instances.fundecl =
    { fun_name = fundecl.fun_name; fun_dbg = fundecl.fun_dbg; fun_end_label }
  in
  { fundecl; available_ranges_vars }, linear_fundecl
