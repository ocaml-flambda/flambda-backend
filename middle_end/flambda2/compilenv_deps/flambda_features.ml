(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2020 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

let flambda2_is_enabled () = Clflags.is_flambda2 ()

(* CR mshinwell: wire this in *)

let join_points () = !Clflags.Flambda2.join_points
let unbox_along_intra_function_control_flow () =
  !Clflags.Flambda2.unbox_along_intra_function_control_flow
let backend_cse_at_toplevel () =
  !Clflags.Flambda2.backend_cse_at_toplevel
let cse_depth () = !Clflags.Flambda2.cse_depth

let safe_string () = Config.safe_string
let flat_float_array () = Config.flat_float_array

let debug () = !Clflags.debug
let opaque () = !Clflags.opaque
let float_const_prop () = !Clflags.float_const_prop
let treat_invalid_code_as_unreachable () =
  false (* !Clflags.treat_invalid_code_as_unreachable *)

let optimize_for_speed () = !Clflags.optimize_for_speed

let inlining_report () = !Clflags.inlining_report
let inlining_report_bin () = false (* !Clflags.inlining_report_bin *)

let colour () = !Clflags.color
let unicode () = true (* !Clflags.flambda_unicode *)

let check_invariants () = !Clflags.flambda_invariant_checks
let context_on_error () = true (* !Clflags.flambda_context_on_error *)

let dump_rawflambda () = !Clflags.dump_rawflambda
let dump_flambda () = !Clflags.dump_flambda
let dump_rawfexpr () = false (* !Clflags.dump_rawfexpr *)
let dump_fexpr () = false (* !Clflags.dump_fexpr *)
let dump_flexpect () = false (* !Clflags.dump_flexpect *)
let dump_let_cont () = false (* !Clflags.dump_let_cont *)
let dump_offset () = false (* !Clflags.dump_offset *)

module Inlining = struct
  module I = Clflags.Int_arg_helper
  module F = Clflags.Float_arg_helper

  let max_depth ~round =
    I.get ~key:round !Clflags.Flambda2.Inlining.max_depth

  let call_cost ~round =
    F.get ~key:round !Clflags.Flambda2.Inlining.call_cost

  let alloc_cost ~round =
    F.get ~key:round !Clflags.Flambda2.Inlining.alloc_cost

  let prim_cost ~round =
    F.get ~key:round !Clflags.Flambda2.Inlining.prim_cost

  let branch_cost ~round =
    F.get ~key:round !Clflags.Flambda2.Inlining.branch_cost

  let indirect_call_cost ~round =
    F.get ~key:round !Clflags.Flambda2.Inlining.indirect_call_cost

  let poly_compare_cost ~round =
    F.get ~key:round !Clflags.Flambda2.Inlining.poly_compare_cost

  let small_function_size ~round =
    I.get ~key:round !Clflags.Flambda2.Inlining.small_function_size

  let large_function_size ~round =
    I.get ~key:round !Clflags.Flambda2.Inlining.large_function_size

  let threshold ~round =
    F.get ~key:round !Clflags.Flambda2.Inlining.threshold
end

module Debug = struct
  let permute_every_name () = !Clflags.Flambda2.Debug.permute_every_name

  let concrete_types_only_on_canonicals () =
    !Clflags.Flambda2.Debug.concrete_types_only_on_canonicals
end

module Expert = struct
  let code_id_and_symbol_scoping_checks () =
    !Clflags.Flambda2.Expert.code_id_and_symbol_scoping_checks
  let fallback_inlining_heuristic () =
    !Clflags.Flambda2.Expert.fallback_inlining_heuristic
  let inline_effects_in_cmm () =
    !Clflags.Flambda2.Expert.inline_effects_in_cmm
  let max_block_size_for_projections () =
    !Clflags.Flambda2.Expert.max_block_size_for_projections
  let phantom_lets () =
    !Clflags.Flambda2.Expert.phantom_lets
  let max_unboxing_depth () =
    !Clflags.Flambda2.Expert.max_unboxing_depth
end
