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

let classic_mode () = !Flambda_backend_flags.Flambda2.classic_mode

let join_points () = !Flambda_backend_flags.Flambda2.join_points

let unbox_along_intra_function_control_flow () =
  !Flambda_backend_flags.Flambda2.unbox_along_intra_function_control_flow

let backend_cse_at_toplevel () =
  !Flambda_backend_flags.Flambda2.backend_cse_at_toplevel

let cse_depth () = !Flambda_backend_flags.Flambda2.cse_depth

let safe_string () = Config.safe_string

let flat_float_array () = Config.flat_float_array

let debug () = !Clflags.debug

let opaque () = !Clflags.opaque

let float_const_prop () = !Clflags.float_const_prop

let treat_invalid_code_as_unreachable () =
  !Flambda_backend_flags.Flambda2.treat_invalid_code_as_unreachable

let optimize_for_speed () = !Clflags.optimize_for_speed

let inlining_report () = !Clflags.inlining_report

let inlining_report_bin () = !Flambda_backend_flags.Flambda2.Inlining.report_bin

let colour () = !Clflags.color

let unicode () = !Flambda_backend_flags.Flambda2.unicode

let check_invariants () = !Clflags.flambda_invariant_checks

let dump_rawflambda () = !Clflags.dump_rawflambda

let dump_flambda () = !Clflags.dump_flambda

let dump_rawfexpr () = !Flambda_backend_flags.Flambda2.Dump.rawfexpr

let dump_fexpr () = !Flambda_backend_flags.Flambda2.Dump.fexpr

let dump_flexpect () = !Flambda_backend_flags.Flambda2.Dump.flexpect

let dump_closure_offsets () =
  !Flambda_backend_flags.Flambda2.Dump.closure_offsets

let freshen_when_printing () = !Flambda_backend_flags.Flambda2.Dump.freshen

module Inlining = struct
  module D = Flambda_backend_flags.Flambda2.Inlining.Default
  module I = Flambda_backend_flags.Flambda2.Inlining
  module IH = Clflags.Int_arg_helper
  module FH = Clflags.Float_arg_helper

  type round_or_default =
    | Round of int
    | Default

  let max_depth round_or_default =
    match round_or_default with
    | Round round -> IH.get ~key:round !I.max_depth
    | Default -> D.max_depth

  let max_rec_depth round_or_default =
    match round_or_default with
    | Round round -> IH.get ~key:round !I.max_rec_depth
    | Default -> D.max_rec_depth

  let call_cost round_or_default =
    match round_or_default with
    | Round round -> FH.get ~key:round !I.call_cost
    | Default -> D.call_cost

  let alloc_cost round_or_default =
    match round_or_default with
    | Round round -> FH.get ~key:round !I.alloc_cost
    | Default -> D.alloc_cost

  let prim_cost round_or_default =
    match round_or_default with
    | Round round -> FH.get ~key:round !I.prim_cost
    | Default -> D.prim_cost

  let branch_cost round_or_default =
    match round_or_default with
    | Round round -> FH.get ~key:round !I.branch_cost
    | Default -> D.branch_cost

  let indirect_call_cost round_or_default =
    match round_or_default with
    | Round round -> FH.get ~key:round !I.indirect_call_cost
    | Default -> D.indirect_call_cost

  let poly_compare_cost round_or_default =
    match round_or_default with
    | Round round -> FH.get ~key:round !I.poly_compare_cost
    | Default -> D.poly_compare_cost

  let small_function_size round_or_default =
    match round_or_default with
    | Round round -> IH.get ~key:round !I.small_function_size
    | Default -> D.small_function_size

  let large_function_size round_or_default =
    match round_or_default with
    | Round round -> IH.get ~key:round !I.large_function_size
    | Default -> D.large_function_size

  let threshold round_or_default =
    match round_or_default with
    | Round round -> FH.get ~key:round !I.threshold
    | Default -> D.threshold

  let speculative_inlining_only_if_arguments_useful () =
    !Flambda_backend_flags.Flambda2.Inlining
     .speculative_inlining_only_if_arguments_useful
end

module Debug = struct
  let permute_every_name () =
    !Flambda_backend_flags.Flambda2.Debug.permute_every_name

  let concrete_types_only_on_canonicals () =
    !Flambda_backend_flags.Flambda2.Debug.concrete_types_only_on_canonicals
end

module Expert = struct
  let code_id_and_symbol_scoping_checks () =
    !Flambda_backend_flags.Flambda2.Expert.code_id_and_symbol_scoping_checks

  let fallback_inlining_heuristic () =
    !Flambda_backend_flags.Flambda2.Expert.fallback_inlining_heuristic

  let inline_effects_in_cmm () =
    !Flambda_backend_flags.Flambda2.Expert.inline_effects_in_cmm

  let max_block_size_for_projections () =
    !Flambda_backend_flags.Flambda2.Expert.max_block_size_for_projections

  let phantom_lets () =
    !Flambda_backend_flags.Flambda2.Expert.phantom_lets

  let max_unboxing_depth () =
    !Flambda_backend_flags.Flambda2.Expert.max_unboxing_depth

  let can_inline_recursive_functions () =
    !Flambda_backend_flags.Flambda2.Expert.can_inline_recursive_functions
end
