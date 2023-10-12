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

let flambda2_is_enabled () = Clflags.is_flambda2 ()

let debug_flambda2 () = !Flambda_backend_flags.Flambda2.debug

let with_default (r : 'a Flambda_backend_flags.or_default)
    ~(f : Flambda_backend_flags.Flambda2.flags -> 'a) =
  match r with
  | Set a -> a
  | Default ->
    f
      (Flambda_backend_flags.Flambda2.default_for_opt_level
         !Flambda_backend_flags.opt_level)

type 'a mode =
  | Normal : [`Normal] mode
  | Classic : [`Classic] mode

type any_mode = Mode : _ mode -> any_mode

let classic_mode () =
  !Flambda_backend_flags.Flambda2.classic_mode
  |> with_default ~f:(fun d -> d.classic_mode)

let mode () = if classic_mode () then Mode Classic else Mode Normal

let join_points () =
  !Flambda_backend_flags.Flambda2.join_points
  |> with_default ~f:(fun d -> d.join_points)

let unbox_along_intra_function_control_flow () =
  !Flambda_backend_flags.Flambda2.unbox_along_intra_function_control_flow
  |> with_default ~f:(fun d -> d.unbox_along_intra_function_control_flow)

let backend_cse_at_toplevel () =
  !Flambda_backend_flags.Flambda2.backend_cse_at_toplevel
  |> with_default ~f:(fun d -> d.backend_cse_at_toplevel)

let cse_depth () =
  !Flambda_backend_flags.Flambda2.cse_depth
  |> with_default ~f:(fun d -> d.cse_depth)

let join_depth () =
  !Flambda_backend_flags.Flambda2.join_depth
  |> with_default ~f:(fun d -> d.join_depth)

let safe_string () = Config.safe_string

let flat_float_array () = Config.flat_float_array

let function_result_types ~is_a_functor =
  let when_ =
    !Flambda_backend_flags.Flambda2.function_result_types
    |> with_default ~f:(fun d -> d.function_result_types)
  in
  match when_ with
  | Never -> false
  | Functors_only -> is_a_functor
  | All_functions -> true

let debug () = !Clflags.debug

let opaque () = !Clflags.opaque

let float_const_prop () = !Clflags.float_const_prop

let optimize_for_speed () = !Clflags.optimize_for_speed

let inlining_report () = !Clflags.inlining_report

let inlining_report_bin () = !Flambda_backend_flags.Flambda2.Inlining.report_bin

let colour () = !Clflags.color

let unicode () =
  !Flambda_backend_flags.Flambda2.unicode
  |> with_default ~f:(fun d -> d.unicode)

let check_invariants () = !Clflags.flambda_invariant_checks

type dump_target = Flambda_backend_flags.Flambda2.Dump.target =
  | Nowhere
  | Main_dump_stream
  | File of Misc.filepath

let dump_rawflambda () = !Clflags.dump_rawflambda

let dump_flambda () = !Clflags.dump_flambda

let dump_rawfexpr () = !Flambda_backend_flags.Flambda2.Dump.rawfexpr

let dump_fexpr () = !Flambda_backend_flags.Flambda2.Dump.fexpr

let dump_flexpect () = !Flambda_backend_flags.Flambda2.Dump.flexpect

let dump_slot_offsets () = !Flambda_backend_flags.Flambda2.Dump.slot_offsets

let dump_flow () = !Flambda_backend_flags.Flambda2.Dump.flow

let freshen_when_printing () = !Flambda_backend_flags.Flambda2.Dump.freshen

module Inlining = struct
  module I = Flambda_backend_flags.Flambda2.Inlining
  module IH = Clflags.Int_arg_helper
  module FH = Clflags.Float_arg_helper

  type round_or_default =
    | Round of int
    | Default of Flambda_backend_flags.opt_level

  let depth_scaling_factor = 10 (* See [Downwards_env.enter_inlined_apply] *)

  let default_for_opt_level (opt_level : Flambda_backend_flags.opt_level) =
    match opt_level with
    | Oclassic -> I.oclassic_arguments
    | O2 -> I.o2_arguments
    | O3 -> I.o3_arguments

  let max_depth round_or_default =
    let depth =
      match round_or_default with
      | Round round -> IH.get ~key:round !I.max_depth
      | Default opt_level -> (default_for_opt_level opt_level).max_depth
    in
    (* This computation (rather than just [depth * depth_scaling_factor]) gives
       a bit more leeway for always-inlined functions, which reduce the depth by
       much less than [depth_scaling_factor], to be inlined. *)
    ((depth + 1) * depth_scaling_factor) - 1

  let max_rec_depth round_or_default =
    match round_or_default with
    | Round round -> IH.get ~key:round !I.max_rec_depth
    | Default opt_level -> (default_for_opt_level opt_level).max_rec_depth

  let call_cost round_or_default =
    match round_or_default with
    | Round round -> FH.get ~key:round !I.call_cost
    | Default opt_level -> (default_for_opt_level opt_level).call_cost

  let alloc_cost round_or_default =
    match round_or_default with
    | Round round -> FH.get ~key:round !I.alloc_cost
    | Default opt_level -> (default_for_opt_level opt_level).alloc_cost

  let prim_cost round_or_default =
    match round_or_default with
    | Round round -> FH.get ~key:round !I.prim_cost
    | Default opt_level -> (default_for_opt_level opt_level).prim_cost

  let branch_cost round_or_default =
    match round_or_default with
    | Round round -> FH.get ~key:round !I.branch_cost
    | Default opt_level -> (default_for_opt_level opt_level).branch_cost

  let indirect_call_cost round_or_default =
    match round_or_default with
    | Round round -> FH.get ~key:round !I.indirect_call_cost
    | Default opt_level -> (default_for_opt_level opt_level).indirect_call_cost

  let poly_compare_cost round_or_default =
    match round_or_default with
    | Round round -> FH.get ~key:round !I.poly_compare_cost
    | Default opt_level -> (default_for_opt_level opt_level).poly_compare_cost

  let small_function_size round_or_default =
    match round_or_default with
    | Round round -> IH.get ~key:round !I.small_function_size
    | Default opt_level -> (default_for_opt_level opt_level).small_function_size

  let large_function_size round_or_default =
    match round_or_default with
    | Round round -> IH.get ~key:round !I.large_function_size
    | Default opt_level -> (default_for_opt_level opt_level).large_function_size

  let threshold round_or_default =
    match round_or_default with
    | Round round -> FH.get ~key:round !I.threshold
    | Default opt_level -> (default_for_opt_level opt_level).threshold

  let speculative_inlining_only_if_arguments_useful () =
    !Flambda_backend_flags.Flambda2.Inlining
     .speculative_inlining_only_if_arguments_useful
end

module Debug = struct
  let concrete_types_only_on_canonicals () =
    !Flambda_backend_flags.Flambda2.Debug.concrete_types_only_on_canonicals

  let keep_invalid_handlers () =
    !Flambda_backend_flags.Flambda2.Debug.keep_invalid_handlers
end

module Expert = struct
  let with_default (r : 'a Flambda_backend_flags.or_default)
      ~(f : Flambda_backend_flags.Flambda2.Expert.flags -> 'a) =
    match r with
    | Set a -> a
    | Default ->
      f
        (Flambda_backend_flags.Flambda2.Expert.default_for_opt_level
           !Flambda_backend_flags.opt_level)

  let fallback_inlining_heuristic () =
    !Flambda_backend_flags.Flambda2.Expert.fallback_inlining_heuristic
    |> with_default ~f:(fun d -> d.fallback_inlining_heuristic)

  let inline_effects_in_cmm () =
    !Flambda_backend_flags.Flambda2.Expert.inline_effects_in_cmm
    |> with_default ~f:(fun d -> d.inline_effects_in_cmm)

  (* CR mshinwell: Remove any uses of this flag, then remove the flag. *)
  let max_block_size_for_projections () =
    !Flambda_backend_flags.Flambda2.Expert.max_block_size_for_projections
    |> with_default ~f:(fun d -> d.max_block_size_for_projections)

  let phantom_lets () =
    !Flambda_backend_flags.Flambda2.Expert.phantom_lets
    |> with_default ~f:(fun d -> d.phantom_lets)

  let max_unboxing_depth () =
    !Flambda_backend_flags.Flambda2.Expert.max_unboxing_depth
    |> with_default ~f:(fun d -> d.max_unboxing_depth)

  let can_inline_recursive_functions () =
    !Flambda_backend_flags.Flambda2.Expert.can_inline_recursive_functions
    |> with_default ~f:(fun d -> d.can_inline_recursive_functions)

  let max_function_simplify_run () =
    !Flambda_backend_flags.Flambda2.Expert.max_function_simplify_run
    |> with_default ~f:(fun d -> d.max_function_simplify_run)

  let shorten_symbol_names () =
    !Flambda_backend_flags.Flambda2.Expert.shorten_symbol_names
    |> with_default ~f:(fun d -> d.shorten_symbol_names)
end

let stack_allocation_enabled () = Config.stack_allocation
