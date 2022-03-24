(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
let use_ocamlcfg = ref false            (* -ocamlcfg *)
let dump_cfg = ref false                (* -dcfg *)

let reorder_blocks_random = ref None    (* -reorder-blocks-random seed *)

let default_heap_reduction_threshold = 500_000_000 / (Sys.word_size / 8)
let heap_reduction_threshold = ref default_heap_reduction_threshold (* -heap-reduction-threshold *)
let use_cpp_mangling = ref false        (* -use-cpp-mangling *)

type function_result_types = Never | Functors_only | All_functions
type opt_level = Oclassic | O2 | O3
type 'a or_default = Set of 'a | Default

let dump_inlining_paths = ref false

let opt_level = ref Default

let flags_by_opt_level ~opt_level ~default ~oclassic ~o2 ~o3 =
  match opt_level with
  | Default -> default
  | Set Oclassic -> oclassic
  | Set O2 -> o2
  | Set O3 -> o3

module Flambda2 = struct
  module Default = struct
    let classic_mode = false
    let join_points = false
    let unbox_along_intra_function_control_flow = true
    let backend_cse_at_toplevel = false
    let cse_depth = 2
    let join_depth = 5
    let function_result_types = Never
    let unicode = true
  end

  type flags = {
    classic_mode : bool;
    join_points : bool;
    unbox_along_intra_function_control_flow : bool;
    backend_cse_at_toplevel : bool;
    cse_depth : int;
    join_depth : int;
    function_result_types : function_result_types;

    unicode : bool;
  }

  let default = {
    classic_mode = Default.classic_mode;
    join_points = Default.join_points;
    unbox_along_intra_function_control_flow = Default.unbox_along_intra_function_control_flow;
    backend_cse_at_toplevel = Default.backend_cse_at_toplevel;
    cse_depth = Default.cse_depth;
    join_depth = Default.join_depth;
    function_result_types = Default.function_result_types;
    unicode = Default.unicode;
  }

  let oclassic = {
    default with
    classic_mode = true;
    backend_cse_at_toplevel = false;
  }

  let o2 = {
    default with
    cse_depth = 2;
    join_points = true;
    unbox_along_intra_function_control_flow = true;
    backend_cse_at_toplevel = false;
  }

  let o3 = {
    o2 with
    function_result_types = Functors_only
  }

  let default_for_opt_level opt_level = flags_by_opt_level ~opt_level ~default ~oclassic ~o2 ~o3

  let classic_mode = ref Default
  let join_points = ref Default
  let unbox_along_intra_function_control_flow = ref Default
  let backend_cse_at_toplevel = ref Default
  let cse_depth = ref Default
  let join_depth = ref Default
  let unicode = ref Default
  let function_result_types = ref Default

  module Dump = struct
    let rawfexpr = ref false
    let fexpr = ref false
    let flexpect = ref false
    let slot_offsets = ref false
    let freshen = ref false
  end

  module Expert = struct
    module Default = struct
      let code_id_and_symbol_scoping_checks = false
      let fallback_inlining_heuristic = false
      let inline_effects_in_cmm = false
      let phantom_lets = false
      let max_block_size_for_projections = None
      let max_unboxing_depth = 3
      let can_inline_recursive_functions = false
    end

    type flags = {
      code_id_and_symbol_scoping_checks : bool;
      fallback_inlining_heuristic : bool;
      inline_effects_in_cmm : bool;
      phantom_lets : bool;
      max_block_size_for_projections : int option;
      max_unboxing_depth : int;
      can_inline_recursive_functions : bool;
    }

    let default = {
      code_id_and_symbol_scoping_checks = Default.code_id_and_symbol_scoping_checks;
      fallback_inlining_heuristic = Default.fallback_inlining_heuristic;
      inline_effects_in_cmm = Default.inline_effects_in_cmm;
      phantom_lets = Default.phantom_lets;
      max_block_size_for_projections = Default.max_block_size_for_projections;
      max_unboxing_depth = Default.max_unboxing_depth;
      can_inline_recursive_functions = Default.can_inline_recursive_functions;
    }

    let oclassic = {
      default with
      fallback_inlining_heuristic = true;
    }

    let o2 = {
      default with
      fallback_inlining_heuristic = false;
    }

    let o3 = default

    let default_for_opt_level opt_level =
      flags_by_opt_level ~opt_level ~default ~oclassic ~o2 ~o3

    let code_id_and_symbol_scoping_checks = ref Default
    let fallback_inlining_heuristic = ref Default
    let inline_effects_in_cmm = ref Default
    let phantom_lets = ref Default
    let max_block_size_for_projections = ref Default
    let max_unboxing_depth = ref Default
    let can_inline_recursive_functions = ref Default
  end

  module Debug = struct
    module Default = struct
      let permute_every_name = false
      let concrete_types_only_on_canonicals = false
      let keep_invalid_handlers = true
    end

    let permute_every_name = ref Default.permute_every_name
    let concrete_types_only_on_canonicals =
      ref Default.concrete_types_only_on_canonicals
    let keep_invalid_handlers = ref Default.keep_invalid_handlers
  end

  module I = Clflags.Int_arg_helper
  module F = Clflags.Float_arg_helper

  module Inlining = struct
    module Default = struct
      let cost_divisor = 8.

      let max_depth = 1
      let max_rec_depth = 0

      let call_cost = 5. /. cost_divisor
      let alloc_cost = 7. /. cost_divisor
      let prim_cost = 3. /. cost_divisor
      let branch_cost = 5. /. cost_divisor
      let indirect_call_cost = 4. /. cost_divisor
      let poly_compare_cost = 10. /. cost_divisor

      let small_function_size = 10
      let large_function_size = 10

      let threshold = 10.

      let speculative_inlining_only_if_arguments_useful = true
    end

    let max_depth = ref (I.default Default.max_depth)
    let max_rec_depth = ref (I.default Default.max_rec_depth)

    let call_cost = ref (F.default Default.call_cost)
    let alloc_cost = ref (F.default Default.alloc_cost)
    let prim_cost = ref (F.default Default.prim_cost)
    let branch_cost = ref (F.default Default.branch_cost)
    let indirect_call_cost = ref (F.default Default.indirect_call_cost)
    let poly_compare_cost = ref (F.default Default.poly_compare_cost)

    let small_function_size = ref (I.default Default.small_function_size)
    let large_function_size = ref (I.default Default.large_function_size)

    let threshold = ref (F.default Default.threshold)

    let speculative_inlining_only_if_arguments_useful =
      ref Default.speculative_inlining_only_if_arguments_useful

    let report_bin = ref false

    type inlining_arguments = {
      max_depth : int option;
      max_rec_depth : int option;
      call_cost : float option;
      alloc_cost : float option;
      prim_cost : float option;
      branch_cost : float option;
      indirect_call_cost : float option;
      poly_compare_cost : float option;
      small_function_size : int option;
      large_function_size : int option;
      threshold : float option;
    }

    let use_inlining_arguments_set ?round (arg : inlining_arguments) =
      let set_int = Clflags.set_int_arg round in
      let set_float = Clflags.set_float_arg round in
      set_int max_depth Default.max_depth arg.max_depth;
      set_int max_rec_depth Default.max_rec_depth arg.max_rec_depth;
      set_float call_cost Default.call_cost arg.call_cost;
      set_float alloc_cost Default.alloc_cost arg.alloc_cost;
      set_float prim_cost Default.prim_cost arg.prim_cost;
      set_float branch_cost Default.branch_cost arg.branch_cost;
      set_float indirect_call_cost
        Default.indirect_call_cost arg.indirect_call_cost;
      set_float poly_compare_cost
        Default.poly_compare_cost arg.poly_compare_cost;
      set_int small_function_size
        Default.small_function_size arg.small_function_size;
      set_int large_function_size
        Default.large_function_size arg.large_function_size;
      set_float threshold Default.threshold arg.threshold

    let oclassic_arguments = {
      max_depth = None;
      max_rec_depth = None;
      call_cost = None;
      alloc_cost = None;
      prim_cost = None;
      branch_cost = None;
      indirect_call_cost = None;
      poly_compare_cost = None;
      (* We set the small and large function sizes to the same value here to
         recover "classic mode" semantics (no speculative inlining). *)
      small_function_size = Some Default.small_function_size;
      large_function_size = Some Default.small_function_size;
      (* [threshold] matches the current compiler's default.  (The factor of
         8 in that default is accounted for by [cost_divisor], above.) *)
      threshold = Some 10.;
    }

    let o2_arguments = {
      max_depth = Some 3;
      max_rec_depth = Some 0;
      call_cost = Some (3.0 *. Default.call_cost);
      alloc_cost = Some (3.0 *. Default.alloc_cost);
      prim_cost = Some (3.0 *. Default.prim_cost);
      branch_cost = Some (3.0 *. Default.branch_cost);
      indirect_call_cost = Some (3.0 *. Default.indirect_call_cost);
      poly_compare_cost = Some (3.0 *. Default.poly_compare_cost);
      small_function_size = Some (10 * Default.small_function_size);
      large_function_size = Some (50 * Default.large_function_size);
      threshold = Some 100.;
    }

    let o3_arguments = { o2_arguments with max_depth = Some 6 }
  end
end

let set_oclassic () =
  if Clflags.is_flambda2 () then begin
    Flambda2.Inlining.use_inlining_arguments_set
      Flambda2.Inlining.oclassic_arguments;
    opt_level := Set Oclassic
  end else begin
    Clflags.Opt_flag_handler.default.set_oclassic ();
  end

let set_o2 () =
  if Clflags.is_flambda2 () then begin
    Flambda2.Inlining.use_inlining_arguments_set Flambda2.Inlining.o2_arguments;
    opt_level := Set O2
  end else begin
    Clflags.Opt_flag_handler.default.set_o2 ();
  end

let set_o3 () =
  if Clflags.is_flambda2 () then begin
    Flambda2.Inlining.use_inlining_arguments_set Flambda2.Inlining.o3_arguments;
    opt_level := Set O3
  end else begin
    Clflags.Opt_flag_handler.default.set_o3 ();
  end

let opt_flag_handler : Clflags.Opt_flag_handler.t =
  { set_oclassic; set_o2; set_o3 }
