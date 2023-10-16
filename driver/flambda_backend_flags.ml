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
let cfg_invariants = ref false          (* -dcfg-invariants *)
let cfg_equivalence_check = ref false   (* -dcfg-equivalence-check *)
let regalloc = ref ""                   (* -regalloc *)
let regalloc_params = ref ([] : string list)  (* -regalloc-param *)
let regalloc_validate = ref true        (* -[no-]regalloc-validate *)

let cfg_peephole_optimize = ref true    (* -[no-]cfg-peephole-optimize *)

let reorder_blocks_random = ref None    (* -reorder-blocks-random seed *)
let basic_block_sections = ref false    (* -basic-block-sections *)

let dasm_comments = ref false (* -dasm-comments *)

let default_heap_reduction_threshold = 500_000_000 / (Sys.word_size / 8)
let heap_reduction_threshold = ref default_heap_reduction_threshold (* -heap-reduction-threshold *)
let dump_checkmach = ref false          (* -dcheckmach *)

type checkmach_details_cutoff =
  | Keep_all
  | At_most of int
  | No_details

let default_checkmach_details_cutoff = At_most 20
let checkmach_details_cutoff = ref default_checkmach_details_cutoff
                                       (* -checkmach-details-cutoff n *)

let disable_poll_insertion = ref (not Config.poll_insertion)
                                        (* -disable-poll-insertion *)
let allow_long_frames = ref true        (* -no-long-frames *)
(* Keep the value of [max_long_frames_threshold] in sync with LONG_FRAME_MARKER
   in ocaml/runtime/roots_nat.c *)
let max_long_frames_threshold = 0x7FFF
let long_frames_threshold = ref max_long_frames_threshold (* -debug-long-frames-threshold n *)

let caml_apply_inline_fast_path = ref false  (* -caml-apply-inline-fast-path *)

type function_result_types = Never | Functors_only | All_functions
type opt_level = Oclassic | O2 | O3
type 'a or_default = Set of 'a | Default

let dump_inlining_paths = ref false
let davail = ref false
let dranges = ref false

let opt_level = ref Default

let internal_assembler = ref false

let gc_timings = ref false

let flags_by_opt_level ~opt_level ~default ~oclassic ~o2 ~o3 =
  match opt_level with
  | Default -> default
  | Set Oclassic -> oclassic
  | Set O2 -> o2
  | Set O3 -> o3

module Flambda2 = struct
  let debug = ref false (* -flambda2-debug *)

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
    type target = Nowhere | Main_dump_stream | File of Misc.filepath

    let rawfexpr = ref Nowhere
    let fexpr = ref Nowhere
    let flexpect = ref Nowhere
    let slot_offsets = ref false
    let freshen = ref false
    let flow = ref false
  end

  module Expert = struct
    module Default = struct
      let fallback_inlining_heuristic = false
      let inline_effects_in_cmm = false
      let phantom_lets = false
      let max_block_size_for_projections = None
      let max_unboxing_depth = 3
      let can_inline_recursive_functions = false
      let max_function_simplify_run = 2
      let shorten_symbol_names = false
    end

    type flags = {
      fallback_inlining_heuristic : bool;
      inline_effects_in_cmm : bool;
      phantom_lets : bool;
      max_block_size_for_projections : int option;
      max_unboxing_depth : int;
      can_inline_recursive_functions : bool;
      max_function_simplify_run : int;
      shorten_symbol_names : bool
    }

    let default = {
      fallback_inlining_heuristic = Default.fallback_inlining_heuristic;
      inline_effects_in_cmm = Default.inline_effects_in_cmm;
      phantom_lets = Default.phantom_lets;
      max_block_size_for_projections = Default.max_block_size_for_projections;
      max_unboxing_depth = Default.max_unboxing_depth;
      can_inline_recursive_functions = Default.can_inline_recursive_functions;
      max_function_simplify_run = Default.max_function_simplify_run;
      shorten_symbol_names = Default.shorten_symbol_names;
    }

    let oclassic = {
      default with
      fallback_inlining_heuristic = true;
      shorten_symbol_names = true;
    }

    let o2 = {
      default with
      fallback_inlining_heuristic = false;
    }

    let o3 = default

    let default_for_opt_level opt_level =
      flags_by_opt_level ~opt_level ~default ~oclassic ~o2 ~o3

    let fallback_inlining_heuristic = ref Default
    let inline_effects_in_cmm = ref Default
    let phantom_lets = ref Default
    let max_block_size_for_projections = ref Default
    let max_unboxing_depth = ref Default
    let can_inline_recursive_functions = ref Default
    let max_function_simplify_run = ref Default
    let shorten_symbol_names = ref Default
  end

  module Debug = struct
    module Default = struct
      let concrete_types_only_on_canonicals = false
      let keep_invalid_handlers = true
    end

    let concrete_types_only_on_canonicals =
      ref Default.concrete_types_only_on_canonicals
    let keep_invalid_handlers = ref Default.keep_invalid_handlers
  end

  module I = Clflags.Int_arg_helper
  module F = Clflags.Float_arg_helper

  module Inlining = struct
    type inlining_arguments = {
      max_depth : int;
      max_rec_depth : int;
      call_cost : float;
      alloc_cost : float;
      prim_cost : float;
      branch_cost : float;
      indirect_call_cost : float;
      poly_compare_cost : float;
      small_function_size : int;
      large_function_size : int;
      threshold : float;
    }

    module Default = struct
      let cost_divisor = 8.

      let default_arguments = {
        max_depth = 1;
        max_rec_depth = 0;
        call_cost = 5. /. cost_divisor;
        alloc_cost = 7. /. cost_divisor;
        prim_cost = 3. /. cost_divisor;
        branch_cost = 5. /. cost_divisor;
        indirect_call_cost = 4. /. cost_divisor;
        poly_compare_cost = 10. /. cost_divisor;
        small_function_size = 10;
        large_function_size = 10;
        threshold = 10.;
      }

      let speculative_inlining_only_if_arguments_useful = true
    end

    let max_depth = ref (I.default Default.default_arguments.max_depth)
    let max_rec_depth = ref (I.default Default.default_arguments.max_rec_depth)

    let call_cost = ref (F.default Default.default_arguments.call_cost)
    let alloc_cost = ref (F.default Default.default_arguments.alloc_cost)
    let prim_cost = ref (F.default Default.default_arguments.prim_cost)
    let branch_cost = ref (F.default Default.default_arguments.branch_cost)
    let indirect_call_cost =
      ref (F.default Default.default_arguments.indirect_call_cost)
    let poly_compare_cost =
      ref (F.default Default.default_arguments.poly_compare_cost)

    let small_function_size =
      ref (I.default Default.default_arguments.small_function_size)
    let large_function_size =
      ref (I.default Default.default_arguments.large_function_size)

    let threshold = ref (F.default Default.default_arguments.threshold)

    let speculative_inlining_only_if_arguments_useful =
      ref Default.speculative_inlining_only_if_arguments_useful

    let report_bin = ref false

    let use_inlining_arguments_set ?round (arg : inlining_arguments) =
      let set_int = Clflags.set_int_arg round in
      let set_float = Clflags.set_float_arg round in
      set_int max_depth Default.default_arguments.max_depth
        (Some arg.max_depth);
      set_int max_rec_depth Default.default_arguments.max_rec_depth
        (Some arg.max_rec_depth);
      set_float call_cost Default.default_arguments.call_cost
        (Some arg.call_cost);
      set_float alloc_cost Default.default_arguments.alloc_cost
        (Some arg.alloc_cost);
      set_float prim_cost Default.default_arguments.prim_cost
        (Some arg.prim_cost);
      set_float branch_cost Default.default_arguments.branch_cost
        (Some arg.branch_cost);
      set_float indirect_call_cost
        Default.default_arguments.indirect_call_cost
        (Some arg.indirect_call_cost);
      set_float poly_compare_cost
        Default.default_arguments.poly_compare_cost
        (Some arg.poly_compare_cost);
      set_int small_function_size
        Default.default_arguments.small_function_size
        (Some arg.small_function_size);
      set_int large_function_size
        Default.default_arguments.large_function_size
        (Some arg.large_function_size);
      set_float threshold Default.default_arguments.threshold
        (Some arg.threshold)

    let oclassic_arguments = {
      Default.default_arguments with
      (* We set the small and large function sizes to the same value here to
         recover "classic mode" semantics (no speculative inlining). *)
      large_function_size = Default.default_arguments.small_function_size;
      (* [threshold] matches the current compiler's default.  (The factor of
         8 in that default is accounted for by [cost_divisor], above.) *)
      threshold = 10.;
    }

    let o2_arguments = {
      max_depth = 3;
      max_rec_depth = 0;
      call_cost = 3.0 *. Default.default_arguments.call_cost;
      alloc_cost = 3.0 *. Default.default_arguments.alloc_cost;
      prim_cost = 3.0 *. Default.default_arguments.prim_cost;
      branch_cost = 3.0 *. Default.default_arguments.branch_cost;
      indirect_call_cost = 3.0 *. Default.default_arguments.indirect_call_cost;
      poly_compare_cost = 3.0 *. Default.default_arguments.poly_compare_cost;
      small_function_size = 10 * Default.default_arguments.small_function_size;
      large_function_size = 50 * Default.default_arguments.large_function_size;
      threshold = 100.;
    }

    let o3_arguments = { o2_arguments with max_depth = 6 }
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

let use_cached_generic_functions = ref false
let cached_generic_functions_path =
  ref (Filename.concat Config.standard_library ("cached-generic-functions" ^ Config.ext_lib))

let () =
  if Clflags.is_flambda2 () then set_o2 ()
