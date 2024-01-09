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
(** Flambda-backend specific command line flags *)
val use_ocamlcfg : bool ref
val dump_cfg : bool ref
val cfg_invariants : bool ref
val cfg_equivalence_check : bool ref
val regalloc : string ref
val regalloc_params : string list ref
val regalloc_validate : bool ref

val cfg_peephole_optimize: bool ref

val reorder_blocks_random : int option ref
val basic_block_sections : bool ref

val dasm_comments : bool ref

val default_heap_reduction_threshold : int
val heap_reduction_threshold : int ref
val dump_checkmach : bool ref

val davail : bool ref
val dranges : bool ref

type checkmach_details_cutoff =
  | Keep_all
  | At_most of int  (* n > 0 *)
  | No_details

val checkmach_details_cutoff : checkmach_details_cutoff ref
val default_checkmach_details_cutoff : checkmach_details_cutoff

val disable_poll_insertion : bool ref
val allow_long_frames : bool ref
val max_long_frames_threshold : int
val long_frames_threshold : int ref
val caml_apply_inline_fast_path : bool ref

type function_result_types = Never | Functors_only | All_functions
type meet_algorithm = Basic | Advanced
type opt_level = Oclassic | O2 | O3
type 'a or_default = Set of 'a | Default

val dump_inlining_paths : bool ref

val opt_level : opt_level or_default ref

val internal_assembler : bool ref

val gc_timings : bool ref

val use_cached_generic_functions : bool ref
val cached_generic_functions_path : string ref

module Flambda2 : sig
  val debug : bool ref

  module Default : sig
    val classic_mode : bool
    val join_points : bool
    val unbox_along_intra_function_control_flow : bool
    val backend_cse_at_toplevel : bool
    val cse_depth : int
    val join_depth : int
    val function_result_types : function_result_types
    val meet_algorithm : meet_algorithm

    val unicode : bool
  end

  (* CR-someday lmaurer: We could eliminate most of the per-flag boilerplate using GADTs
     and heterogeneous maps. Whether that's an improvement is a fair question. *)

  type flags = {
    classic_mode : bool;
    join_points : bool;
    unbox_along_intra_function_control_flow : bool;
    backend_cse_at_toplevel : bool;
    cse_depth : int;
    join_depth : int;
    function_result_types : function_result_types;
    meet_algorithm : meet_algorithm;

    unicode : bool;
  }

  val default_for_opt_level : opt_level or_default -> flags

  val function_result_types : function_result_types or_default ref
  val meet_algorithm : meet_algorithm or_default ref

  val classic_mode : bool or_default ref
  val join_points : bool or_default ref
  val unbox_along_intra_function_control_flow : bool or_default ref
  val backend_cse_at_toplevel : bool or_default ref
  val cse_depth : int or_default ref
  val join_depth : int or_default ref

  val unicode : bool or_default ref

  module Dump : sig
    type target = Nowhere | Main_dump_stream | File of Misc.filepath

    val rawfexpr : target ref
    val fexpr : target ref
    val flexpect : target ref
    val slot_offsets : bool ref
    val freshen : bool ref
    val flow : bool ref
  end

  module Expert : sig
    module Default : sig
      val fallback_inlining_heuristic : bool
      val inline_effects_in_cmm : bool
      val phantom_lets : bool
      val max_block_size_for_projections : int option
      val max_unboxing_depth : int
      val can_inline_recursive_functions : bool
      val max_function_simplify_run : int
      val shorten_symbol_names : bool
    end

    type flags = {
      fallback_inlining_heuristic : bool;
      inline_effects_in_cmm : bool;
      phantom_lets : bool;
      max_block_size_for_projections : int option;
      max_unboxing_depth : int;
      can_inline_recursive_functions : bool;
      max_function_simplify_run : int;
      shorten_symbol_names : bool;
    }

    val default_for_opt_level : opt_level or_default -> flags

    val fallback_inlining_heuristic : bool or_default ref
    val inline_effects_in_cmm : bool or_default ref
    val phantom_lets : bool or_default ref
    val max_block_size_for_projections : int option or_default ref
    val max_unboxing_depth : int or_default ref
    val can_inline_recursive_functions : bool or_default ref
    val max_function_simplify_run : int or_default ref
    val shorten_symbol_names : bool or_default ref
  end

  module Debug : sig
    module Default : sig
      val concrete_types_only_on_canonicals : bool
      val keep_invalid_handlers : bool
    end

    val concrete_types_only_on_canonicals : bool ref
    val keep_invalid_handlers : bool ref
  end

  module Inlining : sig
    type inlining_arguments = private {
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

    module Default : sig
      val default_arguments : inlining_arguments
      val speculative_inlining_only_if_arguments_useful : bool
    end

    val oclassic_arguments : inlining_arguments
    val o2_arguments : inlining_arguments
    val o3_arguments : inlining_arguments

    val max_depth : Clflags.Int_arg_helper.parsed ref
    val max_rec_depth : Clflags.Int_arg_helper.parsed ref

    val call_cost : Clflags.Float_arg_helper.parsed ref
    val alloc_cost : Clflags.Float_arg_helper.parsed ref
    val prim_cost : Clflags.Float_arg_helper.parsed ref
    val branch_cost : Clflags.Float_arg_helper.parsed ref
    val indirect_call_cost : Clflags.Float_arg_helper.parsed ref
    val poly_compare_cost : Clflags.Float_arg_helper.parsed ref

    val small_function_size : Clflags.Int_arg_helper.parsed ref
    val large_function_size : Clflags.Int_arg_helper.parsed ref

    val threshold : Clflags.Float_arg_helper.parsed ref

    val speculative_inlining_only_if_arguments_useful : bool ref

    val report_bin : bool ref
  end
end

val opt_flag_handler : Clflags.Opt_flag_handler.t
