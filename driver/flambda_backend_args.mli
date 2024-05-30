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
(** This module follows the structure of ocaml/driver/main_args.ml{i}.
    It provides a way to (a) share argument implementations between
    different installable tools and (b) override default implementations
    of arguments. *)

(** Command line arguments required for flambda backend.  *)
module type Flambda_backend_options = sig
  val ocamlcfg : unit -> unit
  val no_ocamlcfg : unit -> unit
  val dump_inlining_paths : unit -> unit
  val davail : unit -> unit
  val dranges : unit -> unit
  val ddebug_invariants : unit -> unit
  val dcfg : unit -> unit
  val dcfg_invariants : unit -> unit
  val dcfg_equivalence_check : unit -> unit
  val regalloc : string -> unit
  val regalloc_param : string -> unit
  val regalloc_validate : unit -> unit
  val no_regalloc_validate : unit -> unit

  val cfg_peephole_optimize : unit -> unit
  val no_cfg_peephole_optimize : unit -> unit

  val cfg_cse_optimize : unit -> unit
  val no_cfg_cse_optimize : unit -> unit

  val cfg_zero_alloc_checker : unit -> unit
  val no_cfg_zero_alloc_checker : unit -> unit

  val cfg_stack_checks : unit -> unit
  val no_cfg_stack_checks : unit -> unit
  val cfg_stack_checks_threshold : int -> unit

  val reorder_blocks_random : int -> unit
  val basic_block_sections : unit -> unit

  val dasm_comments : unit -> unit
  val dno_asm_comments : unit -> unit

  val heap_reduction_threshold : int -> unit
  val zero_alloc_check : string -> unit

  val dzero_alloc : unit -> unit
  val disable_zero_alloc_checker : unit -> unit
  val disable_precise_zero_alloc_checker : unit -> unit
  val zero_alloc_checker_details_cutoff : int -> unit
  val zero_alloc_checker_join : int -> unit

  val function_layout : string -> unit
  val disable_poll_insertion : unit -> unit
  val enable_poll_insertion : unit -> unit

  val symbol_visibility_protected : unit -> unit
  val no_symbol_visibility_protected : unit -> unit

  val long_frames : unit -> unit
  val no_long_frames : unit -> unit
  val long_frames_threshold : int -> unit

  val caml_apply_inline_fast_path : unit -> unit
  val internal_assembler : unit -> unit

  val gc_timings : unit -> unit

  val flambda2_debug : unit -> unit
  val no_flambda2_debug : unit -> unit
  val flambda2_join_points : unit -> unit
  val no_flambda2_join_points : unit -> unit
  val flambda2_result_types_functors_only : unit -> unit
  val flambda2_result_types_all_functions : unit -> unit
  val no_flambda2_result_types : unit -> unit
  val flambda2_basic_meet : unit -> unit
  val flambda2_advanced_meet : unit -> unit
  val flambda2_unbox_along_intra_function_control_flow : unit -> unit
  val no_flambda2_unbox_along_intra_function_control_flow : unit -> unit
  val flambda2_backend_cse_at_toplevel : unit -> unit
  val no_flambda2_backend_cse_at_toplevel : unit -> unit
  val flambda2_cse_depth : int -> unit
  val flambda2_join_depth : int -> unit
  val flambda2_expert_fallback_inlining_heuristic : unit -> unit
  val no_flambda2_expert_fallback_inlining_heuristic : unit -> unit
  val flambda2_expert_inline_effects_in_cmm : unit -> unit
  val no_flambda2_expert_inline_effects_in_cmm : unit -> unit
  val flambda2_expert_phantom_lets : unit -> unit
  val no_flambda2_expert_phantom_lets : unit -> unit
  val flambda2_expert_max_block_size_for_projections : int -> unit
  val flambda2_expert_max_unboxing_depth : int -> unit
  val flambda2_expert_can_inline_recursive_functions : unit -> unit
  val no_flambda2_expert_can_inline_recursive_functions : unit -> unit
  val flambda2_expert_max_function_simplify_run : int -> unit
  val flambda2_expert_shorten_symbol_names : unit -> unit
  val no_flambda2_expert_shorten_symbol_names : unit -> unit
  val flambda2_debug_concrete_types_only_on_canonicals : unit -> unit
  val no_flambda2_debug_concrete_types_only_on_canonicals : unit -> unit
  val flambda2_debug_keep_invalid_handlers : unit -> unit
  val no_flambda2_debug_keep_invalid_handlers : unit -> unit

  val flambda2_inline_max_depth : string -> unit
  val flambda2_inline_max_rec_depth : string -> unit
  val flambda2_inline_call_cost : string -> unit
  val flambda2_inline_alloc_cost : string -> unit
  val flambda2_inline_prim_cost : string -> unit
  val flambda2_inline_branch_cost : string -> unit
  val flambda2_inline_indirect_call_cost : string -> unit
  val flambda2_inline_poly_compare_cost : string -> unit
  val flambda2_inline_small_function_size : string -> unit
  val flambda2_inline_large_function_size : string -> unit
  val flambda2_inline_threshold : string -> unit
  val flambda2_speculative_inlining_only_if_arguments_useful : unit -> unit
  val no_flambda2_speculative_inlining_only_if_arguments_useful : unit -> unit

  val flambda2_inlining_report_bin : unit -> unit

  val flambda2_unicode : unit -> unit

  val drawfexpr : unit -> unit
  val drawfexpr_to : string -> unit
  val dfexpr : unit -> unit
  val dfexpr_to : string -> unit
  val dflexpect_to : string -> unit
  val dslot_offsets : unit -> unit
  val dfreshen : unit -> unit
  val dflow : unit -> unit
  val use_cached_generic_functions : unit -> unit
  val cached_generic_functions_path : string -> unit
end

(** Command line arguments required for ocamlopt.*)
module type Debugging_options = sig
  val restrict_to_upstream_dwarf : unit -> unit
  val no_restrict_to_upstream_dwarf : unit -> unit
  val dwarf_inlined_frames : unit -> unit
  val no_dwarf_inlined_frames : unit -> unit
  val dwarf_for_startup_file : unit -> unit
  val no_dwarf_for_startup_file : unit -> unit
  val gdwarf_may_alter_codegen : unit -> unit
  val no_gdwarf_may_alter_codegen : unit -> unit
  val gdwarf_max_function_complexity : int -> unit
end

(** Command line arguments required for ocamlopt. *)
module type Optcomp_options = sig
  include Main_args.Optcomp_options
  include Flambda_backend_options
  include Debugging_options
end

(** Command line arguments required for ocamlnat. *)
module type Opttop_options = sig
  include Main_args.Opttop_options
  include Flambda_backend_options
  include Debugging_options
end

(** Transform required command-line arguments into actual arguments.
    Each tool can define its own argument implementations and
    call the right functor to actualize them into [Arg.t] list. *)
module Make_optcomp_options : Optcomp_options -> Main_args.Arg_list;;
module Make_opttop_options : Opttop_options -> Main_args.Arg_list;;

(** Default implementations of required arguments for each tool.  *)
module Default: sig
  module Optmain: Optcomp_options
  module Opttopmain: Opttop_options
end

(** Extra_params module provides a way to read flambda-backend
    flags from OCAMLPARAM. All command line flags should support it,
    with the exception of debug printing, such as -dcfg.
*)
module Extra_params : sig
  (** [read_param ppf pos name value] returns whether the param was handled.  *)
  val read_param :
      Format.formatter -> Compenv.readenv_position -> string -> string -> bool
end
