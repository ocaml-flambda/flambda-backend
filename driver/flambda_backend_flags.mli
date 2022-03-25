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

val use_cpp_mangling : bool ref

val default_heap_reduction_threshold : int
val heap_reduction_threshold : int ref

type function_result_types = Never | Functors_only | All_functions
type opt_level = Oclassic | O2 | O3
type 'a or_default = Set of 'a | Default

val dump_inlining_paths : bool ref

val opt_level : opt_level or_default ref

module Flambda2 : sig
  module Default : sig
    val classic_mode : bool
    val join_points : bool
    val unbox_along_intra_function_control_flow : bool
    val backend_cse_at_toplevel : bool
    val cse_depth : int
    val join_depth : int
    val function_result_types : function_result_types

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

    unicode : bool;
  }

  val default_for_opt_level : opt_level or_default -> flags

  val function_result_types : function_result_types or_default ref

  val classic_mode : bool or_default ref
  val join_points : bool or_default ref
  val unbox_along_intra_function_control_flow : bool or_default ref
  val backend_cse_at_toplevel : bool or_default ref
  val cse_depth : int or_default ref
  val join_depth : int or_default ref

  val unicode : bool or_default ref

  module Dump : sig
    val rawfexpr : bool ref
    val fexpr : bool ref
    val flexpect : bool ref
    val slot_offsets : bool ref
    val freshen : bool ref
  end

  module Expert : sig
    module Default : sig
      val code_id_and_symbol_scoping_checks : bool
      val fallback_inlining_heuristic : bool
      val inline_effects_in_cmm : bool
      val phantom_lets : bool
      val max_block_size_for_projections : int option
      val max_unboxing_depth : int
      val can_inline_recursive_functions : bool
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

    val default_for_opt_level : opt_level or_default -> flags

    val code_id_and_symbol_scoping_checks : bool or_default ref
    val fallback_inlining_heuristic : bool or_default ref
    val inline_effects_in_cmm : bool or_default ref
    val phantom_lets : bool or_default ref
    val max_block_size_for_projections : int option or_default ref
    val max_unboxing_depth : int or_default ref
    val can_inline_recursive_functions : bool or_default ref
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
    module Default : sig
      val max_depth : int
      val max_rec_depth : int

      val call_cost : float
      val alloc_cost : float
      val prim_cost : float
      val branch_cost : float
      val indirect_call_cost : float
      val poly_compare_cost : float

      val small_function_size : int
      val large_function_size : int

      val threshold : float

      val speculative_inlining_only_if_arguments_useful : bool
    end

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
