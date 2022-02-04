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

type function_result_types = Never | Functors_only | All_functions

module Flambda2 : sig
  module Default : sig
    val classic_mode : bool
    val join_points : bool
    val unbox_along_intra_function_control_flow : bool
    val backend_cse_at_toplevel : bool
    val cse_depth : int
    val join_depth : int
    val treat_invalid_code_as_unreachable : bool
    val function_result_types : function_result_types

    val unicode : bool
  end

  val function_result_types : function_result_types ref

  val classic_mode : bool ref
  val join_points : bool ref
  val unbox_along_intra_function_control_flow : bool ref
  val backend_cse_at_toplevel : bool ref
  val cse_depth : int ref
  val join_depth : int ref
  val treat_invalid_code_as_unreachable : bool ref

  val unicode : bool ref

  module Dump : sig
    val rawfexpr : bool ref
    val fexpr : bool ref
    val flexpect : bool ref
    val closure_offsets : bool ref
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

    val code_id_and_symbol_scoping_checks : bool ref
    val fallback_inlining_heuristic : bool ref
    val inline_effects_in_cmm : bool ref
    val phantom_lets : bool ref
    val max_block_size_for_projections : int option ref
    val max_unboxing_depth : int ref
    val can_inline_recursive_functions : bool ref
  end

  module Debug : sig
    module Default : sig
      val permute_every_name : bool
      val concrete_types_only_on_canonicals : bool
    end

    val permute_every_name : bool ref
    val concrete_types_only_on_canonicals : bool ref
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


val set_oclassic : unit -> unit
val set_o2 : unit -> unit
val set_o3 : unit -> unit
