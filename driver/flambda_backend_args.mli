(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*              Damien Doligez, projet Para, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1998 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
module type Flambda_backend_options = sig
  val _ocamlcfg : unit -> unit
  val _no_ocamlcfg : unit -> unit
  val _flambda2_join_points : unit -> unit
  val _no_flambda2_join_points : unit -> unit
  val _flambda2_unbox_along_intra_function_control_flow : unit -> unit
  val _no_flambda2_unbox_along_intra_function_control_flow : unit -> unit
  val _flambda2_backend_cse_at_toplevel : unit -> unit
  val _no_flambda2_backend_cse_at_toplevel : unit -> unit
  val _flambda2_cse_depth : int -> unit
  val _flambda2_expert_code_id_and_symbol_scoping_checks : unit -> unit
  val _no_flambda2_expert_code_id_and_symbol_scoping_checks : unit -> unit
  val _flambda2_expert_fallback_inlining_heuristic : unit -> unit
  val _no_flambda2_expert_fallback_inlining_heuristic : unit -> unit
  val _flambda2_expert_inline_effects_in_cmm : unit -> unit
  val _no_flambda2_expert_inline_effects_in_cmm : unit -> unit
  val _flambda2_expert_phantom_lets : unit -> unit
  val _no_flambda2_expert_phantom_lets : unit -> unit
  val _flambda2_expert_max_block_size_for_projections : int -> unit
  val _flambda2_expert_max_unboxing_depth : int -> unit
  val _flambda2_expert_can_inline_recursive_functions : unit -> unit
  val _no_flambda2_expert_can_inline_recursive_functions : unit -> unit
  val _flambda2_debug_permute_every_name : unit -> unit
  val _no_flambda2_debug_permute_every_name : unit -> unit
  val _flambda2_debug_concrete_types_only_on_canonicals : unit -> unit
  val _no_flambda2_debug_concrete_types_only_on_canonicals : unit -> unit

  val _flambda2_inline_max_depth : string -> unit
  val _flambda2_inline_max_rec_depth : string -> unit
  val _flambda2_inline_call_cost : string -> unit
  val _flambda2_inline_alloc_cost : string -> unit
  val _flambda2_inline_prim_cost : string -> unit
  val _flambda2_inline_branch_cost : string -> unit
  val _flambda2_inline_indirect_call_cost : string -> unit
  val _flambda2_inline_poly_compare_cost : string -> unit
  val _flambda2_inline_small_function_size : string -> unit
  val _flambda2_inline_large_function_size : string -> unit
  val _flambda2_inline_threshold : string -> unit
  val _flambda2_speculative_inlining_only_if_arguments_useful : unit -> unit
  val _no_flambda2_speculative_inlining_only_if_arguments_useful : unit -> unit

  val _flambda2_inlining_report_bin : unit -> unit

  val _flambda2_unicode : unit -> unit

  val _flambda2_treat_invalid_code_as_unreachable : unit -> unit
  val _no_flambda2_treat_invalid_code_as_unreachable : unit -> unit

  val _drawfexpr : unit -> unit
  val _dfexpr : unit -> unit
  val _dflexpect : unit -> unit
  val _dclosure_offsets : unit -> unit
  val _dfreshen : unit -> unit
end

module type Optcomp_options = sig
  include Main_args.Optcomp_options
  include Flambda_backend_options
end

module type Opttop_options = sig
  include Main_args.Opttop_options
  include Flambda_backend_options
end

module Make_optcomp_options : Optcomp_options -> Main_args.Arg_list;;
module Make_opttop_options : Opttop_options -> Main_args.Arg_list;;

module Default: sig
  module Optmain: Optcomp_options
  module Opttopmain: Opttop_options
end

module Extra_params : sig
  (* This is installed unconditionally for native and bytecode and
     all tools that consume OCAMLPARAM, to avoid warnings about flags
     that are only supported in flambda-backend,
     similarly to the treatment of native-only flags in
     [ocaml/driver/compenv.ml] *)
  (** [read_param ppf pos name value] returns whether the param was handled.  *)
  val read_param :
      Format.formatter -> Compenv.readenv_position -> string -> string -> bool
end
