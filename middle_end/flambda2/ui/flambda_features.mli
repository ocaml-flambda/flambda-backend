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

val flambda2_is_enabled : unit -> bool

val classic_mode : unit -> bool

val join_points : unit -> bool

val unbox_along_intra_function_control_flow : unit -> bool

val backend_cse_at_toplevel : unit -> bool

val cse_depth : unit -> int

val safe_string : unit -> bool

val flat_float_array : unit -> bool

val debug : unit -> bool

val opaque : unit -> bool

val float_const_prop : unit -> bool

val treat_invalid_code_as_unreachable : unit -> bool

val optimize_for_speed : unit -> bool

val inlining_report : unit -> bool

val inlining_report_bin : unit -> bool

val colour : unit -> Misc.Color.setting option

val unicode : unit -> bool

val check_invariants : unit -> bool

val dump_rawflambda : unit -> bool

val dump_flambda : unit -> bool

val dump_rawfexpr : unit -> bool

val dump_fexpr : unit -> bool

val dump_flexpect : unit -> bool

val dump_closure_offsets : unit -> bool

val freshen_when_printing : unit -> bool

module Inlining : sig
  type round_or_default =
    | Round of int
    | Default

  val max_depth : round_or_default -> int

  val call_cost : round_or_default -> float

  val alloc_cost : round_or_default -> float

  val prim_cost : round_or_default -> float

  val branch_cost : round_or_default -> float

  val indirect_call_cost : round_or_default -> float

  val poly_compare_cost : round_or_default -> float

  val small_function_size : round_or_default -> int

  val large_function_size : round_or_default -> int

  val threshold : round_or_default -> float

  val speculative_inlining_only_if_arguments_useful : unit -> bool
end

module Debug : sig
  val permute_every_name : unit -> bool

  val concrete_types_only_on_canonicals : unit -> bool
end

module Expert : sig
  val code_id_and_symbol_scoping_checks : unit -> bool

  val fallback_inlining_heuristic : unit -> bool

  val inline_effects_in_cmm : unit -> bool

  val max_block_size_for_projections : unit -> int option

  val phantom_lets : unit -> bool

  val max_unboxing_depth : unit -> int
end
