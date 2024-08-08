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

val flambda2_is_enabled : unit -> bool

val debug_flambda2 : unit -> bool

type 'a mode =
  | Normal : [`Normal] mode
  | Classic : [`Classic] mode

type any_mode = Mode : _ mode -> any_mode

val classic_mode : unit -> bool

val mode : unit -> any_mode

val join_points : unit -> bool

val unbox_along_intra_function_control_flow : unit -> bool

val backend_cse_at_toplevel : unit -> bool

val cse_depth : unit -> int

val join_depth : unit -> int

val flat_float_array : unit -> bool

val function_result_types : is_a_functor:bool -> bool

val use_better_meet : unit -> bool

val debug : unit -> bool

val opaque : unit -> bool

val float_const_prop : unit -> bool

val optimize_for_speed : unit -> bool

val inlining_report : unit -> bool

val inlining_report_bin : unit -> bool

val colour : unit -> Misc.Color.setting option

val unicode : unit -> bool

val check_invariants : unit -> bool

type dump_target = Flambda_backend_flags.Flambda2.Dump.target =
  | Nowhere
  | Main_dump_stream
  | File of Misc.filepath

val dump_rawflambda : unit -> bool

val dump_flambda : unit -> bool

val dump_rawfexpr : unit -> dump_target

val dump_fexpr : unit -> dump_target

val dump_flexpect : unit -> dump_target

val dump_slot_offsets : unit -> bool

val dump_flow : unit -> bool

val freshen_when_printing : unit -> bool

module Inlining : sig
  type round_or_default =
    | Round of int
    | Default of Flambda_backend_flags.opt_level

  val depth_scaling_factor : int

  (** [max_depth] returns the user's value multipled by [depth_scaling_factor]. *)
  val max_depth : round_or_default -> int

  val max_rec_depth : round_or_default -> int

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
  val concrete_types_only_on_canonicals : unit -> bool

  val keep_invalid_handlers : unit -> bool
end

module Expert : sig
  val fallback_inlining_heuristic : unit -> bool

  val inline_effects_in_cmm : unit -> bool

  val max_block_size_for_projections : unit -> int option

  val phantom_lets : unit -> bool

  val max_unboxing_depth : unit -> int

  val can_inline_recursive_functions : unit -> bool

  val max_function_simplify_run : unit -> int

  val shorten_symbol_names : unit -> bool

  val cont_lifting_budget : unit -> int
end

val stack_allocation_enabled : unit -> bool
