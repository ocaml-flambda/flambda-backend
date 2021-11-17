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
(* Keep in sync with tools/ocaml_compiler_internal_params. *)
let format_default flag = if flag then " (default)" else ""
let format_not_default flag = if flag then "" else " (default)"

let mk_ocamlcfg f =
  "-ocamlcfg", Arg.Unit f, " Use ocamlcfg"

let mk_no_ocamlcfg f =
  "-no-ocamlcfg", Arg.Unit f, " Do not use ocamlcfg"
;;

module Flambda2 = Flambda_backend_flags.Flambda2

let mk_flambda2_join_points f =
  "-flambda2-join-points", Arg.Unit f,
  Printf.sprintf " Propagate information from all incoming edges to a join\n\
      \     point%s (Flambda 2 only)"
    (format_default Flambda2.Default.join_points)
;;

let mk_no_flambda2_join_points f =
  "-no-flambda2-join-points", Arg.Unit f,
  Printf.sprintf " Propagate information to a join point only if there are\n\
      \     zero or one incoming edge(s)%s (Flambda 2 only)"
    (format_not_default Flambda2.Default.join_points)
;;

let mk_flambda2_unbox_along_intra_function_control_flow f =
  "-flambda2-unbox-along-intra-function-control-flow", Arg.Unit f,
  Printf.sprintf " Pass values within\n\
      \     a function as unboxed where possible%s (Flambda 2 only)"
    (format_default Flambda2.Default.unbox_along_intra_function_control_flow)
;;

let mk_no_flambda2_unbox_along_intra_function_control_flow f =
  "-no-flambda2-unbox-along-intra-function-control-flow", Arg.Unit f,
  Printf.sprintf " Pass values within\n\
      \     a function in their normal representation%s (Flambda 2 only)"
    (format_not_default
      Flambda2.Default.unbox_along_intra_function_control_flow)
;;

let mk_flambda2_backend_cse_at_toplevel f =
  "-flambda2-backend-cse-at-toplevel", Arg.Unit f,
  Printf.sprintf " Apply the backend CSE pass to module\n\
      \     initializers%s (Flambda 2 only)"
    (format_default Flambda2.Default.backend_cse_at_toplevel)
;;

let mk_no_flambda2_backend_cse_at_toplevel f =
  "-no-flambda2-backend-cse-at-toplevel", Arg.Unit f,
  Printf.sprintf " Do not apply the backend CSE pass to\n\
      \     module initializers%s (Flambda 2 only)"
    (format_not_default Flambda2.Default.backend_cse_at_toplevel)
;;

let mk_flambda2_cse_depth f =
  "-flambda2-cse-depth", Arg.Int f,
  Printf.sprintf " Depth threshold for eager tracking of CSE equations\n\
      \     (default %d) (Flambda 2 only)"
    Flambda2.Default.cse_depth
;;

let mk_flambda2_expert_code_id_and_symbol_scoping_checks f =
  "-flambda2-expert-code-id-and-symbol-scoping-checks", Arg.Unit f,
  Printf.sprintf " Perform checks on static\n\
      \     scopes of code IDs and symbols during To_cmm%s\n\
      \     (Flambda 2 only)"
    (format_default Flambda2.Expert.Default.code_id_and_symbol_scoping_checks)
;;

let mk_no_flambda2_expert_code_id_and_symbol_scoping_checks f =
  "-no-flambda2-expert-code-id-and-symbol-scoping-checks", Arg.Unit f,
  Printf.sprintf " Do not perform checks\n\
      \     on static scopes of code IDs and symbols during To_cmm%s\n\
      \     (Flambda 2 only)"
    (format_not_default
      Flambda2.Expert.Default.code_id_and_symbol_scoping_checks)
;;

let mk_flambda2_expert_fallback_inlining_heuristic f =
  "-flambda2-expert-fallback-inlining-heuristic", Arg.Unit f,
  Printf.sprintf " Prevent inlining of functions\n\
      \     whose bodies contain closures%s (Flambda 2 only)"
    (format_default Flambda2.Expert.Default.fallback_inlining_heuristic)
;;

let mk_no_flambda2_expert_fallback_inlining_heuristic f =
  "-no-flambda2-expert-fallback-inlining-heuristic", Arg.Unit f,
  Printf.sprintf " Allow inlining of functions\n\
      \     whose bodies contain closures%s (Flambda 2 only)"
    (format_not_default Flambda2.Expert.Default.fallback_inlining_heuristic)
;;

let mk_flambda2_expert_inline_effects_in_cmm f =
  "-flambda2-expert-inline-effects-in-cmm", Arg.Unit f,
  Printf.sprintf " Allow inlining of effectful\n\
      \     expressions in the produced Cmm code%s (Flambda 2 only)"
    (format_default Flambda2.Expert.Default.inline_effects_in_cmm)
;;

let mk_no_flambda2_expert_inline_effects_in_cmm f =
  "-no-flambda2-expert-inline-effects-in-cmm", Arg.Unit f,
  Printf.sprintf " Prevent inlining of effectful\n\
      \     expressions in the produced Cmm code%s (Flambda 2 only)"
    (format_not_default Flambda2.Expert.Default.inline_effects_in_cmm)
;;

let mk_flambda2_expert_phantom_lets f =
  "-flambda2-expert-phantom-lets", Arg.Unit f,
  Printf.sprintf " Generate phantom lets when -g\n\
      \     is specified%s (Flambda 2 only)"
    (format_default Flambda2.Expert.Default.phantom_lets)
;;

let mk_no_flambda2_expert_phantom_lets f =
  "-no-flambda2-expert-phantom-lets", Arg.Unit f,
  Printf.sprintf " Do not generate phantom lets even when -g\n\
      \     is specified%s (Flambda 2 only)"
    (format_not_default Flambda2.Expert.Default.phantom_lets)
;;

let mk_flambda2_expert_max_block_size_for_projections f =
  "-flambda2-expert-max-block-size-for-projections", Arg.Int f,
  Printf.sprintf " Do not simplify projections\n\
      \     from blocks if the block size exceeds this value (default %s)\n\
      \     (Flambda 2 only)"
    (match Flambda2.Expert.Default.max_block_size_for_projections with
     | None -> "not set"
     | Some max -> string_of_int max)
;;

let mk_flambda2_expert_max_unboxing_depth f =
  "-flambda2-expert-max-unboxing-depth", Arg.Int f,
  Printf.sprintf " Do not unbox (nested) values deeper\n\
      \     than this many levels (default %d) (Flambda 2 only)"
    Flambda2.Expert.Default.max_unboxing_depth
;;

let mk_flambda2_expert_can_inline_recursive_functions f =
  "-flambda2-expert-can-inline-recursive-functions", Arg.Unit f,
  Printf.sprintf " Consider inlining\n\
     \      recursive functions (default %s) (Flambda 2 only)"
    (format_default Flambda2.Expert.Default.can_inline_recursive_functions)
;;

let mk_no_flambda2_expert_can_inline_recursive_functions f =
  "-no-flambda2-expert-can-inline-recursive-functions", Arg.Unit f,
  Printf.sprintf " Only inline recursive\n\
      \     functions if forced to so do by an attribute\n\
      \     (default %s) (Flambda 2 only)"
    (format_not_default Flambda2.Expert.Default.can_inline_recursive_functions)
;;

let mk_flambda2_debug_permute_every_name f =
  "-flambda2-debug-permute-every-name", Arg.Unit f,
  Printf.sprintf " Permute every name to test name\n\
      \     permutation code%s (Flambda 2 only)"
    (format_default Flambda2.Debug.Default.permute_every_name)
;;

let mk_no_flambda2_debug_permute_every_name f =
  "-no-flambda2-debug-permute-every-name", Arg.Unit f,
  Printf.sprintf " Do not permute every name to test\n\
      \     name permutation code%s (Flambda 2 only)"
    (format_not_default Flambda2.Debug.Default.permute_every_name)
;;

let mk_flambda2_debug_concrete_types_only_on_canonicals f =
  "-flambda2-debug-concrete-types-only-on-canonicals", Arg.Unit f,
  Printf.sprintf " Check that concrete\n\
      \     types are only assigned to canonical\n\
      \     names%s (Flambda 2 only)"
    (format_default Flambda2.Debug.Default.concrete_types_only_on_canonicals)
;;

let mk_no_flambda2_debug_concrete_types_only_on_canonicals f =
  "-no-flambda2-debug-concrete-types-only-on-canonicals", Arg.Unit f,
  Printf.sprintf " Do not check that\n\
      \     concrete types are only assigned to canonical\n\
      \     names%s (Flambda 2 only)"
    (format_not_default
      Flambda2.Debug.Default.concrete_types_only_on_canonicals)
;;

let mk_flambda2_inline_max_depth f =
  "-flambda2-inline-max-depth", Arg.String f,
  Printf.sprintf "<int>|<round>=<int>[,...]\n\
      \     Maximum depth of search for inlining opportunities inside\n\
      \     inlined functions (default %d) (Flambda 2 only)"
    Flambda_backend_flags.Flambda2.Inlining.Default.max_depth
;;

let mk_flambda2_inline_max_rec_depth f =
  "-flambda2-inline-max-rec-depth", Arg.String f,
  Printf.sprintf "<int>|<round>=<int>[,...]\n\
      \     Maximum depth of search for inlining opportunities inside\n\
      \     inlined recursive functions (default %d) (Flambda 2 only)"
    Flambda_backend_flags.Flambda2.Inlining.Default.max_rec_depth
;;

let mk_flambda2_inline_cost arg descr ~default f =
  Printf.sprintf "-flambda2-inline-%s-cost" arg,
  Arg.String f,
  Printf.sprintf "<float>|<round>=<float>[,...]\n\
      \     The cost of not removing %s during inlining\n\
      \     (default %.03f, higher = more costly) (Flambda 2 only)"
    descr
    default
;;

let mk_flambda2_inline_call_cost =
  mk_flambda2_inline_cost "call" "a call"
    ~default:Flambda_backend_flags.Flambda2.Inlining.Default.call_cost

let mk_flambda2_inline_alloc_cost =
  mk_flambda2_inline_cost "alloc" "an allocation"
    ~default:Flambda_backend_flags.Flambda2.Inlining.Default.alloc_cost

let mk_flambda2_inline_prim_cost =
  mk_flambda2_inline_cost "prim" "a primitive"
    ~default:Flambda_backend_flags.Flambda2.Inlining.Default.prim_cost

let mk_flambda2_inline_branch_cost =
  mk_flambda2_inline_cost "branch" "a conditional"
    ~default:Flambda_backend_flags.Flambda2.Inlining.Default.branch_cost

let mk_flambda2_inline_indirect_call_cost =
  mk_flambda2_inline_cost "indirect" "an indirect call"
    ~default:Flambda_backend_flags.Flambda2.Inlining.Default.indirect_call_cost

let mk_flambda2_inline_poly_compare_cost =
  mk_flambda2_inline_cost "poly-compare" "a polymorphic comparison"
    ~default:Flambda_backend_flags.Flambda2.Inlining.Default.poly_compare_cost

(* CR-someday mshinwell: We should have a check that the parameters provided by
   the user are sensible, e.g. small_function_size <= large_function_size. *)

let mk_flambda2_inline_small_function_size f =
  "-flambda2-inline-small-function-size", Arg.String f,
  Printf.sprintf "<int>|<round>=<int>[,...]\n\
      \     Functions with a cost less than this will always be inlined\n\
      \     unless an attribute instructs otherwise (default %d)\n\
      \     (Flambda 2 only)"
    Flambda_backend_flags.Flambda2.Inlining.Default.small_function_size
;;

let mk_flambda2_inline_large_function_size f =
  "-flambda2-inline-large-function-size", Arg.String f,
  Printf.sprintf "<int>|<round>=<int>[,...]\n\
      \     Functions with a cost greater than this will never be inlined\n\
      \     unless an attribute instructs otherwise (default %d); speculative\n\
      \     inlining will be disabled if equal to the small function size\n\
      \     (Flambda 2 only)"
    Flambda_backend_flags.Flambda2.Inlining.Default.large_function_size
;;

let mk_flambda2_inline_threshold f =
  "-flambda2-inline-threshold", Arg.String f,
    Printf.sprintf "<float>|<round>=<float>[,...]\n\
        \     Aggressiveness of inlining (default %.02f, higher numbers mean\n\
        \     more aggressive) (Flambda 2 only)"
      Flambda_backend_flags.Flambda2.Inlining.Default.threshold

let mk_flambda2_speculative_inlining_only_if_arguments_useful f =
  "-flambda2-speculative-inlining-only-if-arguments-useful", Arg.Unit f,
    Printf.sprintf " Only\n\
        \    perform speculative inlining if the Flambda type system has\n\
        \    useful information about the argument(s) at the call site%s\n\
        \    (Flambda 2 only)"
      (format_default
        Flambda2.Inlining.Default.speculative_inlining_only_if_arguments_useful)

let mk_no_flambda2_speculative_inlining_only_if_arguments_useful f =
  "-no-flambda2-speculative-inlining-only-if-arguments-useful", Arg.Unit f,
    Printf.sprintf " Ignore\n\
        \     whether the Flambda type system has useful information\n\
        \     about the argument(s) at the call site when performing\n\
        \     speculative inlining%s (Flambda 2 only)"
      (format_not_default
        Flambda2.Inlining.Default.speculative_inlining_only_if_arguments_useful)

let mk_flambda2_treat_invalid_code_as_unreachable f =
  "-flambda2-treat-invalid-code-as-unreachable", Arg.Unit f,
  Printf.sprintf " Treat code deemed as\n\
      \     invalid by the Flambda 2 type system as unreachable, thus causing\n\
      \     it (and potentially calling code) to be deleted%s\n\
      \     (Flambda 2 only)"
    (format_default Flambda2.Default.treat_invalid_code_as_unreachable)
;;

let mk_no_flambda2_treat_invalid_code_as_unreachable f =
  "-no-flambda2-treat-invalid-code-as-unreachable", Arg.Unit f,
  Printf.sprintf " Do not treat code deemed as\n\
      \     invalid by the Flambda 2 type system as unreachable, instead\n\
      \     replacing it by a trap (which currently causes a segfault)%s\n\
      \     (Flambda 2 only)"
    (format_not_default Flambda2.Default.treat_invalid_code_as_unreachable)
;;

let mk_flambda2_inlining_report_bin f =
  "-flambda2-inlining-report-bin", Arg.Unit f, " Write inlining report\n\
    \     in binary format (Flambda 2 only)"
;;

let mk_flambda2_unicode f =
  "-flambda2-unicode", Arg.Unit f, " Use Unicode output when printing\n\
    \     Flambda 2 code"
;;

let mk_drawfexpr f =
  "-drawfexpr", Arg.Unit f, " Like -drawflambda but outputs fexpr language\n\
    \     (Flambda 2 only)"
;;

let mk_dfexpr f =
  "-dfexpr", Arg.Unit f, " Like -dflambda but outputs fexpr language\n\
    \     (Flambda 2 only)"
;;

let mk_dflexpect f =
  "-dflexpect", Arg.Unit f, " Like -dflambda but outputs a .flt file\n\
    \     whose basename matches that of the input .ml file (Flambda 2 only)"
;;

let mk_dclosure_offsets f =
  "-dclosure-offsets", Arg.Unit f, " Dump closure offsets (Flambda 2 only)"
;;

let mk_dfreshen f =
  "-dfreshen", Arg.Unit f, " Freshen bound names when printing (Flambda 2 only)"
;;

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

module Make_flambda_backend_options (F : Flambda_backend_options) =
struct
  let list2 = [
    mk_ocamlcfg F._ocamlcfg;
    mk_no_ocamlcfg F._no_ocamlcfg;

    mk_flambda2_join_points F._flambda2_join_points;
    mk_no_flambda2_join_points F._no_flambda2_join_points;
    mk_flambda2_unbox_along_intra_function_control_flow
      F._flambda2_unbox_along_intra_function_control_flow;
    mk_no_flambda2_unbox_along_intra_function_control_flow
      F._no_flambda2_unbox_along_intra_function_control_flow;
    mk_flambda2_backend_cse_at_toplevel F._flambda2_backend_cse_at_toplevel;
    mk_no_flambda2_backend_cse_at_toplevel
      F._no_flambda2_backend_cse_at_toplevel;
    mk_flambda2_cse_depth F._flambda2_cse_depth;
    mk_flambda2_expert_code_id_and_symbol_scoping_checks
      F._flambda2_expert_code_id_and_symbol_scoping_checks;
    mk_no_flambda2_expert_code_id_and_symbol_scoping_checks
      F._no_flambda2_expert_code_id_and_symbol_scoping_checks;
    mk_flambda2_expert_fallback_inlining_heuristic
      F._flambda2_expert_fallback_inlining_heuristic;
    mk_no_flambda2_expert_fallback_inlining_heuristic
      F._no_flambda2_expert_fallback_inlining_heuristic;
    mk_flambda2_expert_inline_effects_in_cmm
      F._flambda2_expert_inline_effects_in_cmm;
    mk_no_flambda2_expert_inline_effects_in_cmm
      F._no_flambda2_expert_inline_effects_in_cmm;
    mk_flambda2_expert_phantom_lets
      F._flambda2_expert_phantom_lets;
    mk_no_flambda2_expert_phantom_lets
      F._no_flambda2_expert_phantom_lets;
    mk_flambda2_expert_max_block_size_for_projections
      F._flambda2_expert_max_block_size_for_projections;
    mk_flambda2_expert_max_unboxing_depth
      F._flambda2_expert_max_unboxing_depth;
    mk_flambda2_expert_can_inline_recursive_functions
      F._flambda2_expert_can_inline_recursive_functions;
    mk_no_flambda2_expert_can_inline_recursive_functions
      F._no_flambda2_expert_can_inline_recursive_functions;
    mk_flambda2_debug_permute_every_name
      F._flambda2_debug_permute_every_name;
    mk_no_flambda2_debug_permute_every_name
      F._no_flambda2_debug_permute_every_name;
    mk_flambda2_debug_concrete_types_only_on_canonicals
      F._flambda2_debug_concrete_types_only_on_canonicals;
    mk_no_flambda2_debug_concrete_types_only_on_canonicals
      F._no_flambda2_debug_concrete_types_only_on_canonicals;

    mk_flambda2_inline_max_depth F._flambda2_inline_max_depth;
    mk_flambda2_inline_max_rec_depth F._flambda2_inline_max_rec_depth;
    mk_flambda2_inline_alloc_cost F._flambda2_inline_alloc_cost;
    mk_flambda2_inline_branch_cost F._flambda2_inline_branch_cost;
    mk_flambda2_inline_call_cost F._flambda2_inline_call_cost;
    mk_flambda2_inline_prim_cost F._flambda2_inline_prim_cost;
    mk_flambda2_inline_indirect_call_cost F._flambda2_inline_indirect_call_cost;
    mk_flambda2_inline_poly_compare_cost F._flambda2_inline_poly_compare_cost;
    mk_flambda2_inline_small_function_size
      F._flambda2_inline_small_function_size;
    mk_flambda2_inline_large_function_size
      F._flambda2_inline_large_function_size;
    mk_flambda2_inline_threshold F._flambda2_inline_threshold;
    mk_flambda2_speculative_inlining_only_if_arguments_useful
      F._flambda2_speculative_inlining_only_if_arguments_useful;
    mk_no_flambda2_speculative_inlining_only_if_arguments_useful
      F._no_flambda2_speculative_inlining_only_if_arguments_useful;

    mk_flambda2_inlining_report_bin F._flambda2_inlining_report_bin;

    mk_flambda2_unicode F._flambda2_unicode;

    mk_flambda2_treat_invalid_code_as_unreachable
      F._flambda2_treat_invalid_code_as_unreachable;
    mk_no_flambda2_treat_invalid_code_as_unreachable
      F._no_flambda2_treat_invalid_code_as_unreachable;

    mk_drawfexpr F._drawfexpr;
    mk_dfexpr F._dfexpr;
    mk_dflexpect F._dflexpect;
    mk_dclosure_offsets F._dclosure_offsets;
    mk_dfreshen F._dfreshen;
  ]
end

module Flambda_backend_options = struct
  let set r () = r := true
  let clear r () = r := false

  let _ocamlcfg = set Flambda_backend_flags.use_ocamlcfg
  let _no_ocamlcfg = clear Flambda_backend_flags.use_ocamlcfg

  let _flambda2_join_points = set Flambda2.join_points
  let _no_flambda2_join_points = clear Flambda2.join_points
  let _flambda2_unbox_along_intra_function_control_flow =
    set Flambda2.unbox_along_intra_function_control_flow
  let _no_flambda2_unbox_along_intra_function_control_flow =
    clear Flambda2.unbox_along_intra_function_control_flow
  let _flambda2_backend_cse_at_toplevel =
    set Flambda2.backend_cse_at_toplevel
  let _no_flambda2_backend_cse_at_toplevel =
    clear Flambda2.backend_cse_at_toplevel
  let _flambda2_cse_depth n = Flambda2.cse_depth := n
  let _flambda2_expert_code_id_and_symbol_scoping_checks =
    set Flambda2.Expert.code_id_and_symbol_scoping_checks
  let _no_flambda2_expert_code_id_and_symbol_scoping_checks =
    clear Flambda2.Expert.code_id_and_symbol_scoping_checks
  let _flambda2_expert_fallback_inlining_heuristic =
    set Flambda2.Expert.fallback_inlining_heuristic
  let _no_flambda2_expert_fallback_inlining_heuristic =
    clear Flambda2.Expert.fallback_inlining_heuristic
  let _flambda2_expert_inline_effects_in_cmm =
    set Flambda2.Expert.inline_effects_in_cmm
  let _no_flambda2_expert_inline_effects_in_cmm =
    clear Flambda2.Expert.inline_effects_in_cmm
  let _flambda2_expert_phantom_lets =
    set Flambda2.Expert.phantom_lets
  let _no_flambda2_expert_phantom_lets =
    clear Flambda2.Expert.phantom_lets
  let _flambda2_expert_max_block_size_for_projections size =
    Flambda2.Expert.max_block_size_for_projections := Some size
  let _flambda2_expert_max_unboxing_depth depth =
    Flambda2.Expert.max_unboxing_depth := depth
  let _flambda2_expert_can_inline_recursive_functions () =
    Flambda2.Expert.can_inline_recursive_functions := true
  let _no_flambda2_expert_can_inline_recursive_functions () =
    Flambda2.Expert.can_inline_recursive_functions := false
  let _flambda2_debug_permute_every_name =
    set Flambda2.Debug.permute_every_name
  let _no_flambda2_debug_permute_every_name =
    clear Flambda2.Debug.permute_every_name
  let _flambda2_debug_concrete_types_only_on_canonicals =
    set Flambda2.Debug.concrete_types_only_on_canonicals
  let _no_flambda2_debug_concrete_types_only_on_canonicals =
    clear Flambda2.Debug.concrete_types_only_on_canonicals

  let _flambda2_inline_max_depth spec =
    Clflags.Int_arg_helper.parse spec
      "Syntax: -flambda2-inline-max-depth <int> | <round>=<int>[,...]"
      Flambda2.Inlining.max_depth

  let _flambda2_inline_max_rec_depth spec =
    Clflags.Int_arg_helper.parse spec
      "Syntax: -flambda2-inline-max-rec-depth <int> | <round>=<int>[,...]"
      Flambda2.Inlining.max_rec_depth
  let _flambda2_inline_alloc_cost spec =
    Clflags.Float_arg_helper.parse spec
      "Syntax: -flambda2-inline-alloc-cost <float> | <round>=<float>[,...]"
      Flambda2.Inlining.alloc_cost

  let _flambda2_inline_branch_cost spec =
    Clflags.Float_arg_helper.parse spec
      "Syntax: -flambda2-inline-branch-cost <float> | <round>=<float>[,...]"
      Flambda2.Inlining.branch_cost

  let _flambda2_inline_call_cost spec =
    Clflags.Float_arg_helper.parse spec
      "Syntax: -flambda2-inline-call-cost <float> | <round>=<float>[,...]"
      Flambda2.Inlining.call_cost

  let _flambda2_inline_prim_cost spec =
    Clflags.Float_arg_helper.parse spec
      "Syntax: -flambda2-inline-prim-cost <float> | <round>=<float>[,...]"
      Flambda2.Inlining.prim_cost

  let _flambda2_inline_indirect_call_cost spec =
    Clflags.Float_arg_helper.parse spec
      "Syntax: -flambda2-inline-indirect-call-cost <float> | \
       <round>=<float>[,...]"
      Flambda2.Inlining.indirect_call_cost

  let _flambda2_inline_poly_compare_cost spec =
    Clflags.Float_arg_helper.parse spec
      "Syntax: -flambda2-inline-poly-compare-cost <float> | \
       <round>=<float>[,...]"
      Flambda2.Inlining.poly_compare_cost

  let _flambda2_inline_small_function_size spec =
    Clflags.Int_arg_helper.parse spec
      "Syntax: -flambda2-inline-small-function-size <int> | \
       <round>=<int>[,...]"
      Flambda2.Inlining.small_function_size

  let _flambda2_inline_large_function_size spec =
    Clflags.Int_arg_helper.parse spec
      "Syntax: -flambda2-inline-large-function-size <int> | \
       <round>=<int>[,...]"
      Flambda2.Inlining.large_function_size

  let _flambda2_inline_threshold spec =
    Clflags.Float_arg_helper.parse spec
      "Syntax: -flambda2-inline-threshold <float> | <round>=<float>[,...]"
      Flambda2.Inlining.threshold

  let _flambda2_speculative_inlining_only_if_arguments_useful =
    set Flambda2.Inlining.speculative_inlining_only_if_arguments_useful

  let _no_flambda2_speculative_inlining_only_if_arguments_useful =
    clear Flambda2.Inlining.speculative_inlining_only_if_arguments_useful

  let _flambda2_inlining_report_bin = set Flambda2.Inlining.report_bin

  let _flambda2_unicode = set Flambda2.unicode

  let _flambda2_treat_invalid_code_as_unreachable =
    set Flambda2.treat_invalid_code_as_unreachable
  let _no_flambda2_treat_invalid_code_as_unreachable =
    clear Flambda2.treat_invalid_code_as_unreachable

  let _drawfexpr = set Flambda2.Dump.rawfexpr
  let _dfexpr = set Flambda2.Dump.fexpr
  let _dflexpect = set Flambda2.Dump.flexpect
  let _dclosure_offsets = set Flambda2.Dump.closure_offsets
  let _dfreshen = set Flambda2.Dump.freshen
end

module Extra_params = struct
  let read_param ppf _position name v =
    let set option =
      Compenv.setter ppf (fun b -> b) name [ option ] v; true
    in
    let _clear option =
      Compenv.setter ppf (fun b -> not b) name [ option ] v; true
    in
    let set_int option =
      Compenv.int_setter ppf name option v; true
    in
    match name with
    (* override existing params *)
    | "Oclassic" ->
      if Compenv.check_bool ppf "Oclassic" v then
        Flambda_backend_flags.set_oclassic (); true
    | "O2" ->
      if Compenv.check_bool ppf "O2" v then
        Flambda_backend_flags.set_o2 (); true
    | "O3" ->
      if Compenv.check_bool ppf "O3" v then
        Flambda_backend_flags.set_o3 (); true
    (* define new params *)
    | "ocamlcfg" -> set Flambda_backend_flags.use_ocamlcfg
    | "flambda2-join-points" -> set Flambda2.join_points
    | "flambda2-unbox-along-intra-function-control-flow" ->
       set Flambda2.unbox_along_intra_function_control_flow
    | "flambda2-backend-cse-at-toplevel" ->
       set Flambda2.backend_cse_at_toplevel
    | "flambda2-cse-depth" ->
       set_int Flambda2.cse_depth
    | "flambda2-expert-inline-effects-in-cmm" ->
       set Flambda2.Expert.inline_effects_in_cmm
    | "flambda2-expert-phantom-lets" ->
       set Flambda2.Expert.phantom_lets
    | "flambda2-expert-max-unboxing-depth" ->
       set_int Flambda2.Expert.max_unboxing_depth
    | "flambda2-expert-can-inline-recursive-functions" ->
       set Flambda2.Expert.can_inline_recursive_functions
    | "flambda2-inline-max-depth" ->
       Clflags.Int_arg_helper.parse v
         "Bad syntax in OCAMLPARAM for 'flambda2-inline-max-depth'"
         Flambda2.Inlining.max_depth; true
    | "flambda2-inline-max-rec-depth" ->
       Clflags.Int_arg_helper.parse v
         "Bad syntax in OCAMLPARAM for 'flambda2-inline-max-rec-depth'"
         Flambda2.Inlining.max_rec_depth; true
    | "flambda2-inline-call-cost" ->
       Clflags.Float_arg_helper.parse v
         "Bad syntax in OCAMLPARAM for 'flambda2-inline-call-cost'"
         Flambda2.Inlining.call_cost; true
    | "flambda2-inline-alloc-cost" ->
       Clflags.Float_arg_helper.parse v
         "Bad syntax in OCAMLPARAM for 'flambda2-inline-alloc-cost'"
         Flambda2.Inlining.alloc_cost; true
    | "flambda2-inline-prim-cost" ->
       Clflags.Float_arg_helper.parse v
         "Bad syntax in OCAMLPARAM for 'flambda2-inline-prim-cost'"
         Flambda2.Inlining.prim_cost; true
    | "flambda2-inline-branch-cost" ->
       Clflags.Float_arg_helper.parse v
         "Bad syntax in OCAMLPARAM for 'flambda2-inline-branch-cost'"
         Flambda2.Inlining.branch_cost; true
    | "flambda2-inline-indirect-cost" ->
       Clflags.Float_arg_helper.parse v
         "Bad syntax in OCAMLPARAM for 'flambda2-inline-indirect-cost'"
         Flambda2.Inlining.indirect_call_cost; true
    | "flambda2-inline-poly-compare-cost" ->
       Clflags.Float_arg_helper.parse v
         "Bad syntax in OCAMLPARAM for 'flambda2-inline-poly-compare-cost'"
         Flambda2.Inlining.poly_compare_cost; true
    | "flambda2-inline-small-function-size" ->
       Clflags.Int_arg_helper.parse v
         "Bad syntax in OCAMLPARAM for 'flambda2-inline-small-function-size'"
         Flambda2.Inlining.small_function_size; true
    | "flambda2-inline-large-function-size" ->
       Clflags.Int_arg_helper.parse v
         "Bad syntax in OCAMLPARAM for 'flambda2-inline-large-function-size'"
         Flambda2.Inlining.large_function_size; true
    | "flambda2-inline-threshold" ->
       Clflags.Float_arg_helper.parse v
         "Bad syntax in OCAMLPARAM for 'flambda2-inline-threshold'"
         Flambda2.Inlining.threshold; true
    | "flambda2-speculative-inlining-only-if-arguments-useful" ->
       set Flambda2.Inlining.speculative_inlining_only_if_arguments_useful
    | "flambda2-treat-invalid-code-as-unreachable" ->
       set Flambda2.treat_invalid_code_as_unreachable
    | "flambda2-inlining-report-bin" ->
       set Flambda2.Inlining.report_bin
    | "flambda2-expert-code-id-and-symbol-scoping-checks" ->
       set Flambda2.Expert.code_id_and_symbol_scoping_checks
    | "flambda2-expert-fallback-inlining-heuristic" ->
       set Flambda2.Expert.fallback_inlining_heuristic
    | "flambda2-debug-permute-every-name" ->
       set Flambda2.Debug.permute_every_name
    | "flambda2-debug-concrete-types-only-on-canonicals" ->
       set Flambda2.Debug.concrete_types_only_on_canonicals
    | _ -> false
end

module type Optcomp_options = sig
  include Main_args.Optcomp_options
  include Flambda_backend_options
end

module type Opttop_options = sig
  include Main_args.Opttop_options
  include Flambda_backend_options
end

module Make_optcomp_options (F : Optcomp_options) =
struct
  include Make_flambda_backend_options(F)
  include Main_args.Make_optcomp_options(F)
  let list = list2 @ list
  let _o2 () = Flambda_backend_flags.set_o2 ()
  let _o3 () = Flambda_backend_flags.set_o3 ()
  let _classic_inlining () = Flambda_backend_flags.set_oclassic ()
end

module Make_opttop_options (F : Opttop_options) = struct
  include Make_flambda_backend_options(F)
  include Main_args.Make_opttop_options(F)
  let list = list2 @ list
  let _o2 () = Flambda_backend_flags.set_o2 ()
  let _o3 () = Flambda_backend_flags.set_o3 ()
  let _classic_inlining () = Flambda_backend_flags.set_oclassic ()
end

module Default = struct
  module Optmain = struct
    include Main_args.Default.Optmain
    include Flambda_backend_options
  end
  module Opttopmain = struct
    include Main_args.Default.Opttopmain
    include Flambda_backend_options
  end
end
