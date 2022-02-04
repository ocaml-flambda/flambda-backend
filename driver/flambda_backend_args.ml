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
let format_default flag = if flag then " (default)" else ""
let format_not_default flag = if flag then "" else " (default)"

let mk_ocamlcfg f =
  "-ocamlcfg", Arg.Unit f, " Use ocamlcfg"

let mk_no_ocamlcfg f =
  "-no-ocamlcfg", Arg.Unit f, " Do not use ocamlcfg"
;;

let mk_dcfg f =
  "-dcfg", Arg.Unit f, " (undocumented)"
;;

module Flambda2 = Flambda_backend_flags.Flambda2

let mk_flambda2_result_types_functors_only f =
  "-flambda2-result-types-functors-only", Arg.Unit f,
  Printf.sprintf " Infer result types for functors (but no other\n\
      \     functions)%s (Flambda 2 only)"
    (format_default (
      match Flambda2.Default.function_result_types with
      | Functors_only -> true
      | Never | All_functions -> false))
;;

let mk_flambda2_result_types_all_functions f =
  "-flambda2-result-types-all-functions", Arg.Unit f,
  Printf.sprintf " Infer result types for all functions\n\
      \     (including functors)%s (Flambda 2 only)"
    (format_default (
      match Flambda2.Default.function_result_types with
      | All_functions -> true
      | Never | Functors_only -> false))
;;

let mk_no_flambda2_result_types f =
  "-flambda2-result-types", Arg.Unit f,
  Printf.sprintf " Do not infer result types for functions (or\n\
      \     functors)%s (Flambda 2 only)"
    (format_default (
      match Flambda2.Default.function_result_types with
      | Never -> true
      | Functors_only | All_functions -> false))
;;


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

module Debugging = Dwarf_deps.Flags

let mk_g0 f =
  let help = " Do not generate debugging information (default)" in
  "-g0", Arg.Unit f, help
;;

let mk_g1 f =
  let help = " Generate basic debugging information" in
  "-g1", Arg.Unit f, help
;;

let mk_g2 f =
  let help =
    " As for `-g1', but generate simple DWARF information for module and\
      \n     function names, etc."
  in
  "-g2", Arg.Unit f, help
;;

let mk_g3 f =
  let help =
    " Generate DWARF information suitable for extensive use of a\n     \
      platform debugger"
  in
  "-g3", Arg.Unit f, help
;;

let mk_gdwarf_format f =
  let default =
    match Debugging.default_gdwarf_format with
    | Debugging.Thirty_two -> 32
    | Debugging.Sixty_four -> 64
  in
  "-gdwarf-format", Arg.Int f,
    Printf.sprintf "32|64  Set DWARF debug info format (default %d-bit)"
      default
;;

let mk_gdwarf_version f =
  let default =
    match Debugging.default_gdwarf_version with
    | Debugging.Four -> "4+gnu"
    | Debugging.Five -> "5"
  in
  "-gdwarf-version", Arg.String f,
    Printf.sprintf "5  Set DWARF debug info version (default %s; does not\
      \n     affect CFI or line number tables)" default
;;

let mk_gocamldebug f =
  let help =
    " Generate debugging information for use with `ocamldebug'\n     \
      (bytecode only)"
      ^ Debugging.describe_debug_default Debugging.Debug_ocamldebug
  in
  "-gocamldebug", Arg.Unit f, help
;;

let mk_gno_ocamldebug f =
  let help =
    " Do not generate debugging information for use with\n     \
      `ocamldebug' (bytecode only)"
      ^ Debugging.describe_debug_default_negated Debugging.Debug_subprocs
  in
  "-gno-ocamldebug", Arg.Unit f, help
;;

let mk_gjs_of_ocaml f =
  let help =
    " Generate debugging information for use with `js_of_ocaml'\n     \
      (bytecode only)"
      ^ Debugging.describe_debug_default Debugging.Debug_js_of_ocaml
  in
  "-gjs-of-ocaml", Arg.Unit f, help
;;

let mk_gno_js_of_ocaml f =
  let help =
    " Do not generate debugging information for use with\n     \
      `js_of_ocaml' (bytecode only)"
      ^ Debugging.describe_debug_default_negated Debugging.Debug_js_of_ocaml
  in
  "-gno-js-of-ocaml", Arg.Unit f, help
;;

let mk_gsubprocs f =
  let help =
    " Pass the `-g' option to subprocesses (C compiler, ppx,\
      \n     etc.)"
      ^ Debugging.describe_debug_default Debugging.Debug_subprocs
  in
  "-gsubprocs", Arg.Unit f, help
;;

let mk_gno_subprocs f =
  let help =
    " Do not pass the `-g' option to subprocesses (C compiler,\
      \n     linker, ppx, etc.)"
      ^ Debugging.describe_debug_default_negated Debugging.Debug_subprocs
  in
  "-gno-subprocs", Arg.Unit f, help
;;

let mk_gbacktraces f =
  let help =
    " Record backtraces and generate information to show source\
      \n     locations within them"
      ^ Debugging.describe_debug_default Debugging.Debug_backtraces
  in
  "-gbacktraces", Arg.Unit f, help
;;

let mk_gno_backtraces f =
  let help =
    " Do not record backtraces and generate information to show\
      \n     source locations within them"
      ^ Debugging.describe_debug_default_negated Debugging.Debug_backtraces
  in
  "-gno-backtraces", Arg.Unit f, help
;;

let mk_gbounds_checking f =
  let help =
    " Increase the accuracy of bounds-check-failure\
      \n     handlers for debugging (implies -gbacktraces)"
      ^ Debugging.describe_debug_default Debugging.Debug_bounds_checking
  in
  "-gbounds-checking-precision", Arg.Unit f, help
;;

let mk_gno_bounds_checking f =
  let help =
    " Do not increase the accuracy of\
      \n     bounds-check-failure handlers for debugging"
      ^ Debugging.describe_debug_default_negated Debugging.Debug_bounds_checking
  in
  "-gno-bounds-checking-precision", Arg.Unit f, help
;;

let mk_gdisable_bytecode_opt f =
  let help =
    " Disable certain optimisations to assist debugging\
      \n     (bytecode only)"
      ^ Debugging.describe_debug_default Debugging.Debug_disable_bytecode_opt
  in
  "-gdisable-bytecode-opt", Arg.Unit f, help
;;

let mk_gno_disable_bytecode_opt f =
  let help =
    " Do not disable certain optimisations to assist\
      \n     debugging (bytecode only)"
      ^ Debugging.describe_debug_default_negated
          Debugging.Debug_disable_bytecode_opt
  in
  "-gno-disable-bytecode-opt", Arg.Unit f, help
;;

let mk_gdwarf_cfi f =
  let help =
    " Describe call frame information in DWARF, enabling stack\
      \n     unwinding and backtraces in platform debuggers (implies \
      -gsubprocs)\n    "
      ^ Debugging.describe_debug_default Debugging.Debug_dwarf_cfi
  in
  "-gdwarf-cfi", Arg.Unit f, help
;;

let mk_gno_dwarf_cfi f =
  let help =
    " Do not describe call frame information in DWARF\n    "
      ^ Debugging.describe_debug_default_negated Debugging.Debug_dwarf_cfi
  in
  "-gno-dwarf-cfi", Arg.Unit f, help
;;

let mk_gdwarf_loc f =
  let help =
    " Describe source location information in DWARF (implies\
      \n     -gdwarf-cfi)"
      ^ Debugging.describe_debug_default Debugging.Debug_dwarf_loc
  in
  "-gdwarf-loc", Arg.Unit f, help
;;

let mk_gno_dwarf_loc f =
  let help =
    " Do not describe source location information in DWARF\n    "
      ^ Debugging.describe_debug_default_negated Debugging.Debug_dwarf_loc
  in
  "-gno-dwarf-loc", Arg.Unit f, help
;;

let mk_gdwarf_scopes f =
  let help =
    " Describe variable and inlined frame scoping in DWARF\
      \n     (implies -gdwarf-loc)"
      ^ Debugging.describe_debug_default Debugging.Debug_dwarf_scopes
  in
  "-gdwarf-scopes", Arg.Unit f, help
;;

let mk_gno_dwarf_scopes f =
  let help =
    " Do not describe variable and inlined frame scoping in DWARF\
      \n    "
      ^ Debugging.describe_debug_default_negated Debugging.Debug_dwarf_scopes
  in
  "-gno-dwarf-scopes", Arg.Unit f, help
;;

let mk_gdwarf_vars f =
  let help =
    " Describe variables and function parameters in DWARF\n     \
      (implies -gdwarf-scopes and -bin-annot)"
      ^ Debugging.describe_debug_default Debugging.Debug_dwarf_vars
  in
  "-gdwarf-vars", Arg.Unit f, help
;;

let mk_gno_dwarf_vars f =
  let help =
    " Do not describe variables and function parameters in DWARF\
      \n    "
      ^ Debugging.describe_debug_default_negated Debugging.Debug_dwarf_vars
  in
  "-gno-dwarf-vars", Arg.Unit f, help
;;

let mk_gdwarf_call_sites f =
  let help =
    " Describe call sites (and the arguments at such) in\n     DWARF \
      (implies -gdwarf-vars)"
      ^ Debugging.describe_debug_default Debugging.Debug_dwarf_call_sites
  in
  "-gdwarf-call-sites", Arg.Unit f, help
;;

let mk_gno_dwarf_call_sites f =
  let help =
    " Do not describe call sites (and the arguments at such)\n     in DWARF"
      ^ Debugging.describe_debug_default_negated Debugging.Debug_dwarf_call_sites
  in
  "-gno-dwarf-call-sites", Arg.Unit f, help
;;

let mk_gdwarf_cmm f =
  let help =
    " Generate debugging information for functions fabricated by the\n     \
      compiler in the Cmm language and the corresponding .cmm source file\
      \n     (implies -gdwarf-vars)"
      ^ Debugging.describe_debug_default Debugging.Debug_dwarf_cmm
  in
  "-gdwarf-cmm", Arg.Unit f, help
;;

let mk_gno_dwarf_cmm f =
  let help =
    " Neither generate debugging information for functions \n     \
      fabricated by the compiler in the Cmm language nor the corresponding\
      \n     .cmm source file"
      ^ Debugging.describe_debug_default_negated Debugging.Debug_dwarf_cmm
  in
  "-gno-dwarf-cmm", Arg.Unit f, help
;;

let mk_gdwarf_offsets f =
  let help =
    " Generate offset arrays in DWARF-5 location and range list\n     tables"
      ^ (if Debugging.default_gdwarf_offsets then " (default)" else "")
  in
  "-gdwarf-offsets", Arg.Unit f, help
;;

let mk_gno_dwarf_offsets f =
  let help =
    " Do not generate offset arrays in DWARF-5 location and\n     \
      range list tables"
      ^ (if not Debugging.default_gdwarf_offsets then " (default)" else "")
  in
  "-gno-dwarf-offsets", Arg.Unit f, help
;;

let mk_gdwarf_self_tail_calls f =
  let help =
    " Generate DW_TAG_call_site for self tail calls\n     (DWARF-5 only, but \
        not strictly DWARF-5 compliant)"
      ^ (if Debugging.default_gdwarf_self_tail_calls then " (default)" else "")
  in
  "-gdwarf-self-tail-calls", Arg.Unit f, help
;;

let mk_gno_dwarf_self_tail_calls f =
  let help =
    " Do not generate DW_TAG_call_site for self tail\n     calls \
        (DWARF-5 only, but not strictly DWARF-5 compliant)"
      ^ (if not Debugging.default_gdwarf_self_tail_calls
         then " (default)" else "")
  in
  "-gno-dwarf-self-tail-calls", Arg.Unit f, help
;;

let mk_ddebug_invariants f =
  let help =
    " Check invariants during debugging information generation\n     passes"
      ^ (if Debugging.default_ddebug_invariants then " (default)" else "")
  in
  "-ddebug-invariants", Arg.Unit f, help
;;

let mk_dno_debug_invariants f =
  let help =
    " Do not check invariants during debugging information\n     generation \
      passes"
      ^ (if not Debugging.default_ddebug_invariants then " (default)" else "")
  in
  "-dno-debug-invariants", Arg.Unit f, help
;;

module type Flambda_backend_options = sig
  val ocamlcfg : unit -> unit
  val no_ocamlcfg : unit -> unit
  val dcfg : unit -> unit

  val flambda2_join_points : unit -> unit
  val no_flambda2_join_points : unit -> unit
  val flambda2_result_types_functors_only : unit -> unit
  val flambda2_result_types_all_functions : unit -> unit
  val no_flambda2_result_types : unit -> unit
  val flambda2_unbox_along_intra_function_control_flow : unit -> unit
  val no_flambda2_unbox_along_intra_function_control_flow : unit -> unit
  val flambda2_backend_cse_at_toplevel : unit -> unit
  val no_flambda2_backend_cse_at_toplevel : unit -> unit
  val flambda2_cse_depth : int -> unit
  val flambda2_expert_code_id_and_symbol_scoping_checks : unit -> unit
  val no_flambda2_expert_code_id_and_symbol_scoping_checks : unit -> unit
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
  val flambda2_debug_permute_every_name : unit -> unit
  val no_flambda2_debug_permute_every_name : unit -> unit
  val flambda2_debug_concrete_types_only_on_canonicals : unit -> unit
  val no_flambda2_debug_concrete_types_only_on_canonicals : unit -> unit

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

  val flambda2_treat_invalid_code_as_unreachable : unit -> unit
  val no_flambda2_treat_invalid_code_as_unreachable : unit -> unit

  val drawfexpr : unit -> unit
  val dfexpr : unit -> unit
  val dflexpect : unit -> unit
  val dclosure_offsets : unit -> unit
  val dfreshen : unit -> unit
end

module Make_flambda_backend_options (F : Flambda_backend_options) =
struct
  let list2 = [
    mk_ocamlcfg F.ocamlcfg;
    mk_no_ocamlcfg F.no_ocamlcfg;
    mk_dcfg F.dcfg;

    mk_flambda2_join_points F.flambda2_join_points;
    mk_no_flambda2_join_points F.no_flambda2_join_points;
    mk_flambda2_result_types_functors_only
      F.flambda2_result_types_functors_only;
    mk_flambda2_result_types_all_functions
      F.flambda2_result_types_all_functions;
    mk_no_flambda2_result_types
      F.no_flambda2_result_types;
    mk_flambda2_unbox_along_intra_function_control_flow
      F.flambda2_unbox_along_intra_function_control_flow;
    mk_no_flambda2_unbox_along_intra_function_control_flow
      F.no_flambda2_unbox_along_intra_function_control_flow;
    mk_flambda2_backend_cse_at_toplevel F.flambda2_backend_cse_at_toplevel;
    mk_no_flambda2_backend_cse_at_toplevel
      F.no_flambda2_backend_cse_at_toplevel;
    mk_flambda2_cse_depth F.flambda2_cse_depth;
    mk_flambda2_expert_code_id_and_symbol_scoping_checks
      F.flambda2_expert_code_id_and_symbol_scoping_checks;
    mk_no_flambda2_expert_code_id_and_symbol_scoping_checks
      F.no_flambda2_expert_code_id_and_symbol_scoping_checks;
    mk_flambda2_expert_fallback_inlining_heuristic
      F.flambda2_expert_fallback_inlining_heuristic;
    mk_no_flambda2_expert_fallback_inlining_heuristic
      F.no_flambda2_expert_fallback_inlining_heuristic;
    mk_flambda2_expert_inline_effects_in_cmm
      F.flambda2_expert_inline_effects_in_cmm;
    mk_no_flambda2_expert_inline_effects_in_cmm
      F.no_flambda2_expert_inline_effects_in_cmm;
    mk_flambda2_expert_phantom_lets
      F.flambda2_expert_phantom_lets;
    mk_no_flambda2_expert_phantom_lets
      F.no_flambda2_expert_phantom_lets;
    mk_flambda2_expert_max_block_size_for_projections
      F.flambda2_expert_max_block_size_for_projections;
    mk_flambda2_expert_max_unboxing_depth
      F.flambda2_expert_max_unboxing_depth;
    mk_flambda2_expert_can_inline_recursive_functions
      F.flambda2_expert_can_inline_recursive_functions;
    mk_no_flambda2_expert_can_inline_recursive_functions
      F.no_flambda2_expert_can_inline_recursive_functions;
    mk_flambda2_debug_permute_every_name
      F.flambda2_debug_permute_every_name;
    mk_no_flambda2_debug_permute_every_name
      F.no_flambda2_debug_permute_every_name;
    mk_flambda2_debug_concrete_types_only_on_canonicals
      F.flambda2_debug_concrete_types_only_on_canonicals;
    mk_no_flambda2_debug_concrete_types_only_on_canonicals
      F.no_flambda2_debug_concrete_types_only_on_canonicals;

    mk_flambda2_inline_max_depth F.flambda2_inline_max_depth;
    mk_flambda2_inline_max_rec_depth F.flambda2_inline_max_rec_depth;
    mk_flambda2_inline_alloc_cost F.flambda2_inline_alloc_cost;
    mk_flambda2_inline_branch_cost F.flambda2_inline_branch_cost;
    mk_flambda2_inline_call_cost F.flambda2_inline_call_cost;
    mk_flambda2_inline_prim_cost F.flambda2_inline_prim_cost;
    mk_flambda2_inline_indirect_call_cost F.flambda2_inline_indirect_call_cost;
    mk_flambda2_inline_poly_compare_cost F.flambda2_inline_poly_compare_cost;
    mk_flambda2_inline_small_function_size
      F.flambda2_inline_small_function_size;
    mk_flambda2_inline_large_function_size
      F.flambda2_inline_large_function_size;
    mk_flambda2_inline_threshold F.flambda2_inline_threshold;
    mk_flambda2_speculative_inlining_only_if_arguments_useful
      F.flambda2_speculative_inlining_only_if_arguments_useful;
    mk_no_flambda2_speculative_inlining_only_if_arguments_useful
      F.no_flambda2_speculative_inlining_only_if_arguments_useful;

    mk_flambda2_inlining_report_bin F.flambda2_inlining_report_bin;

    mk_flambda2_unicode F.flambda2_unicode;

    mk_flambda2_treat_invalid_code_as_unreachable
      F.flambda2_treat_invalid_code_as_unreachable;
    mk_no_flambda2_treat_invalid_code_as_unreachable
      F.no_flambda2_treat_invalid_code_as_unreachable;

    mk_drawfexpr F.drawfexpr;
    mk_dfexpr F.dfexpr;
    mk_dflexpect F.dflexpect;
    mk_dclosure_offsets F.dclosure_offsets;
    mk_dfreshen F.dfreshen;
  ]
end

module Flambda_backend_options_impl = struct
  let set r () = r := true
  let clear r () = r := false

  let ocamlcfg = set Flambda_backend_flags.use_ocamlcfg
  let no_ocamlcfg = clear Flambda_backend_flags.use_ocamlcfg
  let dcfg = set Flambda_backend_flags.dump_cfg

  let flambda2_join_points = set Flambda2.join_points
  let no_flambda2_join_points = clear Flambda2.join_points
  let flambda2_result_types_functors_only () =
    Flambda2.function_result_types := Flambda_backend_flags.Functors_only
  let flambda2_result_types_all_functions () =
    Flambda2.function_result_types := Flambda_backend_flags.All_functions
  let no_flambda2_result_types () =
    Flambda2.function_result_types := Flambda_backend_flags.Never
  let flambda2_unbox_along_intra_function_control_flow =
    set Flambda2.unbox_along_intra_function_control_flow
  let no_flambda2_unbox_along_intra_function_control_flow =
    clear Flambda2.unbox_along_intra_function_control_flow
  let flambda2_backend_cse_at_toplevel =
    set Flambda2.backend_cse_at_toplevel
  let no_flambda2_backend_cse_at_toplevel =
    clear Flambda2.backend_cse_at_toplevel
  let flambda2_cse_depth n = Flambda2.cse_depth := n
  let flambda2_expert_code_id_and_symbol_scoping_checks =
    set Flambda2.Expert.code_id_and_symbol_scoping_checks
  let no_flambda2_expert_code_id_and_symbol_scoping_checks =
    clear Flambda2.Expert.code_id_and_symbol_scoping_checks
  let flambda2_expert_fallback_inlining_heuristic =
    set Flambda2.Expert.fallback_inlining_heuristic
  let no_flambda2_expert_fallback_inlining_heuristic =
    clear Flambda2.Expert.fallback_inlining_heuristic
  let flambda2_expert_inline_effects_in_cmm =
    set Flambda2.Expert.inline_effects_in_cmm
  let no_flambda2_expert_inline_effects_in_cmm =
    clear Flambda2.Expert.inline_effects_in_cmm
  let flambda2_expert_phantom_lets =
    set Flambda2.Expert.phantom_lets
  let no_flambda2_expert_phantom_lets =
    clear Flambda2.Expert.phantom_lets
  let flambda2_expert_max_block_size_for_projections size =
    Flambda2.Expert.max_block_size_for_projections := Some size
  let flambda2_expert_max_unboxing_depth depth =
    Flambda2.Expert.max_unboxing_depth := depth
  let flambda2_expert_can_inline_recursive_functions () =
    Flambda2.Expert.can_inline_recursive_functions := true
  let no_flambda2_expert_can_inline_recursive_functions () =
    Flambda2.Expert.can_inline_recursive_functions := false
  let flambda2_debug_permute_every_name =
    set Flambda2.Debug.permute_every_name
  let no_flambda2_debug_permute_every_name =
    clear Flambda2.Debug.permute_every_name
  let flambda2_debug_concrete_types_only_on_canonicals =
    set Flambda2.Debug.concrete_types_only_on_canonicals
  let no_flambda2_debug_concrete_types_only_on_canonicals =
    clear Flambda2.Debug.concrete_types_only_on_canonicals

  let flambda2_inline_max_depth spec =
    Clflags.Int_arg_helper.parse spec
      "Syntax: -flambda2-inline-max-depth <int> | <round>=<int>[,...]"
      Flambda2.Inlining.max_depth

  let flambda2_inline_max_rec_depth spec =
    Clflags.Int_arg_helper.parse spec
      "Syntax: -flambda2-inline-max-rec-depth <int> | <round>=<int>[,...]"
      Flambda2.Inlining.max_rec_depth
  let flambda2_inline_alloc_cost spec =
    Clflags.Float_arg_helper.parse spec
      "Syntax: -flambda2-inline-alloc-cost <float> | <round>=<float>[,...]"
      Flambda2.Inlining.alloc_cost

  let flambda2_inline_branch_cost spec =
    Clflags.Float_arg_helper.parse spec
      "Syntax: -flambda2-inline-branch-cost <float> | <round>=<float>[,...]"
      Flambda2.Inlining.branch_cost

  let flambda2_inline_call_cost spec =
    Clflags.Float_arg_helper.parse spec
      "Syntax: -flambda2-inline-call-cost <float> | <round>=<float>[,...]"
      Flambda2.Inlining.call_cost

  let flambda2_inline_prim_cost spec =
    Clflags.Float_arg_helper.parse spec
      "Syntax: -flambda2-inline-prim-cost <float> | <round>=<float>[,...]"
      Flambda2.Inlining.prim_cost

  let flambda2_inline_indirect_call_cost spec =
    Clflags.Float_arg_helper.parse spec
      "Syntax: -flambda2-inline-indirect-call-cost <float> | \
       <round>=<float>[,...]"
      Flambda2.Inlining.indirect_call_cost

  let flambda2_inline_poly_compare_cost spec =
    Clflags.Float_arg_helper.parse spec
      "Syntax: -flambda2-inline-poly-compare-cost <float> | \
       <round>=<float>[,...]"
      Flambda2.Inlining.poly_compare_cost

  let flambda2_inline_small_function_size spec =
    Clflags.Int_arg_helper.parse spec
      "Syntax: -flambda2-inline-small-function-size <int> | \
       <round>=<int>[,...]"
      Flambda2.Inlining.small_function_size

  let flambda2_inline_large_function_size spec =
    Clflags.Int_arg_helper.parse spec
      "Syntax: -flambda2-inline-large-function-size <int> | \
       <round>=<int>[,...]"
      Flambda2.Inlining.large_function_size

  let flambda2_inline_threshold spec =
    Clflags.Float_arg_helper.parse spec
      "Syntax: -flambda2-inline-threshold <float> | <round>=<float>[,...]"
      Flambda2.Inlining.threshold

  let flambda2_speculative_inlining_only_if_arguments_useful =
    set Flambda2.Inlining.speculative_inlining_only_if_arguments_useful

  let no_flambda2_speculative_inlining_only_if_arguments_useful =
    clear Flambda2.Inlining.speculative_inlining_only_if_arguments_useful

  let flambda2_inlining_report_bin = set Flambda2.Inlining.report_bin

  let flambda2_unicode = set Flambda2.unicode

  let flambda2_treat_invalid_code_as_unreachable =
    set Flambda2.treat_invalid_code_as_unreachable
  let no_flambda2_treat_invalid_code_as_unreachable =
    clear Flambda2.treat_invalid_code_as_unreachable

  let drawfexpr = set Flambda2.Dump.rawfexpr
  let dfexpr = set Flambda2.Dump.fexpr
  let dflexpect = set Flambda2.Dump.flexpect
  let dclosure_offsets = set Flambda2.Dump.closure_offsets
  let dfreshen = set Flambda2.Dump.freshen
end

module type Debugging_options = sig
  val _g0 : unit -> unit
  val _g1 : unit -> unit
  val _g2 : unit -> unit
  val _g3 : unit -> unit

  val _gjs_of_ocaml : unit -> unit
  val _gno_js_of_ocaml : unit -> unit
  val _gocamldebug : unit -> unit
  val _gno_ocamldebug : unit -> unit
  val _gsubprocs : unit -> unit
  val _gno_subprocs : unit -> unit
  val _gbacktraces : unit -> unit
  val _gno_backtraces : unit -> unit
  val _gbounds_checking : unit -> unit
  val _gno_bounds_checking : unit -> unit
  val _gdisable_bytecode_opt : unit -> unit
  val _gno_disable_bytecode_opt : unit -> unit

  val _gdwarf_format : int -> unit
  val _gdwarf_version : string -> unit

  val _gdwarf_cfi : unit -> unit
  val _gno_dwarf_cfi : unit -> unit
  val _gdwarf_loc : unit -> unit
  val _gno_dwarf_loc : unit -> unit
  val _gdwarf_scopes : unit -> unit
  val _gno_dwarf_scopes : unit -> unit
  val _gdwarf_vars : unit -> unit
  val _gno_dwarf_vars : unit -> unit
  val _gdwarf_call_sites : unit -> unit
  val _gno_dwarf_call_sites : unit -> unit
  val _gdwarf_cmm : unit -> unit
  val _gno_dwarf_cmm : unit -> unit

  val _gdwarf_offsets : unit -> unit
  val _gno_dwarf_offsets : unit -> unit
  val _gdwarf_self_tail_calls : unit -> unit
  val _gno_dwarf_self_tail_calls : unit -> unit

  val _ddebug_invariants : unit -> unit
  val _dno_debug_invariants : unit -> unit
end

module Make_debugging_options (F : Debugging_options) = struct
  let list3 = [
    mk_g0 F._g0;
    mk_g1 F._g1;
    mk_g2 F._g2;
    mk_g3 F._g3;
    mk_gdwarf_format F._gdwarf_format;
    mk_gdwarf_version F._gdwarf_version;

    mk_gjs_of_ocaml F._gjs_of_ocaml;
    mk_gno_js_of_ocaml F._gno_js_of_ocaml;
    mk_gocamldebug F._gocamldebug;
    mk_gno_ocamldebug F._gno_ocamldebug;
    mk_gsubprocs F._gsubprocs;
    mk_gno_subprocs F._gno_subprocs;
    mk_gbacktraces F._gbacktraces;
    mk_gno_backtraces F._gno_backtraces;
    mk_gbounds_checking F._gbounds_checking;
    mk_gno_bounds_checking F._gno_bounds_checking;
    mk_gdisable_bytecode_opt F._gdisable_bytecode_opt;
    mk_gno_disable_bytecode_opt F._gno_disable_bytecode_opt;

    mk_gdwarf_cfi F._gdwarf_cfi;
    mk_gno_dwarf_cfi F._gno_dwarf_cfi;
    mk_gdwarf_loc F._gdwarf_loc;
    mk_gno_dwarf_loc F._gno_dwarf_loc;
    mk_gdwarf_scopes F._gdwarf_scopes;
    mk_gno_dwarf_scopes F._gno_dwarf_scopes;
    mk_gdwarf_vars F._gdwarf_vars;
    mk_gno_dwarf_vars F._gno_dwarf_vars;
    mk_gdwarf_call_sites F._gdwarf_call_sites;
    mk_gno_dwarf_call_sites F._gno_dwarf_call_sites;
    mk_gdwarf_cmm F._gdwarf_cmm;
    mk_gno_dwarf_cmm F._gno_dwarf_cmm;

    mk_gdwarf_offsets F._gdwarf_offsets;
    mk_gno_dwarf_offsets F._gno_dwarf_offsets;
    mk_gdwarf_self_tail_calls F._gdwarf_self_tail_calls;
    mk_gno_dwarf_self_tail_calls F._gno_dwarf_self_tail_calls;

    mk_ddebug_invariants F._ddebug_invariants;
    mk_dno_debug_invariants F._dno_debug_invariants;
   ]
end

module Debugging_options_impl = struct
  let set r () = r := true
  let clear r () = r := false

  let _g0 () = Debugging.use_g0 ()
  let _g1 () = Debugging.use_g1 ()
  let _g2 () = Debugging.use_g2 ()
  let _g3 () = Debugging.use_g3 ()

  let _gocamldebug () = Debugging.set_debug_thing Debug_ocamldebug
  let _gno_ocamldebug () = Debugging.clear_debug_thing Debug_ocamldebug
  let _gjs_of_ocaml () = Debugging.set_debug_thing Debug_js_of_ocaml
  let _gno_js_of_ocaml () = Debugging.clear_debug_thing Debug_js_of_ocaml
  let _gsubprocs () = Debugging.set_debug_thing Debug_subprocs
  let _gno_subprocs () = Debugging.clear_debug_thing Debug_subprocs
  let _gbacktraces () = Debugging.set_debug_thing Debug_backtraces
  let _gno_backtraces () = Debugging.clear_debug_thing Debug_backtraces
  let _gbounds_checking () =
    Debugging.set_debug_thing Debug_bounds_checking;
    _gbacktraces ()
  let _gno_bounds_checking () = Debugging.clear_debug_thing Debug_bounds_checking
  let _gdisable_bytecode_opt () = Debugging.set_debug_thing Debug_disable_bytecode_opt
  let _gno_disable_bytecode_opt () =
    Debugging.clear_debug_thing Debug_disable_bytecode_opt

  let _gdwarf_format format =
    match format with
    | 32 -> Debugging.gdwarf_format := Debugging.Thirty_two
    | 64 -> Debugging.gdwarf_format := Debugging.Sixty_four
    | _ -> Compenv.fatal "Please specify `32' or `64' for -gdwarf-format"

  let _gdwarf_version version =
    match version with
    | "4+gnu" -> Debugging.gdwarf_version := Debugging.Four
    | "5" -> Debugging.gdwarf_version := Debugging.Five
    | _ -> Compenv.fatal "Please specify `4+gnu' or `5' for -gdwarf-version"

  let _gdwarf_cfi () =
    Debugging.set_debug_thing Debug_dwarf_cfi;
    _gsubprocs ()

  let _gno_dwarf_cfi () =
    Debugging.clear_debug_thing Debug_dwarf_cfi

  let _gdwarf_loc () =
    Debugging.set_debug_thing Debug_dwarf_loc;
    _gdwarf_cfi ()

  let _gno_dwarf_loc () =
    Debugging.clear_debug_thing Debug_dwarf_loc

  let _gdwarf_scopes () =
    Debugging.set_debug_thing Debug_dwarf_scopes;
    _gdwarf_loc ()

  let _gno_dwarf_scopes () =
    Debugging.clear_debug_thing Debug_dwarf_scopes

  let _gdwarf_vars () =
    Debugging.set_debug_thing Debug_dwarf_vars;
    _gdwarf_scopes ();
    set Clflags.binary_annotations ()

  let _gno_dwarf_vars () =
    Debugging.clear_debug_thing Debug_dwarf_vars

  let _gdwarf_call_sites () =
    Debugging.set_debug_thing Debug_dwarf_call_sites;
    _gdwarf_vars ()

  let _gno_dwarf_call_sites () =
    Debugging.clear_debug_thing Debug_dwarf_call_sites

  let _gdwarf_cmm () =
    Debugging.set_debug_thing Debug_dwarf_cmm;
    _gdwarf_vars ()

  let _gno_dwarf_cmm () =
    Debugging.clear_debug_thing Debug_dwarf_cmm

  let _gdwarf_offsets = set Debugging.gdwarf_offsets
  let _gno_dwarf_offsets = clear Debugging.gdwarf_offsets
  let _gdwarf_self_tail_calls = set Debugging.gdwarf_self_tail_calls
  let _gno_dwarf_self_tail_calls = clear Debugging.gdwarf_self_tail_calls

  let _ddebug_invariants = set Debugging.ddebug_invariants
  let _dno_debug_invariants = clear Debugging.ddebug_invariants
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
    | "flambda2-result-types" ->
      (match String.lowercase_ascii v with
      | "never" ->
        Flambda2.function_result_types := Flambda_backend_flags.Never
      | "functors-only" ->
        Flambda2.function_result_types := Flambda_backend_flags.Functors_only
      | "all-functions" ->
        Flambda2.function_result_types := Flambda_backend_flags.All_functions
      | _ ->
        Misc.fatal_error "Syntax: flambda2-result-types=\
          never|functors-only|all-functions");
      true
    | "flambda2-result-types-all-functions" ->
      Flambda2.function_result_types := Flambda_backend_flags.All_functions;
      true
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
  include Debugging_options
end

module type Opttop_options = sig
  include Main_args.Opttop_options
  include Flambda_backend_options
  include Debugging_options
end

module Make_optcomp_options (F : Optcomp_options) =
struct
  include Make_debugging_options(F)  (* provides [list3]  *)
  include Make_flambda_backend_options(F)  (* provides [list2]  *)
  include Main_args.Make_optcomp_options(F)  (* provides [list] *)
  (* Overwrite [list] with the combination of the above options.
     If the same string input can be recognized by two options,
     the flambda-backend implementation will take precedence,
     but this should be avoided. To override an option from Main_args,
     redefine it in the implementation of this functor's argument.
     See the approach below for _o3 in Default. *)
  let list = list3 @ list2 @ list
end

module Make_opttop_options (F : Opttop_options) = struct
  include Make_debugging_options(F)
  include Make_flambda_backend_options(F)
  include Main_args.Make_opttop_options(F)
  let list = list3 @ list2 @ list
end

module Default = struct
  module Optmain = struct
    include Main_args.Default.Optmain
    include Flambda_backend_options_impl
    include Debugging_options_impl
    let _o2 () = Flambda_backend_flags.set_o2 ()
    let _o3 () = Flambda_backend_flags.set_o3 ()
    let _classic_inlining () = Flambda_backend_flags.set_oclassic ()
  end
  module Opttopmain = struct
    include Main_args.Default.Opttopmain
    include Flambda_backend_options_impl
    include Debugging_options_impl
    let _o2 () = Flambda_backend_flags.set_o2 ()
    let _o3 () = Flambda_backend_flags.set_o3 ()
    let _classic_inlining () = Flambda_backend_flags.set_oclassic ()
  end
end
