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

let mk_flambda2_debug f =
  "-flambda2-debug", Arg.Unit f, " Enable debug output for the Flambda2 pass"

let mk_no_flambda2_debug f =
  "-no-flambda2-debug", Arg.Unit f, " Disable debug output for the Flambda2 pass"

let mk_ocamlcfg f =
  "-ocamlcfg", Arg.Unit f, " Use ocamlcfg"

let mk_no_ocamlcfg f =
  "-no-ocamlcfg", Arg.Unit f, " Do not use ocamlcfg"
;;

let mk_dcfg f =
  "-dcfg", Arg.Unit f, " (undocumented)"
;;

let mk_dcfg_invariants f =
  "-dcfg-invariants", Arg.Unit f, " Extra sanity checks on Cfg"

let mk_dcfg_equivalence_check f =
  "-dcfg-equivalence-check", Arg.Unit f, " Extra sanity checks on Cfg transformations"

let mk_regalloc f =
  "-regalloc", Arg.String f, " Select the register allocator"

let mk_regalloc_param f =
  "-regalloc-param", Arg.String f, " Pass a parameter to the register allocator"

let mk_regalloc_validate f =
  "-regalloc-validate", Arg.Unit f, " Validate register allocation"

let mk_no_regalloc_validate f =
  "-no-regalloc-validate", Arg.Unit f, " Do not validate register allocation"

let mk_cfg_peephole_optimize f =
  "-cfg-peephole-optimize", Arg.Unit f, " Apply peephole optimizations to CFG"

let mk_no_cfg_peephole_optimize f =
  "-no-cfg-peephole-optimize", Arg.Unit f, " Do not apply peephole optimizations to CFG"

let mk_cfg_cse_optimize f =
  "-cfg-cse-optimize", Arg.Unit f, " Apply CSE optimizations to CFG"

let mk_no_cfg_cse_optimize f =
  "-no-cfg-cse-optimize", Arg.Unit f, " Do not apply CSE optimizations to CFG"

let mk_reorder_blocks_random f =
  "-reorder-blocks-random",
  Arg.Int f,
  Printf.sprintf "<seed> Randomly reorder basic blocks in every function, \
                  using the provided seed (intended for testing, off by default)."

let mk_basic_block_sections f =
  if Config.function_sections then
    "-basic-block-sections",  Arg.Unit f,
    " Emit each basic block in a separate section if target supports it. Requires \
     -ocamlcfg."
  else
    let err () =
      raise (Arg.Bad "OCaml has been configured without support for \
                      -function-sections which is required for -basic-block-sections")
    in
    "-basic-block-sections", Arg.Unit err, " (option not available)"
;;

let mk_dasm_comments f =
  "-dasm-comments", Arg.Unit f, " Add comments in .s files (e.g. for DWARF)"

let mk_dno_asm_comments f =
  "-dno-asm-comments", Arg.Unit f, " Do not add comments in .s files"

let mk_no_stack_checks_genfuncs f =
  "-no-stack-checks-genfuncs", Arg.Unit f, " Disable stack checks for generated functions"

let mk_heap_reduction_threshold f =
  "-heap-reduction-threshold",
  Arg.Int f,
  Printf.sprintf " Threshold (in major words, defaulting to %d) to trigger a heap reduction before code emission"
    Flambda_backend_flags.default_heap_reduction_threshold
;;

let mk_zero_alloc_check f =
  let annotations = Clflags.Annotations.(List.map to_string all) in
  "-zero-alloc-check", Arg.Symbol (annotations, f),
  " Check that annotated functions do not allocate \
   and do not have indirect calls. "^Clflags.Annotations.doc

let mk_dcheckmach f =
  "-dcheckmach", Arg.Unit f, " (undocumented)"

let mk_checkmach_details_cutoff f =
  "-checkmach-details-cutoff", Arg.Int f,
  Printf.sprintf " Do not show more than this number of error locations \
                  in each function that fails the check \
                  (default %d, negative to show all)"
    (match Flambda_backend_flags.default_checkmach_details_cutoff with
     | Keep_all -> (-1)
     | No_details -> 0
     | At_most n -> n)

let mk_function_layout f =
  let layouts = Flambda_backend_flags.Function_layout.(List.map to_string all) in
  let default = Flambda_backend_flags.Function_layout.(to_string default) in
  "-function-layout", Arg.Symbol (layouts, f),
  (Printf.sprintf " Order of functions in the generated assembly (default: %s)"
     default)

let mk_disable_poll_insertion f =
  "-disable-poll-insertion", Arg.Unit f, " Do not insert poll points"

let mk_enable_poll_insertion f =
  "-enable-poll-insertion", Arg.Unit f, " Insert poll points"

let mk_long_frames f =
  "-long-frames", Arg.Unit f, " Allow stack frames longer than 2^16 bytes"

let mk_no_long_frames f =
  "-no-long-frames", Arg.Unit f, " Do not allow stack frames longer than 2^16 bytes"

let mk_debug_long_frames_threshold f =
  "-debug-long-frames-threshold", Arg.Int f, "n debug only: set long frames threshold"

let mk_caml_apply_inline_fast_path f =
  "-caml-apply-inline-fast-path", Arg.Unit f, " Inline the fast path of caml_applyN"

let mk_dump_inlining_paths f =
  "-dump-inlining-paths", Arg.Unit f, " Dump inlining paths when dumping flambda2 terms"

let mk_davail f =
  "-davail", Arg.Unit f, " Dump register availability information"

let mk_dranges f =
  "-dranges", Arg.Unit f, " Dump results of Compute_ranges"

let mk_ddebug_invariants f =
  "-ddebug-invariants", Arg.Unit f, " Run invariant checks during generation of debugging information"

let mk_internal_assembler f =
  "-internal-assembler", Arg.Unit f, "Write object files directly instead of using the system assembler (x86-64 ELF only)"

let mk_gc_timings f =
  "-dgc-timings", Arg.Unit f, "Output information about time spent in the GC"

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

let mk_flambda2_basic_meet f =
  "-flambda2-basic-meet", Arg.Unit f,
  Printf.sprintf " Use a basic meet algorithm%s (Flambda 2 only)"
    (format_default (
      match Flambda2.Default.meet_algorithm with
      | Basic -> true
      | Advanced -> false))
;;

let mk_flambda2_advanced_meet f =
  "-flambda2-advanced-meet", Arg.Unit f,
  Printf.sprintf " Use an advanced meet algorithm%s (Flambda 2 only)"
    (format_default (
      match Flambda2.Default.meet_algorithm with
      | Basic -> false
      | Advanced -> true))
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

let mk_flambda2_join_depth f =
  "-flambda2-join-depth", Arg.Int f,
  Printf.sprintf " Depth threshold for alias expansion in join\n\
      \     (default %d) (Flambda 2 only)"
    Flambda2.Default.join_depth
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
      \     expressions in the produced Cmm code\n\
      \     into any context%s (Flambda 2 only)"
    (format_default Flambda2.Expert.Default.inline_effects_in_cmm)
;;

let mk_no_flambda2_expert_inline_effects_in_cmm f =
  "-no-flambda2-expert-inline-effects-in-cmm", Arg.Unit f,
  Printf.sprintf " Only allow inlining of effectful\n\
      \     expressions in the produced Cmm code into\n\
      \     the arguments of allocation primitives%s (Flambda 2 only)"
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

let mk_flambda2_expert_max_function_simplify_run f =
  "-flambda2-expert-max-function-simplify-run", Arg.Int f,
  Printf.sprintf " Do not run simplification of function more\n\
      \     than this (default %d) (Flambda 2 only)"
    Flambda2.Expert.Default.max_function_simplify_run
;;

let mk_flambda2_expert_shorten_symbol_names f =
  "-flambda2-expert-shorten-symbol-names", Arg.Unit f,
  " Shorten symbol names (Flambda 2 only, set by\n\
    \     default in classic mode)"
;;

let mk_no_flambda2_expert_shorten_symbol_names f =
  "-no-flambda2-expert-shorten-symbol-names", Arg.Unit f,
  " Do not shorten symbol names (Flambda 2 only, set by\n\
    \     default except for classic mode)"
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

let mk_flambda2_debug_keep_invalid_handlers f =
  "-flambda2-debug-keep-invalid-handlers", Arg.Unit f,
  Printf.sprintf " Keep branches simplified\n\
      \     to Invalid%s (Flambda 2 only)"
    (format_default Flambda2.Debug.Default.keep_invalid_handlers)
;;

let mk_no_flambda2_debug_keep_invalid_handlers f =
  "-no-flambda2-debug-keep-invalid-handlers", Arg.Unit f,
  Printf.sprintf " Delete branches simplified\n\
      \     to Invalid%s (Flambda 2 only)"
    (format_not_default Flambda2.Debug.Default.keep_invalid_handlers)
;;

let mk_flambda2_inline_max_depth f =
  "-flambda2-inline-max-depth", Arg.String f,
  Printf.sprintf "<int>|<round>=<int>[,...]\n\
      \     Maximum depth of search for inlining opportunities inside\n\
      \     inlined functions (default %d) (Flambda 2 only)"
    Flambda_backend_flags.Flambda2.Inlining.Default.default_arguments.max_depth
;;

let mk_flambda2_inline_max_rec_depth f =
  "-flambda2-inline-max-rec-depth", Arg.String f,
  Printf.sprintf "<int>|<round>=<int>[,...]\n\
      \     Maximum depth of search for inlining opportunities inside\n\
      \     inlined recursive functions (default %d) (Flambda 2 only)"
    Flambda_backend_flags.Flambda2.Inlining.Default.default_arguments.
      max_rec_depth
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

module Flambda2_inlining_default =
  Flambda_backend_flags.Flambda2.Inlining.Default

let mk_flambda2_inline_call_cost =
  mk_flambda2_inline_cost "call" "a call"
    ~default:Flambda2_inlining_default.default_arguments.call_cost

let mk_flambda2_inline_alloc_cost =
  mk_flambda2_inline_cost "alloc" "an allocation"
    ~default:Flambda2_inlining_default.default_arguments.alloc_cost

let mk_flambda2_inline_prim_cost =
  mk_flambda2_inline_cost "prim" "a primitive"
    ~default:Flambda2_inlining_default.default_arguments.prim_cost

let mk_flambda2_inline_branch_cost =
  mk_flambda2_inline_cost "branch" "a conditional"
    ~default:Flambda2_inlining_default.default_arguments.branch_cost

let mk_flambda2_inline_indirect_call_cost =
  mk_flambda2_inline_cost "indirect" "an indirect call"
    ~default:Flambda2_inlining_default.default_arguments.indirect_call_cost

let mk_flambda2_inline_poly_compare_cost =
  mk_flambda2_inline_cost "poly-compare" "a polymorphic comparison"
    ~default:Flambda2_inlining_default.default_arguments.poly_compare_cost

(* CR-someday mshinwell: We should have a check that the parameters provided by
   the user are sensible, e.g. small_function_size <= large_function_size. *)

let mk_flambda2_inline_small_function_size f =
  "-flambda2-inline-small-function-size", Arg.String f,
  Printf.sprintf "<int>|<round>=<int>[,...]\n\
      \     Functions with a cost less than this will always be inlined\n\
      \     unless an attribute instructs otherwise (default %d)\n\
      \     (Flambda 2 only)"
    Flambda2_inlining_default.default_arguments.small_function_size
;;

let mk_flambda2_inline_large_function_size f =
  "-flambda2-inline-large-function-size", Arg.String f,
  Printf.sprintf "<int>|<round>=<int>[,...]\n\
      \     Functions with a cost greater than this will never be inlined\n\
      \     unless an attribute instructs otherwise (default %d); speculative\n\
      \     inlining will be disabled if equal to the small function size\n\
      \     (Flambda 2 only)"
    Flambda2_inlining_default.default_arguments.large_function_size
;;

let mk_flambda2_inline_threshold f =
  "-flambda2-inline-threshold", Arg.String f,
    Printf.sprintf "<float>|<round>=<float>[,...]\n\
        \     Aggressiveness of inlining (default %.02f, higher numbers mean\n\
        \     more aggressive) (Flambda 2 only)"
      Flambda2_inlining_default.default_arguments.threshold

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

let mk_drawfexpr_to f =
  "-drawfexpr-to", Arg.String f,
  "<file> Like -drawfexpr but dumps to given file (Flambda 2 only)"
;;

let mk_dfexpr f =
  "-dfexpr", Arg.Unit f, " Like -dflambda but outputs fexpr language\n\
    \     (Flambda 2 only)"
;;

let mk_dfexpr_to f =
  "-dfexpr-to", Arg.String f,
  "<file> Like -dfexpr but dumps to given file (Flambda 2 only)"
;;

let mk_dflexpect_to f =
  "-dflexpect-to", Arg.String f,
  "<file> Combine -drawfexpr and -dfexpr in an .flt file (Flambda 2 only)"
;;

let mk_dslot_offsets f =
  "-dslot-offsets", Arg.Unit f, " Dump closure offsets (Flambda 2 only)"
;;

let mk_dfreshen f =
  "-dfreshen", Arg.Unit f, " Freshen bound names when printing (Flambda 2 only)"
;;

let mk_dflow f =
  "-dflow", Arg.Unit f, " Dump debug info for the flow computation (Flambda 2 only)"
;;

module Debugging = Dwarf_flags

(* CR mshinwell: These help texts should show the default values. *)

let mk_restrict_to_upstream_dwarf f =
  "-gupstream-dwarf", Arg.Unit f, " Only emit the same DWARF information as the upstream compiler"

let mk_no_restrict_to_upstream_dwarf f =
  "-gno-upstream-dwarf", Arg.Unit f, " Emit potentially more DWARF information than the upstream compiler"

let mk_dwarf_for_startup_file f =
  "-gstartup", Arg.Unit f, " Emit potentially more DWARF information\n\
    \     for the startup file than the upstream compiler\n\
    \     (only takes effect with -gno-upstream-dwarf)"

let mk_no_dwarf_for_startup_file f =
  "-gno-startup", Arg.Unit f, " Emit the same DWARF information for the\n\
    \     startup file as the upstream compiler"

let mk_gdwarf_may_alter_codegen f =
  "-gdwarf-may-alter-codegen", Arg.Unit f, " Allow code generation (and\n\
    \     when finalizers may run, etc) to be altered\n\
    \     in order to produce a better debugging experience"

let mk_no_gdwarf_may_alter_codegen f =
  "-gno-dwarf-may-alter-codegen", Arg.Unit f, " Do not alter code\n\
    \     generation when emitting debugging information"

let mk_gdwarf_max_function_complexity f =
  "-gdwarf-max-function-complexity", Arg.Int f,
  Format.sprintf " Maximum function\n\
      \     complexity above which -gno-upstream-dwarf information\n\
      \     will not be emitted, to improve compilation time (default %d)"
    !Dwarf_flags.dwarf_max_function_complexity

let mk_use_cached_generic_functions f =
  "-use-cached-generic-functions", Arg.Unit f, " Use the cached generated functions"
;;

let mk_cached_generic_functions_path f =
  "-cached-generic-functions-path", Arg.String f,
  "<file>  Set the path of the cached generic functions (default to cached-generic-functions.o)"
;;

let set_long_frames_threshold n =
  if n < 0 then
    raise (Arg.Bad "Long frames threshold must be non-negative.");
  if n > Flambda_backend_flags.max_long_frames_threshold then
    raise
      (Arg.Bad
         (Printf.sprintf "Long frames threshold too big: 0x%x, \
                          must be less or equal to 0x%x" n
            Flambda_backend_flags.max_long_frames_threshold));
  Flambda_backend_flags.long_frames_threshold := n

let mk_symbol_visibility_protected f =
  "-symbol-visibility-protected", Arg.Unit f,
  " Emit global symbols with visibility STV_PROTECTED on supported systems"
;;

let mk_no_symbol_visibility_protected f =
  "-no-symbol-visibility-protected", Arg.Unit f,
  " Emit global symbols with visibility STV_DEFAULT"
;;

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

  val reorder_blocks_random : int -> unit
  val basic_block_sections : unit -> unit

  val dasm_comments : unit -> unit
  val dno_asm_comments : unit -> unit

  val no_stack_checks_genfuncs : unit -> unit

  val heap_reduction_threshold : int -> unit
  val zero_alloc_check : string -> unit
  val dcheckmach : unit -> unit
  val checkmach_details_cutoff : int -> unit

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

module Make_flambda_backend_options (F : Flambda_backend_options) =
struct
  let list2 = [
    mk_dump_inlining_paths F.dump_inlining_paths;
    mk_davail F.davail;
    mk_dranges F.dranges;
    mk_ddebug_invariants F.ddebug_invariants;
    mk_ocamlcfg F.ocamlcfg;
    mk_no_ocamlcfg F.no_ocamlcfg;
    mk_dcfg F.dcfg;
    mk_dcfg_invariants F.dcfg_invariants;
    mk_dcfg_equivalence_check F.dcfg_equivalence_check;
    mk_regalloc F.regalloc;
    mk_regalloc_param F.regalloc_param;
    mk_regalloc_validate F.regalloc_validate;
    mk_no_regalloc_validate F.no_regalloc_validate;

    mk_cfg_peephole_optimize F.cfg_peephole_optimize;
    mk_no_cfg_peephole_optimize F.no_cfg_peephole_optimize;

    mk_cfg_cse_optimize F.cfg_cse_optimize;
    mk_no_cfg_cse_optimize F.no_cfg_cse_optimize;

    mk_reorder_blocks_random F.reorder_blocks_random;
    mk_basic_block_sections F.basic_block_sections;

    mk_dasm_comments F.dasm_comments;
    mk_dno_asm_comments F.dno_asm_comments;

    mk_no_stack_checks_genfuncs F.no_stack_checks_genfuncs;

    mk_heap_reduction_threshold F.heap_reduction_threshold;
    mk_zero_alloc_check F.zero_alloc_check;
    mk_dcheckmach F.dcheckmach;
    mk_checkmach_details_cutoff F.checkmach_details_cutoff;

    mk_function_layout F.function_layout;
    mk_disable_poll_insertion F.disable_poll_insertion;
    mk_enable_poll_insertion F.enable_poll_insertion;

    mk_symbol_visibility_protected F.symbol_visibility_protected;
    mk_no_symbol_visibility_protected F.symbol_visibility_protected;

    mk_long_frames F.long_frames;
    mk_no_long_frames F.no_long_frames;
    mk_debug_long_frames_threshold F.long_frames_threshold;

    mk_caml_apply_inline_fast_path F.caml_apply_inline_fast_path;

    mk_internal_assembler F.internal_assembler;

    mk_gc_timings F.gc_timings;

    mk_flambda2_debug F.flambda2_debug;
    mk_no_flambda2_debug F.no_flambda2_debug;
    mk_flambda2_join_points F.flambda2_join_points;
    mk_no_flambda2_join_points F.no_flambda2_join_points;
    mk_flambda2_result_types_functors_only
      F.flambda2_result_types_functors_only;
    mk_flambda2_result_types_all_functions
      F.flambda2_result_types_all_functions;
    mk_no_flambda2_result_types
      F.no_flambda2_result_types;
    mk_flambda2_basic_meet F.flambda2_basic_meet;
    mk_flambda2_advanced_meet F.flambda2_advanced_meet;
    mk_flambda2_unbox_along_intra_function_control_flow
      F.flambda2_unbox_along_intra_function_control_flow;
    mk_no_flambda2_unbox_along_intra_function_control_flow
      F.no_flambda2_unbox_along_intra_function_control_flow;
    mk_flambda2_backend_cse_at_toplevel F.flambda2_backend_cse_at_toplevel;
    mk_no_flambda2_backend_cse_at_toplevel
      F.no_flambda2_backend_cse_at_toplevel;
    mk_flambda2_cse_depth F.flambda2_cse_depth;
    mk_flambda2_join_depth F.flambda2_join_depth;
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
    mk_flambda2_expert_max_function_simplify_run
      F.flambda2_expert_max_function_simplify_run;
    mk_flambda2_expert_shorten_symbol_names
      F.flambda2_expert_shorten_symbol_names;
    mk_no_flambda2_expert_shorten_symbol_names
      F.no_flambda2_expert_shorten_symbol_names;
    mk_flambda2_debug_concrete_types_only_on_canonicals
      F.flambda2_debug_concrete_types_only_on_canonicals;
    mk_no_flambda2_debug_concrete_types_only_on_canonicals
      F.no_flambda2_debug_concrete_types_only_on_canonicals;
    mk_flambda2_debug_keep_invalid_handlers
      F.flambda2_debug_keep_invalid_handlers;
    mk_no_flambda2_debug_keep_invalid_handlers
      F.no_flambda2_debug_keep_invalid_handlers;

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

    mk_drawfexpr F.drawfexpr;
    mk_drawfexpr_to F.drawfexpr_to;
    mk_dfexpr F.dfexpr;
    mk_dfexpr_to F.dfexpr_to;
    mk_dflexpect_to F.dflexpect_to;
    mk_dslot_offsets F.dslot_offsets;
    mk_dfreshen F.dfreshen;
    mk_dflow F.dflow;
    mk_use_cached_generic_functions F.use_cached_generic_functions;
    mk_cached_generic_functions_path F.cached_generic_functions_path;
  ]
end

module Flambda_backend_options_impl = struct
  let set r () = r := Flambda_backend_flags.Set true
  let clear r () = r := Flambda_backend_flags.Set false

  let set' r () = r := true
  let clear' r () = r := false

  let ocamlcfg = set' Flambda_backend_flags.use_ocamlcfg
  let no_ocamlcfg = clear' Flambda_backend_flags.use_ocamlcfg
  let dcfg = set' Flambda_backend_flags.dump_cfg
  let dcfg_invariants = set' Flambda_backend_flags.cfg_invariants
  let dcfg_equivalence_check = set' Flambda_backend_flags.cfg_equivalence_check
  let regalloc x = Flambda_backend_flags.regalloc := x
  let regalloc_param x = Flambda_backend_flags.regalloc_params := x :: !Flambda_backend_flags.regalloc_params
  let regalloc_validate = set' Flambda_backend_flags.regalloc_validate
  let no_regalloc_validate = clear' Flambda_backend_flags.regalloc_validate

  let cfg_peephole_optimize = set' Flambda_backend_flags.cfg_peephole_optimize
  let no_cfg_peephole_optimize = clear' Flambda_backend_flags.cfg_peephole_optimize

  let cfg_cse_optimize = set' Flambda_backend_flags.cfg_cse_optimize
  let no_cfg_cse_optimize = clear' Flambda_backend_flags.cfg_cse_optimize

  let reorder_blocks_random seed =
    Flambda_backend_flags.reorder_blocks_random := Some seed
  let basic_block_sections () =
    set' Flambda_backend_flags.basic_block_sections ()

  let dasm_comments =
    set' Flambda_backend_flags.dasm_comments

  let dno_asm_comments =
    clear' Flambda_backend_flags.dasm_comments

  let no_stack_checks_genfuncs =
    set' Flambda_backend_flags.no_stack_checks_genfuncs

  let dump_inlining_paths = set' Flambda_backend_flags.dump_inlining_paths

  let davail = set' Flambda_backend_flags.davail
  let dranges = set' Flambda_backend_flags.dranges

  let ddebug_invariants = set' Dwarf_flags.ddebug_invariants

  let heap_reduction_threshold x =
    Flambda_backend_flags.heap_reduction_threshold := x

  let zero_alloc_check s =
    match Clflags.Annotations.of_string s with
    | None -> () (* this should not occur as we use Arg.Symbol *)
    | Some a ->
      Clflags.zero_alloc_check := a

  let dcheckmach = set' Flambda_backend_flags.dump_checkmach
  let checkmach_details_cutoff n =
    let c : Flambda_backend_flags.checkmach_details_cutoff =
      if n < 0 then Keep_all
      else if n = 0 then No_details
      else At_most n
    in
    Flambda_backend_flags.checkmach_details_cutoff := c

  let function_layout s =
    match Flambda_backend_flags.Function_layout.of_string s with
    | None -> () (* this should not occur as we use Arg.Symbol *)
    | Some layout ->
      Flambda_backend_flags.function_layout := layout

  let disable_poll_insertion = set' Flambda_backend_flags.disable_poll_insertion
  let enable_poll_insertion = clear' Flambda_backend_flags.disable_poll_insertion

  let symbol_visibility_protected = set' Flambda_backend_flags.symbol_visibility_protected
  let no_symbol_visibility_protected = clear' Flambda_backend_flags.symbol_visibility_protected

  let long_frames =  set' Flambda_backend_flags.allow_long_frames
  let no_long_frames = clear' Flambda_backend_flags.allow_long_frames
  let long_frames_threshold n = set_long_frames_threshold n

  let caml_apply_inline_fast_path =
    set' Flambda_backend_flags.caml_apply_inline_fast_path

  let internal_assembler = set' Flambda_backend_flags.internal_assembler

  let gc_timings = set' Flambda_backend_flags.gc_timings

  let flambda2_debug = set' Flambda_backend_flags.Flambda2.debug
  let no_flambda2_debug = clear' Flambda_backend_flags.Flambda2.debug
  let flambda2_join_points = set Flambda2.join_points
  let no_flambda2_join_points = clear Flambda2.join_points
  let flambda2_result_types_functors_only () =
    Flambda2.function_result_types := Flambda_backend_flags.Set Flambda_backend_flags.Functors_only
  let flambda2_result_types_all_functions () =
    Flambda2.function_result_types := Flambda_backend_flags.Set Flambda_backend_flags.All_functions
  let no_flambda2_result_types () =
    Flambda2.function_result_types := Flambda_backend_flags.Set Flambda_backend_flags.Never
  let flambda2_basic_meet () =
    Flambda2.meet_algorithm := Flambda_backend_flags.Set Flambda_backend_flags.Basic
  let flambda2_advanced_meet () =
    Flambda2.meet_algorithm := Flambda_backend_flags.Set Flambda_backend_flags.Advanced
  let flambda2_unbox_along_intra_function_control_flow =
    set Flambda2.unbox_along_intra_function_control_flow
  let no_flambda2_unbox_along_intra_function_control_flow =
    clear Flambda2.unbox_along_intra_function_control_flow
  let flambda2_backend_cse_at_toplevel =
    set Flambda2.backend_cse_at_toplevel
  let no_flambda2_backend_cse_at_toplevel =
    clear Flambda2.backend_cse_at_toplevel
  let flambda2_cse_depth n = Flambda2.cse_depth := Flambda_backend_flags.Set n
  let flambda2_join_depth n = Flambda2.join_depth := Flambda_backend_flags.Set n
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
    Flambda2.Expert.max_block_size_for_projections := Flambda_backend_flags.Set (Some size)
  let flambda2_expert_max_unboxing_depth depth =
    Flambda2.Expert.max_unboxing_depth := Flambda_backend_flags.Set depth
  let flambda2_expert_can_inline_recursive_functions () =
    Flambda2.Expert.can_inline_recursive_functions := Flambda_backend_flags.Set true
  let no_flambda2_expert_can_inline_recursive_functions () =
    Flambda2.Expert.can_inline_recursive_functions := Flambda_backend_flags.Set false
  let flambda2_expert_max_function_simplify_run runs =
    Flambda2.Expert.max_function_simplify_run := Flambda_backend_flags.Set runs
  let flambda2_expert_shorten_symbol_names () =
    Flambda2.Expert.shorten_symbol_names := Flambda_backend_flags.Set true
  let no_flambda2_expert_shorten_symbol_names () =
    Flambda2.Expert.shorten_symbol_names := Flambda_backend_flags.Set false
  let flambda2_debug_concrete_types_only_on_canonicals =
    set' Flambda2.Debug.concrete_types_only_on_canonicals
  let no_flambda2_debug_concrete_types_only_on_canonicals =
    clear' Flambda2.Debug.concrete_types_only_on_canonicals
  let flambda2_debug_keep_invalid_handlers =
    set' Flambda2.Debug.keep_invalid_handlers
  let no_flambda2_debug_keep_invalid_handlers =
    clear' Flambda2.Debug.keep_invalid_handlers

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
    set' Flambda2.Inlining.speculative_inlining_only_if_arguments_useful

  let no_flambda2_speculative_inlining_only_if_arguments_useful =
    clear' Flambda2.Inlining.speculative_inlining_only_if_arguments_useful

  let flambda2_inlining_report_bin = set' Flambda2.Inlining.report_bin

  let flambda2_unicode = set Flambda2.unicode

  let drawfexpr () = Flambda2.Dump.rawfexpr := Flambda2.Dump.Main_dump_stream
  let drawfexpr_to file = Flambda2.Dump.rawfexpr := Flambda2.Dump.File file
  let dfexpr () = Flambda2.Dump.fexpr := Flambda2.Dump.Main_dump_stream
  let dfexpr_to file = Flambda2.Dump.fexpr := Flambda2.Dump.File file
  let dflexpect_to file = Flambda2.Dump.flexpect := Flambda2.Dump.File file
  let dslot_offsets = set' Flambda2.Dump.slot_offsets
  let dfreshen = set' Flambda2.Dump.freshen
  let dflow = set' Flambda2.Dump.flow
  let use_cached_generic_functions = set' Flambda_backend_flags.use_cached_generic_functions
  let cached_generic_functions_path file = Flambda_backend_flags.cached_generic_functions_path := file
end

module type Debugging_options = sig
  val restrict_to_upstream_dwarf : unit -> unit
  val no_restrict_to_upstream_dwarf : unit -> unit
  val dwarf_for_startup_file : unit -> unit
  val no_dwarf_for_startup_file : unit -> unit
  val gdwarf_may_alter_codegen : unit -> unit
  val no_gdwarf_may_alter_codegen : unit -> unit
  val gdwarf_max_function_complexity : int -> unit
end

module Make_debugging_options (F : Debugging_options) = struct
  let list3 = [
    mk_restrict_to_upstream_dwarf F.restrict_to_upstream_dwarf;
    mk_no_restrict_to_upstream_dwarf F.no_restrict_to_upstream_dwarf;
    mk_dwarf_for_startup_file F.dwarf_for_startup_file;
    mk_no_dwarf_for_startup_file F.no_dwarf_for_startup_file;
    mk_gdwarf_may_alter_codegen F.gdwarf_may_alter_codegen;
    mk_no_gdwarf_may_alter_codegen F.no_gdwarf_may_alter_codegen;
    mk_gdwarf_max_function_complexity F.gdwarf_max_function_complexity;
   ]
end

module Debugging_options_impl = struct
  let restrict_to_upstream_dwarf () =
    Debugging.restrict_to_upstream_dwarf := true
  let no_restrict_to_upstream_dwarf () =
    Debugging.restrict_to_upstream_dwarf := false
  let dwarf_for_startup_file () =
    Debugging.dwarf_for_startup_file := true
  let no_dwarf_for_startup_file () =
    Debugging.dwarf_for_startup_file := false
  let gdwarf_may_alter_codegen () =
    Debugging.gdwarf_may_alter_codegen := true
  let no_gdwarf_may_alter_codegen () =
    Debugging.gdwarf_may_alter_codegen := false
  let gdwarf_max_function_complexity c =
    Debugging.dwarf_max_function_complexity := c
end

module Extra_params = struct
  let read_param ppf _position name v =
    let set option =
      let b = Compenv.check_bool ppf name v in
      option := Flambda_backend_flags.Set b;
      true
    in
    let _clear option =
      let b = Compenv.check_bool ppf name v in
      option := Flambda_backend_flags.Set (not b);
      false
    in
    let set_string option =
      option := v;
      true
    in
    let add_string option =
      option := v :: !option;
      true
    in
    let set_int option =
      begin match Compenv.check_int ppf name v with
      | Some i -> option := Flambda_backend_flags.Set i
      | None -> ()
      end;
      true
    in
    let set' option =
      Compenv.setter ppf (fun b -> b) name [ option ] v; true
    in
    (*let clear' option =
      Compenv.setter ppf (fun b -> not b) name [ option ] v; true
    in*)
    let set_int' option =
      Compenv.int_setter ppf name option v; true
    in
    let set_int_option' option =
      begin match Compenv.check_int ppf name v with
       | Some seed -> option := Some seed
       | None -> ()
      end;
      true
    in
    match name with
    | "internal-assembler" -> set' Flambda_backend_flags.internal_assembler
    | "dgc-timings" -> set' Flambda_backend_flags.gc_timings
    | "ocamlcfg" -> set' Flambda_backend_flags.use_ocamlcfg
    | "cfg-invariants" -> set' Flambda_backend_flags.cfg_invariants
    | "cfg-equivalence-check" -> set' Flambda_backend_flags.cfg_equivalence_check
    | "regalloc" -> set_string Flambda_backend_flags.regalloc
    | "regalloc-param" -> add_string Flambda_backend_flags.regalloc_params
    | "regalloc-validate" -> set' Flambda_backend_flags.regalloc_validate
    | "cfg-peephole-optimize" -> set' Flambda_backend_flags.cfg_peephole_optimize
    | "cfg-cse-optimize" -> set' Flambda_backend_flags.cfg_cse_optimize
    | "dump-inlining-paths" -> set' Flambda_backend_flags.dump_inlining_paths
    | "davail" -> set' Flambda_backend_flags.davail
    | "dranges" -> set' Flambda_backend_flags.dranges
    | "ddebug-invariants" -> set' Dwarf_flags.ddebug_invariants
    | "reorder-blocks-random" ->
       set_int_option' Flambda_backend_flags.reorder_blocks_random
    | "basic-block-sections" -> set' Flambda_backend_flags.basic_block_sections
    | "heap-reduction-threshold" -> set_int' Flambda_backend_flags.heap_reduction_threshold
    | "zero-alloc-check" ->
      (match Clflags.Annotations.of_string v with
       | Some a -> Clflags.zero_alloc_check := a; true
       | None ->
         raise
           (Arg.Bad
              (Printf.sprintf "Unexpected value %s for %s" v name)))
    | "dump-checkmach" -> set' Flambda_backend_flags.dump_checkmach
    | "checkmach-details-cutoff" ->
      begin match Compenv.check_int ppf name v with
      | Some i ->
        Flambda_backend_options_impl.checkmach_details_cutoff i
      | None -> ()
      end;
      true
    | "function-layout" ->
      (match Flambda_backend_flags.Function_layout.of_string v with
       | Some layout -> Flambda_backend_flags.function_layout := layout; true
       | None ->
         raise
           (Arg.Bad
              (Printf.sprintf "Unexpected value %s for %s" v name)))
    | "poll-insertion" -> set' Flambda_backend_flags.disable_poll_insertion
    | "symbol-visibility-protected" -> set' Flambda_backend_flags.disable_poll_insertion
    | "long-frames" -> set' Flambda_backend_flags.allow_long_frames
    | "debug-long-frames-threshold" ->
      begin match Compenv.check_int ppf name v with
      | Some n -> set_long_frames_threshold n; true
      | None ->
        raise
          (Arg.Bad
             (Printf.sprintf "Expected integer between 0 and %d"
                Flambda_backend_flags.max_long_frames_threshold))
      end
    | "caml-apply-inline-fast-path" ->
      set' Flambda_backend_flags.caml_apply_inline_fast_path
    | "dasm-comments" -> set' Flambda_backend_flags.dasm_comments
    | "no-stack-checks-genfuncs" -> set' Flambda_backend_flags.no_stack_checks_genfuncs
    | "gupstream-dwarf" -> set' Debugging.restrict_to_upstream_dwarf
    | "gdwarf-may-alter-codegen" -> set' Debugging.gdwarf_may_alter_codegen
    | "gstartup" -> set' Debugging.dwarf_for_startup_file
    | "gdwarf-max-function-complexity" ->
      set_int' Debugging.dwarf_max_function_complexity
    | "flambda2-debug" -> set' Flambda_backend_flags.Flambda2.debug
    | "flambda2-join-points" -> set Flambda2.join_points
    | "flambda2-result-types" ->
      (match String.lowercase_ascii v with
      | "never" ->
        Flambda2.function_result_types := Flambda_backend_flags.(Set Never)
      | "functors-only" ->
        Flambda2.function_result_types := Flambda_backend_flags.(Set Functors_only)
      | "all-functions" ->
        Flambda2.function_result_types := Flambda_backend_flags.(Set All_functions)
      | _ ->
        Misc.fatal_error "Syntax: flambda2-result-types=\
          never|functors-only|all-functions");
      true
    | "flambda2-result-types-all-functions" ->
      Flambda2.function_result_types := Flambda_backend_flags.(Set All_functions);
      true
    | "flambda2-meet-algorithm" ->
      (match String.lowercase_ascii v with
      | "basic" ->
        Flambda2.meet_algorithm := Flambda_backend_flags.(Set Basic)
      | "advanced" ->
        Flambda2.meet_algorithm := Flambda_backend_flags.(Set Advanced)
      | _ ->
        Misc.fatal_error "Syntax: flambda2-meet_algorithm=basic|advanced");
      true
    | "flambda2-unbox-along-intra-function-control-flow" ->
       set Flambda2.unbox_along_intra_function_control_flow
    | "flambda2-backend-cse-at-toplevel" ->
       set Flambda2.backend_cse_at_toplevel
    | "flambda2-cse-depth" ->
       set_int Flambda2.cse_depth
    | "flambda2-join-depth" ->
       set_int Flambda2.join_depth
    | "flambda2-expert-inline-effects-in-cmm" ->
       set Flambda2.Expert.inline_effects_in_cmm
    | "flambda2-expert-phantom-lets" ->
       set Flambda2.Expert.phantom_lets
    | "flambda2-expert-max-unboxing-depth" ->
       set_int Flambda2.Expert.max_unboxing_depth
    | "flambda2-expert-can-inline-recursive-functions" ->
       set Flambda2.Expert.can_inline_recursive_functions
    | "flambda2-expert-max-function-simplify-run" ->
       set_int Flambda2.Expert.max_function_simplify_run
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
       set' Flambda2.Inlining.speculative_inlining_only_if_arguments_useful
    | "flambda2-inlining-report-bin" ->
       set' Flambda2.Inlining.report_bin
    | "flambda2-expert-fallback-inlining-heuristic" ->
       set Flambda2.Expert.fallback_inlining_heuristic
    | "flambda2-debug-concrete-types-only-on-canonicals" ->
       set' Flambda2.Debug.concrete_types_only_on_canonicals
    | "flambda2-debug-keep-invalid-handlers" ->
       set' Flambda2.Debug.keep_invalid_handlers
    | "use-cached-generic-functions" ->
      set' Flambda_backend_flags.use_cached_generic_functions
    | "cached-generic-functions-path" ->
      Flambda_backend_flags.cached_generic_functions_path := v; true
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
     redefine it in the implementation of this functor's argument. *)
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
  end
  module Opttopmain = struct
    include Main_args.Default.Opttopmain
    include Flambda_backend_options_impl
    include Debugging_options_impl
  end
end
