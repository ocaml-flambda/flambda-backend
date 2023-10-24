#!/bin/sh

set -eux -o pipefail

middle_end_targets="
  _build/default/.ocamloptcomp.objs/native/backend_var.cmx \
  _build/default/.ocamloptcomp.objs/native/clambda.cmx \
  _build/default/.ocamloptcomp.objs/native/clambda_layout.cmx \
  _build/default/.ocamloptcomp.objs/native/clambda_primitives.cmx \
  _build/default/.ocamloptcomp.objs/native/compilenv.cmx \
  _build/default/.ocamloptcomp.objs/native/convert_primitives.cmx \
  _build/default/.ocamloptcomp.objs/native/internal_variable_names.cmx \
  _build/default/.ocamloptcomp.objs/native/mangling.cmx \
  _build/default/.ocamloptcomp.objs/native/printclambda.cmx \
  _build/default/.ocamloptcomp.objs/native/printclambda_primitives.cmx \
  _build/default/.ocamloptcomp.objs/native/semantics_of_primitives.cmx \
  _build/default/.ocamloptcomp.objs/native/symbol_utils.cmx \
  _build/default/.ocamloptcomp.objs/native/variable.cmx"

closure_targets="
  _build/default/.ocamloptcomp.objs/native/closure.cmx \
  _build/default/.ocamloptcomp.objs/native/closure_middle_end.cmx"

flambda1_targets="\
  _build/default/.ocamloptcomp.objs/native/alias_analysis.cmx \
  _build/default/.ocamloptcomp.objs/native/allocated_const.cmx \
  _build/default/.ocamloptcomp.objs/native/augment_specialised_args.cmx \
  _build/default/.ocamloptcomp.objs/native/build_export_info.cmx \
  _build/default/.ocamloptcomp.objs/native/closure_conversion.cmx \
  _build/default/.ocamloptcomp.objs/native/closure_conversion_aux.cmx \
  _build/default/.ocamloptcomp.objs/native/closure_offsets.cmx \
  _build/default/.ocamloptcomp.objs/native/effect_analysis.cmx \
  _build/default/.ocamloptcomp.objs/native/export_info.cmx \
  _build/default/.ocamloptcomp.objs/native/extract_projections.cmx \
  _build/default/.ocamloptcomp.objs/native/find_recursive_functions.cmx \
  _build/default/.ocamloptcomp.objs/native/flambda.cmx \
  _build/default/.ocamloptcomp.objs/native/flambda_invariants.cmx \
  _build/default/.ocamloptcomp.objs/native/flambda_iterators.cmx \
  _build/default/.ocamloptcomp.objs/native/flambda_middle_end.cmx \
  _build/default/.ocamloptcomp.objs/native/flambda_to_clambda.cmx \
  _build/default/.ocamloptcomp.objs/native/flambda_utils.cmx \
  _build/default/.ocamloptcomp.objs/native/freshening.cmx \
  _build/default/.ocamloptcomp.objs/native/import_approx.cmx \
  _build/default/.ocamloptcomp.objs/native/inconstant_idents.cmx \
  _build/default/.ocamloptcomp.objs/native/initialize_symbol_to_let_symbol.cmx \
  _build/default/.ocamloptcomp.objs/native/inline_and_simplify.cmx \
  _build/default/.ocamloptcomp.objs/native/inline_and_simplify_aux.cmx \
  _build/default/.ocamloptcomp.objs/native/inlining_cost.cmx \
  _build/default/.ocamloptcomp.objs/native/inlining_decision.cmx \
  _build/default/.ocamloptcomp.objs/native/inlining_stats.cmx \
  _build/default/.ocamloptcomp.objs/native/inlining_stats_types.cmx \
  _build/default/.ocamloptcomp.objs/native/inlining_transforms.cmx \
  _build/default/.ocamloptcomp.objs/native/invariant_params.cmx \
  _build/default/.ocamloptcomp.objs/native/lift_code.cmx \
  _build/default/.ocamloptcomp.objs/native/lift_constants.cmx \
  _build/default/.ocamloptcomp.objs/native/lift_let_to_initialize_symbol.cmx \
  _build/default/.ocamloptcomp.objs/native/parameter.cmx \
  _build/default/.ocamloptcomp.objs/native/pass_wrapper.cmx \
  _build/default/.ocamloptcomp.objs/native/projection.cmx \
  _build/default/.ocamloptcomp.objs/native/ref_to_variables.cmx \
  _build/default/.ocamloptcomp.objs/native/remove_free_vars_equal_to_args.cmx \
  _build/default/.ocamloptcomp.objs/native/remove_unused_arguments.cmx \
  _build/default/.ocamloptcomp.objs/native/remove_unused_closure_vars.cmx \
  _build/default/.ocamloptcomp.objs/native/remove_unused_program_constructs.cmx \
  _build/default/.ocamloptcomp.objs/native/share_constants.cmx \
  _build/default/.ocamloptcomp.objs/native/simple_value_approx.cmx \
  _build/default/.ocamloptcomp.objs/native/simplify_boxed_integer_ops.cmx \
  _build/default/.ocamloptcomp.objs/native/simplify_common.cmx \
  _build/default/.ocamloptcomp.objs/native/simplify_primitives.cmx \
  _build/default/.ocamloptcomp.objs/native/traverse_for_exported_symbols.cmx \
  _build/default/.ocamloptcomp.objs/native/un_anf.cmx \
  _build/default/.ocamloptcomp.objs/native/unbox_closures.cmx \
  _build/default/.ocamloptcomp.objs/native/unbox_free_vars_of_closures.cmx \
  _build/default/.ocamloptcomp.objs/native/unbox_specialised_args.cmx"

flambda2_targets="\
  _build/default/middle_end/flambda2/identifiers/flambda2_identifiers.cmxa \
  _build/default/middle_end/flambda2/ui/flambda2_ui.cmxa \
  _build/default/middle_end/flambda2/classic_mode_types/flambda2_classic_mode_types.cmxa \
  _build/default/middle_end/flambda2/cmx/flambda2_cmx.cmxa \
  _build/default/middle_end/flambda2/simplify/flambda2_simplify.cmxa \
  _build/default/middle_end/flambda2/kinds/flambda2_kinds.cmxa \
  _build/default/middle_end/flambda2/flambda2.cmxa \
  _build/default/middle_end/flambda2/term_basics/flambda2_term_basics.cmxa \
  _build/default/middle_end/flambda2/from_lambda/flambda2_from_lambda.cmxa \
  _build/default/middle_end/flambda2/lattices/flambda2_lattices.cmxa \
  _build/default/middle_end/flambda2/simplify_shared/flambda2_simplify_shared.cmxa \
  _build/default/middle_end/flambda2/import/flambda2_import.cmxa \
  _build/default/middle_end/flambda2/to_cmm/flambda2_to_cmm.cmxa \
  _build/default/middle_end/flambda2/terms/flambda2_terms.cmxa \
  _build/default/middle_end/flambda2/algorithms/flambda2_algorithms.cmxa \
  _build/default/middle_end/flambda2/numbers/flambda2_numbers.cmxa \
  _build/default/middle_end/flambda2/nominal/flambda2_nominal.cmxa \
  _build/default/middle_end/flambda2/parser/flambda2_parser.cmxa \
  _build/default/middle_end/flambda2/bound_identifiers/flambda2_bound_identifiers.cmxa \
  _build/default/middle_end/flambda2/types/flambda2_types.cmxa"

targets="$middle_end_targets $closure_targets $flambda1_targets $flambda2_targets"

eval $(opam env)
dune build --root=. --workspace=duneconf/boot.ws -w $targets

