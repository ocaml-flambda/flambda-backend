#!/bin/sh

set -eux -o pipefail

targets="\
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

eval $(opam env)
dune build --root=. --workspace=duneconf/boot.ws -w $targets

