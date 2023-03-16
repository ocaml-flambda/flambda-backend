(** Simple validator for Flambda2

   This is the interface for a simple Flambda2 validator, which performs basic
   semantic equivalence between terms. Given the simplifying pass (after CPS
   translation), this validator does the following:


      [Flambda_unit.t]   ------- Simplify.run --------> [simplify_result]
              |                                                 |
              |                                                 |
          translate                                         translate
    (flambda_expr_to_core)                          (simplify_result_to_core)
              |                                                 |
              |                                                 |
              ↓                                                 ↓
          [core_exp]                                        [core_exp]
              |                                                 |
              |                                                 |
          β - reduce                                        β - reduce
          (normalize)                                       (normalize)
              |                                                 |
              |                                                 |
              ↓                                                 ↓
        [core_exp]              ≅[validate]                [core_exp]

   i.e. the validate function in this module performs the equivalence check
   between the source and target of the flambda2 simplification.

   To use this validator, use the [-validate] flag.

   N.B. Note the difference between this validator and the comparison check
   function available through [compare/compare.ml].
   The [compare] function takes as assumption that the output of the
   simplifier has not changed and shows a syntactic equality up to
   alpha-equivalence. **)

val normalize : Flambda2_core.core_exp -> Flambda2_core.core_exp
