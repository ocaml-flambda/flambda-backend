open! Flambda
open! Flambda2_core

(** The core language simplifies any bureaucratic redundancies, and alleviates
    syntactic restrictions of terms in the negative position (i.e.arguments).

    Note that the syntactic restrictions are a necessary part of the "Compiling-
    with- continuations" style optimization used in Flambda2; however this
    validator removes this restriction to check for term equality *)

(** [simple_to_core] is a value-lifting translation:

    [Simple.t] corresponds to a register-sized value.
    By using this translation, we can allow for more liberal β - reductions in
    while normalizing [core_exp] terms. **)

val simple_to_core : Simple.t -> core_exp

val prim_to_core : Flambda_primitive.t -> primitive

val flambda_expr_to_core : Flambda.expr -> core_exp

val flambda_unit_to_core : Flambda_unit.t -> core_exp

val tagged_immediate_to_core : Targetint_31_63.t -> core_exp
