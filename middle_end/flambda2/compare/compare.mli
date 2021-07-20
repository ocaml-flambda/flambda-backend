module Comparison : sig
  (* Given two things x and y, a judgment of whether they are alpha-equivalent.
   * In the case that they are not, this includes an "approximator," a term
   * that's alpha-equivalent to x and as close as possible to y.  The
   * approximator can be used to generate a diff that (hopefully) eliminates
   * spurious differences. *)
  type 'a t =
    | Equivalent (* x and y are alpha-equivalent *)
    | Different of { approximant : 'a }
      (* x and y are not alpha-equivalent, and furthermore [approximant] is
       * alpha-equivalent to x and textually the same as y in places where x and
       * y agree *)

  val print
         : (Format.formatter -> 'a -> unit)
        -> Format.formatter
        -> 'a t
        -> unit
end

val flambda_units
   : Flambda_unit.t
  -> Flambda_unit.t
  -> Flambda_unit.t Comparison.t
