(* TEST
   flags = "-extension layouts"
   * expect
*)

(* This file contains typing tests for the layout [float64].

   Runtime tests for the type [float#] can be found in the [unboxed_float] and
   [alloc] tests in this directory.  The type [float#] here is used as a
   convenient example of a concrete [float64] type in some tests, but its
   behavior isn't the primary purpose of this test. *)

(* CR layouts: Bring tests here from [basics_alpha.ml] once we have float64 by
   default *)

type t_float64 [@@float64]
type ('a : float64) t_float64_id = 'a;;
[%%expect{|
Line 1, characters 15-26:
1 | type t_float64 [@@float64]
                   ^^^^^^^^^^^
Error: Layout float64 is used here, but the appropriate layouts extension is not enabled
|}]

