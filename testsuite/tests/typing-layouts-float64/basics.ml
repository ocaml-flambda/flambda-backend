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

(* CR layouts: The below test checks that we give an acceptable error for cases
   where a float64 was used from a library in a file where the user forgot to
   enable the relevant extension.  It can be deleted when float64 is on by
   default (though at that time we should add tests for explicitly disabling the
   layouts extension). *)
let f x = Stdlib__Float_u.sin x
[%%expect{|

Line 1, characters 6-31:
1 | let f x = Stdlib__Float_u.sin x
          ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Non-value layout float64 detected as sort for type float#,
       but this requires extension layouts_beta, which is not enabled.
       If you intended to use this layout, please add this flag to your build file.
       Otherwise, please report this error to the Jane Street compilers team.
|}]
