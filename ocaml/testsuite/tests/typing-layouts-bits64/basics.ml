(* TEST
   flags = "-extension layouts"
   * expect
*)

(* This file contains typing tests for the layout [bits64].

   Runtime tests for the type [int64#] can be found in the
   [unboxed_int64], [alloc], and [stdlib__int64_u] tests in this
   directory.  The type [int64#] here is used as a convenient example of a
   concrete [bits64] type in some tests, but its behavior isn't the primary
   purpose of this test. *)

(* CR layouts: We'll be moving code from [basics_beta] to this file as support
   for different features becomes available at this level of stability. *)

type t_bits64 : bits64
type ('a : bits64) t_bits64_id = 'a;;
[%%expect{|
Line 1, characters 16-22:
1 | type t_bits64 : bits64
                    ^^^^^^
Error: Layout bits64 is more experimental than allowed by the enabled layouts extension.
       You must enable -extension layouts_alpha to use this feature.
|}]
