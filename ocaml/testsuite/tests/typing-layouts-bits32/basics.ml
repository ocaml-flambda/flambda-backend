(* TEST
   flags = "-extension layouts_beta"
   * expect
*)

(* This file contains typing tests for the layout [bits32].

   Runtime tests for the type [int32#] can be found in the
   [unboxed_int32], [alloc], and [stdlib__int32_u] tests in this
   directory.  The type [int32#] here is used as a convenient example of a
   concrete [bits32] type in some tests, but its behavior isn't the primary
   purpose of this test. *)

(* CR layouts: We'll be moving code from [basics_beta] to this file as support
   for different features becomes available at this level of stability. *)

type t_bits32 [@@bits32]
type ('a : bits32) t_bits32_id = 'a;;
[%%expect{|
Line 1, characters 14-24:
1 | type t_bits32 [@@bits32]
                  ^^^^^^^^^^
Error: Layout bits32 is used here, but the appropriate layouts extension is not enabled
|}]
