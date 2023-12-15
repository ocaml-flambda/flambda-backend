(* TEST
   flags = "-extension layouts_beta"
   * expect
*)

(* This file contains typing tests for the layout [word].

   Runtime tests for the type [nativeint#] can be found in the
   [unboxed_nativeint], [alloc], and [stdlib__nativeint_u] tests in this
   directory.  The type [nativeint#] here is used as a convenient example of a
   concrete [word] type in some tests, but its behavior isn't the primary
   purpose of this test. *)

(* CR layouts: We'll be moving code from [basics_beta] to this file as support
   for different features becomes available at this level of stability. *)

type t_word [@@word]
type ('a : word) t_word_id = 'a;;
[%%expect{|
Line 1, characters 12-20:
1 | type t_word [@@word]
                ^^^^^^^^
Error: Layout word is used here, but the appropriate layouts extension is not enabled
|}]
