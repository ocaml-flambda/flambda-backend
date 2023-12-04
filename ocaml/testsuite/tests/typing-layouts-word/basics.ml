(* TEST
   flags = "-extension layouts"
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

type t_word : word
type ('a : word) t_word_id = 'a;;
[%%expect{|
Line 1, characters 14-18:
1 | type t_word : word
                  ^^^^
Error: Layout word is more experimental than allowed by the enabled layouts extension.
       You must enable -extension layouts_alpha to use this feature.
|}]
