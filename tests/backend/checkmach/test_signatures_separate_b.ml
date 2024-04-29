(* Tests showing how [zero_alloc] information from a signature can be used in
   checking uses of the corresponding functions.

   This is part of a test of separate compilation. This file uses the library
   [test_signatures_separate_a.ml]. The dune rules delete its compiled
   implementation first, so we're only using its signature.
*)

module A = Test_signatures_separate_a

(* Basic behavior with strict and assume *)
let[@zero_alloc] f_id_is_za x = A.f_id x

let[@zero_alloc strict] f_id_is_not_za_strict x = A.f_id x

let[@zero_alloc] f_id_strict_is_za x = A.f_id_strict x

let[@zero_alloc strict] f_id_strict_is_za_strict x = A.f_id_strict x

let[@zero_alloc] f_tuple_assume_is_za x = A.f_tuple_assume x

let[@zero_alloc strict] f_tuple_assume_is_not_za_strict x = A.f_tuple_assume x

let[@zero_alloc] f_tuple_no_assume_is_not_za x = A.f_tuple_no_assume x

let[@zero_alloc strict] f_tuple_no_assume_is_not_za_strict x =
  A.f_tuple_no_assume x

(* opt behavior - these functions are marked zero-alloc opt in A. We use expect
   to accept these as functions as "zero_alloc opt" in B always (because when
   compiling with `-zero_alloc_check all` we make those assumptions available,
   and when compiling without it we don't check "zero_alloc opt" facts here.

   However, we can also use these assumptions even to prove normal non-opt
   zero_alloc facts, when compiling this module with -zero-alloc-check all. This
   is basically a bug, but because the whole build is compiled with one
   -zero-alloc-check X setting, we're not too worried about it.  These tests
   document the current behavior, but it would be OK if we changed things so
   that the ones without "opt" fail.
*)
let[@zero_alloc opt] f_id_opt_is_za_opt x =
  A.f_id_opt x

let[@zero_alloc] f_id_opt_is_za_iff_all x =
  A.f_id_opt x

let[@zero_alloc strict opt] f_id_opt_is_not_za_strict_opt x =
  A.f_id_opt x

let[@zero_alloc strict] f_id_opt_is_not_za_strict x =
  A.f_id_opt x

let[@zero_alloc strict opt] f_id_strict_opt_is_za_strict_opt x =
  A.f_id_strict_opt x

(* arity tests - you can only use an assume if the arity matches. *)
let[@zero_alloc] f_arity_one_is_za_one_arg x = A.f_arity_one x

let[@zero_alloc] f_arity_one_is_not_za_two_args x = A.f_arity_one (fun y -> y) x

let[@zero_alloc] f_arity_two_is_not_za_one_arg x = A.f_arity_two x

let[@zero_alloc] f_arity_two_is_za_two_args x y = A.f_arity_two x y

let[@zero_alloc] f_arity_two_is_not_za_three_args x y z = A.f_arity_two x y z

let[@zero_alloc] f_arity_three_is_not_za_one_arg x = A.f_arity_three x

let[@zero_alloc] f_arity_three_is_not_za_two_args x y = A.f_arity_three x y

let[@zero_alloc] f_arity_three_is_za_three_args x y z = A.f_arity_three x y z

let[@zero_alloc] f_arity_two_sig_is_not_za_one_arg x =
  A.M_arity.f_arity_two_sig x

let[@zero_alloc] f_arity_two_sig_is_za_two_args x y =
  A.M_arity.f_arity_two_sig x y

let[@zero_alloc] f_arity_two_sig_is_not_za_three_args x y z =
  A.M_arity.f_arity_two_sig x y z
