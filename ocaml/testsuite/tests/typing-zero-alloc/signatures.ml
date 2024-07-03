(* TEST
   expect;
*)

(* This tests the typing behavior of `[@zero_alloc]` annotation in signatures.

   These tests are just about what is allowed and not allowed by the
   typechecker.  The implementation of the actual `[@zero_alloc]` backend checks
   (including how the annotations in signatures affect those checks) are tested
   in the `tests/backend/checkmach` directory at the root of the project.
*)

(*******************************************)
(* Test 1: Allowed and disallowed payloads *)
module type S_payloads_base = sig
  val[@zero_alloc] f : int -> int
end

module type S_payloads_opt = sig
  val[@zero_alloc opt] f : int -> int
end

module type S_payloads_strict = sig
  val[@zero_alloc strict] f : int -> int
end

module type S_payloads_strict_opt = sig
  val[@zero_alloc strict opt] f : int -> int
end
[%%expect{|
module type S_payloads_base = sig val f : int -> int [@@zero_alloc] end
module type S_payloads_opt = sig val f : int -> int [@@zero_alloc opt] end
module type S_payloads_strict =
  sig val f : int -> int [@@zero_alloc strict] end
module type S_payloads_strict_opt =
  sig val f : int -> int [@@zero_alloc strict opt] end
|}]

module type S_payloads_assume = sig
  val[@zero_alloc assume] f : int -> int
end
[%%expect{|
Line 2, characters 2-40:
2 |   val[@zero_alloc assume] f : int -> int
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: zero_alloc "assume" attributes are not supported in signatures
|}]

module type S_payloads_ignore = sig
  val[@zero_alloc ignore] f : int -> int
end
[%%expect{|
Line 2, characters 2-40:
2 |   val[@zero_alloc ignore] f : int -> int
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: zero_alloc "ignore" attributes are not supported in signatures
|}]

(******************************)
(* Test 2: allowed inclusions *)
module type S_good_inc_base = sig
  val[@zero_alloc] f : 'a -> 'a
end

module M_absent : S_good_inc_base = struct
  (* This one works by inferring the annotation on [f], tested further
     elsewhere. *)
  let f x = x
end

module M_base : S_good_inc_base = struct
  let[@zero_alloc] f x = x
end

module M_assume : S_good_inc_base = struct
  let[@zero_alloc assume] f x = x
end

module M_assume_nrn : S_good_inc_base = struct
  let[@zero_alloc assume never_returns_normally] f x = x
end

module M_stict : S_good_inc_base = struct
  let[@zero_alloc strict] f x = x
end

module M_assume_strict : S_good_inc_base = struct
  let[@zero_alloc assume strict] f x = x
end

module M_assume_strict_nrn : S_good_inc_base = struct
  let[@zero_alloc assume strict never_returns_normally] f x = x
end

[%%expect{|
module type S_good_inc_base = sig val f : 'a -> 'a [@@zero_alloc] end
module M_absent : S_good_inc_base
module M_base : S_good_inc_base
module M_assume : S_good_inc_base
module M_assume_nrn : S_good_inc_base
module M_stict : S_good_inc_base
module M_assume_strict : S_good_inc_base
module M_assume_strict_nrn : S_good_inc_base
|}]

module type S_good_inc_opt = sig
  val[@zero_alloc opt] f : 'a -> 'a
end

module M_absent : S_good_inc_opt = struct
  (* This one works by inferring the annotation on [f], tested further
     elsewhere. *)
  let f x = x
end

module M_base : S_good_inc_opt = struct
  let[@zero_alloc] f x = x
end

module M_opt : S_good_inc_opt = struct
  let[@zero_alloc opt] f x = x
end

module M_assume : S_good_inc_opt = struct
  let[@zero_alloc assume] f x = x
end

module M_assume_nrn : S_good_inc_opt = struct
  let[@zero_alloc assume never_returns_normally] f x = x
end

module M_strict : S_good_inc_opt = struct
  let[@zero_alloc strict] f x = x
end

module M_strict_opt : S_good_inc_opt = struct
  let[@zero_alloc strict opt] f x = x
end

module M_assume_strict : S_good_inc_opt = struct
  let[@zero_alloc assume strict] f x = x
end

module M_assume_strict_nrn : S_good_inc_opt = struct
  let[@zero_alloc assume strict never_returns_normally] f x = x
end

[%%expect{|
module type S_good_inc_opt = sig val f : 'a -> 'a [@@zero_alloc opt] end
module M_absent : S_good_inc_opt
module M_base : S_good_inc_opt
module M_opt : S_good_inc_opt
module M_assume : S_good_inc_opt
module M_assume_nrn : S_good_inc_opt
module M_strict : S_good_inc_opt
module M_strict_opt : S_good_inc_opt
module M_assume_strict : S_good_inc_opt
module M_assume_strict_nrn : S_good_inc_opt
|}]

module type S_good_inc_strict = sig
  val[@zero_alloc strict] f : 'a -> 'a
end

module M_absent : S_good_inc_strict = struct
  (* This one works by inferring the annotation on [f], tested further
     elsewhere. *)
  let f x = x
end

module M_strict : S_good_inc_strict = struct
  let[@zero_alloc strict] f x = x
end

module M_assume_strict : S_good_inc_strict = struct
  let[@zero_alloc assume strict] f x = x
end

module M_assume_strict_nrn : S_good_inc_strict = struct
  let[@zero_alloc assume strict never_returns_normally] f x = x
end

[%%expect{|
module type S_good_inc_strict =
  sig val f : 'a -> 'a [@@zero_alloc strict] end
module M_absent : S_good_inc_strict
module M_strict : S_good_inc_strict
module M_assume_strict : S_good_inc_strict
module M_assume_strict_nrn : S_good_inc_strict
|}]

module type S_good_inc_strict_opt = sig
  val[@zero_alloc strict opt] f : 'a -> 'a
end

module M_absent : S_good_inc_strict_opt = struct
  (* This one works by inferring the annotation on [f], tested further
     elsewhere. *)
  let f x = x
end

module M_strict : S_good_inc_strict_opt = struct
  let[@zero_alloc strict] f x = x
end

module M_strict_opt : S_good_inc_strict_opt = struct
  let[@zero_alloc strict opt] f x = x
end

module M_assume_strict : S_good_inc_strict_opt = struct
  let[@zero_alloc assume strict] f x = x
end

module M_assume_strict_nrn : S_good_inc_strict_opt = struct
  let[@zero_alloc assume strict never_returns_normally] f x = x
end

[%%expect{|
module type S_good_inc_strict_opt =
  sig val f : 'a -> 'a [@@zero_alloc strict opt] end
module M_absent : S_good_inc_strict_opt
module M_strict : S_good_inc_strict_opt
module M_strict_opt : S_good_inc_strict_opt
module M_assume_strict : S_good_inc_strict_opt
module M_assume_strict_nrn : S_good_inc_strict_opt
|}]


(*********************************)
(* Test 3: disallowed inclusions *)

module type S_bad_inc_base = sig
  val[@zero_alloc] f : 'a -> 'a
end

module M_opt : S_bad_inc_base = struct
  let[@zero_alloc opt] f x = x
end

[%%expect{|
module type S_bad_inc_base = sig val f : 'a -> 'a [@@zero_alloc] end
Lines 5-7, characters 32-3:
5 | ................................struct
6 |   let[@zero_alloc opt] f x = x
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'a -> 'a [@@zero_alloc opt] end
       is not included in
         S_bad_inc_base
       Values do not match:
         val f : 'a -> 'a [@@zero_alloc opt]
       is not included in
         val f : 'a -> 'a [@@zero_alloc]
       The former provides a weaker "zero_alloc" guarantee than the latter.
|}]

module type S_bad_inc_strict = sig
  val[@zero_alloc strict] f : 'a -> 'a
end

module M_base : S_bad_inc_strict = struct
  let[@zero_alloc] f x = x
end
[%%expect{|
module type S_bad_inc_strict = sig val f : 'a -> 'a [@@zero_alloc strict] end
Lines 5-7, characters 35-3:
5 | ...................................struct
6 |   let[@zero_alloc] f x = x
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'a -> 'a [@@zero_alloc] end
       is not included in
         S_bad_inc_strict
       Values do not match:
         val f : 'a -> 'a [@@zero_alloc]
       is not included in
         val f : 'a -> 'a [@@zero_alloc strict]
       The former provides a weaker "zero_alloc" guarantee than the latter.
|}]

module M_assume : S_bad_inc_strict = struct
  let[@zero_alloc assume] f x = x
end

[%%expect{|
Lines 1-3, characters 37-3:
1 | .....................................struct
2 |   let[@zero_alloc assume] f x = x
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'a -> 'a [@@zero_alloc] end
       is not included in
         S_bad_inc_strict
       Values do not match:
         val f : 'a -> 'a [@@zero_alloc]
       is not included in
         val f : 'a -> 'a [@@zero_alloc strict]
       The former provides a weaker "zero_alloc" guarantee than the latter.
|}]

module M_opt : S_bad_inc_strict = struct
  let[@zero_alloc opt] f x = x
end

[%%expect{|
Lines 1-3, characters 34-3:
1 | ..................................struct
2 |   let[@zero_alloc opt] f x = x
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'a -> 'a [@@zero_alloc opt] end
       is not included in
         S_bad_inc_strict
       Values do not match:
         val f : 'a -> 'a [@@zero_alloc opt]
       is not included in
         val f : 'a -> 'a [@@zero_alloc strict]
       The former provides a weaker "zero_alloc" guarantee than the latter.
|}]

module M_strict_opt : S_bad_inc_strict = struct
  let[@zero_alloc strict opt] f x = x
end

[%%expect{|
Lines 1-3, characters 41-3:
1 | .........................................struct
2 |   let[@zero_alloc strict opt] f x = x
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'a -> 'a [@@zero_alloc strict opt] end
       is not included in
         S_bad_inc_strict
       Values do not match:
         val f : 'a -> 'a [@@zero_alloc strict opt]
       is not included in
         val f : 'a -> 'a [@@zero_alloc strict]
       The former provides a weaker "zero_alloc" guarantee than the latter.
|}]

module M_assume_nrn : S_bad_inc_strict = struct
  let[@zero_alloc assume never_returns_normally] f x = x
end

[%%expect{|
Lines 1-3, characters 41-3:
1 | .........................................struct
2 |   let[@zero_alloc assume never_returns_normally] f x = x
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'a -> 'a [@@zero_alloc] end
       is not included in
         S_bad_inc_strict
       Values do not match:
         val f : 'a -> 'a [@@zero_alloc]
       is not included in
         val f : 'a -> 'a [@@zero_alloc strict]
       The former provides a weaker "zero_alloc" guarantee than the latter.
|}]

module type S_strict_opt = sig
  val[@zero_alloc strict opt] f : 'a -> 'a
end

module M_assume : S_strict_opt = struct
  let[@zero_alloc assume] f x = x
end

[%%expect{|
module type S_strict_opt = sig val f : 'a -> 'a [@@zero_alloc strict opt] end
Lines 5-7, characters 33-3:
5 | .................................struct
6 |   let[@zero_alloc assume] f x = x
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'a -> 'a [@@zero_alloc] end
       is not included in
         S_strict_opt
       Values do not match:
         val f : 'a -> 'a [@@zero_alloc]
       is not included in
         val f : 'a -> 'a [@@zero_alloc strict opt]
       The former provides a weaker "zero_alloc" guarantee than the latter.
|}]

module M_opt : S_strict_opt = struct
  let[@zero_alloc opt] f x = x
end

[%%expect{|
Lines 1-3, characters 30-3:
1 | ..............................struct
2 |   let[@zero_alloc opt] f x = x
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'a -> 'a [@@zero_alloc opt] end
       is not included in
         S_strict_opt
       Values do not match:
         val f : 'a -> 'a [@@zero_alloc opt]
       is not included in
         val f : 'a -> 'a [@@zero_alloc strict opt]
       The former provides a weaker "zero_alloc" guarantee than the latter.
|}]

module M_assume_nrn : S_strict_opt = struct
  let[@zero_alloc assume never_returns_normally] f x = x
end

[%%expect{|
Lines 1-3, characters 37-3:
1 | .....................................struct
2 |   let[@zero_alloc assume never_returns_normally] f x = x
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'a -> 'a [@@zero_alloc] end
       is not included in
         S_strict_opt
       Values do not match:
         val f : 'a -> 'a [@@zero_alloc]
       is not included in
         val f : 'a -> 'a [@@zero_alloc strict opt]
       The former provides a weaker "zero_alloc" guarantee than the latter.
|}]

(*************************************************************************)
(* Test 4: Requires valid arity, inferred or provided, without expansion *)

module type S_non_func_int = sig
  val[@zero_alloc] x : int
end
[%%expect{|
Line 2, characters 2-26:
2 |   val[@zero_alloc] x : int
      ^^^^^^^^^^^^^^^^^^^^^^^^
Error: In signatures, zero_alloc is only supported on function declarations.
       Found no arrows in this declaration's type.
       Hint: You can write "[@zero_alloc arity n]" to specify the arity
       of an alias (for n > 0).
|}]

module type S_non_func_alias = sig
  type t = string
  val[@zero_alloc] x : t
end
[%%expect{|
Line 3, characters 2-24:
3 |   val[@zero_alloc] x : t
      ^^^^^^^^^^^^^^^^^^^^^^
Error: In signatures, zero_alloc is only supported on function declarations.
       Found no arrows in this declaration's type.
       Hint: You can write "[@zero_alloc arity n]" to specify the arity
       of an alias (for n > 0).
|}]

module type S_func_alias = sig
  type t = int -> int
  type s = t
  val[@zero_alloc] x : s
end
[%%expect{|
Line 4, characters 2-24:
4 |   val[@zero_alloc] x : s
      ^^^^^^^^^^^^^^^^^^^^^^
Error: In signatures, zero_alloc is only supported on function declarations.
       Found no arrows in this declaration's type.
       Hint: You can write "[@zero_alloc arity n]" to specify the arity
       of an alias (for n > 0).
|}]

module type S_func_alias = sig
  type t = int -> int
  val[@zero_alloc arity 0] x : t
end
[%%expect{|
Line 3, characters 2-32:
3 |   val[@zero_alloc arity 0] x : t
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In signatures, zero_alloc is only supported on function declarations.
       Found no arrows in this declaration's type.
       Hint: You can write "[@zero_alloc arity n]" to specify the arity
       of an alias (for n > 0).
|}]

module type S_arity_0 = sig
  val[@zero_alloc arity 0] f : int -> int
end
[%%expect{|
Line 2, characters 2-41:
2 |   val[@zero_alloc arity 0] f : int -> int
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Invalid zero_alloc attribute: arity must be greater than 0.
|}]

module type S_arity_negative = sig
  val[@zero_alloc arity (-1)] f : int -> int
end
[%%expect{|
Line 2, characters 2-44:
2 |   val[@zero_alloc arity (-1)] f : int -> int
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Invalid zero_alloc attribute: arity must be greater than 0.
|}]

(********************************************)
(* Test 5: impl arity must match intf arity *)

type t_two_args = int -> int -> int

module type S_arity_int_int = sig
  val[@zero_alloc] f : int -> int -> int
end

module M_int_int_params : S_arity_int_int = struct
  let[@zero_alloc] f x y = x + y
end

module M_int_param_int_case : S_arity_int_int = struct
  let[@zero_alloc] f x =
    function 0 -> x + x
           | n -> x + n
end

module M_nested_functions : S_arity_int_int = struct
  let[@zero_alloc] f x = fun y -> x + y
end
[%%expect{|
type t_two_args = int -> int -> int
module type S_arity_int_int =
  sig val f : int -> int -> int [@@zero_alloc] end
module M_int_int_params : S_arity_int_int
module M_int_param_int_case : S_arity_int_int
Lines 17-19, characters 46-3:
17 | ..............................................struct
18 |   let[@zero_alloc] f x = fun y -> x + y
19 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : int -> int -> int [@@zero_alloc arity 1] end
       is not included in
         S_arity_int_int
       Values do not match:
         val f : int -> int -> int [@@zero_alloc arity 1]
       is not included in
         val f : int -> int -> int [@@zero_alloc]
       zero_alloc arity mismatch:
       When using "zero_alloc" in a signature, the syntactic arity of
       the implementation must match the function type in the interface.
       Here the former is 1 and the latter is 2.
|}]

module type S_alias_no_arity = sig
  val[@zero_alloc] f : t_two_args
end
[%%expect{|
Line 2, characters 2-33:
2 |   val[@zero_alloc] f : t_two_args
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In signatures, zero_alloc is only supported on function declarations.
       Found no arrows in this declaration's type.
       Hint: You can write "[@zero_alloc arity n]" to specify the arity
       of an alias (for n > 0).
|}]

module type S_alias_explicit_arity_2 = sig
  val[@zero_alloc arity 2] f : t_two_args
end

module M_good_explicit_arity_2 : S_alias_explicit_arity_2 = struct
  let[@zero_alloc] f x y = x + y
end
[%%expect{|
module type S_alias_explicit_arity_2 =
  sig val f : t_two_args [@@zero_alloc arity 2] end
module M_good_explicit_arity_2 : S_alias_explicit_arity_2
|}]

module M_bad_explicit_arity_2 : S_alias_explicit_arity_2 = struct
  let[@zero_alloc] f x = fun y -> x + y
end
[%%expect{|
Lines 1-3, characters 59-3:
1 | ...........................................................struct
2 |   let[@zero_alloc] f x = fun y -> x + y
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : int -> int -> int [@@zero_alloc arity 1] end
       is not included in
         S_alias_explicit_arity_2
       Values do not match:
         val f : int -> int -> int [@@zero_alloc arity 1]
       is not included in
         val f : t_two_args [@@zero_alloc arity 2]
       zero_alloc arity mismatch:
       When using "zero_alloc" in a signature, the syntactic arity of
       the implementation must match the function type in the interface.
       Here the former is 1 and the latter is 2.
|}]

module type S_alias_explicit_arity_1 = sig
  val[@zero_alloc arity 1] f : t_two_args
end

module M_bad_explicit_arity_1 : S_alias_explicit_arity_1 = struct
  let[@zero_alloc] f x y = x + y
end
[%%expect{|
module type S_alias_explicit_arity_1 =
  sig val f : t_two_args [@@zero_alloc arity 1] end
Lines 5-7, characters 59-3:
5 | ...........................................................struct
6 |   let[@zero_alloc] f x y = x + y
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : int -> int -> int [@@zero_alloc] end
       is not included in
         S_alias_explicit_arity_1
       Values do not match:
         val f : int -> int -> int [@@zero_alloc]
       is not included in
         val f : t_two_args [@@zero_alloc arity 1]
       zero_alloc arity mismatch:
       When using "zero_alloc" in a signature, the syntactic arity of
       the implementation must match the function type in the interface.
       Here the former is 2 and the latter is 1.
|}]

module M_good_explicit_arity_1 : S_alias_explicit_arity_1 = struct
  let[@zero_alloc] f x = fun y -> x + y
end
[%%expect{|
module M_good_explicit_arity_1 : S_alias_explicit_arity_1
|}]

(******************************************************************)
(* Test 6: we don't update the arity as a result of substitutions *)

module type S_abstract = sig
  type t
  val[@zero_alloc] f : int -> t
end

module M_abstract : S_abstract = struct
  type t = int
  let[@zero_alloc] f x = x
end

module type S_subst = S_abstract with type t = int -> int

module M_subst_good : S_subst  = struct
  type t = int -> int
  let[@zero_alloc] f x = fun y -> x + y
end

module M_subst_bad : S_subst  = struct
  type t = int -> int
  let[@zero_alloc] f x y = x + y
end
[%%expect{|
module type S_abstract = sig type t val f : int -> t [@@zero_alloc] end
module M_abstract : S_abstract
module type S_subst =
  sig type t = int -> int val f : int -> t [@@zero_alloc] end
module M_subst_good : S_subst
Lines 18-21, characters 32-3:
18 | ................................struct
19 |   type t = int -> int
20 |   let[@zero_alloc] f x y = x + y
21 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = int -> int val f : int -> int -> int [@@zero_alloc] end
       is not included in
         S_subst
       Values do not match:
         val f : int -> int -> int [@@zero_alloc]
       is not included in
         val f : int -> t [@@zero_alloc]
       zero_alloc arity mismatch:
       When using "zero_alloc" in a signature, the syntactic arity of
       the implementation must match the function type in the interface.
       Here the former is 2 and the latter is 1.
|}]

(********************************************************************)
(* Test 7: A practicalish example of a non-obvious zero_alloc arity *)

module type S_fun_in_fun = sig
  val[@zero_alloc arity 2] f : int -> int -> int -> int*int
end

(* The expected behavior from the backend analysis for the two funtions below
   is checked in [tests/backend/checkmach/test_arity.ml] *)

module M_fun_in_fun_good : S_fun_in_fun = struct
  let[@zero_alloc] f x y =
    if x = y+1 then fun z -> (z,z) else fun z -> (z,0)
end

module M_fun_in_fun_bad : S_fun_in_fun = struct
  let[@zero_alloc] f x y z =
    if x = y+1 then (z,z) else (z,0)
end

[%%expect{|
module type S_fun_in_fun =
  sig val f : int -> int -> int -> int * int [@@zero_alloc arity 2] end
module M_fun_in_fun_good : S_fun_in_fun
Lines 13-16, characters 41-3:
13 | .........................................struct
14 |   let[@zero_alloc] f x y z =
15 |     if x = y+1 then (z,z) else (z,0)
16 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : int -> int -> int -> int * int [@@zero_alloc] end
       is not included in
         S_fun_in_fun
       Values do not match:
         val f : int -> int -> int -> int * int [@@zero_alloc]
       is not included in
         val f : int -> int -> int -> int * int [@@zero_alloc arity 2]
       zero_alloc arity mismatch:
       When using "zero_alloc" in a signature, the syntactic arity of
       the implementation must match the function type in the interface.
       Here the former is 3 and the latter is 2.
|}]

(*************************************)
(* Test 8: Parsing "arity n" works *)

module type S_arity_42 = sig
  val[@zero_alloc arity 42] f : int -> int
end

module type S_arity_42_strict = sig
  val[@zero_alloc arity 42 strict] f : int -> int
end

module type S_strict_arity_42 = sig
  val[@zero_alloc strict arity 42] f : int -> int
end

module type S_arity_42_opt_strict = sig
  val[@zero_alloc arity 42 opt strict] f : int -> int
end

module type S_opt_arity_42_strict = sig
  val[@zero_alloc opt arity 42 strict] f : int -> int
end

module type S_opt_strict_arity_42 = sig
  val[@zero_alloc opt strict arity 42] f : int -> int
end

[%%expect{|
module type S_arity_42 = sig val f : int -> int [@@zero_alloc arity 42] end
module type S_arity_42_strict =
  sig val f : int -> int [@@zero_alloc strict arity 42] end
module type S_strict_arity_42 =
  sig val f : int -> int [@@zero_alloc strict arity 42] end
module type S_arity_42_opt_strict =
  sig val f : int -> int [@@zero_alloc strict opt arity 42] end
module type S_opt_arity_42_strict =
  sig val f : int -> int [@@zero_alloc strict opt arity 42] end
module type S_opt_strict_arity_42 =
  sig val f : int -> int [@@zero_alloc strict opt arity 42] end
|}]

(**************************************************)
(* Test 9: arity n in structures gives warning 47 *)

module M_struct_arity_let_1 = struct
  let[@zero_alloc arity 2] f x y = x + y
end

[%%expect{|
Line 2, characters 7-17:
2 |   let[@zero_alloc arity 2] f x y = x + y
           ^^^^^^^^^^
Warning 47 [attribute-payload]: illegal payload for attribute 'zero_alloc'.
The "arity" field is only supported on "zero_alloc" in signatures

module M_struct_arity_let_1 :
  sig val f : int -> int -> int [@@zero_alloc] end
|}]

module M_struct_arity_let_2 = struct
  let[@zero_alloc arity 2] f = fun x y -> x + y
end
[%%expect{|
Line 2, characters 7-17:
2 |   let[@zero_alloc arity 2] f = fun x y -> x + y
           ^^^^^^^^^^
Warning 47 [attribute-payload]: illegal payload for attribute 'zero_alloc'.
The "arity" field is only supported on "zero_alloc" in signatures

module M_struct_arity_let_2 :
  sig val f : int -> int -> int [@@zero_alloc] end
|}]

module M_struct_arity_let_fun_1 = struct
  let f = fun[@zero_alloc arity 2]  x y -> x + y
end
[%%expect{|
Line 2, characters 15-25:
2 |   let f = fun[@zero_alloc arity 2]  x y -> x + y
                   ^^^^^^^^^^
Warning 47 [attribute-payload]: illegal payload for attribute 'zero_alloc'.
The "arity" field is only supported on "zero_alloc" in signatures

module M_struct_arity_let_fun_1 :
  sig val f : int -> int -> int [@@zero_alloc] end
|}]

module M_struct_arity_let_fun_2 = struct
  let f x =
    if x = 42 then
      fun[@zero_alloc arity 1] y -> y
    else
      fun y -> y + 1
end
[%%expect{|
Line 4, characters 11-21:
4 |       fun[@zero_alloc arity 1] y -> y
               ^^^^^^^^^^
Warning 47 [attribute-payload]: illegal payload for attribute 'zero_alloc'.
The "arity" field is only supported on "zero_alloc" in signatures

module M_struct_arity_let_fun_2 : sig val f : int -> int -> int end
|}]

(*********************************)
(* Test 10: module type of works *)

module M_base_for_mto = struct
  let[@zero_alloc] f x = x+1
end

module type S_base_mto = module type of M_base_for_mto

module M_mto_base_good : S_base_mto = struct
  let[@zero_alloc] f x = x + 2
end

module M_mto_base_bad : S_base_mto = struct
  let[@zero_alloc opt] f x = x + 3
end

[%%expect{|
module M_base_for_mto : sig val f : int -> int [@@zero_alloc] end
module type S_base_mto = sig val f : int -> int [@@zero_alloc] end
module M_mto_base_good : S_base_mto
Lines 11-13, characters 37-3:
11 | .....................................struct
12 |   let[@zero_alloc opt] f x = x + 3
13 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : int -> int [@@zero_alloc opt] end
       is not included in
         S_base_mto
       Values do not match:
         val f : int -> int [@@zero_alloc opt]
       is not included in
         val f : int -> int [@@zero_alloc]
       The former provides a weaker "zero_alloc" guarantee than the latter.
|}]

module M_strict_for_mto = struct
  let[@zero_alloc strict] f x = x+1
end

module type S_strict_mto = module type of M_strict_for_mto

module M_mto_strict_good : S_strict_mto = struct
  let[@zero_alloc strict] f x = x + 2
end

module M_mto_strict_bad : S_strict_mto = struct
  let[@zero_alloc] f x = x + 3
end

[%%expect{|
module M_strict_for_mto : sig val f : int -> int [@@zero_alloc strict] end
module type S_strict_mto = sig val f : int -> int [@@zero_alloc strict] end
module M_mto_strict_good : S_strict_mto
Lines 11-13, characters 41-3:
11 | .........................................struct
12 |   let[@zero_alloc] f x = x + 3
13 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : int -> int [@@zero_alloc] end
       is not included in
         S_strict_mto
       Values do not match:
         val f : int -> int [@@zero_alloc]
       is not included in
         val f : int -> int [@@zero_alloc strict]
       The former provides a weaker "zero_alloc" guarantee than the latter.
|}]

(* You can't sneakily get never_returns_normally or assume in a signature with
   module type of, but nice try.  These tests rely on the fact that the
   printer would show that information, but indeed it would (see printtyp).

   (At the moment the backend doesn't allow for checking never_return_normally,
   but it wouldn't be hard to add, and then we could revisit this). *)
module M_assume_for_mto = struct
  let[@zero_alloc assume] f x = (x+1,x+2)
end

module type S_no_assume = module type of M_assume_for_mto

module M_nrn_for_mto = struct
  let[@zero_alloc assume never_returns_normally] f x = (x+1,x+2)
end

module type S_no_nrn = module type of M_nrn_for_mto

[%%expect{|
module M_assume_for_mto : sig val f : int -> int * int [@@zero_alloc] end
module type S_no_assume = sig val f : int -> int * int [@@zero_alloc] end
module M_nrn_for_mto : sig val f : int -> int * int [@@zero_alloc] end
module type S_no_nrn = sig val f : int -> int * int [@@zero_alloc] end
|}]

(**********************************************)
(* Test 11: inference from signatures, basics *)

(* As mentioned above, we aren't testing here that the functions whose need to
   be checked for zero_alloc is inferred actually get checked.  That is done in
   the backend tests.  Here we are just showing that type system's view of the
   zero_allocness of a function is inferrable.
*)

(* Should work by setting zero_alloc variables in f and g. *)
module M_infer1 : sig
  val[@zero_alloc] f : int -> int
  val g : int -> int
end = struct
  let f x = x
  let g x = x
end

(* Should be rejected because the signature we have for M_infer1 doesn't have
   vars in it (and its zero_alloc values are too weak). *)
module M_infer1' : sig
  val[@zero_alloc] f : int -> int
  val[@zero_alloc] g : int -> int
end = M_infer1
[%%expect{|
module M_infer1 :
  sig val f : int -> int [@@zero_alloc] val g : int -> int end
Line 14, characters 6-14:
14 | end = M_infer1
           ^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig val f : int -> int [@@zero_alloc] val g : int -> int end
       is not included in
         sig
           val f : int -> int [@@zero_alloc]
           val g : int -> int [@@zero_alloc]
         end
       Values do not match:
         val g : int -> int
       is not included in
         val g : int -> int [@@zero_alloc]
       The former provides a weaker "zero_alloc" guarantee than the latter.
       Hint: Add a "zero_alloc" attribute to the implementation.
|}]

module M_infer2 = struct
  let f x = x
  let g x = x
end

(* Should work by updating the variables. *)
module M_infer2' : sig
  val[@zero_alloc opt] f : int -> int
  val[@zero_alloc] g : int -> int
end = M_infer2 (* 1 *)

(* Should work by zero-alloc subtyping. *)
module M_infer2'' : sig
  val[@zero_alloc opt] f : int -> int
  val[@zero_alloc opt] g : int -> int
end = M_infer2 (* 2 *)

(* Should work by updating the variables again. *)
module M_infer2''' : sig
  val[@zero_alloc] f : int -> int
  val[@zero_alloc] g : int -> int
end = M_infer2 (* 3 *)

(* Here we update the variable on [f] but [g] works by subtyping. *)
module M_infer2'''' : sig
  val[@zero_alloc strict] f : int -> int
  val[@zero_alloc opt] g : int -> int
end = M_infer2 (* 4 *)
[%%expect{|
module M_infer2 : sig val f : 'a -> 'a val g : 'a -> 'a end
module M_infer2' :
  sig
    val f : int -> int [@@zero_alloc opt]
    val g : int -> int [@@zero_alloc]
  end
module M_infer2'' :
  sig
    val f : int -> int [@@zero_alloc opt]
    val g : int -> int [@@zero_alloc opt]
  end
module M_infer2''' :
  sig val f : int -> int [@@zero_alloc] val g : int -> int [@@zero_alloc] end
module M_infer2'''' :
  sig
    val f : int -> int [@@zero_alloc strict]
    val g : int -> int [@@zero_alloc opt]
  end
|}]

(* At this point, the inferred signature for [M_infer2] has the accumulated
   constraints. Note that its final signature has the strictest version of the
   checks done on [f] and [g] individually, even though we never compared
   against exactly that combined signature.  This shows, for example, that when
   the constraint for [M_infer2''''] was done we modified [f]'s variable but
   didn't accidentally weaken the variable on [g]. *)
module type S = module type of M_infer2
[%%expect{|
module type S =
  sig
    val f : 'a -> 'a [@@zero_alloc strict]
    val g : 'a -> 'a [@@zero_alloc]
  end
|}]

(********************************************)
(* Test 12: inference plays well with arity *)

(* If the arity doesn't match the signature, you get an error. *)
module M_inf_too_many_args = struct
  type t = int -> int
  let f : int -> t = fun x _ -> x
end

module _ : sig
  type t
  val[@zero_alloc] f : int -> t
end = M_inf_too_many_args
[%%expect{|
module M_inf_too_many_args : sig type t = int -> int val f : int -> t end
Line 9, characters 6-25:
9 | end = M_inf_too_many_args
          ^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig type t = int -> int val f : int -> t end
       is not included in
         sig type t val f : int -> t [@@zero_alloc] end
       Values do not match:
         val f : int -> t
       is not included in
         val f : int -> t [@@zero_alloc]
       zero_alloc arity mismatch:
       When using "zero_alloc" in a signature, the syntactic arity of
       the implementation must match the function type in the interface.
       Here the former is 2 and the latter is 1.
|}]

module M_inf_too_few_args = struct
  let f x = fun y -> x + y
end

module _ : sig
  val[@zero_alloc] f : int -> int -> int
end = M_inf_too_few_args
[%%expect{|
module M_inf_too_few_args : sig val f : int -> int -> int end
Line 7, characters 6-24:
7 | end = M_inf_too_few_args
          ^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig val f : int -> int -> int end
       is not included in
         sig val f : int -> int -> int [@@zero_alloc] end
       Values do not match:
         val f : int -> int -> int
       is not included in
         val f : int -> int -> int [@@zero_alloc]
       zero_alloc arity mismatch:
       When using "zero_alloc" in a signature, the syntactic arity of
       the implementation must match the function type in the interface.
       Here the former is 1 and the latter is 2.
|}]

(* You can fix it with an explicit arity. *)
module M_explicit_arity_2 = struct
  type t = int -> int
  let f : int -> t = fun x _ -> x
end

module _ : sig
  type t
  val[@zero_alloc (arity 2)] f : int -> t
end = M_explicit_arity_2

module type S = module type of M_explicit_arity_2
[%%expect{|
module M_explicit_arity_2 : sig type t = int -> int val f : int -> t end
module type S =
  sig type t = int -> int val f : int -> t [@@zero_alloc arity 2] end
|}]

(***************************)
(* Test 13: module type of *)

(* When you take the module type of a module, you get the current state of its
   variables.  The computed signature itself has no variables, and the original
   module keeps its variables (but changing them won't change the signature you
   got the first time, just the signature you'll get if you do [module type of]
   again). *)
module M_for_mto = struct
  let f x = x+1
end

(* The current state of the variables is "no check" *)
module type S = module type of M_for_mto
[%%expect{|
module M_for_mto : sig val f : int -> int end
module type S = sig val f : int -> int end
|}]

(* [S] itself is fixed. *)
module F (X : S) : sig val[@zero_alloc] f : int -> int end = X
[%%expect{|
Line 1, characters 61-62:
1 | module F (X : S) : sig val[@zero_alloc] f : int -> int end = X
                                                                 ^
Error: Signature mismatch:
       Modules do not match:
         sig val f : int -> int end
       is not included in
         sig val f : int -> int [@@zero_alloc] end
       Values do not match:
         val f : int -> int
       is not included in
         val f : int -> int [@@zero_alloc]
       The former provides a weaker "zero_alloc" guarantee than the latter.
       Hint: Add a "zero_alloc" attribute to the implementation.
|}]

(* But [M_for_mto] retains its variables.  Changing them changes what you get
   next time you do [module type of] on it, but not [S]. *)
module _ : sig
  val[@zero_alloc] f : int -> int
end = M_for_mto

module type S' = module type of M_for_mto
module F' (X : S') : sig val[@zero_alloc] f : int -> int end = X
module F (X : S) : sig val[@zero_alloc] f : int -> int end = X
[%%expect{|
module type S' = sig val f : int -> int [@@zero_alloc] end
module F' : functor (X : S') -> sig val f : int -> int [@@zero_alloc] end
Line 7, characters 61-62:
7 | module F (X : S) : sig val[@zero_alloc] f : int -> int end = X
                                                                 ^
Error: Signature mismatch:
       Modules do not match:
         sig val f : int -> int end
       is not included in
         sig val f : int -> int [@@zero_alloc] end
       Values do not match:
         val f : int -> int
       is not included in
         val f : int -> int [@@zero_alloc]
       The former provides a weaker "zero_alloc" guarantee than the latter.
       Hint: Add a "zero_alloc" attribute to the implementation.
|}]
