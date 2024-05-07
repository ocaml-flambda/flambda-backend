(* TEST
   flags = "-extension layouts_beta";
   expect;
*)

(* We should move these back into [basics.ml] once
   mixed blocks are out of beta. *)

type t_bits32 : bits32
type ('a : bits32) t_bits32_id = 'a

[%%expect{|
type t_bits32 : bits32
type ('a : bits32) t_bits32_id = 'a
|}];;

(* Test 5: structures *)

type t5_1 = { x : t_bits32 };;
[%%expect{|
Line 1, characters 0-28:
1 | type t5_1 = { x : t_bits32 };;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The enabled layouts extension does not allow for mixed records.
       You must enable -extension layouts_alpha to use this feature.
|}];;

(* CR layouts v5: this should work *)
type t5_2 = { y : int; x : t_bits32 };;
[%%expect{|
Line 1, characters 0-37:
1 | type t5_2 = { y : int; x : t_bits32 };;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The enabled layouts extension does not allow for mixed records.
       You must enable -extension layouts_alpha to use this feature.
|}];;

(* CR layouts: this runs afoul of the mixed block restriction, but should work
   once we relax that. *)
type t5_2' = { y : string; x : t_bits32 };;
[%%expect{|
Line 1, characters 0-41:
1 | type t5_2' = { y : string; x : t_bits32 };;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The enabled layouts extension does not allow for mixed records.
       You must enable -extension layouts_alpha to use this feature.
|}];;

type t5_4 = A of t_bits32;;
[%%expect{|
Line 1, characters 12-25:
1 | type t5_4 = A of t_bits32;;
                ^^^^^^^^^^^^^
Error: The enabled layouts extension does not allow for mixed constructors.
       You must enable -extension layouts_alpha to use this feature.
|}];;

type t5_5 = A of int * t_bits32;;
[%%expect{|
Line 1, characters 12-31:
1 | type t5_5 = A of int * t_bits32;;
                ^^^^^^^^^^^^^^^^^^^
Error: The enabled layouts extension does not allow for mixed constructors.
       You must enable -extension layouts_alpha to use this feature.
|}];;

type ('a : bits32) t5_7 = A of int
type ('a : bits32) t5_8 = A of 'a;;
[%%expect{|
type ('a : bits32) t5_7 = A of int
Line 2, characters 26-33:
2 | type ('a : bits32) t5_8 = A of 'a;;
                              ^^^^^^^
Error: The enabled layouts extension does not allow for mixed constructors.
       You must enable -extension layouts_alpha to use this feature.
|}]

(* not allowed: value in flat suffix *)
type 'a t_disallowed = A of t_bits32 * 'a

[%%expect{|
Line 1, characters 23-41:
1 | type 'a t_disallowed = A of t_bits32 * 'a
                           ^^^^^^^^^^^^^^^^^^
Error: Expected all flat constructor arguments after non-value argument,
       t_bits32, but found boxed argument, 'a.
|}]

(* Test 11: Extensible variants *)

type t11_1 = ..

type t11_1 += A of t_bits32;;
[%%expect{|
type t11_1 = ..
Line 3, characters 14-27:
3 | type t11_1 += A of t_bits32;;
                  ^^^^^^^^^^^^^
Error: The enabled layouts extension does not allow for mixed constructors.
       You must enable -extension layouts_alpha to use this feature.
|}]

type t11_1 += B of int32#;;
[%%expect{|
Line 1, characters 14-25:
1 | type t11_1 += B of int32#;;
                  ^^^^^^^^^^^
Error: The enabled layouts extension does not allow for mixed constructors.
       You must enable -extension layouts_alpha to use this feature.
|}]

type ('a : bits32) t11_2 = ..

type 'a t11_2 += A of int

type 'a t11_2 += B of 'a;;

[%%expect{|
type ('a : bits32) t11_2 = ..
type 'a t11_2 += A of int
Line 5, characters 17-24:
5 | type 'a t11_2 += B of 'a;;
                     ^^^^^^^
Error: The enabled layouts extension does not allow for mixed constructors.
       You must enable -extension layouts_alpha to use this feature.
|}]

(* not allowed: value in flat suffix *)
type 'a t11_2 += C : 'a * 'b -> 'a t11_2

[%%expect{|
Line 1, characters 17-40:
1 | type 'a t11_2 += C : 'a * 'b -> 'a t11_2
                     ^^^^^^^^^^^^^^^^^^^^^^^
Error: Expected all flat constructor arguments after non-value argument, 'a,
       but found boxed argument, 'b.
|}]
