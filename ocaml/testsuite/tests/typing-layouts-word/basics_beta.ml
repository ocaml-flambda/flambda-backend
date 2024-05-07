(* TEST
   flags = "-extension layouts_beta";
   expect;
*)

(* We should move these back into [basics.ml] once
   mixed blocks are out of beta. *)

type t_word : word
type ('a : word) t_word_id = 'a

[%%expect{|
type t_word : word
type ('a : word) t_word_id = 'a
|}];;

(****************************************************)
(* Test 5: Allowed in some structures in typedecls. *)

type t5_1 = { x : t_word };;
[%%expect{|
Line 1, characters 0-26:
1 | type t5_1 = { x : t_word };;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The enabled layouts extension does not allow for mixed records.
       You must enable -extension layouts_alpha to use this feature.
|}];;

type t5_2 = { y : int; x : t_word };;
[%%expect{|
Line 1, characters 0-35:
1 | type t5_2 = { y : int; x : t_word };;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The enabled layouts extension does not allow for mixed records.
       You must enable -extension layouts_alpha to use this feature.
|}];;

type t5_2' = { y : string; x : t_word };;
[%%expect{|
Line 1, characters 0-39:
1 | type t5_2' = { y : string; x : t_word };;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The enabled layouts extension does not allow for mixed records.
       You must enable -extension layouts_alpha to use this feature.
|}];;

type t5_4 = A of t_word;;
[%%expect{|
Line 1, characters 12-23:
1 | type t5_4 = A of t_word;;
                ^^^^^^^^^^^
Error: The enabled layouts extension does not allow for mixed constructors.
       You must enable -extension layouts_alpha to use this feature.
|}];;

type t5_5 = A of int * t_word;;
[%%expect{|
Line 1, characters 12-29:
1 | type t5_5 = A of int * t_word;;
                ^^^^^^^^^^^^^^^^^
Error: The enabled layouts extension does not allow for mixed constructors.
       You must enable -extension layouts_alpha to use this feature.
|}];;

type ('a : word) t5_7 = A of int
type ('a : word) t5_8 = A of 'a;;
[%%expect{|
type ('a : word) t5_7 = A of int
Line 2, characters 24-31:
2 | type ('a : word) t5_8 = A of 'a;;
                            ^^^^^^^
Error: The enabled layouts extension does not allow for mixed constructors.
       You must enable -extension layouts_alpha to use this feature.
|}]

(* not allowed: value in flat suffix *)
type 'a t_disallowed = A of t_word * 'a

[%%expect{|
Line 1, characters 23-39:
1 | type 'a t_disallowed = A of t_word * 'a
                           ^^^^^^^^^^^^^^^^
Error: Expected all flat constructor arguments after non-value argument,
       t_word, but found boxed argument, 'a.
|}]

(***************************************************)
(* Test 11: Allow word in some extensible variants *)

type t11_1 = ..

type t11_1 += A of t_word;;
[%%expect{|
type t11_1 = ..
Line 3, characters 14-25:
3 | type t11_1 += A of t_word;;
                  ^^^^^^^^^^^
Error: The enabled layouts extension does not allow for mixed constructors.
       You must enable -extension layouts_alpha to use this feature.
|}]

type t11_1 += B of nativeint#;;
[%%expect{|
Line 1, characters 14-29:
1 | type t11_1 += B of nativeint#;;
                  ^^^^^^^^^^^^^^^
Error: The enabled layouts extension does not allow for mixed constructors.
       You must enable -extension layouts_alpha to use this feature.
|}]

type ('a : word) t11_2 = ..

type 'a t11_2 += A of int

type 'a t11_2 += B of 'a;;

[%%expect{|
type ('a : word) t11_2 = ..
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
