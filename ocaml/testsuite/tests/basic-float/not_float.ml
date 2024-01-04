(* TEST
   * expect
*)

type t1 = private unit not_float
type !'a t2 = private 'a not_float
type !+'a t3 = private 'a not_float
type !-'a t4 = private 'a not_float
type +'a t5 = private unit not_float
type -'a t6 = private unit not_float
type 'a t7 = private unit not_float
type (!+'a, !-'b, 'c, !'d, +'e, -'f) t8 = private ('a * 'b * 'd) not_float

[%%expect{|
type t1 = private unit not_float
type 'a t2 = private 'a not_float
type +'a t3 = private 'a not_float
type -'a t4 = private 'a not_float
type +'a t5 = private unit not_float
type -'a t6 = private unit not_float
type 'a t7 = private unit not_float
type (+'a, -'b, 'c, 'd, +'e, -'f) t8 = private ('a * 'b * 'd) not_float
|}]

type !'a t9 = private unit not_float

[%%expect{|
Line 1, characters 0-36:
1 | type !'a t9 = private unit not_float
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be injective invariant,
       but it is unrestricted.
|}]
