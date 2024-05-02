(* TEST
 expect;
*)

(* For each example with regular variants, this test also includes an example
   with extensible variants.
*)

type t_ext = ..
[%%expect {|
type t_ext = ..
|}];;

(* Mixed float-float# constructor args are OK, but the float args aren't flat *)
type t_cstr_boxed_float = A of float * float#

[%%expect{|
Line 1, characters 26-45:
1 | type t_cstr_boxed_float = A of float * float#
                              ^^^^^^^^^^^^^^^^^^^
Error: The enabled layouts extension does not allow for mixed constructors.
       You must enable -extension layouts_alpha to use this feature.
|}];;

type t_ext += A of float * float#

[%%expect{|
Line 1, characters 14-33:
1 | type t_ext += A of float * float#
                  ^^^^^^^^^^^^^^^^^^^
Error: The enabled layouts extension does not allow for mixed constructors.
       You must enable -extension layouts_alpha to use this feature.
|}];;

(* The fact that the float args aren't flat is evidenced by the fact this
   type decl doesn't type-check.
*)
type t_cstr_boxed_float_bad = A of float# * float

[%%expect{|
Line 1, characters 30-49:
1 | type t_cstr_boxed_float_bad = A of float# * float
                                  ^^^^^^^^^^^^^^^^^^^
Error: Expected all flat constructor arguments after non-value argument,
       float#, but found boxed argument, float.
|}];;

type t_ext += A of float# * float

[%%expect{|
Line 1, characters 14-33:
1 | type t_ext += A of float# * float
                  ^^^^^^^^^^^^^^^^^^^
Error: Expected all flat constructor arguments after non-value argument,
       float#, but found boxed argument, float.
|}];;

(* You can't trick the type-checker by adding more constructors *)
type t_cstr_boxed_float_bad_multi_constr =
  | Const
  | A of float# * float

[%%expect{|
Line 3, characters 2-23:
3 |   | A of float# * float
      ^^^^^^^^^^^^^^^^^^^^^
Error: Expected all flat constructor arguments after non-value argument,
       float#, but found boxed argument, float.
|}];;

type t_ext +=
  | Const
  | A of float# * float

[%%expect{|
Line 3, characters 2-23:
3 |   | A of float# * float
      ^^^^^^^^^^^^^^^^^^^^^
Error: Expected all flat constructor arguments after non-value argument,
       float#, but found boxed argument, float.
|}];;

(* When a non-float/float# field appears, [float]
   fields continue to not be considered flat. *)
type t_cstr_boxed_float_plus_more =
  | A of float# * float * int

[%%expect{|
Line 2, characters 2-29:
2 |   | A of float# * float * int
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Expected all flat constructor arguments after non-value argument,
       float#, but found boxed argument, float.
|}];;

type t_ext +=
  | A of float# * float * int

[%%expect{|
Line 2, characters 2-29:
2 |   | A of float# * float * int
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Expected all flat constructor arguments after non-value argument,
       float#, but found boxed argument, float.
|}];;

(* [float] appearing as a non-flat field in the value prefix. *)
type t_cstr_boxed_float = A of float * float# * int

[%%expect{|
Line 1, characters 26-51:
1 | type t_cstr_boxed_float = A of float * float# * int
                              ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The enabled layouts extension does not allow for mixed constructors.
       You must enable -extension layouts_alpha to use this feature.
|}];;

type t_ext += A of float * float# * int

[%%expect{|
Line 1, characters 14-39:
1 | type t_ext += A of float * float# * int
                  ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The enabled layouts extension does not allow for mixed constructors.
       You must enable -extension layouts_alpha to use this feature.
|}];;

(* The third field can't be flat because a non-float/float# field [d] appears.*)
type t_cstr_multi_boxed_float_bad = A of float * float# * float * int

[%%expect{|
Line 1, characters 36-69:
1 | type t_cstr_multi_boxed_float_bad = A of float * float# * float * int
                                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Expected all flat constructor arguments after non-value argument,
       float#, but found boxed argument, float.
|}];;

type t_ext += A of float * float# * float * int

[%%expect{|
Line 1, characters 14-47:
1 | type t_ext += A of float * float# * float * int
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Expected all flat constructor arguments after non-value argument,
       float#, but found boxed argument, float.
|}];;

(* String can't appear in the flat suffix *)
type t_cstr_flat_string_bad1 = A of float# * string

[%%expect{|
Line 1, characters 31-51:
1 | type t_cstr_flat_string_bad1 = A of float# * string
                                   ^^^^^^^^^^^^^^^^^^^^
Error: Expected all flat constructor arguments after non-value argument,
       float#, but found boxed argument, string.
|}];;

type t_ext += A of float# * string

[%%expect{|
Line 1, characters 14-34:
1 | type t_ext += A of float# * string
                  ^^^^^^^^^^^^^^^^^^^^
Error: Expected all flat constructor arguments after non-value argument,
       float#, but found boxed argument, string.
|}];;

(* The string can't appear in the flat suffix. *)
type t_cstr_flat_string_bad2 = A of float# * float# * string

[%%expect{|
Line 1, characters 31-60:
1 | type t_cstr_flat_string_bad2 = A of float# * float# * string
                                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Expected all flat constructor arguments after non-value argument,
       float#, but found boxed argument, string.
|}];;

type t_ext += A of float# * float# * string

[%%expect{|
Line 1, characters 14-43:
1 | type t_ext += A of float# * float# * string
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Expected all flat constructor arguments after non-value argument,
       float#, but found boxed argument, string.
|}];;

(* The int [c] can appear in the flat suffix. *)
type t_cstr_flat_int = A of float# * float# * int

[%%expect{|
Line 1, characters 23-49:
1 | type t_cstr_flat_int = A of float# * float# * int
                           ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The enabled layouts extension does not allow for mixed constructors.
       You must enable -extension layouts_alpha to use this feature.
|}];;

type t_ext += A of float# * float# * int

[%%expect{|
Line 1, characters 14-40:
1 | type t_ext += A of float# * float# * int
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The enabled layouts extension does not allow for mixed constructors.
       You must enable -extension layouts_alpha to use this feature.
|}];;

type t_cstr_flat_int_multi =
  | A of float# * float# * int
  | B of int
  | C of float# * int
  | D of float# * int * float#
  | E of int * float# * int * float#

[%%expect{|
Line 2, characters 2-30:
2 |   | A of float# * float# * int
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The enabled layouts extension does not allow for mixed constructors.
       You must enable -extension layouts_alpha to use this feature.
|}];;

type t_ext +=
  | A of float# * float# * int
  | B of int
  | C of float# * int
  | D of float# * int * float#
  | E of int * float# * int * float#

[%%expect{|
Line 2, characters 2-30:
2 |   | A of float# * float# * int
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The enabled layouts extension does not allow for mixed constructors.
       You must enable -extension layouts_alpha to use this feature.
|}];;

(* Parameterized types *)

type ('a : float64) t_cstr_param1 = A of string * 'a
[%%expect{|
Line 1, characters 36-52:
1 | type ('a : float64) t_cstr_param1 = A of string * 'a
                                        ^^^^^^^^^^^^^^^^
Error: The enabled layouts extension does not allow for mixed constructors.
       You must enable -extension layouts_alpha to use this feature.
|}];;

type ('a : float64) t_cstr_param_ext1 = ..
type 'a t_cstr_param_ext1 += A of string * 'a
[%%expect{|
type ('a : float64) t_cstr_param_ext1 = ..
Line 2, characters 29-45:
2 | type 'a t_cstr_param_ext1 += A of string * 'a
                                 ^^^^^^^^^^^^^^^^
Error: The enabled layouts extension does not allow for mixed constructors.
       You must enable -extension layouts_alpha to use this feature.
|}];;

type ('a : float64, 'b : immediate) t_cstr_param2 = A of string * 'a * 'b
[%%expect{|
Line 1, characters 52-73:
1 | type ('a : float64, 'b : immediate) t_cstr_param2 = A of string * 'a * 'b
                                                        ^^^^^^^^^^^^^^^^^^^^^
Error: The enabled layouts extension does not allow for mixed constructors.
       You must enable -extension layouts_alpha to use this feature.
|}];;

type ('a : float64, 'b : immediate) t_cstr_param_ext2 = ..
type ('a, 'b) t_cstr_param_ext2 += A of string * 'a * 'b;;

[%%expect{|
type ('a : float64, 'b : immediate) t_cstr_param_ext2 = ..
Line 2, characters 35-56:
2 | type ('a, 'b) t_cstr_param_ext2 += A of string * 'a * 'b;;
                                       ^^^^^^^^^^^^^^^^^^^^^
Error: The enabled layouts extension does not allow for mixed constructors.
       You must enable -extension layouts_alpha to use this feature.
|}];;

type 'a t_cstr_bad_value_after_float = C of float# * 'a

[%%expect{|
Line 1, characters 39-55:
1 | type 'a t_cstr_bad_value_after_float = C of float# * 'a
                                           ^^^^^^^^^^^^^^^^
Error: Expected all flat constructor arguments after non-value argument,
       float#, but found boxed argument, 'a.
|}];;

(* Recursive groups. There's not a good way to exercise the same functionality
   for extensible variants, so we omit that aspect of this test.
*)

type ('a : float64) t_float64_id = 'a
type ('a : immediate) t_immediate_id = 'a
[%%expect{|
type ('a : float64) t_float64_id = 'a
type ('a : immediate) t_immediate_id = 'a
|}];;

type 'a t_float = 'a t_float64_id
and 'a t_imm = 'a t_immediate_id
and ('a, 'b, 'ptr) t_cstr1 = A of 'ptr * 'a * 'a t_float * 'b * 'b t_imm
[%%expect{|
Line 3, characters 46-56:
3 | and ('a, 'b, 'ptr) t_cstr1 = A of 'ptr * 'a * 'a t_float * 'b * 'b t_imm
                                                  ^^^^^^^^^^
Error: Layout mismatch in final type declaration consistency check.
       This is most often caused by the fact that type inference is not
       clever enough to propagate layouts through variables in different
       declarations. It is also not clever enough to produce a good error
       message, so we'll say this instead:
         The layout of 'a is float64, because
           of the definition of t_float64_id at line 1, characters 0-37.
         But the layout of 'a must overlap with value, because
           it instantiates an unannotated type parameter of t_cstr1, defaulted to layout value.
       A good next step is to add a layout annotation on a parameter to
       the declaration where this error is reported.
|}];;

type 'a t_float = 'a t_float64_id
and 'a t_imm = 'a t_immediate_id
and ('a : float64, 'b : immediate, 'ptr) t_cstr2 =
  A of 'ptr * 'a * 'a t_float * 'b * 'b t_imm
[%%expect{|
Line 4, characters 2-45:
4 |   A of 'ptr * 'a * 'a t_float * 'b * 'b t_imm
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The enabled layouts extension does not allow for mixed constructors.
       You must enable -extension layouts_alpha to use this feature.
|}];;

(* There is a cap on the number of fields in the scannable prefix. *)
type ptr = string
type t_cstr_capped =
  A of
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    int * float#
[%%expect{|
type ptr = string
Lines 3-36, characters 2-16:
 3 | ..A of
 4 |     ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
 5 |     ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
 6 |     ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
 7 |     ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
...
33 |     ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
34 |     ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
35 |     ptr * ptr * ptr * ptr * ptr * ptr * ptr *
36 |     int * float#
Error: The enabled layouts extension does not allow for mixed constructors.
       You must enable -extension layouts_alpha to use this feature.
|}];;

type t_ext +=
  A of
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    int * float#
[%%expect{|
Lines 2-35, characters 2-16:
 2 | ..A of
 3 |     ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
 4 |     ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
 5 |     ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
 6 |     ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
...
32 |     ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
33 |     ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
34 |     ptr * ptr * ptr * ptr * ptr * ptr * ptr *
35 |     int * float#
Error: The enabled layouts extension does not allow for mixed constructors.
       You must enable -extension layouts_alpha to use this feature.
|}];;

(* GADT syntax *)

type ('a : float64) tf : float64
type ('a : value) tv : value

[%%expect {|
type ('a : float64) tf : float64
type 'a tv : value
|}]

type ('a : any) t_gadt_any =
  | A : 'a tf -> 'a t_gadt_any
  | B : 'b tv -> 'a t_gadt_any

[%%expect {|
Line 2, characters 2-30:
2 |   | A : 'a tf -> 'a t_gadt_any
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The enabled layouts extension does not allow for mixed constructors.
       You must enable -extension layouts_alpha to use this feature.
|}]

type ('a : any) t_gadt_any_multiple_fields =
  | A : float# * 'a tf -> 'a t_gadt_any_multiple_fields
  | B : 'b tv * float# -> 'a t_gadt_any_multiple_fields

[%%expect {|
Line 2, characters 2-55:
2 |   | A : float# * 'a tf -> 'a t_gadt_any_multiple_fields
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The enabled layouts extension does not allow for mixed constructors.
       You must enable -extension layouts_alpha to use this feature.
|}]

type ('a : any) t_gadt_any_bad =
  | A : float# * 'a tv -> 'a t_gadt_any_bad

[%%expect{|
Line 2, characters 2-43:
2 |   | A : float# * 'a tv -> 'a t_gadt_any_bad
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Expected all flat constructor arguments after non-value argument,
       float#, but found boxed argument, 'a tv.
|}]
