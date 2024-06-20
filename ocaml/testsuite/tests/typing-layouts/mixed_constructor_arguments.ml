(* TEST
 {
   flags = "-extension layouts_alpha";
   expect;
 }{
   flags = "-extension layouts_beta";
   expect;
 }{
   expect;
 }
*)

(* For each example with regular variants, this test also includes an example
   with (i) variants with inline record arguments, and (ii) extensible variants.
   (At time of writing, extensible variants aren't supported.)
*)

type t_ext = ..
[%%expect {|
type t_ext = ..
|}];;

(* Mixed float-float# constructor args are OK, but the float args aren't flat *)
type t_cstr_boxed_float = A of float * float#

[%%expect{|
type t_cstr_boxed_float = A of float * float#
|}];;

type t_cstr_boxed_float_record = A of { x : float; y : float# }

[%%expect{|
type t_cstr_boxed_float_record = A of { x : float; y : float#; }
|}];;

type t_ext += A of float * float#

[%%expect{|
Line 1, characters 14-33:
1 | type t_ext += A of float * float#
                  ^^^^^^^^^^^^^^^^^^^
Error: Extensible types can't have fields of unboxed type. Consider wrapping the unboxed fields in a record.
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

type t_cstr_boxed_float_bad_record = A of { x : float#; y : float }

[%%expect{|
Line 1, characters 44-55:
1 | type t_cstr_boxed_float_bad_record = A of { x : float#; y : float }
                                                ^^^^^^^^^^^
Error: Expected all flat fields after non-value field, x,
       but found boxed field, y.
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

type t_cstr_boxed_float_bad_multi_constr_record =
  | Const
  | A of { x : float#; y : float }

[%%expect{|
Line 3, characters 11-22:
3 |   | A of { x : float#; y : float }
               ^^^^^^^^^^^
Error: Expected all flat fields after non-value field, x,
       but found boxed field, y.
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

type t_cstr_boxed_float_plus_more_record =
  | A of { x : float#; y : float; z : int }

[%%expect{|
Line 2, characters 11-22:
2 |   | A of { x : float#; y : float; z : int }
               ^^^^^^^^^^^
Error: Expected all flat fields after non-value field, x,
       but found boxed field, y.
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
type t_cstr_boxed_float = A of float * float# * int
|}];;

type t_cstr_boxed_float_record = A of { x : float; y : float#; z : int }

[%%expect{|
type t_cstr_boxed_float_record = A of { x : float; y : float#; z : int; }
|}];;

type t_ext += A of float * float# * int

[%%expect{|
Line 1, characters 14-39:
1 | type t_ext += A of float * float# * int
                  ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Extensible types can't have fields of unboxed type. Consider wrapping the unboxed fields in a record.
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

type t_cstr_multi_boxed_float_record_bad = A of { a : float; b : float#;
                                                  c : float; d : int; }

[%%expect{|
Line 1, characters 61-72:
1 | type t_cstr_multi_boxed_float_record_bad = A of { a : float; b : float#;
                                                                 ^^^^^^^^^^^
Error: Expected all flat fields after non-value field, b,
       but found boxed field, c.
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

type t_cstr_flat_string_record_bad1 = A of { x : float#; y : string }

[%%expect{|
Line 1, characters 45-56:
1 | type t_cstr_flat_string_record_bad1 = A of { x : float#; y : string }
                                                 ^^^^^^^^^^^
Error: Expected all flat fields after non-value field, x,
       but found boxed field, y.
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

type t_cstr_flat_string_record_bad2 = A of { x : float#; y : float#; z : string }

[%%expect{|
Line 1, characters 45-56:
1 | type t_cstr_flat_string_record_bad2 = A of { x : float#; y : float#; z : string }
                                                 ^^^^^^^^^^^
Error: Expected all flat fields after non-value field, x,
       but found boxed field, z.
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
type t_cstr_flat_int = A of float# * float# * int
|}];;

type t_cstr_flat_int_record = A of { a : float#; b : float#; c : int }

[%%expect{|
type t_cstr_flat_int_record = A of { a : float#; b : float#; c : int; }
|}];;

type t_ext += A of float# * float# * int

[%%expect{|
Line 1, characters 14-40:
1 | type t_ext += A of float# * float# * int
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Extensible types can't have fields of unboxed type. Consider wrapping the unboxed fields in a record.
|}];;

type t_cstr_flat_int_multi =
  | A of float# * float# * int
  | B of int
  | C of float# * int
  | D of float# * int * float#
  | E of int * float# * int * float#

[%%expect{|
type t_cstr_flat_int_multi =
    A of float# * float# * int
  | B of int
  | C of float# * int
  | D of float# * int * float#
  | E of int * float# * int * float#
|}];;

type t_cstr_flat_int_multi_record =
  | A of { a : float#; b : float#; c : int }
  | B of { a : int }
  | C of { a : float#; b : int; c : int }
  | D of { a : float#; b : int; c : float# }
  | E of { a : int; b : float#; c : int; d : float# }

[%%expect{|
type t_cstr_flat_int_multi_record =
    A of { a : float#; b : float#; c : int; }
  | B of { a : int; }
  | C of { a : float#; b : int; c : int; }
  | D of { a : float#; b : int; c : float#; }
  | E of { a : int; b : float#; c : int; d : float#; }
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
Error: Extensible types can't have fields of unboxed type. Consider wrapping the unboxed fields in a record.
|}];;

(* Parameterized types *)

type ('a : float64) t_cstr_param1 = A of string * 'a
[%%expect{|
type ('a : float64) t_cstr_param1 = A of string * 'a
|}];;

type ('a : float64) t_cstr_param_record1 = A of { a : string; b : 'a }
[%%expect{|
type ('a : float64) t_cstr_param_record1 = A of { a : string; b : 'a; }
|}];;

type ('a : float64) t_cstr_param_ext1 = ..
type 'a t_cstr_param_ext1 += A of string * 'a
[%%expect{|
type ('a : float64) t_cstr_param_ext1 = ..
Line 2, characters 29-45:
2 | type 'a t_cstr_param_ext1 += A of string * 'a
                                 ^^^^^^^^^^^^^^^^
Error: Extensible types can't have fields of unboxed type. Consider wrapping the unboxed fields in a record.
|}];;

type ('a : float64, 'b : immediate) t_cstr_param2 = A of string * 'a * 'b
[%%expect{|
type ('a : float64, 'b : immediate) t_cstr_param2 = A of string * 'a * 'b
|}];;

type ('a : float64, 'b : immediate) t_cstr_param_record2 =
    A of { x : string; y : 'a; z : 'b }
[%%expect{|
type ('a : float64, 'b : immediate) t_cstr_param_record2 =
    A of { x : string; y : 'a; z : 'b; }
|}];;

type ('a : float64, 'b : immediate) t_cstr_param_ext2 = ..
type ('a, 'b) t_cstr_param_ext2 += A of string * 'a * 'b;;

[%%expect{|
type ('a : float64, 'b : immediate) t_cstr_param_ext2 = ..
Line 2, characters 35-56:
2 | type ('a, 'b) t_cstr_param_ext2 += A of string * 'a * 'b;;
                                       ^^^^^^^^^^^^^^^^^^^^^
Error: Extensible types can't have fields of unboxed type. Consider wrapping the unboxed fields in a record.
|}];;

type 'a t_cstr_bad_value_after_float = C of float# * 'a

[%%expect{|
Line 1, characters 39-55:
1 | type 'a t_cstr_bad_value_after_float = C of float# * 'a
                                           ^^^^^^^^^^^^^^^^
Error: Expected all flat constructor arguments after non-value argument,
       float#, but found boxed argument, 'a.
|}];;

type 'a t_cstr_bad_value_after_float_record = C of { x : float#; y : 'a }

[%%expect{|
Line 1, characters 53-64:
1 | type 'a t_cstr_bad_value_after_float_record = C of { x : float#; y : 'a }
                                                         ^^^^^^^^^^^
Error: Expected all flat fields after non-value field, x,
       but found boxed field, y.
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
type ('a : float64) t_float = 'a t_float64_id
and ('a : immediate) t_imm = 'a t_immediate_id
and ('a : float64, 'b : immediate, 'ptr) t_cstr2 =
    A of 'ptr * 'a * 'a t_float * 'b * 'b t_imm
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
Error: Mixed constructors may contain at most 254 value fields prior to the flat suffix, but this one contains 255.
|}];;

type t_cstr_capped_record = A of {
 a0:ptr; a1:ptr; a2:ptr; a3:ptr; a4:ptr; a5:ptr; a6:ptr; a7:ptr; a8:ptr; a9:ptr;
 b0:ptr; b1:ptr; b2:ptr; b3:ptr; b4:ptr; b5:ptr; b6:ptr; b7:ptr; b8:ptr; b9:ptr;
 c0:ptr; c1:ptr; c2:ptr; c3:ptr; c4:ptr; c5:ptr; c6:ptr; c7:ptr; c8:ptr; c9:ptr;
 d0:ptr; d1:ptr; d2:ptr; d3:ptr; d4:ptr; d5:ptr; d6:ptr; d7:ptr; d8:ptr; d9:ptr;
 e0:ptr; e1:ptr; e2:ptr; e3:ptr; e4:ptr; e5:ptr; e6:ptr; e7:ptr; e8:ptr; e9:ptr;
 f0:ptr; f1:ptr; f2:ptr; f3:ptr; f4:ptr; f5:ptr; f6:ptr; f7:ptr; f8:ptr; f9:ptr;
 g0:ptr; g1:ptr; g2:ptr; g3:ptr; g4:ptr; g5:ptr; g6:ptr; g7:ptr; g8:ptr; g9:ptr;
 h0:ptr; h1:ptr; h2:ptr; h3:ptr; h4:ptr; h5:ptr; h6:ptr; h7:ptr; h8:ptr; h9:ptr;
 i0:ptr; i1:ptr; i2:ptr; i3:ptr; i4:ptr; i5:ptr; i6:ptr; i7:ptr; i8:ptr; i9:ptr;
 j0:ptr; j1:ptr; j2:ptr; j3:ptr; j4:ptr; j5:ptr; j6:ptr; j7:ptr; j8:ptr; j9:ptr;
 k0:ptr; k1:ptr; k2:ptr; k3:ptr; k4:ptr; k5:ptr; k6:ptr; k7:ptr; k8:ptr; k9:ptr;
 l0:ptr; l1:ptr; l2:ptr; l3:ptr; l4:ptr; l5:ptr; l6:ptr; l7:ptr; l8:ptr; l9:ptr;
 m0:ptr; m1:ptr; m2:ptr; m3:ptr; m4:ptr; m5:ptr; m6:ptr; m7:ptr; m8:ptr; m9:ptr;
 n0:ptr; n1:ptr; n2:ptr; n3:ptr; n4:ptr; n5:ptr; n6:ptr; n7:ptr; n8:ptr; n9:ptr;
 o0:ptr; o1:ptr; o2:ptr; o3:ptr; o4:ptr; o5:ptr; o6:ptr; o7:ptr; o8:ptr; o9:ptr;
 p0:ptr; p1:ptr; p2:ptr; p3:ptr; p4:ptr; p5:ptr; p6:ptr; p7:ptr; p8:ptr; p9:ptr;
 q0:ptr; q1:ptr; q2:ptr; q3:ptr; q4:ptr; q5:ptr; q6:ptr; q7:ptr; q8:ptr; q9:ptr;
 r0:ptr; r1:ptr; r2:ptr; r3:ptr; r4:ptr; r5:ptr; r6:ptr; r7:ptr; r8:ptr; r9:ptr;
 s0:ptr; s1:ptr; s2:ptr; s3:ptr; s4:ptr; s5:ptr; s6:ptr; s7:ptr; s8:ptr; s9:ptr;
 t0:ptr; t1:ptr; t2:ptr; t3:ptr; t4:ptr; t5:ptr; t6:ptr; t7:ptr; t8:ptr; t9:ptr;
 u0:ptr; u1:ptr; u2:ptr; u3:ptr; u4:ptr; u5:ptr; u6:ptr; u7:ptr; u8:ptr; u9:ptr;
 v0:ptr; v1:ptr; v2:ptr; v3:ptr; v4:ptr; v5:ptr; v6:ptr; v7:ptr; v8:ptr; v9:ptr;
 w0:ptr; w1:ptr; w2:ptr; w3:ptr; w4:ptr; w5:ptr; w6:ptr; w7:ptr; w8:ptr; w9:ptr;
 x0:ptr; x1:ptr; x2:ptr; x3:ptr; x4:ptr; x5:ptr; x6:ptr; x7:ptr; x8:ptr; x9:ptr;
 y0:ptr; y1:ptr; y2:ptr; y3:ptr; y4:ptr; y5:ptr; y6:ptr; y7:ptr; y8:ptr; y9:ptr;
 z0:ptr; z1:ptr; z2:ptr; z3:ptr; z4:ptr;
 int:int; unboxed:float#
 }
[%%expect{|
Lines 1-29, characters 28-2:
 1 | ............................A of {
 2 |  a0:ptr; a1:ptr; a2:ptr; a3:ptr; a4:ptr; a5:ptr; a6:ptr; a7:ptr; a8:ptr; a9:ptr;
 3 |  b0:ptr; b1:ptr; b2:ptr; b3:ptr; b4:ptr; b5:ptr; b6:ptr; b7:ptr; b8:ptr; b9:ptr;
 4 |  c0:ptr; c1:ptr; c2:ptr; c3:ptr; c4:ptr; c5:ptr; c6:ptr; c7:ptr; c8:ptr; c9:ptr;
 5 |  d0:ptr; d1:ptr; d2:ptr; d3:ptr; d4:ptr; d5:ptr; d6:ptr; d7:ptr; d8:ptr; d9:ptr;
...
26 |  y0:ptr; y1:ptr; y2:ptr; y3:ptr; y4:ptr; y5:ptr; y6:ptr; y7:ptr; y8:ptr; y9:ptr;
27 |  z0:ptr; z1:ptr; z2:ptr; z3:ptr; z4:ptr;
28 |  int:int; unboxed:float#
29 |  }
Error: Mixed inline record arguments to constructors may contain at most 254 value fields prior to the flat suffix, but this one contains 255.
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
Error: Mixed constructors may contain at most 254 value fields prior to the flat suffix, but this one contains 255.
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
  | A_record : { x : 'a tf } -> 'a t_gadt_any
  | B_record : { x : 'a tv } -> 'a t_gadt_any

[%%expect {|
type ('a : any) t_gadt_any =
    A : ('a : float64). 'a tf -> 'a t_gadt_any
  | B : 'b tv -> 'a t_gadt_any
  | A_record : ('a : float64). { x : 'a tf; } -> 'a t_gadt_any
  | B_record : { x : 'a tv; } -> 'a t_gadt_any
|}]

type ('a : any) t_gadt_any_multiple_fields =
  | A : float# * 'a tf -> 'a t_gadt_any_multiple_fields
  | B : 'b tv * float# -> 'a t_gadt_any_multiple_fields
  | A_record : { x : float#; y : 'a tf } -> 'a t_gadt_any_multiple_fields
  | B_record : { x :  'b tv; y : float# } -> 'a t_gadt_any_multiple_fields

[%%expect {|
type ('a : any) t_gadt_any_multiple_fields =
    A : ('a : float64). float# * 'a tf -> 'a t_gadt_any_multiple_fields
  | B : 'b tv * float# -> 'a t_gadt_any_multiple_fields
  | A_record : ('a : float64). { x : float#; y : 'a tf;
    } -> 'a t_gadt_any_multiple_fields
  | B_record : { x : 'b tv; y : float#; } -> 'a t_gadt_any_multiple_fields
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

type ('a : any) t_gadt_any_record_bad =
  | A : { x : float#; y : 'a tv } -> 'a t_gadt_any_record_bad

[%%expect{|
Line 2, characters 10-21:
2 |   | A : { x : float#; y : 'a tv } -> 'a t_gadt_any_record_bad
              ^^^^^^^^^^^
Error: Expected all flat fields after non-value field, x,
       but found boxed field, y.
|}]
