(* TEST

readonly_files = "a.ml b.ml"
flags = "-extension layouts_beta"
* setup-ocamlc.byte-build-env
** ocamlc.byte
module = "a.ml"
*** ocamlc.byte
module = "b.ml"
**** script
script = "rm -f a.cmi"
***** expect
*)

(* CR layouts v2.5: The commented out code in this file uses void, but could
   use any non-value layout. *)

#directory "ocamlc.byte";;
#load "b.cmo";;

open B

type ('a : immediate) imm_arg
type ('b : value) value_arg
(* type ('c : @void) void_arg *)

(* These should work *)
type foo = b_imm imm_arg
type bar = b_value value_arg
(* type baz = b_void void_arg *)
type boz = b_imm value_arg;;
[%%expect {|
type ('a : immediate) imm_arg
type 'b value_arg
type foo = B.b_imm imm_arg
type bar = B.b_value value_arg
type boz = B.b_imm value_arg
|}];;

(* CR layouts v2.9: the error message below is unreviewed *)

(* These should not *)
type err1 = b_value imm_arg;;
[%%expect {|
Line 1, characters 12-19:
1 | type err1 = b_value imm_arg;;
                ^^^^^^^
Error: This type B.b_value = A.a_value should be an instance of type
         ('a : immediate)
       The layout of B.b_value is value, because
         the .cmi file for A.a_value is missing.
       But the layout of B.b_value must be a sublayout of immediate, because
         of the definition of imm_arg at line 3, characters 0-29.
       No .cmi file found containing A.a_value.
       Hint: Adding "a" to your dependencies might help.
|}];;

(* type err2 = b_void value_arg;;
 * [%%expect {|
 * Line 1, characters 12-18:
 * 1 | type err2 = b_void value_arg;;
 *                 ^^^^^^
 * Error: This type B.b_void = A.a_void should be an instance of type
 *          ('a : value)
 *        B.b_void has layout void, which is not a sublayout of value.
 * |}];;
 *
 * type err3 = b_void imm_arg;;
 * [%%expect {|
 * Line 1, characters 12-18:
 * 1 | type err3 = b_void imm_arg;;
 *                 ^^^^^^
 * Error: This type B.b_void = A.a_void should be an instance of type
 *          ('a : immediate)
 *        B.b_void has layout void, which is not a sublayout of immediate.
 * |}];;
 *
 * type err4 = b_value void_arg;;
 * [%%expect {|
 * Line 1, characters 12-19:
 * 1 | type err4 = b_value void_arg;;
 *                 ^^^^^^^
 * Error: This type B.b_value = A.a_value should be an instance of type
 *          ('a : void)
 *        B.b_value has layout value, which is not a sublayout of void.
 * |}];;
 *
 * type err5 = b_imm void_arg;;
 * [%%expect {|
 * Line 1, characters 12-17:
 * 1 | type err5 = b_imm void_arg;;
 *                 ^^^^^
 * Error: This type B.b_imm = A.a_imm should be an instance of type ('a : void)
 *        B.b_imm has layout immediate, which is not a sublayout of void.
 * |}];; *)
