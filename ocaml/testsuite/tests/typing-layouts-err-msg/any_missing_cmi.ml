(* TEST

readonly_files = "any_missing_cmi_lib.ml any_missing_cmi_lib2.ml"
* setup-ocamlc.byte-build-env
** ocamlc.byte
module = "any_missing_cmi_lib2.ml"
*** ocamlc.byte
module = "any_missing_cmi_lib.ml"
**** script
script = "rm -f any_missing_cmi_lib2.cmi"
***** expect
*)

#directory "ocamlc.byte";;
#load "any_missing_cmi_lib.cmo";;

let f = Any_missing_cmi_lib.f (assert false)
[%%expect{|
Line 1, characters 30-44:
1 | let f = Any_missing_cmi_lib.f (assert false)
                                  ^^^^^^^^^^^^^^
Error: Function arguments and returns must be representable.
       The layout of Any_missing_cmi_lib2.t is any, because
         the .cmi file for Any_missing_cmi_lib2.t is missing.
       But the layout of Any_missing_cmi_lib2.t must be representable, because
         it's used as a function argument.
       No .cmi file found containing Any_missing_cmi_lib2.t.
       Hint: Adding "any_missing_cmi_lib2" to your dependencies might help.
|}]
