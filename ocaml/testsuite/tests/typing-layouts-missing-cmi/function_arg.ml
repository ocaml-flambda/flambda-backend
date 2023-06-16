(* TEST

readonly_files = "function_a.ml function_b.ml"
* setup-ocamlc.byte-build-env
** ocamlc.byte
module = "function_a.ml"
*** ocamlc.byte
module = "function_b.ml"
**** script
script = "rm -f function_a.cmi"
***** expect
*)

#directory "ocamlc.byte";;
#load "function_b.cmo";;

(* This tests that sorts are correctly extracted from function types,
   even in the presence of a missing cmi file. *)

let f0 (g : Function_b.fun_t) = g ~arg1:(assert false)

[%%expect{|
blah
|}]

let f1 (g : Function_b.fun_t) = g ()

[%%expect{|
blah
|}]

let f2 : Function_b.fun_t = fun ~arg1:_ ~arg2 () -> arg2

[%%expect{|
blah
|}]
