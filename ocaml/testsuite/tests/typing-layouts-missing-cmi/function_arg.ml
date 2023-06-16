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

let f : Function_b.fun_t = fun ~arg1:_ ~arg2 -> arg2
