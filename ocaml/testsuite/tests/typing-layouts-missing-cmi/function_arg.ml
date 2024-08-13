(* TEST
 readonly_files = "function_a.ml function_b.ml";
 setup-ocamlc.byte-build-env;
 module = "function_a.ml";
 ocamlc.byte;
 module = "function_b.ml";
 ocamlc.byte;
 script = "rm -f function_a.cmi";
 script;
 expect;
*)

#directory "ocamlc.byte";;
#load "function_b.cmo";;

(* This tests that sorts are correctly extracted from function types,
   even in the presence of a missing cmi file. *)

let f0 (g : Function_b.fun_t) = g ~arg1:(assert false)

[%%expect{|
Unknown directive "directory".
|}]

let f1 (g : Function_b.fun_t) = g ()

[%%expect{|
Line 1, characters 12-28:
1 | let f1 (g : Function_b.fun_t) = g ()
                ^^^^^^^^^^^^^^^^
Error: Unbound module "Function_b"
|}]

let f2 : Function_b.fun_t = fun ~arg1:_ ~arg2 () -> arg2

[%%expect{|
Line 1, characters 9-25:
1 | let f2 : Function_b.fun_t = fun ~arg1:_ ~arg2 () -> arg2
             ^^^^^^^^^^^^^^^^
Error: Unbound module "Function_b"
|}]

let f3 : Function_b.return_t = fun () -> assert false

[%%expect{|
Line 1, characters 9-28:
1 | let f3 : Function_b.return_t = fun () -> assert false
             ^^^^^^^^^^^^^^^^^^^
Error: Unbound module "Function_b"
|}]

let f4 (_ : Function_b.take_t) = ()
let x1 = f4 Function_b.f_opt

[%%expect{|
Line 1, characters 12-29:
1 | let f4 (_ : Function_b.take_t) = ()
                ^^^^^^^^^^^^^^^^^
Error: Unbound module "Function_b"
|}]

let f5 (_ : Function_b.return_t) = ()
let x2 = f5 Function_b.f_opt_2

[%%expect{|
Line 1, characters 12-31:
1 | let f5 (_ : Function_b.return_t) = ()
                ^^^^^^^^^^^^^^^^^^^
Error: Unbound module "Function_b"
|}]
