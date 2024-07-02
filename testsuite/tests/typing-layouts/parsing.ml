(* TEST
 {
   compiler_reference = "${test_source_directory}/parsing_stable_beta.compilers.reference";
   toplevel;
 }{
   flags = "-extension layouts_beta";
   compiler_reference = "${test_source_directory}/parsing_stable_beta.compilers.reference";
   toplevel;
 }{
   flags = "-extension layouts_alpha";
   compiler_reference = "${test_source_directory}/parsing_alpha.compilers.reference";
   toplevel;
 }
*)

type ('a : value) t0 = 'a list;;

type ('a : immediate) t0 = 'a list;;

type ('a : void) t0 = 'a list;;

type ('a : valu) t0 = 'a list;;

type t = float#;;

type t = int#;;

type t = Float.t#;;

type ('a : any, 'b : any, 'c : any) t;;

type 'a s1 = ('a : float64, int, bool) t;;

let f : ('a, _ : value, bool) t -> int = fun _ -> 42;;

type ('a, 'b, 'c) s2 = ('a, 'b, 'c : bits32) t;;
