(* TEST
 {
   compiler_reference = "${test_source_directory}/parsing_stable.compilers.reference";
   toplevel;
 }{
   flags = "-extension layouts_beta";
   compiler_reference = "${test_source_directory}/parsing_beta.compilers.reference";
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
