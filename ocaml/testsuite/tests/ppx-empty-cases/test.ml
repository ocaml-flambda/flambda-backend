(* TEST
readonly_files = "ppx_empty_cases.ml"
include ocamlcommon
* setup-ocamlc.byte-build-env
** ocamlc.byte
program = "${test_build_directory}/ppx_empty_cases.exe"
all_modules = "ppx_empty_cases.ml"
*** ocamlc.byte
module = "test.ml"
flags = "-I ${test_build_directory} \
         -ppx ${program} \
         -extension layouts_alpha \
         -dlambda"
**** check-ocamlc.byte-output
*)

(* It's possible for ppx code to generate empty function cases. This is
   compiled as a function that always raises [Match_failure].

   In this test, we confirm that (i) we can handle these cases, and (ii) the
   layout information in lambda is correct.
 *)

type t

(* "function [%empty] -> ." is rewritten by a ppx in this directory to
   a zero-case function. *)
let empty_cases_returning_string  : t -> string = function [%empty] -> .
let empty_cases_returning_float64 : t -> float# = function [%empty] -> .
let empty_cases_accepting_string  : string -> t = function [%empty] -> .
let empty_cases_accepting_float64 : float# -> t = function [%empty] -> .
let non_empty_cases_returning_string  : t -> string = function _ -> assert false
let non_empty_cases_returning_float64 : t -> float# = function _ -> assert false
let non_empty_cases_accepting_string  : string -> t = function _ -> assert false
let non_empty_cases_accepting_float64 : float# -> t = function _ -> assert false

