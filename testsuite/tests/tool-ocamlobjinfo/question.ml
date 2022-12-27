(* TEST
* shared-libraries
** setup-ocamlopt.byte-build-env
*** ocamlopt.byte
flags = "-shared"
all_modules = "question.ml"
program = "question.cmxs"
**** check-ocamlopt.byte-output
***** ocamlobjinfo
****** check-program-output

***** ocamlobjinfo
program = "question.cmx"
(* The cmx output varies too much to check. We're just happy it didn't
   segfault on us. *)
*)

(* We use a function rather than a value of type int to ensure that there
   is an Flambda 2 code section. *)
let answer () = 42
