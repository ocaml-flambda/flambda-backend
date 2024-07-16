(* TEST
<<<<<<< HEAD
 shared-libraries;
 setup-ocamlopt.byte-build-env;
 flags = "-shared";
 all_modules = "question.ml";
 program = "question.cmxs";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
 {
   ocamlobjinfo;

   program = "-no-code -no-approx question.cmx";
   ocamlobjinfo;

   program = "-no-code question.cmx";
   ocamlobjinfo;

   runtime5;
   check-program-output;
 }{
   program = "question.cmx";
   ocamlobjinfo;
   (* The full cmx output varies too much to check. We're just happy it didn't
      segfault on us. *)
 }
||||||| 121bedcfd2
* shared-libraries
** setup-ocamlopt.byte-build-env
*** ocamlopt.byte
flags = "-shared"
all_modules = "question.ml"
program = "question.cmxs"
**** check-ocamlopt.byte-output
***** ocamlobjinfo
****** check-program-output
=======
 shared-libraries;
 setup-ocamlopt.byte-build-env;
 flags = "-shared";
 all_modules = "question.ml";
 program = "question.cmxs";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
 ocamlobjinfo;
 check-program-output;
>>>>>>> 5.2.0
*)

(* We use a function rather than a value of type int to ensure that there
   is an Flambda 2 code section. *)
let answer () = 42
