(* TEST
 shared-libraries;
 setup-ocamlopt.byte-build-env;
 flags = "-shared";
 all_modules = "question.ml";
 program = "question.cmxs";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
 {
   ocamlobjinfo;
   check-program-output;
 }{
   program = "question.cmx";
   ocamlobjinfo;
   (* The cmx output varies too much to check. We're just happy it didn't
      segfault on us. *)
 }
*)

(* We use a function rather than a value of type int to ensure that there
   is an Flambda 2 code section. *)
let answer () = 42
