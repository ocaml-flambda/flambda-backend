<<<<<<< HEAD
(* TEST
 flags = "-bin-annot -bin-annot-occurrences";
 compile_only = "true";
 all_modules = "index_modules.ml";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 program = "-quiet -index -decls index_modules.cmt";
 output = "out_objinfo";
 check-ocamlc.byte-output;
 ocamlobjinfo;
 check-program-output;
*)

(* Local modules: *)

let () =
  let module A = struct let x = 42 end in
  let open A in
  print_int (x + A.x)
||||||| 121bedcfd2
=======
(* TEST

flags = "-bin-annot -bin-annot-occurrences";
compile_only = "true";
setup-ocamlc.byte-build-env;
all_modules = "index_modules.ml";
ocamlc.byte;
check-ocamlc.byte-output;

program = "-quiet -index -decls index_modules.cmt";
output = "out_objinfo";
ocamlobjinfo;

check-program-output;
*)

(* Local modules: *)

let () =
  let module A = struct let x = 42 end in
  let open A in
  print_int (x + A.x)
>>>>>>> 5.2.0
