(* TEST
 native-compiler;
 setup-ocamlopt.byte-build-env;
 flags = "-save-ir-after linearization -S";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
 script = "sh ${test_source_directory}/save_ir_after_linearization.sh";
 script;
*)

let foo f x =
  if x > 0 then x * 7 else f x

let bar x y = x + y
