(* TEST
   include ocamlcommon
   readonly_files = "example_syntax.ml"
   reference = "${test_source_directory}/example_syntax.ml"
*)

let () =
  let fname = "example_syntax.ml" in
  Clflags.Extension.enable Local;
  let ic = open_in fname in
  let lexbuf = Lexing.from_channel ic in
  Location.init lexbuf fname;
  let ast = Parse.implementation lexbuf in
  close_in ic;
  Format.printf "%a@." Pprintast.structure ast
