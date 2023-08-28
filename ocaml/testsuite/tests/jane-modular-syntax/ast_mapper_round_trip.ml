(* TEST
   include ocamlcommon *)

let () = Language_extension.enable Comprehensions ();;

let ast_to_string ast =
  ignore (Format.flush_str_formatter () : string);
  try
    Pprintast.expression Format.str_formatter ast;
    Format.flush_str_formatter ()
  with _ -> "FAILURE: [Pprintast] raised an unexpected exception."
;;

let run_test ?(ast_mapper = Ast_mapper.default_mapper) program_text ~summary =
  print_endline "---";
  Printf.printf "Test: %s\n" summary;
  print_endline "Program text:";
  print_endline program_text;
  let pre = Parse.expression (Lexing.from_string program_text) in
  print_endline "Pprintast (before Ast_mapper):";
  print_endline (ast_to_string pre);
  let post = ast_mapper.expr ast_mapper pre in
  print_endline "Pprintast (after Ast_mapper):";
  print_endline (ast_to_string post);
  if ast_to_string pre <> ast_to_string post then
    print_endline "FAILURE: The Ast_mapper round trip was not a no-op."
;;

let () = run_test "(local_ x) [@outer]" ~summary:"Local: outer attribute"
let () = run_test "local_ (x [@inner])" ~summary:"Local: inner attribute"
let () = run_test "(local_ (x [@inner])) [@outer]" ~summary:"Local: inner and outer attribute"
let () = run_test "(local_ (x [@inner1] [@inner2])) [@outer1] [@outer2]" ~summary:"Local: multiple attributes"

let () = run_test "[ x for x = 0 to 1 ] [@outer]" ~summary:"Comprehension: outer attribute"
let () = run_test "[ x[@inner] for x = 0 to 1 ] " ~summary:"Comprehension: inner attribute"
let () = run_test "[ x[@inner] for x = 0 to 1 ] [@outer] " ~summary:"Comprehension: inner and outer attribute"
