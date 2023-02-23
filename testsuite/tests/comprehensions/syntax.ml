(* TEST
   include ocamlcommon *)

let () = Clflags.Extension.enable Comprehensions;;

let printf = Printf.printf;;

let this_test_tests_that =
  let first_time = ref true in
  let separator o () =
    if !first_time
    then first_time := false
    else Printf.fprintf o "\n"
  in
  fun msg -> printf "%a**** Test that %s ****\n\n" separator () msg
;;

(******************************************************************************)

this_test_tests_that
  "we print list comprehensions correctly"
;;

let test_printing () =
  let parsed = (* The wonky formatting here is the best way to keep the line
                   break visible in the string and also keep things within 80
                   characters here *)
"[(i, j)\n\
\  for i = 0 to 9 when (i mod 2) = 0 for j = 0 to i when (i > 4) && (j > 4)]"
  in
  let expr = Parse.expression (Lexing.from_string parsed) in
  let printed = Pprintast.string_of_expression expr in
  if parsed = printed then
    printf
      "Parsing and printing round-tripped successfully!\n\n\
       %s\n"
      parsed
  else
    printf
      "Parsing and printing failed to round-trip:\n\n\
       %s\n\n\
       became\n\n\
       %s\n"
      parsed printed
;;

let () = test_printing ();;

(******************************************************************************)

this_test_tests_that
  "compiler-generated extension nodes start with \"extension.\""
;;

(* In particular: As long as we're in ocaml-jst, the compiler translates list
   comprehensions to an AST in terms of %-annotated extension nodes.  We want to
   confirm that we're only touching ones we intend to use, which we mark by
   beginning them with the string "extension.".  We print out the other ones we
   find, which should be the exact four that were present in the source. *)

let starts_with pfx str =
  String.length str >= String.length pfx
  &&
  String.sub str 0 (String.length pfx) = pfx
;;

let test_iteration () =
  let example =
    "[ [%expr payload] for i = [%low] to [%high] when [%cond] ]"
  in
  let expr = Parse.expression (Lexing.from_string example) in
  let extension it ((name, _) : Parsetree.extension) =
      Printf.printf "  [%%%s ...]\n" name.txt
  in
  let iterator =
    { Ast_iterator.default_iterator with extension }
  in
  Printf.printf "User [%%extension] nodes found:\n";
  iterator.expr iterator expr
;;

let () = test_iteration ();;
