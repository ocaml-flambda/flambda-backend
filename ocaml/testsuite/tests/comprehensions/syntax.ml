(* TEST
   include ocamlcommon *)

let () = Language_extension.enable Comprehensions;;

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

let test_printing parsed =
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

let () =
  (* The wonky formatting here is the best way to keep the line
     break visible in the string and also keep things within 80
     characters here *)
 test_printing
    "[(i, j)\n\
    \  for i = 0 to 9 when (i mod 2) = 0 for j = 0 to i when (i > 4) && (j > 4)]";
 (* Check that only user attributes survive pprintast *)
 test_printing
   "(([((x)[@attr1 ]) for ((x)[@attr2 ]) in (([])[@attr3 ])])[@attr4 ])";
;;

(******************************************************************************)

this_test_tests_that
  "compiler-generated attributes start with \"jane.\""
;;

(* In particular: As long as we're in the Jane Street compiler, the compiler
   translates list comprehensions to an AST in terms of attributes.  We want to
   confirm that we're only touching ones we intend to use,
   which we mark by beginning them with the string "jane.".  We print out
   the other ones we find, which should be the exact four that were present in
   the source. *)

let starts_with pfx str =
  String.length str >= String.length pfx
  &&
  String.sub str 0 (String.length pfx) = pfx
;;

let test_iteration () =
  let example =
    "[ payload [@expr] for i = j [@low] to k [@high] when cond [@cond] ]"
  in
  let expr = Parse.expression (Lexing.from_string example) in
  let attribute it ({ attr_name; _ } : Parsetree.attribute) =
      Printf.printf "  [@%s ...]\n" attr_name.txt
  in
  let iterator =
    { Ast_iterator.default_iterator with attribute }
  in
  Printf.printf "User attribute [@...] nodes found:\n";
  iterator.expr iterator expr
;;

let () = test_iteration ();;
