(* TEST
   include ocamlcommon *)

let () = Language_extension.enable Comprehensions ();;

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
  "compiler-generated attributes start with \"jane.\""
;;

(* In particular: As long as we're in the Jane Street compiler, the compiler
   translates list comprehensions to an AST in terms of attributes and/or
   extension nodes. We want to confirm that we're only touching ones we intend
   to use, which we mark by beginning them with the string "jane.". We print out
   the other ones we find, which should be the exact four of each that were
   present in the source.
*)

let starts_with pfx str =
  String.length str >= String.length pfx
  &&
  String.sub str 0 (String.length pfx) = pfx
;;

let test_iteration () =
  let example =
    "[ [%e] [@expr] for i = [%l] [@low] to [%h] [@high] when [%c] [@cond] ]"
  in
  let expr = Parse.expression (Lexing.from_string example) in
  let attribute it ({ attr_name; _ } : Parsetree.attribute) =
      Printf.printf "  [@%s ...]\n" attr_name.txt
  in
  let extension it ((name, _) : Parsetree.extension) =
    Printf.printf "  [%%%s ...]\n" name.txt
  in
  let iterator =
    { Ast_iterator.default_iterator with attribute; extension }
  in
  Printf.printf "User attributes [@...] and extension nodes [%%...] found:\n";
  iterator.expr iterator expr
;;

let () = test_iteration ();;
