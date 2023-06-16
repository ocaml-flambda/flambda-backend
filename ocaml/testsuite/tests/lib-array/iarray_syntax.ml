(* TEST
   include ocamlcommon *)

let printf = Printf.printf;;

let () = Language_extension.enable Immutable_arrays ();;

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
 test_printing
   "match x with\n\
    | [::] -> [::]\n\
    | (([:x:])[@attr1 ]) -> (([:x:])[@attr1 ])\n\
    | (([:x;y:])[@attr2 ][@attr3 ]) -> (([:x;y:])[@attr2 ][@attr3 ])"
;;
