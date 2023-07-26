(* TEST
   include ocamlcommon *)

let () = Language_extension.enable Comprehensions ();;
let () = Language_extension.enable Immutable_arrays ();;

(* A list deduped by physical equality. We use it to check that attribute
   strings are shared and not allocated multiple times.
*)
module Deduped_list : sig
  type 'a t
  val empty : 'a t
  val add : 'a t -> 'a -> 'a t
  val to_list : 'a t -> 'a list
end = struct
  type 'a t = 'a list
  let empty = []
  let add t x = if List.memq x t then t else x :: t
  let to_list t = List.rev t
end

module String_set = Set.Make (String)

let gather_attribute_names program_text : string list =
  let parsed = Parse.expression (Lexing.from_string program_text) in
  let deduped_attr_txts = ref Deduped_list.empty in
  let add_all_attributes attributes =
    List.iter
      (fun { Parsetree.attr_name } ->
         deduped_attr_txts := Deduped_list.add !deduped_attr_txts attr_name.txt)
      attributes
  in
  (* We can't use an [attribute] iterator because Jane Syntax attributes are
     skipped by the default iterator. To undo this behavior, we instead override
     the expression iterator to look at attributes literally.
  *)
  let expr iterator (x : Parsetree.expression) =
    add_all_attributes x.pexp_attributes;
    Ast_iterator.default_iterator.expr iterator { x with pexp_attributes = [] }
  in
  let iterator = { Ast_iterator.default_iterator with expr } in
  iterator.expr iterator parsed;
  Deduped_list.to_list !deduped_attr_txts
;;

let run_test program_text ~summary =
  print_endline "---";
  Printf.printf "Test: %s\n" summary;
  print_endline "Program text:";
  print_endline program_text;
  print_endline "Attributes, deduped by physical equality (one per line):";
  let deduped_by_physical_equality = gather_attribute_names program_text in
  List.iter print_endline deduped_by_physical_equality;
  let deduped_by_string_contents =
    String_set.of_list deduped_by_physical_equality
  in
  if
    String_set.cardinal deduped_by_string_contents
      <> List.length deduped_by_physical_equality
  then
    failwith
      "There are multiple strings with the same contents, suggesting that Jane \
       Syntax isn't memoizing attribute payloads."
;;

let () =
  run_test ~summary:"single Jane Syntax construct"
    "[ x for x in [ 1; 2; 3 ]]"
;;

let () =
  run_test ~summary:"multiple Jane Syntax constructs"
    "let x1 = [ x for x in [ 1; 2; 3 ]] in\n\
     let x2 = [ y for y = 1 to 100 ] in\n\
     let x3 = [ y for y = 1 to 100 ] in\n\
     let x4 = [ y for y = 100 downto 1 ] in\n\
     let x5 = [: y for y = 100 downto 1 :] in\n\
     let x6 = [| y for y = 100 downto 1 |] in\n\
     let x7 = [ y for y = 1 to 100 when y = 50 ] in\n\
     ()"
;;
