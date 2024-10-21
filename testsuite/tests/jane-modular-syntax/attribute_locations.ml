(* TEST
 include ocamlcommon;
*)

let () = Language_extension.enable Comprehensions ();;

 module Location_map = struct
   include Map.Make (struct
     type t = Location.t
     let compare = compare
  end)

   let add_multi key data t =
     update key (function
       | None -> Some [data]
       | Some x -> Some (data :: x))
       t
end

let gather_attributes_by_location program_text =
  let program_text_buf = Lexing.from_string program_text in
  Lexing.set_filename program_text_buf "<no filename in test>";
  let parsed = Parse.expression program_text_buf in
  let attrs_by_location = ref Location_map.empty in
  let record_attribute { Parsetree.attr_name; attr_loc } =
    attrs_by_location :=
       Location_map.add_multi attr_loc attr_name.txt !attrs_by_location
  in
   (* We can't use an [attribute] iterator because Jane Syntax attributes are
      skipped by the default iterator. To undo this behavior, we instead override
      the expression iterator to look at attributes literally.
   *)
   let expr iterator (x : Parsetree.expression) =
     List.iter record_attribute (List.rev x.pexp_attributes);
     Ast_iterator.default_iterator.expr iterator { x with pexp_attributes = [] }
   in
   let iterator = { Ast_iterator.default_iterator with expr } in
   iterator.expr iterator parsed;
   !attrs_by_location
;;

let run_test program_text ~summary =
  print_endline "---";
  Printf.printf "Test: %s\n" summary;
  print_endline "Program text:";
  print_endline program_text;
  print_endline "Attributes and their locations:";
  let attributes_by_location = gather_attributes_by_location program_text in
  Location_map.iter
    (fun loc at_loc ->
      let printf fmt = Format.fprintf Format.std_formatter fmt in
      printf "\tAt location %a:\n" Location.print_loc loc;
      List.iter (printf "\t\t%s\n") at_loc)
    attributes_by_location
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
     let x6 = [| y for y = 100 downto 1 |] in\n\
     let x7 = [ y for y = 1 to 100 when y = 50 ] in\n\
     ()"
;;
