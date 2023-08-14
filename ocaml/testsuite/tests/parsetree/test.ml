(* TEST
   include ocamlcommon
   readonly_files = "source.ml source_jane_street.ml"
*)

(* (c) Alain Frisch / Lexifi *)
(* cf. PR#7200 *)

let diff =
  match Array.to_list Sys.argv with
  | [_; diff] -> diff
  | _ -> "diff -u"

let report_err exn =
  Location.report_exception Format.std_formatter exn

let remove_locs =
  let open Ast_mapper in
  { default_mapper with
    location = (fun _mapper _loc -> Location.none);
    attributes =
      (fun mapper attrs ->
         let attrs = default_mapper.attributes mapper attrs in
         List.filter (fun a ->
           a.Parsetree.attr_name.Location.txt <> "#punning#")
           attrs (* this is to accommodate a LexiFi custom extension *)
      )
  }

let from_file parse_fun filename =
  Location.input_name := filename;
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  Location.init lexbuf filename;
  let ast = parse_fun lexbuf in
  close_in ic;
  ast

let from_string parse_fun str =
  Location.input_name := "<str>";
  let lexbuf = Lexing.from_string str in
  Location.init lexbuf "<str>";
  parse_fun lexbuf

let to_string print_fun ast =
  Format.fprintf Format.str_formatter "%a@." print_fun ast;
  Format.flush_str_formatter ()

let to_tmp_file print_fun ast =
  let fn, oc = Filename.open_temp_file "ocamlparse" ".txt" in
  output_string oc (to_string print_fun ast);
  close_out oc;
  fn

let test parse_fun pprint print map filename ~extra_checks =
  match from_file parse_fun filename with
  | exception exn ->
      Printf.printf "%s: FAIL, CANNOT PARSE\n" filename;
      report_err exn;
      print_endline "====================================================="
  | ast ->
      let str = to_string pprint ast in
      begin
        match extra_checks str with
        | Ok () -> ()
        | Error reason ->
            Printf.printf "%s: FAIL, %s\n" filename reason;
            print_endline str;
            print_endline"====================================================="
      end;
      match from_string parse_fun str with
      | exception exn ->
          Printf.printf "%s: FAIL, CANNOT REPARSE\n" filename;
          report_err exn;
          print_endline str;
          print_endline "====================================================="
      | ast2 ->
          let ast = map remove_locs remove_locs ast in
          let ast2 = map remove_locs remove_locs ast2 in
          if ast <> ast2 then begin
            Printf.printf "%s:  FAIL, REPARSED AST IS DIFFERENT\n%!" filename;
            let f1 = to_tmp_file print ast in
            let f2 = to_tmp_file print ast2 in
            let cmd = Printf.sprintf "%s %s %s" diff
                (Filename.quote f1) (Filename.quote f2) in
            let _ret = Sys.command cmd in
            print_endline"====================================================="
          end

let test parse_fun pprint print map filename ~extra_checks =
  try test parse_fun pprint print map filename ~extra_checks
  with exn -> report_err exn

let rec process path ~extra_checks =
  if Sys.is_directory path then
    let files = Sys.readdir path in
    Array.iter (fun s -> process (Filename.concat path s) ~extra_checks) files
  else if Filename.check_suffix path ".ml" then
    test
      Parse.implementation
      Pprintast.structure
      Printast.implementation
      (fun mapper -> mapper.Ast_mapper.structure)
      path
      ~extra_checks
  else if Filename.check_suffix path ".mli" then
    test
      Parse.interface
      Pprintast.signature
      Printast.interface
      (fun mapper -> mapper.Ast_mapper.signature)
      path
      ~extra_checks

let process ?(extra_checks = fun _ -> Ok ()) text = process text ~extra_checks

(* Produce an error if any attribute/extension node does not start with the
   text prefix.

   This over-conservatively produces an error for attributes/extension nodes
   that don't appear literally as '[@' or '[%' followed immediately by an
   identifier.  Some things that aren't handled:
      - [@ blah]
      - [% blah]
      - [@@blah]
      - [%%blah]

  We've chosen to keep those constructs out of the test file in preference
  to updating this logic to properly handle them (which is hard).
*)
let check_all_attributes_and_extensions_start_with text ~prefix =
  let check introduction_string =
    String.split_on_char '[' text
    |> List.for_all (fun s ->
        if String.starts_with s ~prefix:introduction_string
        then String.starts_with s ~prefix:(introduction_string ^ prefix)
        else true)
  in
  if check "%" && check "@"
  then Ok ()
  else
      Error
        (Printf.sprintf
          "Pprintast produced an extension node or attribute that doesn't \
           begin with `%s'"
           prefix)
;;

let () =
  process "source.ml";
  Language_extension.enable_maximal ();
  process "source_jane_street.ml" ~extra_checks:(fun text ->
  (* Check that printing Jane Street language extensions produces no more
     attributes or extension nodes than the input program, all of whose
     attributes begin with "test". This ensures that Jane Syntax attributes
     aren't printed.
   *)
    check_all_attributes_and_extensions_start_with text ~prefix:"test");
;;
