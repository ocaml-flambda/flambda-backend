let [@ocamlformat "disable"] print_header () =
  Format.printf
{|; Generated automatically by this directory's dune.
; Run inc.sh to generate a new .inc file.

(alias (name regen))
|}


let [@ocamlformat "disable"] print_test_rule ~test_runner ~file =
  Format.printf
{|
 (rule
 (alias runtest)
  (deps %s)
  (action
   (progn
   (run %s %s))))
|}
    file test_runner file

let _ =
  let list_files = Sys.readdir "./" in
  let fl_files =
    List.filter (fun x -> Filename.extension x = ".fl")
      (Array.to_list list_files)
  in
  let run basename =
    print_test_rule ~test_runner:"../tools/flvalidate.exe" ~file:basename
  in
  print_header ();
  List.map run fl_files
