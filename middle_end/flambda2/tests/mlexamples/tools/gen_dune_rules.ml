let [@ocamlformat "disable"] print_header () =
  Format.printf
{|; Generated automatically by this directory's dune.
; Use `make regen-flambda2-test-dune-rules` to regenerate.

(alias (name regen))
|}

let [@ocamlformat "disable"] print_test_rule ~test_runner ~file =
  Format.printf
{|
(rule
 (alias runtest)
 (action
  (progn
   (run %s %s)
   (diff? %s %s.corrected))))
|}
    test_runner file file file

let [@ocamlformat "disable"] print_regen_rule ~basename =
  Format.printf
{|
(rule
 (alias regen)
 (targets %s.flt.new %s_in.fl.new %s_out.fl.new)
 (deps %s.ml)
 (action
   (run ocamlopt -c %s.ml -nopervasives -nostdlib
        -drawfexpr-to %s_in.fl.new
        -dfexpr-to %s_out.fl.new
        -dflexpect-to %s.flt.new)))

(rule
 (alias regen)
 (action (diff %s_in.fl %s_in.fl.new)))

(rule
 (alias regen)
 (action (diff %s_out.fl %s_out.fl.new)))

(rule
 (alias regen)
 (action (diff %s.flt %s.flt.new)))
|}
    basename basename basename basename basename basename basename basename
    basename basename basename basename basename basename

let print_rules ~basename =
  print_test_rule ~test_runner:"../tools/flexpect.exe" ~file:(basename ^ ".flt");
  print_test_rule ~test_runner:"../tools/roundtrip.exe"
    ~file:(basename ^ "_in.fl");
  print_test_rule ~test_runner:"../tools/roundtrip.exe"
    ~file:(basename ^ "_out.fl");
  print_regen_rule ~basename

let () =
  print_header ();
  for i = 1 to Array.length Sys.argv - 1 do
    print_rules ~basename:Sys.argv.(i)
  done
