let [@ocamlformat "disable"] print_rule ~test_runner ~file =
  Format.printf
{|(rule
 (alias runtest)
 (action
  (progn
   (run %s %s)
   (diff? %s %s.corrected))))

|}
    test_runner file file file

let () =
  let () =
    if Array.length Sys.argv < 2
    then (
      Format.eprintf "usage: %s TEST_RUNNER.exe TEST1 [...]\n" Sys.argv.(0);
      exit 1)
  in
  let test_runner = Sys.argv.(1) in
  for i = 2 to Array.length Sys.argv - 1 do
    print_rule ~test_runner ~file:Sys.argv.(i)
  done
