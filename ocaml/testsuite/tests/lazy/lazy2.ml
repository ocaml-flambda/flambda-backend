(* TEST
 ocamlopt_flags += " -O3 ";
 runtime5;
 { bytecode; }
 { native; }
*)

open Domain

let () =
  let l = lazy (print_string "Lazy Forced\n") in
  let d = spawn (fun () -> Lazy.force l) in
  join d
