(* TEST
   ocamlopt_flags += " -O3 "
   * skip
     reason = "OCaml 5 only"
*)

open Domain

let () =
  let l = lazy (print_string "Lazy Forced\n") in
  let d = spawn (fun () -> Lazy.force l) in
  join d
