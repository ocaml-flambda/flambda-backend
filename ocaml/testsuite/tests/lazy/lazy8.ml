(* TEST
   ocamlopt_flags += " -O3 "
   * skip
     reason = "CR ocaml 5 domains: re-enable this test"
*)

exception E

let main () =
  let l = lazy (raise E) in

  begin try Lazy.force_val l with
  | E -> ()
  end;

  begin try Lazy.force_val l with
  | Lazy.Undefined -> ()
  end;

  let d = Domain.spawn (fun () ->
    begin try Lazy.force_val l with
    | Lazy.Undefined -> ()
    end)
  in
  Domain.join d;
  print_endline "OK"

let _ = main ()
