(* TEST
   * setup-ocamlc.byte-build-env
   ** ocamlc.byte
   flags = "-g"
   ocamlrunparam += ",b=1"
*)

(* CR mshinwell: re-enable for native once Flambda 2 fixed to say
   "Raised by primitive operation" in backtraces rather than just
   "Raised" for bounds check failures *)

(* #11436: bad backtrace for out-of-bounds exception *)

let xs = [| 0; 1; 2 |]

let [@inline never] bad_bound_fn x =
  !x + xs.(100)

let _ =
  try
    ignore (Sys.opaque_identity (bad_bound_fn (ref 0)));
  with exn ->
    Printf.printf "Uncaught exception %s\n" (Printexc.to_string exn);
    Printexc.print_backtrace stdout;

  print_endline "OK"
