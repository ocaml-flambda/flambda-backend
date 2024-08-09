(* TEST
 runtime5;
 { bytecode; }
 { native; }
*)

(* Test Mutex.try_lock *)

let () =
  let m = Mutex.create () in
  assert (Mutex.try_lock m);
  Mutex.unlock m;
  print_endline "passed"
