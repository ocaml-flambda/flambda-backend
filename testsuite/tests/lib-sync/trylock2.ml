<<<<<<< HEAD
(* TEST
 runtime5;
 { bytecode; }
 { native; }
*)
||||||| 121bedcfd2
(* TEST
*)
=======
(* TEST *)
>>>>>>> 5.2.0

(* Test Mutex.try_lock *)

let () =
  let m = Mutex.create () in
  assert (Mutex.try_lock m);
  Mutex.unlock m;
  print_endline "passed"
