(* TEST
<<<<<<< HEAD
 reason = "CR ocaml 5 domains: re-enable this test";
 skip;
 include unix;
 hasunix;
 {
   bytecode;
 }{
   native;
 }
||||||| 121bedcfd2
* hasunix
include unix
** bytecode
** native
=======
 include unix;
 hasunix;
 {
   bytecode;
 }{
   native;
 }
>>>>>>> 5.2.0
*)


let _ =
  (* start a dummy domain and shut it down to cause a domain reuse *)
  let d = Domain.spawn (fun _ -> ()) in
  Domain.join d;
  let _d = Domain.spawn (fun _ ->
    Unix.sleep 10;
    print_endline "Should not reach here!") in
  Gc.full_major ();
  print_endline "OK"
