(* TEST
<<<<<<< HEAD
 include unix;
 reason = "CR ocaml 5 domains: re-enable this test";
 skip;
 hasunix;
 not-windows;
 {
   bytecode;
 }{
   native;
 }
||||||| 121bedcfd2
include unix
* hasunix
** not-windows
*** bytecode
*** native
=======
 include unix;
 hasunix;
 not-windows;
 {
   bytecode;
 }{
   native;
 }
>>>>>>> 5.2.0
*)

(* on Multicore, fork is not allowed is another domain is, and was running. *)
(* this test checks that we can't fork if a domain is currently running. *)

let () =
  let d = Domain.spawn (fun () -> Unix.sleep 1) in
  let res = match Unix.fork () with
    | exception Failure _ -> 0
    | _ -> 1
  in
  Domain.join d;
  exit res
