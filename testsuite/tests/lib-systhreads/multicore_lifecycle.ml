(* TEST
<<<<<<< HEAD
 reason = "CR ocaml 5 domains: re-enable this test";
 skip;
 include systhreads;
 hassysthreads;
 {
   bytecode;
 }{
   native;
 }
||||||| 121bedcfd2
* hassysthreads
include systhreads
** bytecode
** native
=======
 include systhreads;
 hassysthreads;
 {
   bytecode;
 }{
   native;
 }
>>>>>>> 5.2.0
*)

let _ =
  let t = ref (Thread.self ()) in
  let d = Domain.spawn begin fun () ->
     let thread_func () = Unix.sleep 5 in
     let tt = Thread.create thread_func () in
     t := tt;
    ()
   end
  in
  Domain.join d;
  Thread.join (!t);
  Domain.join @@ Domain.spawn (fun () -> print_endline "ok")
