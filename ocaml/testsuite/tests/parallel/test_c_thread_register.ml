(* TEST
<<<<<<< HEAD
 modules = "test_c_thread_register_cstubs.c";
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
   modules = "test_c_thread_register_cstubs.c"
   * hassysthreads
   include systhreads
   ** bytecode
   ** native
=======
 modules = "test_c_thread_register_cstubs.c";
 include systhreads;
 hassysthreads;
 {
   bytecode;
 }{
   native;
 }
>>>>>>> 5.2.0
*)

(* spins a external thread from C and register it to the OCaml runtime *)

external spawn_thread : (unit -> unit) -> unit = "spawn_thread"

let passed () = Printf.printf "passed\n"

let _ =
  let d =
    Domain.spawn begin fun () ->
      spawn_thread passed;
      Thread.delay 0.5
    end
  in
  let t = Thread.create (fun () -> Thread.delay 1.0) () in
  Thread.join t;
  Domain.join d
