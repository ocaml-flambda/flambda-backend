(* TEST_BELOW
(* Blank lines added here to preserve locations. *)







*)

(* Testing if uncaught exception handlers are behaving properly  *)

let () = Printexc.record_backtrace true

exception UncaughtHandlerExn
exception CallbackExn

let handler (final_exn @ portable) exn =
  let id = Thread.self () |> Thread.id in
  let msg = Printexc.to_string exn in
  Printf.eprintf "[thread %d] caught %s\n" id msg;
  Printexc.print_backtrace stderr;
  flush stderr;
  let final_exn = final_exn () in
  raise final_exn

(* don't inline to get consistent backtraces *)
let[@inline never] fn () =
  Printexc.raise_with_backtrace
    CallbackExn (Printexc.get_raw_backtrace ())

let _ =
  let th = Thread.create fn () in
  Thread.join th;
  Thread.set_uncaught_exception_handler (handler (fun () -> UncaughtHandlerExn));
  let th = Thread.create fn () in
  Thread.join th;
  Thread.set_uncaught_exception_handler (handler (fun () -> Thread.Exit));
  let th = Thread.create fn () in
  Thread.join th

(* TEST
 flags = "-g";
 ocamlrunparam += ",b=1";
 include systhreads;
 hassysthreads;
 {
   bytecode;
 }{
   native;
 }
*)
