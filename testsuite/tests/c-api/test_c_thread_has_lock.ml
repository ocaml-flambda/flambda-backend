(* TEST
 modules = "test_c_thread_has_lock_cstubs.c";
 runtime5;
 {
   bytecode;
 }{
   native;
 }
*)

external test_with_lock : unit -> bool = "with_lock"
external test_without_lock : unit -> bool = "without_lock"

let passed b = Printf.printf (if b then "passed\n" else "failed\n")

let f () =
  passed (test_with_lock ());
    (* without systhreads, caml_state is NULL outside the domain lock,
       hence the _not_ here *)
  passed (not (test_without_lock ()))

let _ =
  f ();
