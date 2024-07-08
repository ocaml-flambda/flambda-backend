(* TEST
<<<<<<< HEAD
 modules = "test_c_thread_has_lock_cstubs.c";
 runtime5;
 {
   bytecode;
 }{
   native;
 }
||||||| 121bedcfd2
   modules = "test_c_thread_has_lock_cstubs.c"
   * bytecode
   * native
=======
 modules = "test_c_thread_has_lock_cstubs.c";
 {
   bytecode;
 }{
   native;
 }
>>>>>>> 5.2.0
*)

external test_with_lock : unit -> bool = "with_lock"
external test_without_lock : unit -> bool = "without_lock"

let passed b = Printf.printf (if b then "passed\n" else "failed\n")

let f () =
  passed (not (test_without_lock ())) ;
  passed (test_with_lock ())

let _ =
  f ();
