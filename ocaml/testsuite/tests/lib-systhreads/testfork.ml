(* TEST
   * hassysthreads
   include systhreads
   ** not-bsd
   *** libunix
   **** bytecode
   **** native
*)

(* POSIX threads and fork() *)

let compute_thread c = ignore c
(*
  while true do
    print_char c; flush stdout;
    for i = 1 to 100000 do ignore(ref []) done
  done
*)

let main () =
  ignore(Thread.create compute_thread '1');
  Thread.delay 0.1;
  print_string "Forking..."; print_newline();
  match Unix.fork() with
  | 0 ->
      Thread.delay 0.05;
      print_string "In child..."; print_newline();
      Gc.minor();
      print_string "Child did minor GC."; print_newline();
      ignore(Thread.create compute_thread '2');
      Thread.delay 0.1;
      print_string "Child is exiting."; print_newline();
      exit 0
  | pid ->
      print_string "In parent..."; print_newline();
      Thread.delay 0.4;
      print_string "Parent is exiting."; print_newline();
      exit 0

let _ = main()
