(* TEST
   modules = "signal_handler_run_in_c_thread_stubs.c"
   * native
   ** threads
*)

(* This doesn't actually need systhreads, but the requirement should
   ensure the C pthreads-using code will build. *)

external c_stub : unit -> unit = "test_signal_handler_run_in_c_thread"

let () =
  Sys.set_signal Sys.sigusr1 (Signal_handle (fun _ -> exit 0));
  c_stub ();
  while true do
    (* Ensure pending actions are run, by forcing allocation *)
    ignore (Sys.opaque_identity (Random.int 42, Random.int 42))
  done
