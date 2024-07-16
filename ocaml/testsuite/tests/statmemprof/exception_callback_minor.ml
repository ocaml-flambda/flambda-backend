<<<<<<< HEAD
(* TEST *)
||||||| 121bedcfd2
(* TEST
   exit_status = "2"
   * skip
   reason = "port stat-mem-prof : https://github.com/ocaml/ocaml/pull/8634"
*)
=======
(* TEST
 exit_status = "2";
 reason = "port stat-mem-prof : https://github.com/ocaml/ocaml/pull/8634";
 skip;
*)
>>>>>>> 5.2.0

(* Tests that an exception in the alloc_minor callback propagates
   correctly to the top level. *)

module MP = Gc.Memprof

let _ =

try
 Sys.with_async_exns (fun () ->
   let _:MP.t = MP.start ~callstack_size:10 ~sampling_rate:1.
                  { MP.null_tracker with
                      alloc_minor =
                        fun _ -> raise Sys.Break } in
     (ignore (Sys.opaque_identity (ref (ref 42)));
      MP.stop ())
 )
with
  Sys.Break -> (MP.stop();
                Printf.printf "Exception from memprof.\n")
