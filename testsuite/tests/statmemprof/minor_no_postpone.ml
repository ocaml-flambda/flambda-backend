(* TEST
<<<<<<< HEAD
 modules = "minor_no_postpone_stub.c";
||||||| 121bedcfd2
   modules = "minor_no_postpone_stub.c"
   * skip
   reason = "port stat-mem-prof : https://github.com/ocaml/ocaml/pull/8634"
=======
 modules = "minor_no_postpone_stub.c";
 reason = "port stat-mem-prof : https://github.com/ocaml/ocaml/pull/8634";
 skip;
>>>>>>> 5.2.0
*)

module MP = Gc.Memprof

let profile ref_ok ref_done =
  MP.start ~callstack_size:0 ~sampling_rate:1.
   { MP.null_tracker with
    alloc_minor = (fun _ ->
      assert !ref_ok;
      ref_done := true;
      None);
    }

let () =
  let callback_ok = ref true in
  let callback_done = ref false in
  let _:MP.t = profile callback_ok callback_done in
  ignore (Sys.opaque_identity (ref 0));
  assert(!callback_done);
  callback_ok := false;
  MP.stop ()

external alloc_stub : unit -> unit ref = "alloc_stub"

let () =
  let callback_ok = ref false in
  let callback_done = ref false in
  let _:MP.t = profile callback_ok callback_done in
  ignore (Sys.opaque_identity (alloc_stub ()));
  assert(not !callback_done);
  callback_ok := true;
  ignore (Sys.opaque_identity (ref ()));
  assert(!callback_done);
  MP.stop ()
