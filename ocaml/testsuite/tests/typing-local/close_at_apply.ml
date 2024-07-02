(* TEST
   modules = "cstubs.c";
   stack-allocation;
   flambda2;
   {
     ocamlopt_flags="-Oclassic";
   }{
     ocamlopt_flags="-O3";
   }
*)

external local_stack_offset : unit -> int = "caml_local_stack_offset"

external ignore : ('a[@local_opt]) -> unit = "%ignore"
external opaque_identity : ('a[@local_opt]) -> ('a[@local_opt]) = "%opaque"

external ( +. )
  :  (float[@local_opt])
  -> (float[@local_opt])
  -> (float[@local_opt])
  = "%addfloat"

let locally_allocate =
  let f = 1.0 in
  fun [@inline never] () -> ignore (opaque_identity (f +. 1.0) : float)
;;

let saved_stack_offset = ref 0

let foo list =
  (* The Simplif local function optimization will transform [run] and
     [wrapper] into continuations. *)
  let[@local always][@inline never] run f = f () in
  let[@local always][@inline never] wrapper f =
    match Sys.opaque_identity true with
    | true ->
      (* Make a local allocation *)
      ignore (opaque_identity (opaque_identity 1.0 +. 1.0));
      (* This tail call gets moved into non-tail position after the above
         inlining transformation.  As such it's important that its
         [Rc_close_at_apply] semantics is respected carefully: only the
         current region, and not the parent one, must be closed (since [f]
         is locally allocated in that parent region). *)
      f ()
    | false -> run (fun () -> f ()) [@nontail]
  in
  let f = fun _ ->
    assert (local_stack_offset () = !saved_stack_offset);
    locally_allocate ();
    Stdlib.List.hd list
  in
  saved_stack_offset := local_stack_offset ();
  wrapper f [@nontail]
;;

let _ : int = foo [ 1 ]
