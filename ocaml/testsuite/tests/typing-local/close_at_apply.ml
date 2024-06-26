(* TEST
   flambda2;
   {
     ocamlopt_flags="-Oclassic";
   }{
     ocamlopt_flags="-O3";
   }
*)

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

let foo list =
  (* The Simplif local function optimization will transform [run] and
     [wrapper] into continuations. *)
  let[@inline never] run f = f () in
  let[@inline never] wrapper f =
    match Sys.opaque_identity true with
    | true ->
      (* This tail call gets moved into non-tail position after the above
         inlining transformation.  As such it's important that its
         [Rc_close_at_apply] semantics is respected carefully: only the
         current region, and not the parent one, must be closed (since [f]
         is locally allocated in that parent region). *)
      f ()
    | false -> run (fun () -> f ()) [@nontail]
  in
  wrapper (fun _ ->
    locally_allocate ();
    Stdlib.List.hd list) [@nontail]
;;

let _ : int = foo [ 1 ]
