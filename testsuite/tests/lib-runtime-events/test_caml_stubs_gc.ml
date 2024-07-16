(* TEST
<<<<<<< HEAD
 {
   runtime4;
   skip;
 }{
   include runtime_events;
   runtime5;
   { bytecode; }
   { native; }
 }
||||||| 121bedcfd2
include runtime_events
=======
 include runtime_events;
>>>>>>> 5.2.0
*)
open Runtime_events

let got_minor = ref false

let () =
    start ();
    let cursor = create_cursor None in
    let runtime_begin domain_id ts phase =
      match phase with
      | EV_MINOR ->
        Gc.full_major ();
        got_minor := true
      | _ -> () in
    let callbacks = Callbacks.create ~runtime_begin ()
    in
    Gc.full_major ();
    ignore(read_poll cursor callbacks (Some 1_000));
    assert(!got_minor)
