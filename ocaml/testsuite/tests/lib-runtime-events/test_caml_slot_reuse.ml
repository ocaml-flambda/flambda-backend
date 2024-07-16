(* TEST
<<<<<<< HEAD
 include runtime_events;
 reason = "CR OCaml 5 domains";
 skip;
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
    for i = 0 to 256 do
      Domain.join (Domain.spawn (fun _ -> ()))
    done;
    let cursor = create_cursor None in
    let runtime_begin domain_id ts phase =
      match phase with
      | EV_MINOR ->
        got_minor := true
      | _ -> () in
    let callbacks = Callbacks.create ~runtime_begin ()
    in
    Domain.join (Domain.spawn (fun _ -> Gc.full_major ()));
    ignore(read_poll cursor callbacks (Some 1_000_000));
    assert(!got_minor)
