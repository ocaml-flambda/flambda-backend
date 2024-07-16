(* TEST
<<<<<<< HEAD
 {
   include runtime_events;
   runtime5;
   { bytecode; }
   { native; }
 }{
   runtime4;
   skip;
 }
||||||| 121bedcfd2
include runtime_events
=======
 include runtime_events;
>>>>>>> 5.2.0
*)
open Runtime_events

let () =
    start ();
    let cursor = create_cursor None in
    let empty_callbacks = Callbacks.create () in
    let runtime_begin domain_id ts phase =
      match phase with
      | EV_MINOR ->
        ignore(read_poll cursor empty_callbacks None)
      | _ -> () in
    let callbacks = Callbacks.create ~runtime_begin ()
    in
    Gc.full_major ();
    try begin
      ignore(read_poll cursor callbacks None);
      Printf.printf "Exception ignored"
    end with
      Failure(_) ->
        (* Got an exception because we tried to reenter *)
        Printf.printf "OK"
