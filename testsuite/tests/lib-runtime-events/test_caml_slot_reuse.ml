(* TEST
 {
   runtime4;
   skip;
 }{
   include runtime_events;
   runtime5;
   flags += "-alert -unsafe_parallelism -alert -unsafe_multidomain";
   { bytecode; }
   { native; }
 }
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
