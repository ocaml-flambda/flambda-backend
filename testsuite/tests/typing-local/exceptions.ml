(* TEST
 native;
*)

external local_stack_offset : unit -> int = "caml_local_stack_offset"

let leak () =
  let r = local_ ref 42 in
  let[@inline never] f () = incr r in
  f ();
  f ();
  raise Exit

let count_leak f =
  let before = local_stack_offset () in
  f ();
  let after = local_stack_offset () in
  after - before

let glob = ref 0

let tests =
  [ "try", (fun () ->
      begin try leak () with Exit -> () end;
      incr glob);
    "tailtry", (fun () ->
      try leak () with Exit -> ());
    "match", (fun () ->
      let g = match leak () with exception Exit -> glob | () -> glob in
      incr g);
    "tailmatch", (fun () ->
      match leak () with exception Exit -> () | () -> ())
  ]

let () =
  Sys.opaque_identity tests |> List.iter (fun (s,f) ->
    Printf.printf "%15s: %d\n" s (count_leak f))
