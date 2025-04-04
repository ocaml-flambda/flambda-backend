(* TEST
   bytecode;
   native;
*)

open Effect
open Effect.Deep

let () = Sys.catch_break true

(* The structure of this test follows one in async_exns_1.ml. *)

let[@inline never] allocate_bytes finished =
  let b = Bytes.create 42 in
  Gc.finalise_last (fun () ->
      finished := true;
      raise Sys.Break)
    b;
  ref (Some b)

let fiber_stack_limit = 500

exception Ok
let test1 () =
  try
    let finished = ref false in
    let r = allocate_bytes finished in
    try
      Sys.with_async_exns (fun () ->
        try
          r := None;
          (* Make nested fiber stacks, then continue allocating until the
             finaliser gets called *)
          let rec make_new_stacks num_stacks =
            let _ = Sys.opaque_identity (42, Random.int 42) in
            if num_stacks > fiber_stack_limit then
              make_new_stacks num_stacks
            else
              match_with make_new_stacks (num_stacks + 1)
                { retc = (fun _ ->
                    Printf.printf "1. retc was called\n%!";
                    assert false);
                  exnc = (fun exn ->
                    Printf.printf "1. exnc was called\n%!";
                    Printf.printf "exn: %s" (Printexc.to_string exn);
                    assert false);
                  effc = (fun (type a) (_eff: a t) ->
                    Printf.printf "1. effc was called\n%!";
                    assert false)
                }
          in
          make_new_stacks 0
        with exn -> Printf.printf "1. wrong handler\n%!"; assert false
      )
    with
    | Sys.Break -> assert !finished; raise Ok
    | _ -> assert false
  with
  | Ok -> Printf.printf "1. OK\n%!"
  | _ -> assert false

let () = test1 ()

let pre_fiber_stack_limit = 100

(* As above, but create some stacks first, so the async exn handler isn't on
   the first stack. *)
let () =
  let rec make_new_stacks num_stacks =
    if num_stacks > pre_fiber_stack_limit then
      test1 ()
    else
      match_with make_new_stacks (num_stacks + 1)
        { retc = (fun _ -> ());
          exnc = (fun exn ->
            Printf.printf "2. exnc was called\n%!";
            Printf.printf "exn: %s" (Printexc.to_string exn);
            assert false);
          effc = (fun (type a) (_eff: a t) ->
            Printf.printf "2. effc was called\n%!";
            assert false)
        }
  in
  make_new_stacks 0
