(* TEST *)

let () = Sys.catch_break true

(* Lifted out to ensure this works in bytecode, where [b] is easily
   held on to as a root on the stack. *)
let[@inline never] allocate_bytes finished =
  let b = Bytes.create 42 in
  Gc.finalise_last (fun () ->
      finished := true;
      raise Sys.Break)
    b;
  ref (Some b)

let () =
  let finished = ref false in
  let r = allocate_bytes finished in
  try
    Sys.with_async_exns (fun () ->
      try
        r := None;
        while true do
          let _ = Sys.opaque_identity (42, Random.int 42) in
          ()
        done
      with exn -> Printf.printf "wrong handler\n%!"; assert false
    )
  with
  | Sys.Break -> assert !finished; Printf.printf "OK\n%!"
  | _ -> assert false

