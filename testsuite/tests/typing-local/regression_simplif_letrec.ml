(* TEST *)

(* Regression test for incorrect Simplif substitution of Alias bindings
   into Lletrec closures *)

let ok () =
  Gc.full_major ();
  print_endline "ok"

let[@inline never] test1 f =
  match
    if Sys.opaque_identity false
    then (Sys.opaque_identity 1), 3
    else (Sys.opaque_identity 2), 4
  with
  | a, b ->
    let rec loop () =
      let retry () =
        ignore (Sys.opaque_identity b);
        loop ()
      in
      f ();
      if Sys.opaque_identity false then retry ()
    in
    loop ()

let () = test1 ok

type 'a g = { g: 'a @@ global }

let[@inline never] test2 f =
  let { g = r } = { g = ref 42 } in
  let rec loop () =
    f ();
    ignore (Sys.opaque_identity r);
    if Sys.opaque_identity false then loop ()
  in
  loop

let () = test2 ok ()
