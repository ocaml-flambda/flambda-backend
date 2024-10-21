(* TEST *)

let final = ref false
let rtrue = ref true
let ev s = Printf.printf "%15s: %b\n" s !final

let[@inline never] alloc () =
  let r = ref 42 in
  Gc.finalise (fun _ -> final := true) r;
  ev "initial";
  Gc.full_major ();
  ev "live reg";
  r

let[@inline never] use (local_ _) = ()

let[@inline never] live_local () =
  if !rtrue then begin
    let g =
      if !rtrue then begin
        let s = local_ (Some (Some (alloc ()))) in
        Gc.full_major ();
        ev "live local";
        use s;

        let rec local_ f x = if x then (use s; ()) else (g (); ())
        and g () = f true; ()
        in
        g
      end else (fun () -> assert false) in
    Gc.full_major ();
    ev "live infix";
    g ();
  end;
  Gc.full_major ();
  ev "dead local";
  ()

let () =
  live_local ();
  Gc.full_major ();
  ev "after return"
