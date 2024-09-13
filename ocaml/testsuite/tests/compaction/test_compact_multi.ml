(* TEST
 reason = "CR ocaml 5 domains: re-enable this test";
 skip;
*)
let num_domains = 2

let release = Atomic.make false

let lots_of_compactions () =
  while not (Atomic.get release) do
    ()
  done;
  for y = 1 to 10 do
    for x = 1 to 5 do
      let n = x*100000 in
      Gc.compact ();
      let c = Array.make n None in
      for i = 0 to n-1 do
        c.(i) <- Some (i, i)
      done;
      for d = 0 to 10 do
        (* now we keep only every d *)
        for i = 0 to n-1 do
          if d > 0 && i mod d <> 0 then c.(i) <- None
        done;
        (* force a GC *)
        Gc.compact ();
      done;
    done;
  done;
  Gc.compact ()

let () =
  (* spawn three domains*)
  let domains = Array.init num_domains (fun _ -> Domain.spawn lots_of_compactions) in
  (* release the domains *)
  Atomic.set release true;
  (* join the domains *)
  Array.iter Domain.join domains

