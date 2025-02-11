(* TEST *)

let () =
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
  Gc.compact ();
