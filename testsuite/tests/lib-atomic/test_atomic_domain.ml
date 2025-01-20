(* TEST
   runtime5;
*)

let[@alert "-unsafe_parallelism"] () = Domain.spawn (fun () ->
  let r = Atomic.make 1 in
  let () = assert (Atomic.get r = 1) in

  let () = Atomic.set r 2 in
  let () = assert (Atomic.get r = 2) in

  let () = assert (Atomic.exchange r 3 = 2) in

  let () = assert (Atomic.compare_and_set r 3 4 = true) in
  let () = assert (Atomic.get r = 4) in

  let () = assert (Atomic.compare_and_set r 3 (-4) = false) in
  let () = assert (Atomic.get r = 4 ) in

  let () = assert (Atomic.compare_and_set r 3 4 = false) in

  let () = assert (Atomic.fetch_and_add r 2 = 4) in
  let () = assert (Atomic.get r = 6) in

  let () = assert (Atomic.fetch_and_add r (-2) = 6) in
  let () = assert (Atomic.get r = 4) in

  let () = assert ((Atomic.incr r; Atomic.get r) = 5) in

  let () = assert ((Atomic.decr r; Atomic.get r) = 4) in

  let () = assert ((Atomic.add r 3; Atomic.get r) = 7) in
  let () = assert ((Atomic.sub r 3; Atomic.get r) = 4) in
  let () = assert ((Atomic.logand r 2; Atomic.get r) = 0) in
  let () = assert ((Atomic.logor r 2; Atomic.get r) = 2) in
  let () = assert ((Atomic.logxor r 3; Atomic.get r) = 1) in

  let () =
    let r = Atomic.make 0 in
    let cur = Atomic.get r in
    ignore (Atomic.set r (cur + 1), Atomic.set r (cur - 1));
    assert (Atomic.get r <> cur)
  in

  let () =
    let r = Atomic.make 0 in
    let cur = Atomic.get r in
    ignore (Atomic.incr r, Atomic.decr r);
    assert (Atomic.get r = cur)
  in

  (* Test primitives with non-immediate types *)

  let a = ref 1 in
  let r = Atomic.make a in
  let () = assert (Atomic.get r == a) in

  let b = ref 2 in
  let () = Atomic.set r b in
  let () = assert (Atomic.get r == b) in

  let c = ref 3 in
  let () = assert (Atomic.exchange r c == b) in

  let d = ref 4 in
  let () = assert (Atomic.compare_and_set r c d = true) in
  let () = assert (Atomic.get r == d) in

  let e = ref (-4) in
  let () = assert (Atomic.compare_and_set r c e = false) in
  let () = assert (Atomic.get r == d) in

  let () = assert (Atomic.compare_and_set r c d = false) in

  ()) |> Domain.join
