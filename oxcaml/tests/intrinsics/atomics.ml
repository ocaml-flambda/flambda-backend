let r = Atomic.make 1

let () = assert (Atomic.get r = 1)

let () = Atomic.set r 2

let () = assert (Atomic.get r = 2)

let () = assert (Atomic.exchange r 3 = 2)

let () = assert (Atomic.compare_and_set r 3 4 = true)

let () = assert (Atomic.get r = 4)

let () = assert (Atomic.compare_and_set r 3 (-4) = false)

let () = assert (Atomic.get r = 4)

let () = assert (Atomic.compare_and_set r 3 4 = false)

let () = assert (Atomic.fetch_and_add r 2 = 4)

let () = assert (Atomic.get r = 6)

let () = assert (Atomic.fetch_and_add r (-2) = 6)

let () = assert (Atomic.get r = 4)

let () =
  assert (
    (Atomic.incr r;
     Atomic.get r)
    = 5)

let () =
  assert (
    (Atomic.decr r;
     Atomic.get r)
    = 4)

let () =
  assert (
    (Atomic.add r 3;
     Atomic.get r)
    = 7)

let () =
  assert (
    (Atomic.sub r 3;
     Atomic.get r)
    = 4)

let () =
  assert (
    (Atomic.logand r 2;
     Atomic.get r)
    = 0)

let () =
  assert (
    (Atomic.logor r 2;
     Atomic.get r)
    = 2)

let () =
  assert (
    (Atomic.logxor r 3;
     Atomic.get r)
    = 1)

let () =
  let r = Atomic.make 0 in
  let cur = Atomic.get r in
  ignore (Atomic.set r (cur + 1), Atomic.set r (cur - 1));
  assert (Atomic.get r <> cur)

let () =
  let r = Atomic.make 0 in
  let cur = Atomic.get r in
  ignore (Atomic.incr r, Atomic.decr r);
  assert (Atomic.get r = cur)

(* Test primitives with non-immediate types *)

let a = ref 1

let r = Atomic.make a

let () = assert (Atomic.get r == a)

let b = ref 2

let () = Atomic.set r b

let () = assert (Atomic.get r == b)

let c = ref 3

let () = assert (Atomic.exchange r c == b)

let d = ref 4

let () = assert (Atomic.compare_and_set r c d = true)

let () = assert (Atomic.get r == d)

let e = ref (-4)

let () = assert (Atomic.compare_and_set r c e = false)

let () = assert (Atomic.get r == d)

let () = assert (Atomic.compare_and_set r c d = false)
