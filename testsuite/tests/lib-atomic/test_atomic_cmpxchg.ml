(* TEST
 flags = "-alert -unsafe_multidomain";
*)

let r = Atomic.make 1
let () = assert (Atomic.get r = 1)

let () = Atomic.set r 2
let () = assert (Atomic.get r = 2)

let () = assert (Atomic.exchange r 3 = 2)

let () = assert (Atomic.compare_exchange r 3 4 = 3)
let () = assert (Atomic.get r = 4)

let () = assert (Atomic.compare_exchange r 3 (-4) = 4)
let () = assert (Atomic.get r = 4)

let () = assert (Atomic.compare_exchange r 3 4 = 4)
let () = assert (Atomic.get r = 4)

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
let () = assert (Atomic.compare_exchange r c d == c)
let () = assert (Atomic.get r == d)

let e = ref (-4)
let () = assert (Atomic.compare_exchange r c e == d)
let () = assert (Atomic.get r == d)

let () = assert (Atomic.compare_exchange r c d == d)
let () = assert (Atomic.get r == d)
