(* TEST *)

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
