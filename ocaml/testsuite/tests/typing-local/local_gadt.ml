(* TEST *)

type (_, _) eq = Eq : ('a, 'a) eq

let[@inline always] cast (type a b) (x : a) (Eq : (a, b) eq) : b = x

let test (f : local_ 'a -> local_ 'b) (eq : (local_ 'a -> local_ 'b, 'a -> 'b) eq) (x : 'a) : 'b =
  (cast f eq) x 

type 'a box = Box of 'a

let localf : (local_ 'a -> local_ 'a box) = fun x -> exclave_ (local_ (Box x))

let g eq x = test localf eq x