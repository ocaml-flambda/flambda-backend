[@@@ocaml.flambda_o3]

type (_,_) eq = Eq : ('a,'a) eq

let test (u : (int -> int64#, int -> int -> int) eq) a b =
  let[@local always] cast : type a b. (a,b) eq -> a -> b = fun Eq x -> x in
  let[@inline never][@local never] f _ = #0L in
  (cast u f) 0 0
