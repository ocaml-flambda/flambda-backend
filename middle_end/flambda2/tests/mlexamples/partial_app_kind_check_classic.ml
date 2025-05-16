[@@@ocaml.flambda_oclassic]

type (_,_) eq = Eq : ('a,'a) eq

let test (u : (int -> int -> int, int -> int64#) eq) a b =
  let[@local always] cast : type a b. (a,b) eq -> a -> b = fun Eq x -> x in
  let[@inline never][@local never] f _ _ = 0 in
  (cast u f) 0
