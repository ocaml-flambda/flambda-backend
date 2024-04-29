(* Tests showing how [zero_alloc] information in the signatures of first class
   modules can be used.

   For more comprehensive tests of using zero_alloc information from signatures,
   see [test_signatures_separate_{a,b}.ml]. *)

(* Most basic use case. *)
module type S_basic = sig
  val[@zero_alloc] f : int -> int
end

let[@zero_alloc] g x y =
  let module X = (val x : S_basic) in
  X.f y

(* Can package it back up *)
let h x =
  let module X = (val x : S_basic) in
  (module struct
    let[@zero_alloc] f y = X.f (y+1)
  end : S_basic)

(* A non-strict assumption won't help you with a strict check. *)
let[@zero_alloc strict] g_strict x y =
  let module X = (val x : S_basic) in
  X.f y

module type S_strict = sig
  val[@zero_alloc strict] f : int -> int
end

let h_strict x =
  let module X = (val x : S_basic) in
  (module struct
    let[@zero_alloc strict] f y = X.f (y+1)
   end : S_strict)

