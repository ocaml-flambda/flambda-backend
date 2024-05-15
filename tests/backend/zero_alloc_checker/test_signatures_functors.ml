(* Tests showing how [zero_alloc] information from a functor parameter's
   signature can be used in checking the functor's body.

   For more comprehensive tests of using zero_alloc information from signatures,
   see [test_signatures_separate_{a,b}.ml]. *)

(* Most basic use case. *)
module type S_basic = sig
  val[@zero_alloc] f : int -> int
end

module F_basic (X : S_basic) = struct
  let[@zero_alloc] g x = X.f x
end

(* A non-strict assumption won't help you with a strict check. *)
module F_strict_bad (X : S_basic) = struct
  let[@zero_alloc strict] g x = X.f x
end
