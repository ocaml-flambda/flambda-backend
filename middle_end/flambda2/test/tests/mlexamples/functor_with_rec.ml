(* This defines a functor, with a recursive function inside. Recursive functions
   cannot be inlined by default, but when inlining the functor we want to be
   able to specialise [foo] to replace the call to [X.test] with a direct
   call. *)

module type S = sig
  val test : int -> bool
end

module F (X : S) = struct
  let rec foo x = if X.test x then x else foo (x + 1)
end
