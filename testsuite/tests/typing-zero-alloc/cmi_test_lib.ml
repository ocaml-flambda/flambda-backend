(* This file is part of a test that zero_alloc variables don't remain in
   cmis. *)

let f_unconstrained_variable x = x+1

module M_constrained_variable = struct
  let f x = x+2
end

module _ : sig val[@zero_alloc] f : int -> int end = M_constrained_variable

module M_no_variable : sig
  val f : int -> int
end = struct
  let f x = x+3
end
