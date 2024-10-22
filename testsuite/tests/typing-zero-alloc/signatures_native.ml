(* TEST
   native;
*)

(* This test is just checking that zero_alloc attributes in signatures don't
   result in warning 199 when compiling to native code (which was a bug in the
   initial implementation. *)

module type S = sig
  type t

  val[@zero_alloc] f : int -> int

  val[@zero_alloc arity 3] g : t
end
