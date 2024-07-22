(* TEST
 expect;
*)

(** The aim of this file is to keep track of programs that are "far" from being well-typed *)


(** Arity mismatch between structure and signature *)

module M : sig
  type (_, _) t
  val f : (_, _) t -> unit
end = struct
  type _ t
  let f _ = ()
end

[%%expect{|
Line 10, characters 2-10:
10 |   type _ t
       ^^^^^^^^
Error: This type declaration is incompatible with the corresponding
       declaration in the signature: expected type (_, _) t.
|}]
