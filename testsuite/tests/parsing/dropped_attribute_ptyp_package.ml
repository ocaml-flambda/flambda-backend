(* TEST
   toplevel;
*)

(* CR tdelvecchio: The attributes below are silently dropped by the parser. *)

module type T = sig
  type t
end;;

module type U = sig
  val foo : (module T [@attr] with type t = 'a) -> unit
end;;

module U : U = struct
  let foo (type a) (module M : T [@attr] with type t = a) = ()
end;;
