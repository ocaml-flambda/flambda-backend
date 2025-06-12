(* TEST
   toplevel;
*)

(* There is no place for the following attributes to attach to; the compiler should error
   rather than silently dropping them (as it used to do). *)

module type T = sig
  type t
end;;

module type U = sig
  val foo : (module T [@attr] with type t = 'a) -> unit
end;;

module U : U = struct
  let foo (type a) (module M : T [@attr] with type t = a) = ()
end;;
