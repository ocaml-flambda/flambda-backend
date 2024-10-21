(* Normal lists and arrays (should not be counted) *)

val x : 'a list

val y : 'a array

module type M = sig
  val x : 'a list

  val y : 'a array
end
