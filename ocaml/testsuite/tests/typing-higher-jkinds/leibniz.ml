(* TEST
  flags = "-extension layouts_alpha";
  expect;
*)

module Eq : sig
  type ('a : top, 'b : top) eq

  val refl : unit -> ('a, 'a) eq
  val subst : 'a 'b ('f : top => value). ('a, 'b) eq -> ('a 'f -> 'b 'f)
end = struct
  type eq : value => value => value
  let refl = Obj.magic ()
  let subst ab a = Obj.magic a
end

[%%expect{|
module Eq :
  sig
    type eq : value => value => value
    val refl : 'a ('a eq)
    val subst : 'a 'b ('c : value => value). 'b ('a eq) -> 'a 'c -> 'b 'c
  end
|}]

open Eq

let trans ab bc : 'c ('a eq) = subst bc ab

[%%expect{|
val trans : 'b ('a Eq.eq) -> 'c ('b Eq.eq) -> 'c ('a Eq.eq) = <fun>
|}]
