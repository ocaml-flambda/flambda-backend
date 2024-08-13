(* TEST
  flags = "-extension layouts_alpha";
  expect;
*)

module Eq : sig
  type eq : top => top => value

  val refl : 'a ('a eq)
  val subst : 'a 'b ('f : top => value). 'b ('a eq) -> ('a 'f -> 'b 'f)
end = struct
  type eq : top => top => value
  let refl = Obj.magic ()
  let subst ab a = Obj.magic a
end

[%%expect{|
module Eq :
  sig
    type eq : top => top => value
    val refl : 'a ('a eq)
    val subst : 'a 'b ('f : top => value). 'b ('a eq) -> 'a 'f -> 'b 'f
  end
|}]

open Eq

let trans ab bc : 'c ('a eq) = subst bc ab

[%%expect{|
val trans : ('a : top) 'b 'c. 'b ('a Eq.eq) -> 'c ('b Eq.eq) -> 'c ('a Eq.eq) =
  <fun>
|}]
