(* TEST
  reason = "Leibniz equality is better with partial type application";
  skip;
  flags = "-extension layouts_alpha";
  expect;
*)

module Eq : sig
  type ('a : top, 'b : top) eq

  val refl : unit -> ('a, 'a) eq
  val subst : 'a 'b ('f : top => value). ('a, 'b) eq -> ('a 'f -> 'b 'f)
end = struct
  type ('a : top, 'b : top) eq = { subst : ('f : top => value). 'a 'f -> 'b 'f }
  let refl () = { subst = (fun x -> x) }
  let subst eq x = eq.subst x
end

[%%expect{|
module Eq :
  sig
    type ('a : top, 'b : top) eq
    val refl : unit -> ('a, 'a) eq
    val subst :
      ('a : top) ('b : top) ('f : top => value).
        ('a, 'b) eq -> 'a 'f -> 'b 'f
  end
|}]

open Eq
(* FIXME: this needs both inference and abstract datatypes *)
let trans : ('a, 'b) eq -> ('b, 'c) eq -> ('a, 'c) eq = fun ab bc -> subst bc ab

(* how does this error?? some intermediate type gets placed in a Tapp, I guess? *)
[%%expect{|
Uncaught exception: Failure("no jkind for tconstr???")

|}]
