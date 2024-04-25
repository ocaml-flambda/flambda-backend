(* TEST
 reason = "bugs not fixed yet";
 skip;
 flags = "-extension-universe alpha";
 expect;
*)

(* CR layouts v1.5: merge this with annots.ml after these all pass *)

class c : object
  val f : 'a -> 'a
end = object
  val f = fun (x : ('a : immediate)) -> x
end
;;
[%%expect {|
fail
|}]

class c : object
  method m : ('a : immediate). 'a -> 'a
  val f : ('a : immediate) -> 'a
end = object
  method m : type (a : immediate). a -> a = fun x -> x
  val f = fun (x : ('a : immediate)) -> x
end

[%%expect {|
success with both types quantified over immediates
|}]

type _ g = | MkG : ('a : immediate) ('b : void). 'a -> 'b g

type ('a : void) t3 = ..
type _ t3 += MkG : ('a : immediate) 'b. 'a -> 'b t3

[%%expect {|
success (I think)
|}]

let f_gadt : ('a : value). 'a -> 'a g -> 'a = fun x MkG -> f_imm x

[%%expect {|
success
|}]
