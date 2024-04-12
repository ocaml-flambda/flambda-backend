(* TEST
  flags = " -rectypes ";
  expect;
*)


module type T = sig
  type t
end

[%%expect{|
module type T = sig type t end
|}]

let f (x : {M : T} -> {M : T} -> 'a as 'a) =
  (x : ({N : T} -> 'b) as 'b)

[%%expect{|
val f : ({N : T} -> 'a as 'a) -> 'a = <fun>
|}, Principal{|
val f : ({M : T} -> {M : T} -> 'a as 'a) -> ({N : T} -> 'b as 'b) = <fun>
|}]


let f (x : {M : T} -> ({M : T} -> 'a as 'a)) =
  (x : ({N : T} -> 'b) as 'b)

[%%expect{|
val f : ({M : T} -> ({N : T} -> 'a as 'a)) -> 'a = <fun>
|}, Principal{|
val f : ({M : T} -> ({M : T} -> 'a as 'a)) -> ({N : T} -> 'b as 'b) = <fun>
|}]

let f {M : T} (x : {M : T} -> 'a as 'a) =
  x {M} {M} {M} {M} {M}

[%%expect{|
val f : {M : T} -> ({M : T} -> 'a as 'a) -> 'a = <fun>
|}, Principal{|
val f : {M : T} -> ({M : T} -> 'a as 'a) -> ({M : T} -> 'b as 'b) = <fun>
|}]

let f (x : {M : T} -> (M.t * ({N : T} -> 'a) as 'a)) =
  (x : ({O : T} -> O.t * 'b) as 'b)

[%%expect{|
Line 2, characters 3-4:
2 |   (x : ({O : T} -> O.t * 'b) as 'b)
       ^
Error: This expression has type "{M : T} -> (M.t * ({N : T} -> 'a) as 'a)"
       but an expression was expected of type "{O : T} -> O.t * 'b as 'b"
       The module "O" would escape its scope
|}]

let f (x : {M : T} -> (M.t * ({N : T} -> (N.t * 'a) as 'a))) =
  (x : ({O : T} -> O.t * 'b) as 'b)

[%%expect{|
val f : ({M : T} -> M.t * ({O : T} -> O.t * 'a as 'a)) -> 'a = <fun>
|}, Principal{|
val f :
  ({M : T} -> M.t * ({N : T} -> N.t * 'a as 'a)) ->
  ({O : T} -> O.t * 'b as 'b) = <fun>
|}]
