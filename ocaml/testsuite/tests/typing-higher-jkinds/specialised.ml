(* TEST
  reason = "GADTs don't work with higher kinds";
  skip;
  flags = "-extension layouts_alpha";
  expect;
*)

type ('m : value => value) functor_instance = {
  return : 'a. 'a -> 'a 'm;
  map : 'a 'b. ('a -> 'b) -> ('a 'm -> 'b 'm)
}

[%%expect{|
type ('m : value => value) functor_instance = {
  return : 'a. 'a -> 'a 'm;
  map : 'a 'b. ('a -> 'b) -> 'a 'm -> 'b 'm;
}
|}]

type 'a id = { id : 'a }

type ('m : value => value) funct =
  | Id : id funct
  | List : list funct
  | Instance : 'm functor_instance -> 'm funct

[%%expect{|
type 'a id = { id : 'a; }
type ('m : value => value) funct =
    Id : id funct
  | List : list funct
  | Instance : ('m : value => value). 'm functor_instance -> 'm funct
|}]

let return : type a (m : value => value). m funct -> a -> a m = fun f x -> match f with
  | Id -> { id = x }
  | List -> [x]
  | Instance inst -> inst.return x

[%%expect{|
Line 2, characters 10-20:
2 |   | Id -> { id = x }
              ^^^^^^^^^^
Error: This expression has type a id but an expression was expected of type
         a m
|}]

let map : type a b (m : value => value). m funct -> (a -> b) -> a m -> b m = fun f t x -> match f with
  | Id -> { id = t x.id }
  | List -> List.map t x
  | Instance inst -> inst.map t x

[%%expect{|
Line 2, characters 19-20:
2 |   | Id -> { id = t x.id }
                       ^
Error: This expression has type a m but an expression was expected of type
         'a id
|}]
