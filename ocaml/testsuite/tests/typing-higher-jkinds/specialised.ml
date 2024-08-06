(* TEST
  flags = "-extension layouts_alpha";
  expect;
*)

type l : value => value = list
let x: int l = [1]
[%%expect{|
type l = list
Line 2, characters 15-18:
2 | let x: int l = [1]
                   ^^^
Error: This expression has type 'a list
       but an expression was expected of type int l
|}]

(* Basic GADTs *)

type ('a : value => value) t =
  | List : list t
  | Option : option t
[%%expect{|
type ('a : value => value) t = List : list t | Option : option t
|}]

let foo : type (a : value => value). a t -> int a = function
  | List -> [1]
  | Option -> Some 2
[%%expect{|
Line 2, characters 12-15:
2 |   | List -> [1]
                ^^^
Error: This expression has type 'a list
       but an expression was expected of type int a
|}]

type l : value => value = list
let x: int l = [1]
[%%expect{|
type l = list
Line 2, characters 15-18:
2 | let x: int l = [1]
                   ^^^
Error: This expression has type 'a list
       but an expression was expected of type int l
|}]

(* Basic GADTs *)

type ('a : value => value) t =
  | List : list t
  | Option : option t
[%%expect{|
type ('a : value => value) t = List : list t | Option : option t
|}]

let foo : type (a : value => value). a t -> int a = function
  | List -> [1]
  | Option -> Some 2
[%%expect{|
Line 2, characters 12-15:
2 |   | List -> [1]
                ^^^
Error: This expression has type 'a list
       but an expression was expected of type int a
|}]

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
Line 6, characters 15-17:
6 |   | Instance : 'm functor_instance -> 'm funct
                   ^^
Error: This type ('m : '_representable_layout_1)
       should be an instance of type ('a : value => value)
       The layout of 'm is '_representable_layout_1, because
         it's a fresh unification variable.
       But the layout of 'm must overlap with ((value) => value), because
         of the definition of functor_instance at lines 1-4, characters 0-1.
|}]

let return : type a (m : value => value). m funct -> a -> a m = fun f x -> match f with
  | Id -> { id = x }
  | List -> [x]
  | Instance inst -> inst.return x

[%%expect{|
Line 1, characters 44-49:
1 | let return : type a (m : value => value). m funct -> a -> a m = fun f x -> match f with
                                                ^^^^^
Error: Unbound type constructor funct
Hint: Did you mean unit?
|}]

let map : type a b (m : value => value). m funct -> (a -> b) -> a m -> b m = fun f t x -> match f with
  | Id -> { id = t x.id }
  | List -> List.map t x
  | Instance inst -> inst.map t x

[%%expect{|
Line 1, characters 43-48:
1 | let map : type a b (m : value => value). m funct -> (a -> b) -> a m -> b m = fun f t x -> match f with
                                               ^^^^^
Error: Unbound type constructor funct
Hint: Did you mean unit?
|}]
