(* TEST
  flags = "-extension layouts_alpha";
  expect;
*)

type l : value => value = list
let x: int l = [1]
[%%expect{|
type l = list
val x : int l = <abstr>
|}]

(* Basic GADTs *)

type ('a : value => value) t =
  | List : list t
  | Option : option t
[%%expect{|
type ('a : value => value) t = List : list t | Option : option t
|}]

let foo : type (a : value => value). a t -> int a = function
  | List -> ([1] : int a)
  | Option -> Some 2
[%%expect{|
val foo : ('a : value => value). 'a t -> int 'a = <fun>
|}]

type l : value => value = list
let x: int l = [1]
[%%expect{|
type l = list
val x : int l = <abstr>
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
val foo : ('a : value => value). 'a t -> int 'a = <fun>
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
Uncaught exception: File "ocaml/typing/jkind.ml", line 183, characters 26-32: Assertion failed

|}]

let map : type a b (m : value => value). m funct -> (a -> b) -> a m -> b m = fun f t x -> match f with
  | Id -> { id = t x.id }
  | List -> List.map t x
  | Instance inst -> inst.map t x

[%%expect{|
Uncaught exception: File "ocaml/typing/jkind.ml", line 183, characters 26-32: Assertion failed

|}]
