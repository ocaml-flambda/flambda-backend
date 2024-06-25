(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

type ('a : value) t : value_or_null = 'a or_null =
  | Null
  | This of 'a

[%%expect{|
type 'a t = 'a or_null = Null | This of 'a
|}]

let to_option (x : 'a or_null) =
  match x with
  | Null -> None
  | This x -> Some x

[%%expect{|
val to_option : 'a or_null -> 'a option = <fun>
|}]

let of_option (x : 'a option) =
  match x with
  | None -> Null
  | Some x -> This x

[%%expect{|
val of_option : 'a option -> 'a t = <fun>
|}]

let pi = This 3.14

[%%expect{|
val pi : float t = This 3.14
|}]

let pi' =
  let value = This 3.14 in
  value

[%%expect{|
val pi' : float t = This 3.14
|}]

type myrec = { x : int
         ; y : int or_null
         }

[%%expect{|
type myrec = { x : int; y : int or_null; }
|}]

let fst { x; y } = x

[%%expect{|
val fst : myrec -> int = <fun>
|}]

let snd { x; y } = y

[%%expect{|
val snd : myrec -> int or_null = <fun>
|}]

let snd' (a : myrec) = a.y

[%%expect{|
val snd' : myrec -> int or_null = <fun>
|}]

let mk n = { x = n; y = This n }

[%%expect{|
val mk : int -> myrec = <fun>
|}]

let test =
  let a = mk 4 in
  let a' = { a with y = Null } in
  a'.y

[%%expect{|
val test : int or_null = Null
|}]

let mytup = (4, This 5)

[%%expect{|
val mytup : int * int t = (4, This 5)
|}]

type mytup' = int * int t

[%%expect{|
type mytup' = int * int t
|}]

type nested = int or_null or_null

[%%expect{|
Line 1, characters 14-25:
1 | type nested = int or_null or_null
                  ^^^^^^^^^^^
Error: This type int or_null should be an instance of type ('a : value)
       The layout of int or_null is value_or_null, because
         it is the primitive value_or_null type or_null.
       But the layout of int or_null must be a sublayout of value, because
         the type argument of or_null has layout value.
|}]

let should_fail = This (This 5)

[%%expect{|
Line 1, characters 23-31:
1 | let should_fail = This (This 5)
                           ^^^^^^^^
Error: This expression has type 'a t = 'a or_null
       but an expression was expected of type ('b : value)
       The layout of 'a t is value_or_null, because
         it is the primitive value_or_null type or_null.
       But the layout of 'a t must be a sublayout of value, because
         of the definition of t at lines 1-3, characters 0-14.
|}]

let mk' n = `Foo (This n)

[%%expect{|
val mk' : 'a -> [> `Foo of 'a t ] = <fun>
|}]
