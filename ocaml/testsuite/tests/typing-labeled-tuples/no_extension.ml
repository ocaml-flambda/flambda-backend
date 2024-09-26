(* TEST
   flags = "-extension-universe no_extensions";
   expect;
*)

type t = x:int * y:string

[%%expect{|
type t = x:int * y:string
|}]

let f x y = ~x, ~y

[%%expect{|
val f : 'a -> 'b -> x:'a * y:'b = <fun>
|}]

let x = ~x:5, ~y:"hi"

[%%expect{|
val x : x:int * y:string = (~x:5, ~y:"hi")
|}]

let f x y = ~(x : int), ~(y : string)

[%%expect{|
val f : int -> string -> x:int * y:string = <fun>
|}]

let f (~x, ~y) = x, y

[%%expect{|
val f : (x:'a * y:'b) -> 'a * 'b = <fun>
|}]

let f (~x:_, ~y:_) = ()

[%%expect{|
val f : (x:'a * y:'b) -> unit = <fun>
|}]

let f (~(x:int), ~(y:string)) = x, y

[%%expect{|
val f : (x:int * y:string) -> int * string = <fun>
|}]

let f ((x, ..) : (int * string * float)) = x

[%%expect{|
val f : int * string * float -> int = <fun>
|}]

let f ((x, y, ..) : (int * string * float)) = x, y

[%%expect{|
val f : int * string * float -> int * string = <fun>
|}]
