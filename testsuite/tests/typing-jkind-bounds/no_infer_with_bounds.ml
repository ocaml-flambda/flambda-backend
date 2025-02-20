(* TEST
   expect;
*)

(****************************)
(* Test 1: unboxed products *)

let use_portable (_ : (_ : value & value) @@ portable) = ()
let use_uncontended (_ : (_ : value & value) @@ uncontended) = ()

[%%expect{|
val use_portable : ('a : value & value). 'a @ portable -> unit = <fun>
val use_uncontended : ('a : value & value). 'a -> unit = <fun>
|}]

let f (x : #( (int -> int) list * int ref list ) @@ nonportable contended) =
  use_portable x

[%%expect{|
Line 2, characters 15-16:
2 |   use_portable x
                   ^
Error: This value is "nonportable" but expected to be "portable".
|}]

let f (x : #( (int -> int) list * int ref list ) @@ nonportable contended) =
  use_uncontended x

[%%expect{|
Line 2, characters 18-19:
2 |   use_uncontended x
                      ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let f (x : #( unit list * string list ) @@ nonportable contended) =
  use_portable x;
  use_uncontended x

[%%expect{|
val f : #(unit list * string list) @ contended -> unit = <fun>
|}]
