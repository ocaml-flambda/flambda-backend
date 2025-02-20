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
val f : #((int -> int) list * int ref list) @ contended -> unit = <fun>
|}]

let f (x : #( (int -> int) list * int ref list ) @@ nonportable contended) =
  use_uncontended x

[%%expect{|
val f : #((int -> int) list * int ref list) @ contended -> unit = <fun>
|}]

let f (x : #( unit list * string list ) @@ nonportable contended) =
  use_portable x;
  use_uncontended x

[%%expect{|
val f : #(unit list * string list) @ contended -> unit = <fun>
|}]
