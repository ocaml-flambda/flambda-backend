(* TEST
   expect;
   flags = "-extension layouts_alpha";
*)

type 'a t1 = { x : 'a }

[@@@infer_with_bounds]

type 'a t2 = { x : 'a }

[%%expect{|
type 'a t1 = { x : 'a; }
type 'a t2 = { x : 'a; }
|}]

let use_portable (_ @ portable) = ()

[%%expect{|
val use_portable : 'a @ portable -> unit = <fun>
|}]

let f1 (x : int t1 @@ nonportable) = use_portable x

[%%expect{|
Line 1, characters 50-51:
1 | let f1 (x : int t1 @@ nonportable) = use_portable x
                                                      ^
Error: This value is "nonportable" but expected to be "portable".
|}]

let f2 (x : int t2 @@ nonportable) = use_portable x

[%%expect{|
val f2 : int t2 -> unit = <fun>
|}]
