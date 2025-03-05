(* TEST
    expect;
*)

let use_global : 'a @ global -> unit = fun _ -> ()
let use_unique : 'a @ unique -> unit = fun _ -> ()
let use_uncontended : 'a @ uncontended -> unit = fun _ -> ()
let use_portable : 'a @ portable -> unit = fun _ -> ()
let use_many : 'a @ many -> unit = fun _ -> ()
type ('a : value mod contended) require_contended
type ('a : value mod portable) require_portable

type 'a portable_unboxed : value mod portable = { portable : 'a @@ portable } [@@unboxed]
type 'a contended_unboxed : value mod contended = { contended : 'a @@ contended } [@@unboxed]
type 'a portended_unboxed : value mod portable contended = { portended : 'a @@ portable contended } [@@unboxed]
[%%expect{|
val use_global : 'a -> unit = <fun>
val use_unique : 'a @ unique -> unit = <fun>
val use_uncontended : 'a -> unit = <fun>
val use_portable : 'a @ portable -> unit = <fun>
val use_many : 'a -> unit = <fun>
type ('a : value mod contended) require_contended
type ('a : value mod portable) require_portable
type 'a portable_unboxed = { portable : 'a @@ portable; } [@@unboxed]
type 'a contended_unboxed = { contended : 'a @@ contended; } [@@unboxed]
type 'a portended_unboxed = { portended : 'a @@ portable contended; } [@@unboxed]
|}]


type 'a portable_boxed : value mod portable = { portable : 'a @@ portable }
type 'a contended_boxed : value mod contended = { contended : 'a @@ contended }
type 'a portended_boxed : value mod portable contended = { portended : 'a @@ portable contended }
[%%expect{|
type 'a portable_boxed = { portable : 'a @@ portable; }
type 'a contended_boxed = { contended : 'a @@ contended; }
type 'a portended_boxed = { portended : 'a @@ portable contended; }
|}]


let foo (x : (int -> int) portable_boxed @@ nonportable) = use_portable x
let foo (x : (int ref) contended_boxed) = use_uncontended x
let foo (x : ((int -> int) ref) portended_boxed @@ nonportable) =
  use_uncontended x;
  use_portable x
[%%expect{|
val foo : (int -> int) portable_boxed -> unit = <fun>
val foo : int ref contended_boxed -> unit = <fun>
Line 5, characters 15-16:
5 |   use_portable x
                   ^
Error: This value is "nonportable" but expected to be "portable".
|}]

let foo (x : ((int -> int) portable_unboxed) @@ nonportable) = use_portable x
(* CR layouts v2.8: This should be accepted *)
[%%expect{|
Line 1, characters 76-77:
1 | let foo (x : ((int -> int) portable_unboxed) @@ nonportable) = use_portable x
                                                                                ^
Error: This value is "nonportable" but expected to be "portable".
|}]

let foo (x : ((int -> int) portable_unboxed portable_unboxed portable_unboxed) @@ nonportable) = use_portable x
(* CR layouts v2.8: This should be accepted *)
[%%expect{|
Line 1, characters 110-111:
1 | let foo (x : ((int -> int) portable_unboxed portable_unboxed portable_unboxed) @@ nonportable) = use_portable x
                                                                                                                  ^
Error: This value is "nonportable" but expected to be "portable".
|}]

let foo (x : (((int -> int) ref) portable_unboxed contended_unboxed portable_unboxed contended_unboxed) @@ nonportable contended) =
  use_portable x
(* CR layouts v2.8: This should be accepted *)
[%%expect{|
Line 1, characters 85-102:
1 | let foo (x : (((int -> int) ref) portable_unboxed contended_unboxed portable_unboxed contneded_unboxed) @@ nonportable contended) =
                                                                                         ^^^^^^^^^^^^^^^^^
Error: Unbound type constructor "contneded_unboxed"
Hint: Did you mean "contended_unboxed"?
|}]
