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

type 'a portable : value mod portable = { portable : 'a @@ portable } [@@unboxed]
type 'a contended : value mod contended = { contended : 'a @@ contended } [@@unboxed]
type 'a portended : value mod portable contended = { portended : 'a @@ portable contended } [@@unboxed]
[%%expect{|
val use_global : 'a -> unit = <fun>
val use_unique : 'a @ unique -> unit = <fun>
val use_uncontended : 'a -> unit = <fun>
val use_portable : 'a @ portable -> unit = <fun>
val use_many : 'a -> unit = <fun>
type ('a : value mod contended) require_contended
type ('a : value mod portable) require_portable
type 'a portable = { portable : 'a @@ portable; } [@@unboxed]
type 'a contended = { contended : 'a @@ contended; } [@@unboxed]
type 'a portended = { portended : 'a @@ portable contended; } [@@unboxed]
|}]

let foo (x : (int -> int) portable @@ nonportable) = use_portable x
let foo (x : (int ref) contended) = use_uncontended x
let foo (x : ((int -> int) ref) portended @@ nonportable) =
  use_uncontended x;
  use_portable x
(* CR layouts v2.8: This should be accepted *)
[%%expect{|
val foo : (int -> int) portable -> unit = <fun>
val foo : int ref contended -> unit = <fun>
Line 5, characters 15-16:
5 |   use_portable x
                   ^
Error: This value is "nonportable" but expected to be "portable".
|}]

let foo (x : (int -> int) ref portable @@ nonportable) = use_portable x
[%%expect{|
Line 1, characters 70-71:
1 | let foo (x : (int -> int) ref portable @@ nonportable) = use_portable x
                                                                          ^
Error: This value is "nonportable" but expected to be "portable".
|}]

let foo (x : ((int -> int) portable) @@ nonportable) = use_portable x
[%%expect{|
val foo : (int -> int) portable -> unit = <fun>
|}]

let foo (x : ((int -> int) portable portable portable) @@ nonportable) = use_portable x
[%%expect{|
val foo : (int -> int) portable portable portable -> unit = <fun>
|}]

let foo (x : (((int -> int) ref) portable contended portable contended) @@ nonportable contended) =
  use_uncontended x;
  use_portable x
(* CR layouts v2.8: This should be accepted *)
[%%expect{|
Line 3, characters 15-16:
3 |   use_portable x
                   ^
Error: This value is "nonportable" but expected to be "portable".
|}]

(* CR aspsmith: other tests to add:

   - unboxed modalit(ies) inside unboxed tuple(s)
   - subkind check (using require_...)
   - subkind for nesting
   - [@@unboxed] variants with regular fields
   - [@@unboxed] variants with single-field inlined records
   - [@@unboxed] gadt constructors with regular fields
   - [@@unboxed] gadt constructors with single-field inlined records
   - unboxed products
*)
