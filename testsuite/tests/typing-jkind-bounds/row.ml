(* TEST
    expect;
*)

let use_global : 'a @ global -> unit = fun _ -> ()
let use_unique : 'a @ unique -> unit = fun _ -> ()
let use_uncontended : 'a @ uncontended -> unit = fun _ -> ()
let use_portable : 'a @ portable -> unit = fun _ -> ()
let use_many : 'a @ many -> unit = fun _ -> ()
[%%expect{|
val use_global : 'a -> unit = <fun>
val use_unique : 'a @ unique -> unit = <fun>
val use_uncontended : 'a -> unit = <fun>
val use_portable : 'a @ portable -> unit = <fun>
val use_many : 'a -> unit = <fun>
|}]

(* Simple cases : closed polymorphic variants with fields *)

type 'a t : immutable_data with 'a = [ `A of 'a ]

[%%expect{|
type 'a t = [ `A of 'a ]
|}]

type 'a u : mutable_data with 'a = [ `B of 'a ref ]
[%%expect{|
type 'a u = [ `B of 'a ref ]
|}]

type 'a v : value mod contended with 'a = [ `C of 'a | `D of 'a -> 'a | `E of 'a option ]
[%%expect{|
type 'a v = [ `C of 'a | `D of 'a -> 'a | `E of 'a option ]
|}]

type 'a w : value mod contended with 'a = [ 'a t | 'a v ]
[%%expect{|
type 'a w = [ `A of 'a | `C of 'a | `D of 'a -> 'a | `E of 'a option ]
|}]

let cross_contention (x : int t @ contended) = use_uncontended x
let cross_portability (x : int t @ nonportable) = use_portable x
let cross_linearity (x : int t @ once) = use_many x
[%%expect{|
val cross_contention : int t @ contended -> unit = <fun>
val cross_portability : int t -> unit = <fun>
val cross_linearity : int t @ once -> unit = <fun>
|}]

let don't_cross_unique (x : int t @ aliased) = use_unique x
[%%expect{|
Line 1, characters 58-59:
1 | let don't_cross_unique (x : int t @ aliased) = use_unique x
                                                              ^
Error: This value is "aliased" but expected to be "unique".
|}]

let don't_cross_locality (x : int t @ local) = use_global x
[%%expect{|
Line 1, characters 58-59:
1 | let don't_cross_locality (x : int t @ local) = use_global x
                                                              ^
Error: This value escapes its region.
  Hint: This argument cannot be local,
  because it is an argument in a tail call.
|}]


let cross_contention (x : int w @ contended) = use_uncontended x
[%%expect{|
val cross_contention : int w @ contended -> unit = <fun>
|}]

let don't_cross_portability (x : int w @ nonportable) = use_portable x
[%%expect{|
Line 1, characters 69-70:
1 | let don't_cross_portability (x : int w @ nonportable) = use_portable x
                                                                         ^
Error: This value is "nonportable" but expected to be "portable".
|}]

(* Quality *)

type 'a record : immutable_data with [ `A of 'a ] = { inner : 'a }
[%%expect{|
type 'a record = { inner : 'a; }
|}]

(* Harder cases: row variables *)

(* CR layouts v2.8: These are both correct, but we could probably infer a more precise kind for both. *)
type ('a, 'b) t : immediate = [< `X | `Y of 'a] as 'b
[%%expect{|
Line 1, characters 0-53:
1 | type ('a, 'b) t : immediate = [< `X | `Y of 'a] as 'b
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "[< `X | `Y of 'a ]" is value
         because it's a polymorphic variant type.
       But the kind of type "[< `X | `Y of 'a ]" must be a subkind of immediate
         because of the definition of t at line 1, characters 0-53.
|}]
type ('a, 'b) u : immediate = [> `X | `Y of 'a] as 'b
[%%expect{|
Line 1, characters 0-53:
1 | type ('a, 'b) u : immediate = [> `X | `Y of 'a] as 'b
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "[> `X | `Y of 'a ]" is value
         because it's a polymorphic variant type.
       But the kind of type "[> `X | `Y of 'a ]" must be a subkind of immediate
         because of the definition of u at line 1, characters 0-53.
|}]

(* less-than rows *)

let f (x : [< `A of int | `B of string] @ contended) =
  use_uncontended x
(* CR layouts v2.8: This should probably be accepted *)
[%%expect{|
Line 2, characters 18-19:
2 |   use_uncontended x
                      ^
Error: This value is "contended" but expected to be "uncontended".
|}]

module M : sig
  type 'a t : immutable_data with 'a = private [< `A of 'a | `B of ('a * 'a) | `C ]
end = struct
  type 'a t = [ `C ]
end
[%%expect{|
Line 2, characters 2-83:
2 |   type 'a t : immutable_data with 'a = private [< `A of 'a | `B of ('a * 'a) | `C ]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "[< `A of 'a | `B of 'a * 'a | `C ]" is value
         because it's a polymorphic variant type.
       But the kind of type "[< `A of 'a | `B of 'a * 'a | `C ]" must be a subkind of
         immutable_data with 'a
         because of the definition of t at line 2, characters 2-83.
|}]

(* Tunivar-ified row variables *)

type t1 = { f : ('a : value mod portable). ([> `Foo of int] as 'a) -> unit }
(* CR layouts v2.8: This should be accepted *)
[%%expect{|
Line 1, characters 64-65:
1 | type t1 = { f : ('a : value mod portable). ([> `Foo of int] as 'a) -> unit }
                                                                    ^
Error: This alias is bound to type "[> `Foo of int ]"
       but is used as an instance of type "('a : value mod portable)"
       The kind of [> `Foo of int ] is value
         because it's a polymorphic variant type.
       But the kind of [> `Foo of int ] must be a subkind of
         value mod portable
         because of the annotation on the universal variable 'a.
|}]

type t2 = { f : ('a : value mod portable). ([< `Foo of int] as 'a) -> unit }
(* CR layouts v2.8: This should be accepted *)
[%%expect{|
Line 1, characters 64-65:
1 | type t2 = { f : ('a : value mod portable). ([< `Foo of int] as 'a) -> unit }
                                                                    ^
Error: This alias is bound to type "[< `Foo of int ]"
       but is used as an instance of type "('a : value mod portable)"
       The kind of [< `Foo of int ] is value
         because it's a polymorphic variant type.
       But the kind of [< `Foo of int ] must be a subkind of
         value mod portable
         because of the annotation on the universal variable 'a.
|}]
