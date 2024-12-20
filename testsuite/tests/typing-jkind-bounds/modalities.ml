(* TEST
    flags = "-extension layouts_alpha";
    expect;
*)
let use_uncontended : 'a @ uncontended -> unit = fun _ -> ()
let use_portable : 'a @ portable -> unit = fun _ -> ()
let cross_uncontended : ('a : value mod uncontended) -> unit = fun _ -> ()
type ('a : value mod uncontended) require_uncontended
[%%expect{|
val use_uncontended : ('a : value_or_null). 'a -> unit = <fun>
val use_portable : ('a : value_or_null). 'a @ portable -> unit = <fun>
val cross_uncontended : ('a : value mod uncontended). 'a -> unit = <fun>
type ('a : value mod uncontended) require_uncontended
|}]

type 'a t = { contended : 'a @@ contended }
[%%expect{|
type 'a t = { contended : 'a @@ contended; }
|}]

type t_test = int t require_uncontended
type t_test = int ref t require_uncontended
[%%expect{|
type t_test = int t require_uncontended
type t_test = int ref t require_uncontended
|}]

let foo (t : int ref t @@ contended) = use_uncontended t
[%%expect{|
val foo : int ref t @ contended -> unit = <fun>
|}]

let foo (t : int ref t @@ contended) = use_uncontended t.contended
[%%expect{|
Line 1, characters 55-66:
1 | let foo (t : int ref t @@ contended) = use_uncontended t.contended
                                                           ^^^^^^^^^^^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (t : int ref t @@ contended) = cross_uncontended t
[%%expect{|
val foo : int ref t @ contended -> unit = <fun>
|}]

let foo (t : int t @@ nonportable) = use_portable t
[%%expect{|
val foo : int t -> unit = <fun>
|}]

let foo (t : _ t @@ nonportable) = use_portable t
[%%expect{|
Line 1, characters 48-49:
1 | let foo (t : _ t @@ nonportable) = use_portable t
                                                    ^
Error: This value is "nonportable" but expected to be "portable".
|}]

type 'a t : immutable_data with 'a @@ many = { x : 'a @@ many }
type 'a t : immutable_data with 'a @@ contended = { x : 'a @@ contended }
type 'a t : immutable_data with 'a @@ portable = { x : 'a @@ portable }
[%%expect{|
Line 1, characters 0-63:
1 | type 'a t : immutable_data with 'a @@ many = { x : 'a @@ many }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is
         value mod many uncontended with 'a portable with 'a
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of
         value mod many uncontended with 'a portable with 'a
         because of the annotation on the declaration of the type t.
|}]
(* CR layouts v2.8: this should be accepted *)
