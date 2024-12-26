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
type 'a t = { x : 'a @@ many; }
type 'a t = { x : 'a @@ contended; }
type 'a t = { x : 'a @@ portable; }
|}]

let use_global : ('a : value & value). 'a @ global -> unit = fun _ -> ()
let cross_global : ('a : value & value mod global). 'a -> unit = fun _ -> ()
let use_portable : ('a : value & value). 'a @ portable -> unit = fun _ -> ()
type 'a t = #{ x : 'a @@ global; y : 'a @@ global }

[%%expect{|
val use_global : ('a : value & value). 'a -> unit = <fun>
val cross_global : ('a : value mod global & value mod global). 'a -> unit =
  <fun>
val use_portable : ('a : value & value). 'a @ portable -> unit = <fun>
type 'a t = #{ global_ x : 'a; global_ y : 'a; }
|}]

let foo (t : string t @@ local) = use_global t

[%%expect{|
val foo : local_ string t -> unit = <fun>
|}]

let foo (t : string t @@ local) = cross_global t

[%%expect{|
val foo : local_ string t -> unit = <fun>
|}, Principal{|
Line 1, characters 47-48:
1 | let foo (t : string t @@ local) = cross_global t
                                                   ^
Error: This expression has type "string t"
       but an expression was expected of type
         "('a : value mod global & value mod global)"
       The kind of string t is
         value_or_null mod global unique with string
string
                           many with string
string
                           uncontended with string
string
                           portable with string
string
                           external_ with string
string
                           non_null with string
string
         & value_or_null mod global unique with string
string
                             many with string
string
                             uncontended with string
string
                             portable with string
string
                             external_ with string
string
                             non_null with string
string
         because of the definition of t at line 4, characters 0-51.
       But the kind of string t must be a subkind of
         value mod global & value mod global
         because of the definition of cross_global at line 2, characters 4-16.
|}]

let foo (t : string t @@ nonportable) = use_portable t

[%%expect{|
val foo : string t -> unit = <fun>
|}]

let foo (t : (string -> string) t @@ nonportable) = use_portable t

[%%expect{|
Line 1, characters 65-66:
1 | let foo (t : (string -> string) t @@ nonportable) = use_portable t
                                                                     ^
Error: This value is "nonportable" but expected to be "portable".
|}, Principal{|
Line 1, characters 65-66:
1 | let foo (t : (string -> string) t @@ nonportable) = use_portable t
                                                                     ^
Error: This expression has type "(string -> string) t"
       but an expression was expected of type "('a : value & value)"
       The kind of (string -> string) t is
         value_or_null mod global
                           unique with string -> string
string -> string
                           many with string -> string
string -> string
                           uncontended with string -> string
string -> string
                           portable with string -> string
string -> string
                           external_ with string -> string
string -> string
                           non_null with string -> string
string -> string
         & value_or_null mod global
                             unique with string -> string
string -> string
                             many with string -> string
string -> string
                             uncontended with string -> string
string -> string
                             portable with string -> string
string -> string
                             external_ with string -> string
string -> string
                             non_null with string -> string
string -> string
         because of the definition of t at line 4, characters 0-51.
       But the kind of (string -> string) t must be a subkind of
         value & value
         because of the definition of use_portable at line 3, characters 4-16.
|}]
