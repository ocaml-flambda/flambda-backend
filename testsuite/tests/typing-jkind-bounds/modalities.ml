(* TEST
    flags = "-extension layouts_alpha";
    expect;
*)
let use_uncontended : 'a @ uncontended -> unit = fun _ -> ()
let use_portable : 'a @ portable -> unit = fun _ -> ()
let cross_contended : ('a : value mod contended) -> unit = fun _ -> ()
type ('a : value mod contended) require_contended
[%%expect{|
val use_uncontended : 'a -> unit = <fun>
val use_portable : 'a @ portable -> unit = <fun>
val cross_contended : ('a : value mod contended). 'a -> unit = <fun>
type ('a : value mod contended) require_contended
|}]

type 'a t = { contended : 'a @@ contended }
[%%expect{|
type 'a t = { contended : 'a @@ contended; }
|}]

type t_test = int t require_contended
type t_test = int ref t require_contended
[%%expect{|
type t_test = int t require_contended
type t_test = int ref t require_contended
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

let foo (t : int ref t @@ contended) = cross_contended t
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

(* Types with modalities cross even if the inner type doesn't, at deep levels of nesting *)

type 'a portable : value mod portable = { portable : 'a @@ portable }
[%%expect{|
type 'a portable = { portable : 'a @@ portable; }
|}]

let foo (x : (int -> int) ref portable @@ nonportable) = use_portable x
[%%expect{|
val foo : (int -> int) ref portable -> unit = <fun>
|}]

(* Product layouts *)

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
         immediate with string @@ global & immediate with string @@ global
         because of the definition of t at line 4, characters 0-51.
       But the kind of string t must be a subkind of
         value mod global & value mod global
         because of the definition of cross_global at line 2, characters 4-16.

       The first mode-crosses less than the second along:
         nullability: mod non_null with string ≰ mod non_null
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
         immediate with string -> string @@ global & immediate
         with string -> string @@ global
         because of the definition of t at line 4, characters 0-51.
       But the kind of (string -> string) t must be a subkind of
         value & value
         because of the definition of use_portable at line 3, characters 4-16.

       The first mode-crosses less than the second along:
         nullability: mod non_null with string -> string ≰ mod non_null
|}]
