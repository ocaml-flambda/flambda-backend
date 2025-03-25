(* TEST
   expect;
*)

let use_global : 'a @ global -> unit = fun _ -> ()
let use_unique : 'a @ unique -> unit = fun _ -> ()
let use_uncontended : 'a @ uncontended -> unit = fun _ -> ()
let use_portable : 'a @ portable -> unit = fun _ -> ()
let use_many : 'a @ many -> unit = fun _ -> ()

let cross_global : ('a : value mod global) -> unit = fun _ -> ()
let cross_aliased : ('a : value mod aliased) -> unit = fun _ -> ()
let cross_contended : ('a : value mod contended) -> unit = fun _ -> ()
let cross_portable : ('a : value mod portable) -> unit = fun _ -> ()
let cross_many : ('a : value mod many) -> unit = fun _ -> ()
let cross_nonnull : ('a : value mod non_null) -> unit = fun _ -> ()
let cross_external : ('a : value mod external_) -> unit = fun _ -> ()

type ('a : value mod global) require_global
type ('a : value mod aliased) require_aliased
type ('a : value mod contended) require_contended
type ('a : value mod portable) require_portable
type ('a : value mod many) require_many
type ('a : value mod non_null) require_nonnull
type ('a : value mod external_) require_external
[%%expect{|
val use_global : 'a -> unit = <fun>
val use_unique : 'a @ unique -> unit = <fun>
val use_uncontended : 'a -> unit = <fun>
val use_portable : 'a @ portable -> unit = <fun>
val use_many : 'a -> unit = <fun>
val cross_global : ('a : value mod global). 'a -> unit = <fun>
val cross_aliased : ('a : value mod aliased). 'a -> unit = <fun>
val cross_contended : ('a : value mod contended). 'a -> unit = <fun>
val cross_portable : ('a : value mod portable). 'a -> unit = <fun>
val cross_many : ('a : value mod many). 'a -> unit = <fun>
val cross_nonnull : 'a -> unit = <fun>
val cross_external : ('a : value mod external_). 'a -> unit = <fun>
type ('a : value mod global) require_global
type ('a : value mod aliased) require_aliased
type ('a : value mod contended) require_contended
type ('a : value mod portable) require_portable
type ('a : value mod many) require_many
type 'a require_nonnull
type ('a : value mod external_) require_external
|}]

(**** Test 1: Annotations without "with" are accepted when appropriate ****)

(* immutable records *)
type t : immutable_data = { x : int }
type t : immutable_data = { x : int; y : string }
type 'a t : immutable_data = { x : int; y : string }
type ('a : immutable_data) t : immutable_data = { x : 'a }
type ('a : immutable_data) t : immutable_data = { x : 'a; y : 'a; z : int }
type ('a : immutable_data, 'b : immutable_data) t : immutable_data = { x : 'a; y : 'b }
type ('a : immutable_data, 'b) t : immutable_data = { x : 'a }
type t : immutable_data = { x : int option }
type ('a : immutable_data) t : immutable_data = { x : 'a option }
[%%expect{|
type t = { x : int; }
type t = { x : int; y : string; }
type 'a t = { x : int; y : string; }
type ('a : immutable_data) t = { x : 'a; }
type ('a : immutable_data) t = { x : 'a; y : 'a; z : int; }
type ('a : immutable_data, 'b : immutable_data) t = { x : 'a; y : 'b; }
type ('a : immutable_data, 'b) t = { x : 'a; }
type t = { x : int option; }
type ('a : immutable_data) t = { x : 'a option; }
|}]

(* mutable or immutable records annotated as mutable *)
type t : mutable_data = { mutable x : int }
type t : mutable_data = { x : int; y : int; mutable z : int }
type t : mutable_data = { mutable x : int ref }
type t : mutable_data = { x : int }
type ('a : mutable_data) t : mutable_data = { x : 'a }
type ('a : immutable_data) t : mutable_data = { x : 'a }
type t : mutable_data = { x : int ref; y : string }
type ('a : mutable_data) t : mutable_data = { x : 'a option }
[%%expect {|
type t = { mutable x : int; }
type t = { x : int; y : int; mutable z : int; }
type t = { mutable x : int ref; }
type t = { x : int; }
type ('a : mutable_data) t = { x : 'a; }
type ('a : immutable_data) t = { x : 'a; }
type t = { x : int ref; y : string; }
type ('a : mutable_data) t = { x : 'a option; }
|}]

(* annotations that aren't mutable_data or immutable_data *)
type t : value mod contended = { x : unit -> unit }
type 'a t : value mod contended = { x : 'a -> 'a }
type ('a : value mod contended portable, 'b : value mod portable) t : value mod portable
    = { x : 'a; y : 'b }
type ('a : value mod many) t : value mod many = { x : 'a }
type t : value mod contended portable = { x : int }
type ('a : value mod portable many) t : value mod many = { mutable x : 'a }
type 'a t : value mod non_null = { x : 'a }
[%%expect {|
type t = { x : unit -> unit; }
type 'a t = { x : 'a -> 'a; }
type ('a : value mod contended portable, 'b : value mod portable) t = {
  x : 'a;
  y : 'b;
}
type ('a : value mod many) t = { x : 'a; }
type t = { x : int; }
type ('a : value mod many portable) t = { mutable x : 'a; }
type 'a t = { x : 'a; }
|}]

(* bad annotations *)
type 'a t : immutable_data = { x : 'a }
[%%expect {|
Line 1, characters 0-39:
1 | type 'a t : immutable_data = { x : 'a }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data with 'a
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of immutable_data
         because of the annotation on the declaration of the type t.
|}]

type 'a t : immutable_data = { mutable x : 'a }
[%%expect {|
Line 1, characters 0-47:
1 | type 'a t : immutable_data = { mutable x : 'a }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is mutable_data with 'a @@ many unyielding
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of immutable_data
         because of the annotation on the declaration of the type t.

       The first mode-crosses less than the second along:
         contention: mod uncontended ≰ mod contended
         portability: mod portable with 'a ≰ mod portable
|}]

type t : immutable_data = { x : int ref }
[%%expect {|
Line 1, characters 0-41:
1 | type t : immutable_data = { x : int ref }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is mutable_data
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of immutable_data
         because of the annotation on the declaration of the type t.
|}]

type t : immutable_data = { x : unit -> unit }
[%%expect {|
Line 1, characters 0-46:
1 | type t : immutable_data = { x : unit -> unit }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is value mod contended
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of immutable_data
         because of the annotation on the declaration of the type t.
|}]

type 'a t : immutable_data = { x : 'a option }
[%%expect {|
Line 1, characters 0-46:
1 | type 'a t : immutable_data = { x : 'a option }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data with 'a
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of immutable_data
         because of the annotation on the declaration of the type t.
|}]

type t : immutable_data = { x : int; y : int; mutable z : int }
[%%expect {|
Line 1, characters 0-63:
1 | type t : immutable_data = { x : int; y : int; mutable z : int }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is mutable_data
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of immutable_data
         because of the annotation on the declaration of the type t.
|}]

type t : mutable_data = { x : unit -> unit }
[%%expect {|
Line 1, characters 0-44:
1 | type t : mutable_data = { x : unit -> unit }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is value mod contended
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of mutable_data
         because of the annotation on the declaration of the type t.
|}]

type ('a : value mod portable) t : value mod many = { x : 'a }
[%%expect {|
Line 1, characters 0-62:
1 | type ('a : value mod portable) t : value mod many = { x : 'a }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data with 'a
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of value mod many
         because of the annotation on the declaration of the type t.
|}]

type ('a : value mod global) t : value mod global = { x : 'a }
[%%expect {|
Line 1, characters 0-62:
1 | type ('a : value mod global) t : value mod global = { x : 'a }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data with 'a
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of value mod global
         because of the annotation on the declaration of the type t.
|}]

type ('a : value mod aliased) t : value mod aliased = { x : 'a }
[%%expect {|
Line 1, characters 0-64:
1 | type ('a : value mod aliased) t : value mod aliased = { x : 'a }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data with 'a
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of value mod aliased
         because of the annotation on the declaration of the type t.
|}]

type ('a : value mod external_) t : value mod external_ = { x : 'a }
[%%expect {|
Line 1, characters 0-68:
1 | type ('a : value mod external_) t : value mod external_ = { x : 'a }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data with 'a
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of value mod external_
         because of the annotation on the declaration of the type t.
|}]

(**** Test 2: Annotations with "with" are accepted when appropriate ****)
type 'a t : immutable_data with 'a = { x : int }
type 'a t : immutable_data with 'a = { x : 'a }
type 'a t : mutable_data with 'a = { mutable x : 'a }
type 'a t : mutable_data with 'a = { x : 'a ref }
type ('a, 'b) t : immutable_data with 'a with 'b = { x : 'a; y : 'b; z : 'a }
type ('a, 'b) t : mutable_data with 'a with 'b = { x : 'a; y : 'b; mutable z : 'a }
type 'a t : value mod contended with 'a = { x : unit -> unit; y : 'a }
type 'a t : immutable_data with 'a = { x : int }
type 'a t : value mod contended with 'a = { x : int }
type 'a t : immutable_data with 'a = { x : 'a option }
type 'a t : immutable_data with 'a -> 'a = { x : 'a -> 'a }
[%%expect {|
type 'a t = { x : int; }
type 'a t = { x : 'a; }
type 'a t = { mutable x : 'a; }
type 'a t = { x : 'a ref; }
type ('a, 'b) t = { x : 'a; y : 'b; z : 'a; }
type ('a, 'b) t = { x : 'a; y : 'b; mutable z : 'a; }
type 'a t = { x : unit -> unit; y : 'a; }
type 'a t = { x : int; }
type 'a t = { x : int; }
type 'a t = { x : 'a option; }
type 'a t = { x : 'a -> 'a; }
|}]

type 'a t : immutable_data with 'a = { mutable x : 'a }
[%%expect {|
Line 1, characters 0-55:
1 | type 'a t : immutable_data with 'a = { mutable x : 'a }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is mutable_data with 'a @@ many unyielding
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of immutable_data with 'a
         because of the annotation on the declaration of the type t.

       The first mode-crosses less than the second along:
         contention: mod uncontended ≰ mod contended with 'a
|}]

type 'a t : immutable_data with 'a = { x : 'a -> 'a }
[%%expect {|
Line 1, characters 0-53:
1 | type 'a t : immutable_data with 'a = { x : 'a -> 'a }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is value mod contended
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of immutable_data with 'a
         because of the annotation on the declaration of the type t.
|}]

type 'a t : value mod global with 'a = { x : 'a }
[%%expect {|
Line 1, characters 0-49:
1 | type 'a t : value mod global with 'a = { x : 'a }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data with 'a
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of value mod global with 'a
         because of the annotation on the declaration of the type t.
|}]

type 'a t : value mod aliased with 'a = { x : 'a }
[%%expect {|
Line 1, characters 0-50:
1 | type 'a t : value mod aliased with 'a = { x : 'a }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data with 'a
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of value mod aliased with 'a
         because of the annotation on the declaration of the type t.
|}]

type 'a t : value mod external_ with 'a = { x : 'a }
[%%expect {|
Line 1, characters 0-52:
1 | type 'a t : value mod external_ with 'a = { x : 'a }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data with 'a
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of value mod external_
         with 'a
         because of the annotation on the declaration of the type t.
|}]

(**** Test 3: Record values cross when appropriate ****)

type t = { x : int }
let foo (t : t @@ nonportable contended once) =
  use_portable t;
  use_uncontended t;
  use_many t
[%%expect {|
type t = { x : int; }
val foo : t @ once contended -> unit = <fun>
|}]

let foo (t : t @@ local) = use_global t [@nontail]
[%%expect {|
Line 1, characters 38-39:
1 | let foo (t : t @@ local) = use_global t [@nontail]
                                          ^
Error: This value escapes its region.
|}]

let foo (t : t @@ aliased) = use_unique t
[%%expect {|
Line 1, characters 40-41:
1 | let foo (t : t @@ aliased) = use_unique t
                                            ^
Error: This value is "aliased" but expected to be "unique".
|}]

type t = { mutable x : int }
let foo (t : t @@ nonportable once) =
  use_portable t;
  use_many t
[%%expect {|
type t = { mutable x : int; }
val foo : t @ once -> unit = <fun>
|}]

let foo (t : t @@ local) = use_global t [@nontail]
[%%expect {|
Line 1, characters 38-39:
1 | let foo (t : t @@ local) = use_global t [@nontail]
                                          ^
Error: This value escapes its region.
|}]

let foo (t : t @@ aliased) = use_unique t
[%%expect {|
Line 1, characters 40-41:
1 | let foo (t : t @@ aliased) = use_unique t
                                            ^
Error: This value is "aliased" but expected to be "unique".
|}]

let foo (t : t @@ contended) = use_uncontended t
[%%expect {|
Line 1, characters 47-48:
1 | let foo (t : t @@ contended) = use_uncontended t
                                                   ^
Error: This value is "contended" but expected to be "uncontended".
|}]

type 'a t = { x : 'a }
let foo (t : int t @@ nonportable contended once) =
  use_portable t;
  use_uncontended t;
  use_many t
[%%expect {|
type 'a t = { x : 'a; }
val foo : int t @ once contended -> unit = <fun>
|}]

let foo (t : int t @@ local) = use_global t [@nontail]
[%%expect {|
Line 1, characters 42-43:
1 | let foo (t : int t @@ local) = use_global t [@nontail]
                                              ^
Error: This value escapes its region.
|}]

let foo (t : int t @@ aliased) = use_unique t
[%%expect {|
Line 1, characters 44-45:
1 | let foo (t : int t @@ aliased) = use_unique t
                                                ^
Error: This value is "aliased" but expected to be "unique".
|}]

type 'a t = { x : 'a }

let foo (t : _ t @@ nonportable) = use_portable t
[%%expect {|
type 'a t = { x : 'a; }
Line 3, characters 48-49:
3 | let foo (t : _ t @@ nonportable) = use_portable t
                                                    ^
Error: This value is "nonportable" but expected to be "portable".
|}]

let foo (t : _ t @@ contended) = use_uncontended t
[%%expect {|
Line 1, characters 49-50:
1 | let foo (t : _ t @@ contended) = use_uncontended t
                                                     ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (t : _ t @@ once) = use_many t
[%%expect {|
Line 1, characters 37-38:
1 | let foo (t : _ t @@ once) = use_many t
                                         ^
Error: This value is "once" but expected to be "many".
|}]

let foo (t : _ t @@ local) = use_global t [@nontail]
[%%expect {|
Line 1, characters 40-41:
1 | let foo (t : _ t @@ local) = use_global t [@nontail]
                                            ^
Error: This value escapes its region.
|}]

let foo (t : _ t @@ aliased) = use_unique t
[%%expect {|
Line 1, characters 42-43:
1 | let foo (t : _ t @@ aliased) = use_unique t
                                              ^
Error: This value is "aliased" but expected to be "unique".
|}]

type 'a t = { x : 'a }
let foo (t : ('a : immutable_data) t @@ nonportable contended once) =
  use_portable t;
  use_uncontended t;
  use_many t
(* CR layouts v2.8: fix this in the principal case *)
[%%expect {|
type 'a t = { x : 'a; }
val foo : ('a : immutable_data). 'a t @ once contended -> unit = <fun>
|}, Principal{|
type 'a t = { x : 'a; }
Line 3, characters 15-16:
3 |   use_portable t;
                   ^
Error: This value is "once" but expected to be "many".
|}]

let foo (t : ('a : immutable_data) t @@ local) = use_global t [@nontail]
[%%expect {|
Line 1, characters 60-61:
1 | let foo (t : ('a : immutable_data) t @@ local) = use_global t [@nontail]
                                                                ^
Error: This value escapes its region.
|}]

let foo (t : ('a : immutable_data) t @@ aliased) = use_unique t
[%%expect {|
Line 1, characters 62-63:
1 | let foo (t : ('a : immutable_data) t @@ aliased) = use_unique t
                                                                  ^
Error: This value is "aliased" but expected to be "unique".
|}]

type ('a : immutable_data) t = { x : 'a }
let foo (t : _ t @@ nonportable contended once) =
  use_portable t;
  use_uncontended t;
  use_many t
(* CR layouts v2.8: fix this in the principal case *)
[%%expect {|
type ('a : immutable_data) t = { x : 'a; }
val foo : ('a : immutable_data). 'a t @ once contended -> unit = <fun>
|}, Principal{|
type ('a : immutable_data) t = { x : 'a; }
Line 3, characters 15-16:
3 |   use_portable t;
                   ^
Error: This value is "once" but expected to be "many".
|}]

let foo (t : _ t @@ local) = use_global t [@nontail]
[%%expect {|
Line 1, characters 40-41:
1 | let foo (t : _ t @@ local) = use_global t [@nontail]
                                            ^
Error: This value escapes its region.
|}]

let foo (t : _ t @@ aliased) = use_unique t
[%%expect {|
Line 1, characters 42-43:
1 | let foo (t : _ t @@ aliased) = use_unique t
                                              ^
Error: This value is "aliased" but expected to be "unique".
|}]

type 'a t = { x : 'a }
let foo (t : (unit -> unit) t @@ contended) = use_uncontended t
[%%expect {|
type 'a t = { x : 'a; }
val foo : (unit -> unit) t @ contended -> unit = <fun>
|}]

let foo (t : (unit -> unit) t @@ nonportable) = use_portable t
[%%expect {|
Line 1, characters 61-62:
1 | let foo (t : (unit -> unit) t @@ nonportable) = use_portable t
                                                                 ^
Error: This value is "nonportable" but expected to be "portable".
|}]

(**** Test 4: Record types satisfy type constraints when appropriate ****)
type t = { x : int; y : string }
let t = { x = 10; y = "hello" }
let () =
  cross_many t;
  cross_portable t;
  cross_contended t

[%%expect{|
type t = { x : int; y : string; }
val t : t = {x = 10; y = "hello"}
|}]

let () = cross_global t

[%%expect {|
Line 1, characters 22-23:
1 | let () = cross_global t
                          ^
Error: This expression has type "t" but an expression was expected of type
         "('a : value mod global)"
       The kind of t is immutable_data
         because of the definition of t at line 1, characters 0-32.
       But the kind of t must be a subkind of value mod global
         because of the definition of cross_global at line 7, characters 53-64.
|}]

type 'a t = { x : 'a }
let int = { x = 10 }
let func = { x = fun () -> () }
[%%expect {|
type 'a t = { x : 'a; }
val int : int t = {x = 10}
val func : (unit -> unit) t = {x = <fun>}
|}]

let () =
  cross_many int;
  cross_portable int;
  cross_contended int;
  cross_nonnull int

let () =
  cross_contended func;
  cross_nonnull func
(* CR layouts v2.8: fix in principal case *)
[%%expect {|
|}, Principal{|
Line 2, characters 13-16:
2 |   cross_many int;
                 ^^^
Error: This expression has type "int t" but an expression was expected of type
         "('a : value mod many)"
       The kind of int t is immutable_data with int
         because of the definition of t at line 1, characters 0-22.
       But the kind of int t must be a subkind of value mod many
         because of the definition of cross_many at line 11, characters 49-60.
|}]

let () = cross_aliased int
[%%expect {|
Line 1, characters 23-26:
1 | let () = cross_aliased int
                           ^^^
Error: This expression has type "int t" but an expression was expected of type
         "('a : value mod aliased)"
       The kind of int t is immutable_data
         because of the definition of t at line 1, characters 0-22.
       But the kind of int t must be a subkind of value mod aliased
         because of the definition of cross_aliased at line 8, characters 55-66.
|}, Principal{|
Line 1, characters 23-26:
1 | let () = cross_aliased int
                           ^^^
Error: This expression has type "int t" but an expression was expected of type
         "('a : value mod aliased)"
       The kind of int t is immutable_data with int
         because of the definition of t at line 1, characters 0-22.
       But the kind of int t must be a subkind of value mod aliased
         because of the definition of cross_aliased at line 8, characters 55-66.
|}]

let () = cross_portable func
[%%expect {|
Line 1, characters 24-28:
1 | let () = cross_portable func
                            ^^^^
Error: This expression has type "(unit -> unit) t"
       but an expression was expected of type "('a : value mod portable)"
       The kind of (unit -> unit) t is value mod contended
         because of the definition of t at line 1, characters 0-22.
       But the kind of (unit -> unit) t must be a subkind of
         value mod portable
         because of the definition of cross_portable at line 10, characters 57-68.
|}, Principal{|
Line 1, characters 24-28:
1 | let () = cross_portable func
                            ^^^^
Error: This expression has type "(unit -> unit) t"
       but an expression was expected of type "('a : value mod portable)"
       The kind of (unit -> unit) t is immutable_data with unit -> unit
         because of the definition of t at line 1, characters 0-22.
       But the kind of (unit -> unit) t must be a subkind of
         value mod portable
         because of the definition of cross_portable at line 10, characters 57-68.
|}]

let () = cross_external func
[%%expect {|
Line 1, characters 24-28:
1 | let () = cross_external func
                            ^^^^
Error: This expression has type "(unit -> unit) t"
       but an expression was expected of type "('a : value mod external_)"
       The kind of (unit -> unit) t is value mod contended
         because of the definition of t at line 1, characters 0-22.
       But the kind of (unit -> unit) t must be a subkind of
         value mod external_
         because of the definition of cross_external at line 13, characters 58-69.
|}, Principal{|
Line 1, characters 24-28:
1 | let () = cross_external func
                            ^^^^
Error: This expression has type "(unit -> unit) t"
       but an expression was expected of type "('a : value mod external_)"
       The kind of (unit -> unit) t is immutable_data with unit -> unit
         because of the definition of t at line 1, characters 0-22.
       But the kind of (unit -> unit) t must be a subkind of
         value mod external_
         because of the definition of cross_external at line 13, characters 58-69.
|}]

type 'a t = { x : 'a }
type t_test = int t require_many
type t_test = int t require_portable
type t_test = int t require_contended
type t_test = (unit -> unit) t require_contended
type ('a : value mod contended) t_test = 'a t require_contended
type 'a t_test = 'a t require_nonnull
(* CR layouts v2.8: fix principal case *)
[%%expect {|
type 'a t = { x : 'a; }
type t_test = int t require_many
type t_test = int t require_portable
type t_test = int t require_contended
type t_test = (unit -> unit) t require_contended
type ('a : value mod contended) t_test = 'a t require_contended
type 'a t_test = 'a t require_nonnull
|}, Principal{|
type 'a t = { x : 'a; }
Line 2, characters 14-19:
2 | type t_test = int t require_many
                  ^^^^^
Error: This type "int t" should be an instance of type "('a : value mod many)"
       The kind of int t is immutable_data with int
         because of the definition of t at line 1, characters 0-22.
       But the kind of int t must be a subkind of value mod many
         because of the definition of require_many at line 19, characters 0-39.
|}]

type t_test = int t require_global
[%%expect {|
Line 1, characters 14-19:
1 | type t_test = int t require_global
                  ^^^^^
Error: This type "int t" should be an instance of type "('a : value mod global)"
       The kind of int t is immutable_data
         because of the definition of t at line 1, characters 0-22.
       But the kind of int t must be a subkind of value mod global
         because of the definition of require_global at line 15, characters 0-43.
|}, Principal{|
Line 1, characters 14-19:
1 | type t_test = int t require_global
                  ^^^^^
Error: This type "int t" should be an instance of type "('a : value mod global)"
       The kind of int t is immutable_data with int
         because of the definition of t at line 1, characters 0-22.
       But the kind of int t must be a subkind of value mod global
         because of the definition of require_global at line 15, characters 0-43.
|}]

type t_test = int t require_aliased
[%%expect {|
Line 1, characters 14-19:
1 | type t_test = int t require_aliased
                  ^^^^^
Error: This type "int t" should be an instance of type "('a : value mod aliased)"
       The kind of int t is immutable_data
         because of the definition of t at line 1, characters 0-22.
       But the kind of int t must be a subkind of value mod aliased
         because of the definition of require_aliased at line 16, characters 0-45.
|}, Principal{|
Line 1, characters 14-19:
1 | type t_test = int t require_aliased
                  ^^^^^
Error: This type "int t" should be an instance of type "('a : value mod aliased)"
       The kind of int t is immutable_data with int
         because of the definition of t at line 1, characters 0-22.
       But the kind of int t must be a subkind of value mod aliased
         because of the definition of require_aliased at line 16, characters 0-45.
|}]

type t_test = (unit -> unit) t require_portable
[%%expect {|
Line 1, characters 14-30:
1 | type t_test = (unit -> unit) t require_portable
                  ^^^^^^^^^^^^^^^^
Error: This type "(unit -> unit) t" should be an instance of type
         "('a : value mod portable)"
       The kind of (unit -> unit) t is value mod contended
         because of the definition of t at line 1, characters 0-22.
       But the kind of (unit -> unit) t must be a subkind of
         value mod portable
         because of the definition of require_portable at line 18, characters 0-47.
|}, Principal{|
Line 1, characters 14-30:
1 | type t_test = (unit -> unit) t require_portable
                  ^^^^^^^^^^^^^^^^
Error: This type "(unit -> unit) t" should be an instance of type
         "('a : value mod portable)"
       The kind of (unit -> unit) t is immutable_data with unit -> unit
         because of the definition of t at line 1, characters 0-22.
       But the kind of (unit -> unit) t must be a subkind of
         value mod portable
         because of the definition of require_portable at line 18, characters 0-47.
|}]

type ('a : value mod contended) t_test = 'a t require_portable
[%%expect {|
Line 1, characters 41-45:
1 | type ('a : value mod contended) t_test = 'a t require_portable
                                             ^^^^
Error: This type "'a t" should be an instance of type "('b : value mod portable)"
       The kind of 'a t is immutable_data with 'a
         because of the definition of t at line 1, characters 0-22.
       But the kind of 'a t must be a subkind of value mod portable
         because of the definition of require_portable at line 18, characters 0-47.
|}]

type ('a : value mod external_) t_test = 'a t require_external
[%%expect {|
Line 1, characters 41-45:
1 | type ('a : value mod external_) t_test = 'a t require_external
                                             ^^^^
Error: This type "'a t" should be an instance of type
         "('b : value mod external_)"
       The kind of 'a t is immutable_data with 'a
         because of the definition of t at line 1, characters 0-22.
       But the kind of 'a t must be a subkind of value mod external_
         because of the definition of require_external at line 21, characters 0-48.
|}]

(**** Test 5: Module inclusion check ****)

module M : sig
  type t : immutable_data
end = struct
  type t = { x : int }
end
[%%expect {|
module M : sig type t : immutable_data end
|}]

module M : sig
  type t : mutable_data
end = struct
  type t = { mutable x : int; y : int }
end
[%%expect {|
module M : sig type t : mutable_data end
|}]

module M : sig
  type t : immutable_data
end = struct
  type t : mutable_data = { x : int }
end
[%%expect {|
module M : sig type t : immutable_data end
|}]

module M : sig
  type ('a : immutable_data) t : immutable_data
end = struct
  type ('a : immutable_data) t = { x : 'a }
end
[%%expect {|
module M : sig type ('a : immutable_data) t : immutable_data end
|}]

module M : sig
  type 'a t : immutable_data with 'a
end = struct
  type 'a t = { x : 'a }
end
[%%expect {|
module M : sig type 'a t : immutable_data with 'a end
|}]

module M : sig
  type 'a t : immutable_data with 'a
end = struct
  type 'a t = { x : 'a; y : int }
end
[%%expect {|
module M : sig type 'a t : immutable_data with 'a end
|}]

module M : sig
  type 'a t : immutable_data with int with 'a
end = struct
  type 'a t = { x : 'a; y : int }
end
[%%expect {|
module M : sig type 'a t : immutable_data with 'a end
|}]

module M : sig
  type t : mutable_data
end = struct
  type t = { x : int; y : string }
end

[%%expect{|
module M : sig type t : mutable_data end
|}]

module M : sig
  type t : immutable_data
end = struct
  type t = { mutable x : int; y : string }
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = { mutable x : int; y : string }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = { mutable x : int; y : string; } end
       is not included in
         sig type t : immutable_data end
       Type declarations do not match:
         type t = { mutable x : int; y : string; }
       is not included in
         type t : immutable_data
       The kind of the first is mutable_data
         because of the definition of t at line 4, characters 2-42.
       But the kind of the first must be a subkind of immutable_data
         because of the definition of t at line 2, characters 2-25.
|}]

module M : sig
  type t : immutable_data
end = struct
  type t = { x : int ref; y : string }
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = { x : int ref; y : string }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = { x : int ref; y : string; } end
       is not included in
         sig type t : immutable_data end
       Type declarations do not match:
         type t = { x : int ref; y : string; }
       is not included in
         type t : immutable_data
       The kind of the first is mutable_data
         because of the definition of t at line 4, characters 2-38.
       But the kind of the first must be a subkind of immutable_data
         because of the definition of t at line 2, characters 2-25.
|}]

module M : sig
  type 'a t : immutable_data
end = struct
  type 'a t = { x : 'a; y : string }
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type 'a t = { x : 'a; y : string }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = { x : 'a; y : string; } end
       is not included in
         sig type 'a t : immutable_data end
       Type declarations do not match:
         type 'a t = { x : 'a; y : string; }
       is not included in
         type 'a t : immutable_data
       The kind of the first is immutable_data with 'a
         because of the definition of t at line 4, characters 2-36.
       But the kind of the first must be a subkind of immutable_data
         because of the definition of t at line 2, characters 2-28.
|}]

module M : sig
  type 'a t : immutable_data
end = struct
  type 'a t = { x : int; y : string }
end

[%%expect{|
module M : sig type 'a t : immutable_data end
|}]

(****************************************)

type ('a : immutable_data) t : immutable_data = { x : 'a list }
[%%expect {|
type ('a : immutable_data) t = { x : 'a list; }
|}]

type ('a : immutable_data) t : immutable_data = { x : 'a option }
type ('a : immutable_data) t : immutable_data = { x : 'a }
type ('a : immutable_data) t : immutable_data = 'a list
[%%expect {|
type ('a : immutable_data) t = { x : 'a option; }
type ('a : immutable_data) t = { x : 'a; }
type ('a : immutable_data) t = 'a list
|}]
