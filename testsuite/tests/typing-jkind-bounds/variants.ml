(* TEST
    flags = "-infer-with-bounds";
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

(* immutable variants *)
type t : immutable_data = Foo
type t : immutable_data = Foo of int
type t : immutable_data = Foo | Bar of int | Baz of int * string * int
type 'a t : immutable_data = Foo of int | Bar of string
type ('a : immutable_data) t : immutable_data = Foo of 'a
type ('a : immutable_data) t : immutable_data = Foo of { x : 'a; y : 'a; z : int }
type ('a : immutable_data, 'b : immutable_data) t : immutable_data = Foo of { x : 'a; y : 'b }
type ('a : immutable_data, 'b) t : immutable_data = Foo of 'a
type t : immutable_data = Foo of int option
type ('a : immutable_data) t : immutable_data = Foo of 'a option
type t : immediate = Foo | Bar | Baz
[%%expect{|
type t = Foo
type t = Foo of int
type t = Foo | Bar of int | Baz of int * string * int
type 'a t = Foo of int | Bar of string
type ('a : immutable_data) t = Foo of 'a
type ('a : immutable_data) t = Foo of { x : 'a; y : 'a; z : int; }
type ('a : immutable_data, 'b : immutable_data) t =
    Foo of { x : 'a; y : 'b; }
type ('a : immutable_data, 'b) t = Foo of 'a
type t = Foo of int option
type ('a : immutable_data) t = Foo of 'a option
type t = Foo | Bar | Baz
|}]

(* mutable or immutable records annotated as mutable *)
type t : mutable_data = Foo of { mutable x : int }
type t : mutable_data = Foo of { x : int } | Bar of { y : int; mutable z : int }
type t : mutable_data = Foo of { mutable x : int ref }
type t : mutable_data = Foo of int
type ('a : mutable_data) t : mutable_data = Foo of 'a
type ('a : immutable_data) t : mutable_data = Foo of 'a | Bar
type t : mutable_data = Foo of int ref | Bar of string
type ('a : mutable_data) t : mutable_data = Foo of { x : 'a option }
[%%expect {|
type t = Foo of { mutable x : int; }
type t = Foo of { x : int; } | Bar of { y : int; mutable z : int; }
Line 3, characters 0-54:
3 | type t : mutable_data = Foo of { mutable x : int ref }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is value mod many unyielding
         because it's a boxed variant type.
       But the kind of type "t" must be a subkind of mutable_data
         because of the annotation on the declaration of the type t.
|}]

(* annotations that aren't mutable_data or immutable_data *)
type t : value mod contended = Foo of { x : unit -> unit }
type 'a t : value mod contended = Foo of ('a -> 'a) | Bar
type ('a : value mod contended portable, 'b : value mod portable) t : value mod portable
    = Foo of 'a | Bar of 'b
type ('a : value mod many) t : value mod many = Foo of { x : 'a }
type t : value mod contended portable = Foo of int
type ('a : value mod portable many) t : value mod many = Foo of { mutable x : 'a } | Bar
type 'a t : value mod non_null = Foo of 'a
[%%expect {|
type t = Foo of { x : unit -> unit; }
type 'a t = Foo of ('a -> 'a) | Bar
type ('a : value mod contended portable, 'b : value mod portable) t =
    Foo of 'a
  | Bar of 'b
type ('a : value mod many) t = Foo of { x : 'a; }
type t = Foo of int
type ('a : value mod many portable) t = Foo of { mutable x : 'a; } | Bar
type 'a t = Foo of 'a
|}]

(* bad annotations *)
type 'a t : immutable_data = Foo of 'a
[%%expect {|
Line 1, characters 0-38:
1 | type 'a t : immutable_data = Foo of 'a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data
         because it's a boxed variant type.
       But the kind of type "t" must be a subkind of immutable_data
         because of the annotation on the declaration of the type t.
|}]

type 'a t : immutable_data = Foo of { mutable x : 'a }
[%%expect {|
Line 1, characters 0-54:
1 | type 'a t : immutable_data = Foo of { mutable x : 'a }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is mutable_data
         because it's a boxed variant type.
       But the kind of type "t" must be a subkind of immutable_data
         because of the annotation on the declaration of the type t.
|}]

type t : immutable_data = Foo | Bar of int ref
[%%expect {|
Line 1, characters 0-46:
1 | type t : immutable_data = Foo | Bar of int ref
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is value
         because it's a boxed variant type.
       But the kind of type "t" must be a subkind of immutable_data
         because of the annotation on the declaration of the type t.
|}]

type t : immutable_data = Foo of (unit -> unit)
[%%expect {|
Line 1, characters 0-47:
1 | type t : immutable_data = Foo of (unit -> unit)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is value mod contended
         because it's a boxed variant type.
       But the kind of type "t" must be a subkind of immutable_data
         because of the annotation on the declaration of the type t.
|}]

type 'a t : immutable_data = Foo of 'a option
[%%expect {|
Line 1, characters 0-45:
1 | type 'a t : immutable_data = Foo of 'a option
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data
         because it's a boxed variant type.
       But the kind of type "t" must be a subkind of immutable_data
         because of the annotation on the declaration of the type t.
|}]

type t : immutable_data = Foo of int * int | Bar of { mutable z : int }
[%%expect {|
Line 1, characters 0-71:
1 | type t : immutable_data = Foo of int * int | Bar of { mutable z : int }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is mutable_data
         because it's a boxed variant type.
       But the kind of type "t" must be a subkind of immutable_data
         because of the annotation on the declaration of the type t.
|}]

type t : mutable_data = Foo of { x : unit -> unit }
[%%expect {|
Line 1, characters 0-51:
1 | type t : mutable_data = Foo of { x : unit -> unit }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is value mod contended
         because it's a boxed variant type.
       But the kind of type "t" must be a subkind of mutable_data
         because of the annotation on the declaration of the type t.
|}]

type ('a : value mod portable) t : value mod many = Foo of 'a
[%%expect {|
Line 1, characters 0-61:
1 | type ('a : value mod portable) t : value mod many = Foo of 'a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data
         because it's a boxed variant type.
       But the kind of type "t" must be a subkind of value mod many
         because of the annotation on the declaration of the type t.
|}]

type ('a : value mod global) t : value mod global = Foo of 'a
[%%expect {|
Line 1, characters 0-61:
1 | type ('a : value mod global) t : value mod global = Foo of 'a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data
         because it's a boxed variant type.
       But the kind of type "t" must be a subkind of value mod global
         because of the annotation on the declaration of the type t.
|}]

type ('a : value mod aliased) t : value mod aliased = Foo of 'a
[%%expect {|
Line 1, characters 0-63:
1 | type ('a : value mod aliased) t : value mod aliased = Foo of 'a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data
         because it's a boxed variant type.
       But the kind of type "t" must be a subkind of value mod aliased
         because of the annotation on the declaration of the type t.
|}]

type ('a : value mod external_) t : value mod external_ = Foo of 'a
[%%expect {|
Line 1, characters 0-67:
1 | type ('a : value mod external_) t : value mod external_ = Foo of 'a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data
         because it's a boxed variant type.
       But the kind of type "t" must be a subkind of value mod external_
         because of the annotation on the declaration of the type t.
|}]

(**** Test 2: Annotations with "with" are accepted when appropriate ****)
type 'a t : immutable_data with 'a = Foo
type 'a t : immutable_data with 'a = Foo of 'a
type 'a t : mutable_data with 'a = Bar of { mutable x : 'a }
type 'a t : mutable_data with 'a = Foo of 'a ref
type ('a, 'b) t : immutable_data with 'a with 'b = Foo of { x : 'a; y : 'b; z : 'a }
type ('a, 'b) t : mutable_data with 'a with 'b = Foo of { x : 'a; y : 'b; mutable z : 'a }
type 'a t : value mod contended with 'a = Foo of { x : unit -> unit; y : 'a }
type 'a t : immutable_data with 'a = Foo | Bar of { x : int }
type 'a t : value mod contended with 'a = Foo of int
type 'a t : immutable_data with 'a = Foo of 'a option
type 'a t : immutable_data with 'a -> 'a = Foo of { x : 'a -> 'a } | Bar of ('a -> 'a)
[%%expect {|
type 'a t = Foo
type 'a t = Foo of 'a
type 'a t = Bar of { mutable x : 'a; }
Line 4, characters 0-48:
4 | type 'a t : mutable_data with 'a = Foo of 'a ref
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is value
         because it's a boxed variant type.
       But the kind of type "t" must be a subkind of mutable_data
         because of the annotation on the declaration of the type t.
|}]

type 'a t : immutable_data with 'a = Foo of { mutable x : 'a }
[%%expect {|
Line 1, characters 0-62:
1 | type 'a t : immutable_data with 'a = Foo of { mutable x : 'a }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is mutable_data
         because it's a boxed variant type.
       But the kind of type "t" must be a subkind of immutable_data
         because of the annotation on the declaration of the type t.
|}]

type 'a t : immutable_data with 'a = Foo of { x : 'a -> 'a }
[%%expect {|
Line 1, characters 0-60:
1 | type 'a t : immutable_data with 'a = Foo of { x : 'a -> 'a }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is value mod contended
         because it's a boxed variant type.
       But the kind of type "t" must be a subkind of immutable_data
         because of the annotation on the declaration of the type t.
|}]

type 'a t : value mod global with 'a = Foo of 'a
[%%expect {|
Line 1, characters 0-48:
1 | type 'a t : value mod global with 'a = Foo of 'a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data
         because it's a boxed variant type.
       But the kind of type "t" must be a subkind of value mod global
         because of the annotation on the declaration of the type t.
|}]

type 'a t : value mod aliased with 'a = Foo of 'a
[%%expect {|
Line 1, characters 0-49:
1 | type 'a t : value mod aliased with 'a = Foo of 'a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data
         because it's a boxed variant type.
       But the kind of type "t" must be a subkind of value mod aliased
         because of the annotation on the declaration of the type t.
|}]

type 'a t : value mod external_ with 'a = Foo of 'a
[%%expect {|
Line 1, characters 0-51:
1 | type 'a t : value mod external_ with 'a = Foo of 'a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data
         because it's a boxed variant type.
       But the kind of type "t" must be a subkind of value mod external_
         because of the annotation on the declaration of the type t.
|}]

(**** Test 3: Variant values cross when appropriate ****)

type t = Foo of int
let foo (t : t @@ nonportable contended once) =
  use_portable t;
  use_uncontended t;
  use_many t
[%%expect {|
type t = Foo of int
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

type t = Foo of { mutable x : int }
let foo (t : t @@ nonportable once) =
  use_portable t;
  use_many t
[%%expect {|
type t = Foo of { mutable x : int; }
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

type 'a t = Foo of { x : 'a } | Bar
let foo (t : int t @@ nonportable contended once) =
  use_portable t;
  use_uncontended t;
  use_many t
[%%expect {|
type 'a t = Foo of { x : 'a; } | Bar
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

type 'a t = Foo of { x : 'a }
let foo (t : ('a : immutable_data) t @@ nonportable contended once) =
  use_portable t;
  use_uncontended t;
  use_many t
(* CR layouts v2.8: fix this in the principal case *)
[%%expect {|
type 'a t = Foo of { x : 'a; }
val foo : ('a : immutable_data). 'a t @ once contended -> unit = <fun>
|}, Principal{|
type 'a t = Foo of { x : 'a; }
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

type ('a : immutable_data) t = Foo of { x : 'a } | Bar of 'a
let foo (t : _ t @@ nonportable contended once) =
  use_portable t;
  use_uncontended t;
  use_many t
(* CR layouts v2.8: fix this in the principal case *)
[%%expect {|
type ('a : immutable_data) t = Foo of { x : 'a; } | Bar of 'a
val foo : ('a : immutable_data). 'a t @ once contended -> unit = <fun>
|}, Principal{|
type ('a : immutable_data) t = Foo of { x : 'a; } | Bar of 'a
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

type 'a t = Foo of { x : 'a }
let foo (t : (unit -> unit) t @@ contended) = use_uncontended t
[%%expect {|
type 'a t = Foo of { x : 'a; }
val foo : (unit -> unit) t @ contended -> unit = <fun>
|}]

let foo (t : (unit -> unit) t @@ nonportable) = use_portable t
[%%expect {|
Line 1, characters 61-62:
1 | let foo (t : (unit -> unit) t @@ nonportable) = use_portable t
                                                                 ^
Error: This value is "nonportable" but expected to be "portable".
|}]

(**** Test 4: Variant types satisfy type constraints when appropriate ****)
type t = Foo of int | Bar of string
let t = Foo 10
let () =
  cross_many t;
  cross_portable t;
  cross_contended t

[%%expect{|
type t = Foo of int | Bar of string
val t : t = Foo 10
|}]

let () = cross_global t
[%%expect {|
Line 1, characters 22-23:
1 | let () = cross_global t
                          ^
Error: This expression has type "t" but an expression was expected of type
         "('a : value mod global)"
       The kind of t is immutable_data
         because of the definition of t at line 1, characters 0-35.
       But the kind of t must be a subkind of value mod global
         because of the definition of cross_global at line 7, characters 53-64.
|}]

type 'a t = Foo of 'a
let int = Foo 10
let func = Foo (fun () -> ())
[%%expect {|
type 'a t = Foo of 'a
val int : int t = Foo 10
val func : (unit -> unit) t = Foo <fun>
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
       The kind of int t is immutable_data
         because of the definition of t at line 1, characters 0-21.
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
         because of the definition of t at line 1, characters 0-21.
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
       The kind of (unit -> unit) t is immutable_data
         because of the definition of t at line 1, characters 0-21.
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
       The kind of (unit -> unit) t is immutable_data
         because of the definition of t at line 1, characters 0-21.
       But the kind of (unit -> unit) t must be a subkind of
         value mod external_
         because of the definition of cross_external at line 13, characters 58-69.
|}]

type 'a t = Foo of 'a
type t_test = int t require_many
type t_test = int t require_portable
type t_test = int t require_contended
type t_test = (unit -> unit) t require_contended
type ('a : value mod contended) t_test = 'a t require_contended
type 'a t_test = 'a t require_nonnull
(* CR layouts v2.8: fix principal case *)
[%%expect {|
type 'a t = Foo of 'a
type t_test = int t require_many
type t_test = int t require_portable
type t_test = int t require_contended
type t_test = (unit -> unit) t require_contended
type ('a : value mod contended) t_test = 'a t require_contended
type 'a t_test = 'a t require_nonnull
|}, Principal{|
type 'a t = Foo of 'a
Line 2, characters 14-19:
2 | type t_test = int t require_many
                  ^^^^^
Error: This type "int t" should be an instance of type "('a : value mod many)"
       The kind of int t is immutable_data
         because of the definition of t at line 1, characters 0-21.
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
         because of the definition of t at line 1, characters 0-21.
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
         because of the definition of t at line 1, characters 0-21.
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
       The kind of (unit -> unit) t is immutable_data
         because of the definition of t at line 1, characters 0-21.
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
       The kind of 'a t is immutable_data
         because of the definition of t at line 1, characters 0-21.
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
       The kind of 'a t is immutable_data
         because of the definition of t at line 1, characters 0-21.
       But the kind of 'a t must be a subkind of value mod external_
         because of the definition of require_external at line 21, characters 0-48.
|}]

(**** Test 5: Module inclusion check ****)

module M : sig
  type t : immutable_data
end = struct
  type t = Foo of int
end
[%%expect {|
module M : sig type t : immutable_data end
|}]

module M : sig
  type t : mutable_data
end = struct
  type t = Foo of { mutable x : int; y : int }
end
[%%expect {|
module M : sig type t : mutable_data end
|}]

module M : sig
  type t : immutable_data
end = struct
  type t : mutable_data = Foo of { x : int }
end
[%%expect {|
module M : sig type t : immutable_data end
|}]

module M : sig
  type ('a : immutable_data) t : immutable_data
end = struct
  type ('a : immutable_data) t = Foo | Bar | Bax of { x : 'a }
end
[%%expect {|
module M : sig type ('a : immutable_data) t : immutable_data end
|}]

module M : sig
  type 'a t : immutable_data with 'a
end = struct
  type 'a t = Foo of 'a
end
[%%expect {|
module M : sig type 'a t : immutable_data end
|}]

module M : sig
  type 'a t : immutable_data with 'a
end = struct
  type 'a t = Foo of 'a * int
end
[%%expect {|
module M : sig type 'a t : immutable_data end
|}]

module M : sig
  type 'a t : immutable_data with int with 'a with string
end = struct
  type 'a t = Foo of 'a * int | Bar of string
end
[%%expect {|
module M : sig type 'a t : immutable_data end
|}]

module M : sig
  type t : mutable_data
end = struct
  type t = Foo of int | Bar of string
end

[%%expect{|
module M : sig type t : mutable_data end
|}]

module M : sig
  type t : immutable_data
end = struct
  type t = Foo of { mutable x : int } | Bar of string
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = Foo of { mutable x : int } | Bar of string
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = Foo of { mutable x : int; } | Bar of string end
       is not included in
         sig type t : immutable_data end
       Type declarations do not match:
         type t = Foo of { mutable x : int; } | Bar of string
       is not included in
         type t : immutable_data
       The kind of the first is mutable_data
         because of the definition of t at line 4, characters 2-53.
       But the kind of the first must be a subkind of immutable_data
         because of the definition of t at line 2, characters 2-25.
|}]

module M : sig
  type t : immutable_data
end = struct
  type t = Foo of int ref | Bar of string
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = Foo of int ref | Bar of string
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = Foo of int ref | Bar of string end
       is not included in
         sig type t : immutable_data end
       Type declarations do not match:
         type t = Foo of int ref | Bar of string
       is not included in
         type t : immutable_data
       The kind of the first is value
         because of the definition of t at line 4, characters 2-41.
       But the kind of the first must be a subkind of immutable_data
         because of the definition of t at line 2, characters 2-25.
|}]

module M : sig
  type 'a t : immutable_data
end = struct
  type 'a t = Foo of 'a | Bar of string
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type 'a t = Foo of 'a | Bar of string
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = Foo of 'a | Bar of string end
       is not included in
         sig type 'a t : immutable_data end
       Type declarations do not match:
         type 'a t = Foo of 'a | Bar of string
       is not included in
         type 'a t : immutable_data
       The kind of the first is immutable_data
         because of the definition of t at line 4, characters 2-39.
       But the kind of the first must be a subkind of immutable_data
         because of the definition of t at line 2, characters 2-28.
|}]

module M : sig
  type 'a t : immutable_data
end = struct
  type 'a t = Foo of int | Bar of string
end

[%%expect{|
module M : sig type 'a t : immutable_data end
|}]

(* Some recursive types *)

type 'a my_list : immutable_data with 'a = [] | ( :: ) of 'a * 'a my_list
[%%expect{|
type 'a my_list = [] | (::) of 'a * 'a my_list
|}]

type 'a my_list : immutable_data with 'a = [] | ( :: ) of 'a * 'a foo
and 'a foo = 'a my_list
[%%expect{|
type 'a my_list = [] | (::) of 'a * 'a foo
and 'a foo = 'a my_list
|}]

type 'a t1 : immutable_data with 'a = Base of 'a | T2 of 'a t2
and 'a t2 : immutable_data with 'a = Base of 'a | T1 of 'a t1
[%%expect{|
type 'a t1 = Base of 'a | T2 of 'a t2
and 'a t2 = Base of 'a | T1 of 'a t1
|}]
