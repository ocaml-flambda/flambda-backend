(* TEST
   flags = "-infer-with-bounds";
   expect;
*)

let use_global : 'a @ global -> unit = fun _ -> ()
let use_unique : 'a @ unique -> unit = fun _ -> ()
let use_uncontended : 'a @ uncontended -> unit = fun _ -> ()
let use_portable : 'a @ portable -> unit = fun _ -> ()
let use_many : 'a @ many -> unit = fun _ -> ()

type ('a : value mod global) require_global
type ('a : value mod unique) require_unique
type ('a : value mod uncontended) require_uncontended
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
type ('a : value mod global) require_global
type ('a : value mod unique) require_unique
type ('a : value mod uncontended) require_uncontended
type ('a : value mod portable) require_portable
type ('a : value mod many) require_many
type 'a require_nonnull
type ('a : value mod external_) require_external
|}]

(* option *)
type t : immutable_data = int option
type t : mutable_data = int ref option
type 'a t : immutable_data with 'a = 'a option
type ('a : immutable_data) t : immutable_data = 'a option
[%%expect {|
type t = int option
type t = int ref option
type 'a t = 'a option
type ('a : immutable_data) t = 'a option
|}]

type 'a t : immutable_data = 'a option
[%%expect {|
Line 1, characters 0-38:
1 | type 'a t : immutable_data = 'a option
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "'a option" is immutable_data
         because it's a boxed variant type.
       But the kind of type "'a option" must be a subkind of immutable_data
         because of the definition of t at line 1, characters 0-38.
|}]

type t : immutable_data = int ref option
[%%expect {|
Line 1, characters 0-40:
1 | type t : immutable_data = int ref option
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "int ref option" is immutable_data
         because it's a boxed variant type.
       But the kind of type "int ref option" must be a subkind of
         immutable_data
         because of the definition of t at line 1, characters 0-40.
|}]

type t_test = int option require_portable
type t_test = int option require_many
type t_test = int option require_uncontended
type ('a : value mod portable) t_test = 'a option require_portable
(* CR layouts v2.8: fix in principal case *)
[%%expect {|
type t_test = int option require_portable
type t_test = int option require_many
type t_test = int option require_uncontended
type ('a : value mod portable) t_test = 'a option require_portable
|}, Principal{|
Line 1, characters 14-24:
1 | type t_test = int option require_portable
                  ^^^^^^^^^^
Error: This type "int option" should be an instance of type
         "('a : value mod portable)"
       The kind of int option is immutable_data
         because it's a boxed variant type.
       But the kind of int option must be a subkind of value mod portable
         because of the definition of require_portable at line 10, characters 0-47.
|}]

type t_test = (unit -> unit) option require_portable
[%%expect {|
Line 1, characters 14-35:
1 | type t_test = (unit -> unit) option require_portable
                  ^^^^^^^^^^^^^^^^^^^^^
Error: This type "(unit -> unit) option" should be an instance of type
         "('a : value mod portable)"
       The kind of (unit -> unit) option is immutable_data
         because it's a boxed variant type.
       But the kind of (unit -> unit) option must be a subkind of
         value mod portable
         because of the definition of require_portable at line 10, characters 0-47.
|}]

type t_test = int option require_global
[%%expect {|
Line 1, characters 14-24:
1 | type t_test = int option require_global
                  ^^^^^^^^^^
Error: This type "int option" should be an instance of type
         "('a : value mod global)"
       The kind of int option is immutable_data
         because it's a boxed variant type.
       But the kind of int option must be a subkind of value mod global
         because of the definition of require_global at line 7, characters 0-43.
|}]

let foo (t : int option @@ contended portable once) =
  use_many t;
  use_uncontended t;
  use_portable t
[%%expect {|
val foo : int option @ once portable contended -> unit = <fun>
|}]

let foo (t : int option @@ local) = use_global t [@nontail]
[%%expect {|
Line 1, characters 47-48:
1 | let foo (t : int option @@ local) = use_global t [@nontail]
                                                   ^
Error: This value escapes its region.
|}]

(* ref *)
type t : mutable_data = int ref
type 'a t : mutable_data with 'a @@ global many = 'a ref
type ('a : mutable_data) t : mutable_data = 'a list
[%%expect {|
type t = int ref
type 'a t = 'a ref
type ('a : mutable_data) t = 'a list
|}]

type t : immutable_data = int ref
[%%expect {|
Line 1, characters 0-33:
1 | type t : immutable_data = int ref
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "int ref" is mutable_data.
       But the kind of type "int ref" must be a subkind of immutable_data
         because of the definition of t at line 1, characters 0-33.
|}]

type 'a t : mutable_data = 'a ref
[%%expect {|
Line 1, characters 0-33:
1 | type 'a t : mutable_data = 'a ref
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "'a ref" is mutable_data.
       But the kind of type "'a ref" must be a subkind of mutable_data
         because of the definition of t at line 1, characters 0-33.
|}]

type t_test = int ref require_portable
type t_test = int ref require_many
type ('a : value mod portable) t_test = 'a ref require_portable
(* CR layouts v2.8: fix in principal case *)
[%%expect {|
type t_test = int ref require_portable
type t_test = int ref require_many
type ('a : value mod portable) t_test = 'a ref require_portable
|}, Principal{|
Line 1, characters 14-21:
1 | type t_test = int ref require_portable
                  ^^^^^^^
Error: This type "int ref" should be an instance of type
         "('a : value mod portable)"
       The kind of int ref is mutable_data.
       But the kind of int ref must be a subkind of value mod portable
         because of the definition of require_portable at line 10, characters 0-47.
|}]

type t_test = int ref require_uncontended
[%%expect {|
Line 1, characters 14-21:
1 | type t_test = int ref require_uncontended
                  ^^^^^^^
Error: This type "int ref" should be an instance of type
         "('a : value mod uncontended)"
       The kind of int ref is mutable_data.
       But the kind of int ref must be a subkind of value mod uncontended
         because of the definition of require_uncontended at line 9, characters 0-53.
|}]

let foo (t : int ref @@ portable once) =
  use_many t;
  use_portable t
[%%expect {|
val foo : int ref @ once portable -> unit = <fun>
|}]

let foo (t : int ref @@ contended) = use_uncontended t
[%%expect {|
Line 1, characters 53-54:
1 | let foo (t : int ref @@ contended) = use_uncontended t
                                                         ^
Error: This value is "contended" but expected to be "uncontended".
|}]

(* list *)
type t : immutable_data = int list
type t : mutable_data = int ref list
type ('a : immutable_data) t : immutable_data = 'a list
[%%expect {|
type t = int list
type t = int ref list
type ('a : immutable_data) t = 'a list
|}]

type 'a t : immutable_data with 'a = 'a list
[%%expect {|
type 'a t = 'a list
|}]

type 'a t : immutable_data = 'a list
[%%expect {|
Line 1, characters 0-36:
1 | type 'a t : immutable_data = 'a list
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "'a list" is immutable_data
         because it's a boxed variant type.
       But the kind of type "'a list" must be a subkind of immutable_data
         because of the definition of t at line 1, characters 0-36.
|}]

type t : immutable_data = int ref list
[%%expect {|
Line 1, characters 0-38:
1 | type t : immutable_data = int ref list
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "int ref list" is immutable_data
         because it's a boxed variant type.
       But the kind of type "int ref list" must be a subkind of immutable_data
         because of the definition of t at line 1, characters 0-38.
|}]

type t_test = int list require_portable
type t_test = int list require_many
type t_test = int list require_uncontended
type ('a : value mod portable) t_test = 'a list require_portable
(* CR layouts v2.8: fix in principal case *)
[%%expect {|
type t_test = int list require_portable
type t_test = int list require_many
type t_test = int list require_uncontended
type ('a : value mod portable) t_test = 'a list require_portable
|}, Principal{|
Line 1, characters 14-22:
1 | type t_test = int list require_portable
                  ^^^^^^^^
Error: This type "int list" should be an instance of type
         "('a : value mod portable)"
       The kind of int list is immutable_data
         because it's a boxed variant type.
       But the kind of int list must be a subkind of value mod portable
         because of the definition of require_portable at line 10, characters 0-47.
|}]

type t_test = (unit -> unit) list require_portable
[%%expect {|
Line 1, characters 14-33:
1 | type t_test = (unit -> unit) list require_portable
                  ^^^^^^^^^^^^^^^^^^^
Error: This type "(unit -> unit) list" should be an instance of type
         "('a : value mod portable)"
       The kind of (unit -> unit) list is immutable_data
         because it's a boxed variant type.
       But the kind of (unit -> unit) list must be a subkind of
         value mod portable
         because of the definition of require_portable at line 10, characters 0-47.
|}]

type t_test = int list require_global
[%%expect {|
Line 1, characters 14-22:
1 | type t_test = int list require_global
                  ^^^^^^^^
Error: This type "int list" should be an instance of type
         "('a : value mod global)"
       The kind of int list is immutable_data
         because it's a boxed variant type.
       But the kind of int list must be a subkind of value mod global
         because of the definition of require_global at line 7, characters 0-43.
|}]

let foo (t : int list @@ contended portable once) =
  use_many t;
  use_uncontended t;
  use_portable t
[%%expect {|
val foo : int list @ once portable contended -> unit = <fun>
|}]

let foo (t : int list @@ local) = use_global t [@nontail]
[%%expect {|
Line 1, characters 45-46:
1 | let foo (t : int list @@ local) = use_global t [@nontail]
                                                 ^
Error: This value escapes its region.
|}]

(* array *)
type t : mutable_data = int array
type 'a t : mutable_data with 'a = 'a array
type ('a : mutable_data) t : mutable_data = 'a array
[%%expect {|
type t = int array
type 'a t = 'a array
type ('a : mutable_data) t = 'a array
|}]

type t : immutable_data = int array
[%%expect {|
Line 1, characters 0-35:
1 | type t : immutable_data = int array
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "int array" is mutable_data
         because it is the primitive value type array.
       But the kind of type "int array" must be a subkind of immutable_data
         because of the definition of t at line 1, characters 0-35.
|}]

type 'a t : mutable_data = 'a array
[%%expect {|
Line 1, characters 0-35:
1 | type 'a t : mutable_data = 'a array
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "'a array" is mutable_data
         because it is the primitive value type array.
       But the kind of type "'a array" must be a subkind of mutable_data
         because of the definition of t at line 1, characters 0-35.
|}]

type t_test = int array require_portable
type t_test = int array require_many
type ('a : value mod portable) t_test = 'a array require_portable
(* CR layouts v2.8: fix in principal case *)
[%%expect {|
type t_test = int array require_portable
type t_test = int array require_many
type ('a : value mod portable) t_test = 'a array require_portable
|}, Principal{|
Line 1, characters 14-23:
1 | type t_test = int array require_portable
                  ^^^^^^^^^
Error: This type "int array" should be an instance of type
         "('a : value mod portable)"
       The kind of int array is mutable_data
         because it is the primitive value type array.
       But the kind of int array must be a subkind of value mod portable
         because of the definition of require_portable at line 10, characters 0-47.
|}]

type t_test = int array require_uncontended
[%%expect {|
Line 1, characters 14-23:
1 | type t_test = int array require_uncontended
                  ^^^^^^^^^
Error: This type "int array" should be an instance of type
         "('a : value mod uncontended)"
       The kind of int array is mutable_data
         because it is the primitive value type array.
       But the kind of int array must be a subkind of value mod uncontended
         because of the definition of require_uncontended at line 9, characters 0-53.
|}]

let foo (t : int array @@ portable once) =
  use_many t;
  use_portable t
[%%expect {|
val foo : int array @ once portable -> unit = <fun>
|}]

let foo (t : int array @@ contended) = use_uncontended t
[%%expect {|
Line 1, characters 55-56:
1 | let foo (t : int array @@ contended) = use_uncontended t
                                                           ^
Error: This value is "contended" but expected to be "uncontended".
|}]

(* iarray *)
type t : immutable_data = int iarray
type t : mutable_data = int ref iarray
type 'a t : immutable_data with 'a = 'a iarray
type ('a : immutable_data) t : immutable_data = 'a iarray
[%%expect {|
type t = int iarray
type t = int ref iarray
type 'a t = 'a iarray
type ('a : immutable_data) t = 'a iarray
|}]

type 'a t : immutable_data = 'a iarray
[%%expect {|
Line 1, characters 0-38:
1 | type 'a t : immutable_data = 'a iarray
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "'a iarray" is immutable_data
         because it is the primitive value type iarray.
       But the kind of type "'a iarray" must be a subkind of immutable_data
         because of the definition of t at line 1, characters 0-38.
|}]

type t : immutable_data = int ref iarray
[%%expect {|
Line 1, characters 0-40:
1 | type t : immutable_data = int ref iarray
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "int ref iarray" is immutable_data
         because it is the primitive value type iarray.
       But the kind of type "int ref iarray" must be a subkind of
         immutable_data
         because of the definition of t at line 1, characters 0-40.
|}]

type t_test = int iarray require_portable
type t_test = int iarray require_many
type t_test = int iarray require_uncontended
type ('a : value mod portable) t_test = 'a iarray require_portable
(* CR layouts v2.8: fix in principal case *)
[%%expect {|
type t_test = int iarray require_portable
type t_test = int iarray require_many
type t_test = int iarray require_uncontended
type ('a : value mod portable) t_test = 'a iarray require_portable
|}, Principal{|
Line 1, characters 14-24:
1 | type t_test = int iarray require_portable
                  ^^^^^^^^^^
Error: This type "int iarray" should be an instance of type
         "('a : value mod portable)"
       The kind of int iarray is immutable_data
         because it is the primitive value type iarray.
       But the kind of int iarray must be a subkind of value mod portable
         because of the definition of require_portable at line 10, characters 0-47.
|}]

type t_test = (unit -> unit) iarray require_portable
[%%expect {|
Line 1, characters 14-35:
1 | type t_test = (unit -> unit) iarray require_portable
                  ^^^^^^^^^^^^^^^^^^^^^
Error: This type "(unit -> unit) iarray" should be an instance of type
         "('a : value mod portable)"
       The kind of (unit -> unit) iarray is immutable_data
         because it is the primitive value type iarray.
       But the kind of (unit -> unit) iarray must be a subkind of
         value mod portable
         because of the definition of require_portable at line 10, characters 0-47.
|}]

type t_test = int iarray require_global
[%%expect {|
Line 1, characters 14-24:
1 | type t_test = int iarray require_global
                  ^^^^^^^^^^
Error: This type "int iarray" should be an instance of type
         "('a : value mod global)"
       The kind of int iarray is immutable_data
         because it is the primitive value type iarray.
       But the kind of int iarray must be a subkind of value mod global
         because of the definition of require_global at line 7, characters 0-43.
|}]

let foo (t : int iarray @@ contended portable once) =
  use_many t;
  use_uncontended t;
  use_portable t
[%%expect {|
val foo : int iarray @ once portable contended -> unit = <fun>
|}]

let foo (t : int iarray @@ local) = use_global t [@nontail]
[%%expect {|
Line 1, characters 47-48:
1 | let foo (t : int iarray @@ local) = use_global t [@nontail]
                                                   ^
Error: This value escapes its region.
|}]
