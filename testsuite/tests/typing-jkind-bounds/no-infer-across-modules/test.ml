(* TEST
   readonly_files = "define_with_kinds.ml";
   setup-ocamlc.byte-build-env;
   module = "define_with_kinds.ml";
   flags = "-infer-with-bounds -extension layouts_beta";
   ocamlc.byte;
   flags = "-I ocamlc.byte ocamlc.byte/define_with_kinds.cmo -extension layouts_beta";
   expect;
*)

open Define_with_kinds

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

type my_list_test = int my_list require_portable
(* CR layouts v2.8: fix principal case *)
[%%expect{|
type my_list_test = int Define_with_kinds.my_list require_portable
|}, Principal{|
Line 1, characters 20-31:
1 | type my_list_test = int my_list require_portable
                        ^^^^^^^^^^^
Error: This type "int Define_with_kinds.my_list" should be an instance of type
         "('a : value mod portable)"
       The kind of int Define_with_kinds.my_list is immutable_data.
       But the kind of int Define_with_kinds.my_list must be a subkind of
         value mod portable
         because of the definition of require_portable at line 12, characters 0-47.
|}]

type my_list_test = int ref my_list require_portable
[%%expect{|
Line 1, characters 20-35:
1 | type my_list_test = int ref my_list require_portable
                        ^^^^^^^^^^^^^^^
Error: This type "int ref Define_with_kinds.my_list"
       should be an instance of type "('a : value mod portable)"
       The kind of int ref Define_with_kinds.my_list is immutable_data.
       But the kind of int ref Define_with_kinds.my_list must be a subkind of
         value mod portable
         because of the definition of require_portable at line 12, characters 0-47.
|}]

type my_list_test = (int -> int) my_list require_portable
[%%expect{|
Line 1, characters 20-40:
1 | type my_list_test = (int -> int) my_list require_portable
                        ^^^^^^^^^^^^^^^^^^^^
Error: This type "(int -> int) Define_with_kinds.my_list"
       should be an instance of type "('a : value mod portable)"
       The kind of (int -> int) Define_with_kinds.my_list is immutable_data.
       But the kind of (int -> int) Define_with_kinds.my_list must be a subkind of
         value mod portable
         because of the definition of require_portable at line 12, characters 0-47.
|}]

type t_test = int my_list require_global
[%%expect{|
Line 1, characters 14-25:
1 | type t_test = int my_list require_global
                  ^^^^^^^^^^^
Error: This type "int Define_with_kinds.my_list" should be an instance of type
         "('a : value mod global)"
       The kind of int Define_with_kinds.my_list is immutable_data.
       But the kind of int Define_with_kinds.my_list must be a subkind of
         value mod global
         because of the definition of require_global at line 9, characters 0-43.
|}]

type my_list_test = int ref my_list require_uncontended
[%%expect{|
Line 1, characters 20-35:
1 | type my_list_test = int ref my_list require_uncontended
                        ^^^^^^^^^^^^^^^
Error: This type "int ref Define_with_kinds.my_list"
       should be an instance of type "('a : value mod uncontended)"
       The kind of int ref Define_with_kinds.my_list is immutable_data.
       But the kind of int ref Define_with_kinds.my_list must be a subkind of
         value mod uncontended
         because of the definition of require_uncontended at line 11, characters 0-53.
|}]

type my_list_test = int my_ref my_list require_uncontended
[%%expect{|
Line 1, characters 20-38:
1 | type my_list_test = int my_ref my_list require_uncontended
                        ^^^^^^^^^^^^^^^^^^
Error: This type "int Define_with_kinds.my_ref Define_with_kinds.my_list"
       should be an instance of type "('a : value mod uncontended)"
       The kind of int Define_with_kinds.my_ref Define_with_kinds.my_list is
         immutable_data.
       But the kind of int Define_with_kinds.my_ref Define_with_kinds.my_list must be a subkind of
         value mod uncontended
         because of the definition of require_uncontended at line 11, characters 0-53.
|}]

type my_list_test = int my_ref my_list require_portable
(* CR layouts v2.8: fix principal case *)
[%%expect{|
type my_list_test =
    int Define_with_kinds.my_ref Define_with_kinds.my_list require_portable
|}, Principal{|
Line 1, characters 20-38:
1 | type my_list_test = int my_ref my_list require_portable
                        ^^^^^^^^^^^^^^^^^^
Error: This type "int Define_with_kinds.my_ref Define_with_kinds.my_list"
       should be an instance of type "('a : value mod portable)"
       The kind of int Define_with_kinds.my_ref Define_with_kinds.my_list is
         immutable_data.
       But the kind of int Define_with_kinds.my_ref Define_with_kinds.my_list must be a subkind of
         value mod portable
         because of the definition of require_portable at line 12, characters 0-47.
|}]


(*******)

let foo (t : int my_list @@ contended) = use_uncontended t
[%%expect{|
val foo : int Define_with_kinds.my_list @ contended -> unit = <fun>
|}]

let foo (t : int my_ref my_list @@ contended) = use_uncontended t
[%%expect{|
Line 1, characters 64-65:
1 | let foo (t : int my_ref my_list @@ contended) = use_uncontended t
                                                                    ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (t : (int -> int) my_list @@ contended) = use_portable t
[%%expect{|
val foo : (int -> int) Define_with_kinds.my_list @ portable contended -> unit =
  <fun>
|}]

let foo (t : (int -> int) my_ref my_list @@ contended) = use_uncontended t
[%%expect{|
Line 1, characters 73-74:
1 | let foo (t : (int -> int) my_ref my_list @@ contended) = use_uncontended t
                                                                             ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (t : (int -> int) my_ref my_list @@ contended) = use_global t
[%%expect{|
Line 1, characters 68-69:
1 | let foo (t : (int -> int) my_ref my_list @@ contended) = use_global t
                                                                        ^
Error: This value is "contended" but expected to be "uncontended".
|}]

(********)

let foo (t : int rec1) = use_portable t
[%%expect{|
val foo : int Define_with_kinds.rec1 -> unit = <fun>
|}]

let foo (t : (int -> int) rec1) = use_portable t
[%%expect{|
val foo : (int -> int) Define_with_kinds.rec1 @ portable -> unit = <fun>
|}]

let foo (t : int rec1 @@ contended) = use_uncontended t
[%%expect{|
val foo : int Define_with_kinds.rec1 @ contended -> unit = <fun>
|}]

let foo (t : int my_ref rec1 @@ contended) = use_uncontended t
[%%expect{|
Line 1, characters 61-62:
1 | let foo (t : int my_ref rec1 @@ contended) = use_uncontended t
                                                                 ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (t : int ref rec1 @@ contended) = use_uncontended t
[%%expect{|
Line 1, characters 58-59:
1 | let foo (t : int ref rec1 @@ contended) = use_uncontended t
                                                              ^
Error: This value is "contended" but expected to be "uncontended".
|}]

(******)

let use_uncontended_three_values : ('a : (value & value) & value) @ uncontended -> unit = fun _ -> ()
let use_portable_three_values : ('a : (value & value) & value) @ portable -> unit = fun _ -> ()

let foo (t : #(int * int) unboxed_record @@ contended) = use_uncontended_three_values t
[%%expect{|
val use_uncontended_three_values : ('a : (value & value) & value). 'a -> unit =
  <fun>
val use_portable_three_values :
  ('a : (value & value) & value). 'a @ portable -> unit = <fun>
val foo : #(int * int) Define_with_kinds.unboxed_record @ contended -> unit =
  <fun>
|}]

let foo (t : #(int my_ref * int) unboxed_record @@ contended) = use_uncontended_three_values t
[%%expect{|
Line 1, characters 93-94:
1 | let foo (t : #(int my_ref * int) unboxed_record @@ contended) = use_uncontended_three_values t
                                                                                                 ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (t : #((int -> int) * int) unboxed_record @@ nonportable) = use_portable_three_values t
[%%expect{|
Line 1, characters 94-95:
1 | let foo (t : #((int -> int) * int) unboxed_record @@ nonportable) = use_portable_three_values t
                                                                                                  ^
Error: This value is "nonportable" but expected to be "portable".
|}, Principal{|
Line 1, characters 94-95:
1 | let foo (t : #((int -> int) * int) unboxed_record @@ nonportable) = use_portable_three_values t
                                                                                                  ^
Error: This expression has type
         "#((int -> int) * int) Define_with_kinds.unboxed_record"
       but an expression was expected of type "('a : (value & value) & value)"
       The kind of #((int -> int) * int) Define_with_kinds.unboxed_record is
         (immutable_data & immutable_data) & immutable_data.
       But the kind of #((int -> int) * int) Define_with_kinds.unboxed_record must be a subkind of
         (value & value) & value
         because of the definition of use_portable_three_values at line 2, characters 84-95.
|}]

(******)

module Option : Optionish = struct
  type 'a my_option =
    | Nothing
    | Just of 'a
end
[%%expect{|
module Option : Define_with_kinds.Optionish
|}]

module Option2 : module type of Optionish2 = struct
  type 'a my_option =
    | Nothing
    | Just of 'a
end
[%%expect{|
module Option2 : sig type 'a my_option = Nothing | Just of 'a end
|}]

module rec Foo : Optionish = Foo
[%%expect{|
module rec Foo : Define_with_kinds.Optionish
|}]

type 'a my_option2 = 'a Optionish2.my_option =
  | Nothing
  | Just of 'a
[%%expect{|
type 'a my_option2 =
  'a Define_with_kinds.Optionish2.my_option =
    Nothing
  | Just of 'a
|}]
