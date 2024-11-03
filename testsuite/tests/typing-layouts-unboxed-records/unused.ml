(* TEST
 flags = " -w +A -strict-sequence ";
 expect;
*)

(* Adapted from [testsuite/tests/typing-warnings/unused_types.ml] *)

module Unused_record : sig end = struct
  type t = #{ a : int; b : int }
  let foo (x : t) = x
  let _ = foo
end;;
[%%expect {|
Line 2, characters 13-21:
2 |   type t = { a : int; b : int }
                 ^^^^^^^^
Warning 69 [unused-field]: unused record field a.

Line 2, characters 22-29:
2 |   type t = { a : int; b : int }
                          ^^^^^^^
Warning 69 [unused-field]: unused record field b.

module Unused_record : sig end
|}]

module Unused_field : sig end = struct
  type t = #{ a : int }
  let foo () = #{ a = 0 }
  let _ = foo
end;;
[%%expect {|
Line 2, characters 13-20:
2 |   type t = { a : int }
                 ^^^^^^^
Warning 69 [unused-field]: record field a is never read.
(However, this field is used to build or mutate values.)

module Unused_field : sig end
|}]

module Unused_field : sig end = struct
  type t = #{ a : int; b : int; c : int }
  let foo () = #{ a = 0; b = 0; c = 0 }
  let bar x = x.a
  let baz #{ c; _ } = c
  let _ = foo, bar, baz
end;;
[%%expect {|
Line 2, characters 22-30:
2 |   type t = { a : int; b : int; c : int }
                          ^^^^^^^^
Warning 69 [unused-field]: record field b is never read.
(However, this field is used to build or mutate values.)

module Unused_field : sig end
|}]

module Unused_mutable_field : sig end = struct
  type t = #{ a : int; mutable b : int }
  let foo () = #{ a = 0; b = 0 }
  let bar x = x.a, x.b
  let _ = foo, bar
end;;
[%%expect {|
Line 2, characters 22-37:
2 |   type t = { a : int; mutable b : int }
                          ^^^^^^^^^^^^^^^
Warning 69 [unused-field]: mutable record field b is never mutated.

module Unused_mutable_field : sig end
|}]

module Unused_field_exported_private : sig
  type t = private #{ a : int }
end = struct
  type t = #{ a : int }
end;;
[%%expect {|
module Unused_field_exported_private : sig type t = private { a : int; } end
|}]

module Unused_field_exported_private : sig
  type t = private #{ a : int }
end = struct
  type t = #{ a : int }
  let foo x = x.a
  let _ = foo
end;;
[%%expect {|
module Unused_field_exported_private : sig type t = private { a : int; } end
|}]

module Unused_mutable_field_exported_private : sig
  type t = private #{ a : int; mutable b : int }
end = struct
  type t = #{ a : int; mutable b : int }
  let foo () = #{ a = 0; b = 0 }
  let _ = foo
end;;
[%%expect {|
Line 4, characters 22-37:
4 |   type t = { a : int; mutable b : int }
                          ^^^^^^^^^^^^^^^
Warning 69 [unused-field]: mutable record field b is never mutated.

module Unused_mutable_field_exported_private :
  sig type t = private { a : int; mutable b : int; } end
|}]

module Unused_field_disable_warning : sig
end = struct
  type t = #{ a: int; b:int } [@@warning "-unused-field"]
end;;
[%%expect {|
Line 3, characters 2-56:
3 |   type t = { a: int; b:int } [@@warning "-unused-field"]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 34 [unused-type-declaration]: unused type t.

module Unused_field_disable_warning : sig end
|}]

module Unused_field_disable_one_warning : sig
end = struct
  type t = #{ a: int [@warning "-unused-field"]; b:int }
end;;
[%%expect {|
Line 3, characters 2-55:
3 |   type t = #{ a: int [@warning "-unused-field"]; b:int }
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 34 [unused-type-declaration]: unused type t.

Line 3, characters 48-53:
3 |   type t = #{ a: int [@warning "-unused-field"]; b:int }
                                                    ^^^^^
Warning 69 [unused-field]: unused record field b.

module Unused_field_disable_one_warning : sig end
|}]
