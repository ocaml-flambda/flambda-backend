(* TEST
 flags = " -w +A -strict-sequence -extension layouts_beta";
 expect;
*)

(* Adapted from [testsuite/tests/typing-warnings/unused_types.ml].

   CR layouts v7.2: Once unboxed records are in stable, fold this test back into
   the original or move it to [typing-layouts-products]. *)

module Unused_record : sig end = struct
  type t = #{ a : int; b : int }
  let foo (x : t) = x
  let _ = foo
end;;
[%%expect {|
Line 2, characters 14-22:
2 |   type t = #{ a : int; b : int }
                  ^^^^^^^^
Warning 69 [unused-field]: unused unboxed record field a.

Line 2, characters 23-30:
2 |   type t = #{ a : int; b : int }
                           ^^^^^^^
Warning 69 [unused-field]: unused unboxed record field b.

module Unused_record : sig end
|}]

module Unused_field : sig end = struct
  type t = #{ a : int }
  let foo () = #{ a = 0 }
  let _ = foo
end;;
[%%expect {|
Line 2, characters 14-21:
2 |   type t = #{ a : int }
                  ^^^^^^^
Warning 69 [unused-field]: unboxed record field a is never read.
(However, this field is used to build or mutate values.)

module Unused_field : sig end
|}]

module Unused_field : sig end = struct
  type t = #{ a : int; b : int; c : int }
  let foo () = #{ a = 0; b = 0; c = 0 }
  let bar x = x.#a
  let baz #{ c; _ } = c
  let _ = foo, bar, baz
end;;
[%%expect {|
Line 2, characters 23-31:
2 |   type t = #{ a : int; b : int; c : int }
                           ^^^^^^^^
Warning 69 [unused-field]: unboxed record field b is never read.
(However, this field is used to build or mutate values.)

module Unused_field : sig end
|}]

module Unused_field_exported_private : sig
  type t = private #{ a : int }
end = struct
  type t = #{ a : int }
end;;
[%%expect {|
module Unused_field_exported_private : sig type t = private #{ a : int; } end
|}]

module Unused_field_exported_private : sig
  type t = private #{ a : int }
end = struct
  type t = #{ a : int }
  let foo x = x.#a
  let _ = foo
end;;
[%%expect {|
module Unused_field_exported_private : sig type t = private #{ a : int; } end
|}]

module Unused_field_disable_warning : sig
end = struct
  type t = #{ a: int; b:int } [@@warning "-unused-field"]
end;;
[%%expect {|
Line 3, characters 2-57:
3 |   type t = #{ a: int; b:int } [@@warning "-unused-field"]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 34 [unused-type-declaration]: unused type t.

module Unused_field_disable_warning : sig end
|}]

module Unused_field_disable_one_warning : sig
end = struct
  type t = #{ a: int [@warning "-unused-field"]; b:int }
end;;
[%%expect {|
Line 3, characters 2-56:
3 |   type t = #{ a: int [@warning "-unused-field"]; b:int }
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 34 [unused-type-declaration]: unused type t.

Line 3, characters 49-54:
3 |   type t = #{ a: int [@warning "-unused-field"]; b:int }
                                                     ^^^^^
Warning 69 [unused-field]: unused unboxed record field b.

module Unused_field_disable_one_warning : sig end
|}]
