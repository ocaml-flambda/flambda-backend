(* TEST
 include stdlib_upstream_compatible;
 {
   flags = "-extension layouts_alpha";
   expect;
 }
*)

(* CR layouts v7.1: a future backend PR will change the calling convention for
   unboxed products to meet an existing standard and add tests of their
   behavior. That PR should replace this test.

   For now, we just check that typechecking allows such external declarations on
   alpha but not on beta (see also [externals_beta.ml]).  The backend PR will
   also allow them in beta. *)

external foo1 : #(int * float#) -> int = "foo" "foo'"
[%%expect{|
external foo1 : #(int * float#) -> int = "foo" "foo'"
|}]

external foo2 : int -> #(int * float#) = "foo" "foo'"
[%%expect{|
external foo2 : int -> #(int * float#) = "foo" "foo'"
|}]

(* You can't use the [unboxed] attributes with products. *)
external foo1 : (#(int * float#) [@unboxed]) -> int = "foo" "foo'"
[%%expect{|
Line 1, characters 17-32:
1 | external foo1 : (#(int * float#) [@unboxed]) -> int = "foo" "foo'"
                     ^^^^^^^^^^^^^^^
Error: Don't know how to unbox this type.
       Only "float", "int32", "int64", "nativeint", vector primitives, and
       concrete unboxed types can be marked unboxed.
|}]

external foo2 : int -> (#(int * float#) [@unboxed]) = "foo" "foo'"
[%%expect{|
Line 1, characters 24-39:
1 | external foo2 : int -> (#(int * float#) [@unboxed]) = "foo" "foo'"
                            ^^^^^^^^^^^^^^^
Error: Don't know how to unbox this type.
       Only "float", "int32", "int64", "nativeint", vector primitives, and
       concrete unboxed types can be marked unboxed.
|}]

external foo1 : #(int * float#) -> int = "foo" "foo'" [@@unboxed]
[%%expect{|
Line 1, characters 16-31:
1 | external foo1 : #(int * float#) -> int = "foo" "foo'" [@@unboxed]
                    ^^^^^^^^^^^^^^^
Error: Don't know how to unbox this type.
       Only "float", "int32", "int64", "nativeint", vector primitives, and
       concrete unboxed types can be marked unboxed.
|}]

external foo2 : float# -> #(int * float#) = "foo" "foo'" [@@unboxed]
[%%expect{|
Line 1, characters 26-41:
1 | external foo2 : float# -> #(int * float#) = "foo" "foo'" [@@unboxed]
                              ^^^^^^^^^^^^^^^
Error: Don't know how to unbox this type.
       Only "float", "int32", "int64", "nativeint", vector primitives, and
       concrete unboxed types can be marked unboxed.
|}]

(* You can't use the [untagged] attributes with products. *)
external foo1 : (#(int * float#) [@untagged]) -> int = "foo" "foo'"
[%%expect{|
Line 1, characters 17-32:
1 | external foo1 : (#(int * float#) [@untagged]) -> int = "foo" "foo'"
                     ^^^^^^^^^^^^^^^
Error: Don't know how to untag this type. Only "int"
       and other immediate types can be untagged.
|}]

external foo2 : int -> (#(int * float#) [@untagged]) = "foo" "foo'"
[%%expect{|
Line 1, characters 24-39:
1 | external foo2 : int -> (#(int * float#) [@untagged]) = "foo" "foo'"
                            ^^^^^^^^^^^^^^^
Error: Don't know how to untag this type. Only "int"
       and other immediate types can be untagged.
|}]

external foo1 : #(int * float#) -> int = "foo" "foo'" [@@untagged]
[%%expect{|
Line 1, characters 16-31:
1 | external foo1 : #(int * float#) -> int = "foo" "foo'" [@@untagged]
                    ^^^^^^^^^^^^^^^
Error: Don't know how to untag this type. Only "int"
       and other immediate types can be untagged.
|}]

external foo2 : int -> #(int * float#) = "foo" "foo'" [@@untagged]
[%%expect{|
Line 1, characters 23-38:
1 | external foo2 : int -> #(int * float#) = "foo" "foo'" [@@untagged]
                           ^^^^^^^^^^^^^^^
Error: Don't know how to untag this type. Only "int"
       and other immediate types can be untagged.
|}]
