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
       the corresponding unboxed types can be marked unboxed.
|}]

external foo2 : int -> (#(int * float#) [@unboxed]) = "foo" "foo'"
[%%expect{|
Line 1, characters 24-39:
1 | external foo2 : int -> (#(int * float#) [@unboxed]) = "foo" "foo'"
                            ^^^^^^^^^^^^^^^
Error: Don't know how to unbox this type.
       Only "float", "int32", "int64", "nativeint", vector primitives, and
       the corresponding unboxed types can be marked unboxed.
|}]

external foo1 : #(int * float#) -> int = "foo" "foo'" [@@unboxed]
[%%expect{|
Line 1, characters 16-31:
1 | external foo1 : #(int * float#) -> int = "foo" "foo'" [@@unboxed]
                    ^^^^^^^^^^^^^^^
Error: Don't know how to unbox this type.
       Only "float", "int32", "int64", "nativeint", vector primitives, and
       the corresponding unboxed types can be marked unboxed.
|}]

external foo2 : float# -> #(int * float#) = "foo" "foo'" [@@unboxed]
[%%expect{|
Line 1, characters 26-41:
1 | external foo2 : float# -> #(int * float#) = "foo" "foo'" [@@unboxed]
                              ^^^^^^^^^^^^^^^
Error: Don't know how to unbox this type.
       Only "float", "int32", "int64", "nativeint", vector primitives, and
       the corresponding unboxed types can be marked unboxed.
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

(* You can't smuggle an unrepresentable type into an external inside a
   product. *)
external foo1 : ('a : any). #( string * 'a * float# ) -> int = "foo" "bar"
[%%expect{|
Line 1, characters 28-53:
1 | external foo1 : ('a : any). #( string * 'a * float# ) -> int = "foo" "bar"
                                ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Types in an external must have a representable layout.
       The layout of #(string * 'a * float#) is value & any & float64
         because it is an unboxed tuple.
       But the layout of #(string * 'a * float#) must be representable
         because it's the type of an argument in an external declaration.
|}]

external foo2 : ('a : any). int -> #( string * 'a * float# ) = "foo" "bar"
[%%expect{|
Line 1, characters 35-60:
1 | external foo2 : ('a : any). int -> #( string * 'a * float# ) = "foo" "bar"
                                       ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Types in an external must have a representable layout.
       The layout of #(string * 'a * float#) is value & any & float64
         because it is an unboxed tuple.
       But the layout of #(string * 'a * float#) must be representable
         because it's the type of the result of an external declaration.
|}]
