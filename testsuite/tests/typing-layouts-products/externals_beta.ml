(* TEST
 include stdlib_upstream_compatible;
 {
   flags = "-extension layouts_beta";
   expect;
 }
*)

(* CR layouts v7.1: a future backend PR will change the calling convention for
   unboxed products to meet an existing standard and add tests of their
   behavior. That PR should replace this test.

   For now, we just check that typechecking allows such external declarations on
   alpha but not on beta (see also [externals_alpha.ml]).  The backend PR will
   also allow them in beta.

   The errors here aren't great, but they will go away in that next PR. *)

external foo1 : #(int * float#) -> int = "foo" "foo'"
[%%expect{|
Line 1, characters 16-38:
1 | external foo1 : #(int * float#) -> int = "foo" "foo'"
                    ^^^^^^^^^^^^^^^^^^^^^^
Error: The primitive [foo] is used in an invalid declaration.
       The declaration contains argument/return types with the wrong layout.
|}]

external foo2 : int -> #(int * float#) = "foo" "foo'"
[%%expect{|
Line 1, characters 16-38:
1 | external foo2 : int -> #(int * float#) = "foo" "foo'"
                    ^^^^^^^^^^^^^^^^^^^^^^
Error: The primitive [foo] is used in an invalid declaration.
       The declaration contains argument/return types with the wrong layout.
|}]
