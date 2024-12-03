(* TEST
 flambda2;
 include stdlib_upstream_compatible;
 flags = "-extension layouts_alpha";
 {
   expect;
 }
*)

(*****************************)
(* Unboxed records with void *)

type t_void  : void

type ('a : void) t = #{ x : 'a ; y : t_void }
[%%expect{|
type t_void : void
type ('a : void) t = #{ x : 'a; y : t_void; }
|}]

type t = { x : t_void } [@@unboxed]
[%%expect{|
type t = { x : t_void; } [@@unboxed]
|}]

type bad : void = #{ bad : bad }
[%%expect{|
Line 1, characters 0-32:
1 | type bad : void = #{ bad : bad }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "bad" is recursive without boxing:
         "bad" contains "bad"
|}]

type ('a : void) bad  = #{ bad : 'a bad ; u : 'a}
[%%expect{|
Line 1, characters 0-49:
1 | type ('a : void) bad  = #{ bad : 'a bad ; u : 'a}
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
<<<<<<< HEAD:testsuite/tests/typing-layouts-products/unboxed_records_alpha.ml
Error: The definition of "bad" is recursive without boxing:
         "'a bad" contains "'a bad"
||||||| parent of bf5bf8a120 (Add with-kinds.):testsuite/tests/typing-layouts-unboxed-records/basics_alpha.ml
Error:
       The layout of 'a bad is any & any
         because it is an unboxed record.
       But the layout of 'a bad must be representable
         because it is the type of record field bad.
=======
Error: The layout of type "bad" is (value & value) & void
         because it is an unboxed record.
       But the layout of type "bad" must be a sublayout of value & value
         because it is an unboxed record.
>>>>>>> bf5bf8a120 (Add with-kinds.):testsuite/tests/typing-layouts-unboxed-records/basics_alpha.ml
|}]

(******************************************************************************)
(* The below is adapted from
   [testsuite/tests/typing-layouts-products/basics_alpha.ml]. *)

(* [t3] is allowed for unboxed tuples, and disallowed for (un)boxed records *)
type t1 : any mod non_null
type t2 : value
type t3 : any mod non_null = #{ t1 : t1 ; t2 : t2};;
[%%expect{|
type t1 : any_non_null
type t2
Line 3, characters 32-41:
3 | type t3 : any mod non_null = #{ t1 : t1 ; t2 : t2};;
                                    ^^^^^^^^^
Error: Unboxed record element types must have a representable layout.
       The layout of t1 is any
         because of the definition of t1 at line 1, characters 0-26.
       But the layout of t1 must be representable
         because it is the type of record field t1.
|}]

(* CR layouts v7.2: once [any] is allowed in unboxed record declarations, check
   that [non_null] behaves correctly in the following tests. *)

type t1 : any mod non_null
type t2 : value
type t3 : any & value mod non_null = #{ t1 : t1 ; t2 : t2};;
[%%expect{|
type t1 : any_non_null
type t2
Line 3, characters 40-49:
3 | type t3 : any & value mod non_null = #{ t1 : t1 ; t2 : t2};;
                                            ^^^^^^^^^
Error: Unboxed record element types must have a representable layout.
       The layout of t1 is any
         because of the definition of t1 at line 1, characters 0-26.
       But the layout of t1 must be representable
         because it is the type of record field t1.
|}]

type t1 : any mod non_null
type t2 : value
type t3 : (any mod non_null) & (value mod non_null) = #{ t1 : t1 ; t2 : t2};;
[%%expect{|
type t1 : any_non_null
type t2
Line 3, characters 57-66:
3 | type t3 : (any mod non_null) & (value mod non_null) = #{ t1 : t1 ; t2 : t2};;
                                                             ^^^^^^^^^
Error: Unboxed record element types must have a representable layout.
       The layout of t1 is any
         because of the definition of t1 at line 1, characters 0-26.
       But the layout of t1 must be representable
         because it is the type of record field t1.
|}]

type t1 : any
type t2 : any mod non_null
type t3 : any & (any mod non_null) = #{ t1 : t1 ; t2 : t2 };;
[%%expect{|
type t1 : any
type t2 : any_non_null
Line 3, characters 40-49:
3 | type t3 : any & (any mod non_null) = #{ t1 : t1 ; t2 : t2 };;
                                            ^^^^^^^^^
Error: Unboxed record element types must have a representable layout.
       The layout of t1 is any
         because of the definition of t1 at line 1, characters 0-13.
       But the layout of t1 must be representable
         because it is the type of record field t1.
|}]

(* Should not be allowed for either unboxed tuples or (un)boxed records. *)
type t1 : any
type t2 : any mod non_null
type t3 : any mod non_null = #{ t1 : t1 ; t2 : t2 };;
[%%expect{|
type t1 : any
type t2 : any_non_null
Line 3, characters 32-41:
3 | type t3 : any mod non_null = #{ t1 : t1 ; t2 : t2 };;
                                    ^^^^^^^^^
Error: Unboxed record element types must have a representable layout.
       The layout of t1 is any
         because of the definition of t1 at line 1, characters 0-13.
       But the layout of t1 must be representable
         because it is the type of record field t1.
|}]

type t1 : any
type t2 : any mod non_null
type t3 : any & any mod non_null = #{ t1 : t1 ; t2 : t2 };;
[%%expect{|
type t1 : any
type t2 : any_non_null
Line 3, characters 38-47:
3 | type t3 : any & any mod non_null = #{ t1 : t1 ; t2 : t2 };;
                                          ^^^^^^^^^
Error: Unboxed record element types must have a representable layout.
       The layout of t1 is any
         because of the definition of t1 at line 1, characters 0-13.
       But the layout of t1 must be representable
         because it is the type of record field t1.
|}]

type t1 : any
type t2 : any mod non_null
type t3 : (any mod non_null) & (any mod non_null) = #{ t1 : t1 ; t2 : t2 };;
[%%expect{|
type t1 : any
type t2 : any_non_null
Line 3, characters 55-64:
3 | type t3 : (any mod non_null) & (any mod non_null) = #{ t1 : t1 ; t2 : t2 };;
                                                           ^^^^^^^^^
Error: Unboxed record element types must have a representable layout.
       The layout of t1 is any
         because of the definition of t1 at line 1, characters 0-13.
       But the layout of t1 must be representable
         because it is the type of record field t1.
|}]
