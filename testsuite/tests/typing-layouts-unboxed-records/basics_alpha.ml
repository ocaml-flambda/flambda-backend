(* TEST
 flambda2;
 include stdlib_upstream_compatible;
 flags = "-extension layouts_alpha";
 {
   expect;
 }
*)

(* This test is adapted from [testsuite/tests/typing-layouts-products/basics_alpha.ml] *)

(* Should be allowed *)
type t1 : any mod non_null
type t2 : value
type t3 : any mod non_null = #{ t1 : t1 ; t2 : t2};;
[%%expect{|
type t1 : any mod non_null
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

type t1 : any mod non_null
type t2 : value
type t3 : any & value mod non_null = #{ t1 : t1 ; t2 : t2};;
[%%expect{|
type t1 : any mod non_null
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
type t1 : any mod non_null
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
type t2 : any mod non_null
Line 3, characters 40-49:
3 | type t3 : any & (any mod non_null) = #{ t1 : t1 ; t2 : t2 };;
                                            ^^^^^^^^^
Error: Unboxed record element types must have a representable layout.
       The layout of t1 is any
         because of the definition of t1 at line 1, characters 0-13.
       But the layout of t1 must be representable
         because it is the type of record field t1.
|}]

(* Should not be allowed. *)
type t1 : any
type t2 : any mod non_null
type t3 : any mod non_null = #{ t1 : t1 ; t2 : t2 };;
[%%expect{|
type t1 : any
type t2 : any mod non_null
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
type t2 : any mod non_null
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
type t2 : any mod non_null
Line 3, characters 55-64:
3 | type t3 : (any mod non_null) & (any mod non_null) = #{ t1 : t1 ; t2 : t2 };;
                                                           ^^^^^^^^^
Error: Unboxed record element types must have a representable layout.
       The layout of t1 is any
         because of the definition of t1 at line 1, characters 0-13.
       But the layout of t1 must be representable
         because it is the type of record field t1.
|}]

