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
type bad = #{ bad : bad; }
|}]

type ('a : void) bad  = #{ bad : 'a bad ; u : 'a}
[%%expect{|
Line 1, characters 0-49:
1 | type ('a : void) bad  = #{ bad : 'a bad ; u : 'a}
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error:
       The layout of 'a bad is any & any
         because it is an unboxed record.
       But the layout of 'a bad must be representable
         because it is the type of record field bad.
|}]

(***************************************************************************************)
(* The below is adapted from [testsuite/tests/typing-layouts-products/basics_alpha.ml].

   CR layouts v7.2: once unboxed records are in stable, fold this test back into the
   original or move it to [typing-layouts-products]. *)

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
