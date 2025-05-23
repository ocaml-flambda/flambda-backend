(* TEST
 flambda2;
 include stdlib_upstream_compatible;
 flags = "-extension layouts_alpha";
 {
   expect;
 }
*)
(* CR layouts v3: This test is in [layouts_alpha] just because it is about
   [or_null].  Once [or_null] is appropriately mature, it can be folded into the
   other test file.  Note also that the test involves the nullability of [any],
   which is currently different between [alpha] and [beta], so the test may need
   to be adjusted. *)

(***********************************)
(* Test 1: nullability of products *)

(* In earlier versions we thought the nullability of a product should be the
   meet of its elements.  We've now realized it should be the join, at least for
   now.  This test shows the current behavior. *)

(* Should be allowed *)
type t1 : any mod non_null separable
type t2 : value
type t3 : any mod non_null separable = #(t1 * t2);;
[%%expect{|
type t1 : any_non_null
type t2
type t3 = #(t1 * t2)
|}]

type t1 : any mod non_null separable
type t2 : value
type t3 : any & value mod non_null separable = #(t1 * t2);;
[%%expect{|
type t1 : any_non_null
type t2
type t3 = #(t1 * t2)
|}]

type t1 : any mod non_null separable
type t2 : value
type t3 : (any mod non_null separable) & (value mod non_null separable) = #(t1 * t2);;
[%%expect{|
type t1 : any_non_null
type t2
type t3 = #(t1 * t2)
|}]

type t1 : any
type t2 : any mod non_null separable
type t3 : any & (any mod non_null separable) = #(t1 * t2);;
[%%expect{|
type t1 : any
type t2 : any_non_null
type t3 = #(t1 * t2)
|}]

(* Should not be allowed. *)
type t1 : any
type t2 : any mod non_null separable
type t3 : any mod non_null separable = #(t1 * t2);;
[%%expect{|
type t1 : any
type t2 : any_non_null
Line 3, characters 0-49:
3 | type t3 : any mod non_null separable = #(t1 * t2);;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "#(t1 * t2)" is
         any_non_null mod everything mod non_float with t1 with t2
         & any_non_null mod everything mod non_float with t1 with t2
         because it is an unboxed tuple.
       But the kind of type "#(t1 * t2)" must be a subkind of any_non_null
         because of the definition of t3 at line 3, characters 0-49.
|}]

type t1 : any
type t2 : any mod non_null separable
type t3 : any & any mod non_null separable = #(t1 * t2);;
[%%expect{|
type t1 : any
type t2 : any_non_null
Line 3, characters 0-55:
3 | type t3 : any & any mod non_null separable = #(t1 * t2);;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "#(t1 * t2)" is
         any_non_null mod everything mod non_float with t1 with t2
         & any_non_null mod everything mod non_float with t1 with t2
         because it is an unboxed tuple.
       But the kind of type "#(t1 * t2)" must be a subkind of
         any_non_null & any_non_null
         because of the definition of t3 at line 3, characters 0-55.
|}]

type t1 : any
type t2 : any mod non_null separable
type t3 : (any mod non_null separable) & (any mod non_null separable) = #(t1 * t2);;
[%%expect{|
type t1 : any
type t2 : any_non_null
Line 3, characters 0-82:
3 | type t3 : (any mod non_null separable) & (any mod non_null separable) = #(t1 * t2);;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "#(t1 * t2)" is
         any_non_null mod everything mod non_float with t1 with t2
         & any_non_null mod everything mod non_float with t1 with t2
         because it is an unboxed tuple.
       But the kind of type "#(t1 * t2)" must be a subkind of
         any_non_null & any_non_null
         because of the definition of t3 at line 3, characters 0-82.
|}]
