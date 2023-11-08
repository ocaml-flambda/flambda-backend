(* TEST
   flags = "-extension layouts_alpha"
   * expect
*)

(* This test makes sure that [type t : unboxed = manifest] doesn't get
   (incorrectly) interpreted as [type t = manifest [@@unboxed]].
 *)

type t : unboxed = { single_field : string }

[%%expect{|
Line 1, characters 9-16:
1 | type t : unboxed = { single_field : string }
             ^^^^^^^
Error: Unknown layout unboxed
|}]

(* The below tests make sure that a layout is given only as an
   attribute or as an annotation, but not both, regardless
   of whether the two sources match.
 *)

(* the two sources match *)
type t : immediate = int [@@immediate]

[%%expect{|
Line 1, characters 0-38:
1 | type t : immediate = int [@@immediate]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: A type declaration's layout can be given at most once.
       This declaration has an layout annotation (immediate) and a layout attribute ([@@immediate]).
|}]

(* the two sources don't match, but either is correct *)
type t : immediate64 = int [@@immediate]

[%%expect{|
Line 1, characters 0-40:
1 | type t : immediate64 = int [@@immediate]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: A type declaration's layout can be given at most once.
       This declaration has an layout annotation (immediate64) and a layout attribute ([@@immediate]).
|}]

(* the two sources don't match, and one or the other is incorrect *)
type t : void = int [@@immediate]

[%%expect{|
Line 1, characters 0-33:
1 | type t : void = int [@@immediate]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: A type declaration's layout can be given at most once.
       This declaration has an layout annotation (void) and a layout attribute ([@@immediate]).
|}]

type t : void = string [@@immediate]

[%%expect{|
Line 1, characters 0-36:
1 | type t : void = string [@@immediate]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: A type declaration's layout can be given at most once.
       This declaration has an layout annotation (void) and a layout attribute ([@@immediate]).
|}]
