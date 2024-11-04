(* TEST
 flambda2;
 include stdlib_upstream_compatible;
 flags = "-extension layouts_alpha";
 {
   expect;
 }
*)

type 'a bad = #{ bad : 'a bad ; u : 'a}
[%%expect{|
Line 1, characters 0-39:
1 | type 'a bad = #{ bad : 'a bad ; u : 'a}
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error:
       The layout of 'a bad is any & any
         because it is an unboxed record.
       But the layout of 'a bad must be representable
         because it is the type of record field bad.
|}]

(* But the above is OK for boxed records *)
type 'a t = { t : 'a t ; u : 'a}
[%%expect{|
type 'a t = { t : 'a t; u : 'a; }
|}]

(* ^^^^^^ Move the above to basics.ml ^^^^^ *)

type bad : void = #{ bad : bad }
[%%expect{|
Line 1, characters 0-32:
1 | type bad : void = #{ bad : bad }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Records must contain at least one runtime value.
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

(* This test as placed at the bottom because it pollutes whatever comes after it: *)

(* CR rtjoa: this shouldn't overflow, but it should error 1-1 *)
type bad : float64 = #{ bad : bad ; i : int}
[%%expect{|
Line 1, characters 0-44:
1 | type bad : float64 = #{ bad : bad ; i : int}
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "bad" is ((((((((((((((((((((((((((((((((((((
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (
                                                                    (float64 & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value) & value
         because it is an unboxed record.
       But the layout of type "bad" must be a sublayout of float64
         because of the annotation on the declaration of the type bad.
|}]
