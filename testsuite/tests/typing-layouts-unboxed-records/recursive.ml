(* TEST
 flambda2;
 include stdlib_upstream_compatible;
 flags = "-extension layouts_beta";
 {
   expect;
 }
*)

(* CR layouts v7.2: figure out the story for recursive unboxed products.
   Consider that the following is allowed upstream:
      type t = { t : t } [@@unboxed]
   We should also give good errors for infinite-size unboxed records (see the
   test at the bottom of this file with a depth-100 kind).
*)

(************************************)
(* Basic recursive unboxed products *)

type t : value = #{ t : t }
[%%expect{|
type t = #{ t : t; }
|}]

type t : float64 = #{ t : t }
[%%expect{|
type t = #{ t : t; }
|}]


type t : value = #{ t : t }
[%%expect{|
type t = #{ t : t; }
|}]

(* CR layouts v7.2: Once we support unboxed records with elements of kind [any],
   and detect bad recursive unboxed records with an occurs check, this error
   should improve.
*)
type bad = #{ bad : bad ; i : int}
[%%expect{|
Line 1, characters 0-34:
1 | type bad = #{ bad : bad ; i : int}
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "bad" is (value & value) & value
         because it is an unboxed record.
       But the layout of type "bad" must be a sublayout of value & value
         because it is an unboxed record.
|}]

(* It might be nice to reject, but it seems harmless to accept. *)
type bad = #{ bad : bad }
[%%expect{|
type bad = #{ bad : bad; }
|}]

(* It might be nice to reject, but it seems harmless to accept. *)
type a_bad = #{ b_bad : b_bad }
and b_bad = #{ a_bad : a_bad }
[%%expect{|
type a_bad = #{ b_bad : b_bad; }
and b_bad = #{ a_bad : a_bad; }
|}]

(* It might be nice to reject, but it seems harmless to accept. *)
type bad : any = #{ bad : bad }
[%%expect{|
type bad = #{ bad : bad; }
|}]

type 'a id = #{ a : 'a }
type bad = bad id
[%%expect{|
type 'a id = #{ a : 'a; }
Line 2, characters 0-17:
2 | type bad = bad id
    ^^^^^^^^^^^^^^^^^
Error: The type abbreviation "bad" is cyclic:
         "bad" = "bad id",
         "bad id" contains "bad"
|}]


type 'a bad = #{ bad : 'a bad ; u : 'a}
[%%expect{|
Line 1, characters 0-39:
1 | type 'a bad = #{ bad : 'a bad ; u : 'a}
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "bad" is (value & value) & value
         because it is an unboxed record.
       But the layout of type "bad" must be a sublayout of value & value
         because it is an unboxed record.
|}]

type 'a bad = { bad : 'a bad ; u : 'a}
[%%expect{|
type 'a bad = { bad : 'a bad; u : 'a; }
|}]

(****************************)
(* A particularly bad error *)

type bad : float64 = #{ bad : bad ; i : int}
[%%expect{|
Line 1, characters 0-44:
1 | type bad : float64 = #{ bad : bad ; i : int}
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "bad" is float64 & value
         because it is an unboxed record.
       But the layout of type "bad" must be a sublayout of float64
         because of the annotation on the declaration of the type bad.
|}]
