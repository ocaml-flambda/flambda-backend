(* TEST
 flambda2;
 include stdlib_upstream_compatible;
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
Error:
       The layout of bad is any & any
         because it is an unboxed record.
       But the layout of bad must be representable
         because it is the type of record field bad.
|}]

type bad = #{ bad : bad }
[%%expect{|
Line 1, characters 0-25:
1 | type bad = #{ bad : bad }
    ^^^^^^^^^^^^^^^^^^^^^^^^^
Error:
       The layout of bad is any
         because a dummy kind of any is used to check mutually recursive datatypes.
                 Please notify the Jane Street compilers group if you see this output.
       But the layout of bad must be representable
         because it is the type of record field bad.
|}]

type a_bad = #{ b_bad : b_bad }
and b_bad = #{ a_bad : a_bad }
[%%expect{|
Line 1, characters 0-31:
1 | type a_bad = #{ b_bad : b_bad }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error:
       The layout of a_bad is any
         because a dummy kind of any is used to check mutually recursive datatypes.
                 Please notify the Jane Street compilers group if you see this output.
       But the layout of a_bad must be representable
         because it is the type of record field a_bad.
|}]

type bad : any = #{ bad : bad }
[%%expect{|
Line 1, characters 0-31:
1 | type bad : any = #{ bad : bad }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error:
       The layout of bad is any
         because of the annotation on the declaration of the type bad.
       But the layout of bad must be representable
         because it is the type of record field bad.
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
Error:
       The layout of 'a bad is any & any
         because it is an unboxed record.
       But the layout of 'a bad must be representable
         because it is the type of record field bad.
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
