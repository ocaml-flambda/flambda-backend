(* TEST
 flambda2;
 include stdlib_upstream_compatible;
 flags = "-extension layouts_beta";
 {
   expect;
 }
*)

(* We only allow recursion of unboxed product types through boxing, otherwise
   the type is uninhabitable and usually also infinite-size. *)

(***********************************************)
(* Allowed (guarded) recursive unboxed records *)

(* Guarded by `list` *)
type t = #{ tl: t list }
[%%expect{|
type t = #{ tl : t list; }
|}]

(* Guarded by a function *)
type t = #{ f1 : t -> t ; f2 : t -> t }
[%%expect{|
type t = #{ f1 : t -> t; f2 : t -> t; }
|}]

(* Guarded by a tuple *)
type a = #{ b : b }
and b = a * a
[%%expect{|
type a = #{ b : b; }
and b = a * a
|}]

(* Guarded by a function *)
type a = #{ b : b }
and b = #{ c1 : c ; c2 : c }
and c = unit -> a
[%%expect{|
type a = #{ b : b; }
and b = #{ c1 : c; c2 : c; }
and c = unit -> a
|}]

(* Recursion through modules guarded by a function *)
module rec A : sig
  type t = #{ b1 : B.t ; b2 : B.t }
end = struct
  type t = #{ b1 : B.t ; b2 : B.t }
end
and B : sig
  type t = unit -> A.t
end = struct
  type t = unit -> A.t
end
[%%expect{|
module rec A : sig type t = #{ b1 : B.t; b2 : B.t; } end
and B : sig type t = unit -> A.t end
|}]

(**********************************)
(* Infinite-sized unboxed records *)

type bad = #{ bad : bad ; i : int}
[%%expect{|
Line 1, characters 0-34:
1 | type bad = #{ bad : bad ; i : int}
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "bad" is recursive without boxing:
         "bad" contains "bad"
|}]

type a_bad = #{ b_bad : b_bad }
and b_bad = #{ a_bad : a_bad }
[%%expect{|
Line 1, characters 0-31:
1 | type a_bad = #{ b_bad : b_bad }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "a_bad" is recursive without boxing:
         "a_bad" contains "a_bad"
|}]

type bad : any = #{ bad : bad }
[%%expect{|
Line 1, characters 0-31:
1 | type bad : any = #{ bad : bad }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "bad" is recursive without boxing:
         "bad" contains "bad"
|}]

type 'a record_id = #{ a : 'a }
type 'a alias_id = 'a
[%%expect{|
type 'a record_id = #{ a : 'a; }
type 'a alias_id = 'a
|}]

type bad = bad record_id
[%%expect{|
Line 1, characters 0-24:
1 | type bad = bad record_id
    ^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type abbreviation "bad" is cyclic:
         "bad" = "bad record_id",
         "bad record_id" contains "bad"
|}]

type bad = bad alias_id
[%%expect{|
Line 1, characters 0-23:
1 | type bad = bad alias_id
    ^^^^^^^^^^^^^^^^^^^^^^^
Error: The type abbreviation "bad" is cyclic:
         "bad" = "bad alias_id",
         "bad alias_id" = "bad"
|}]


type 'a bad = #{ bad : 'a bad ; u : 'a}
[%%expect{|
Line 1, characters 0-39:
1 | type 'a bad = #{ bad : 'a bad ; u : 'a}
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "bad" is recursive without boxing:
         "'a bad" contains "'a bad"
|}]

type 'a bad = { bad : 'a bad ; u : 'a}
[%%expect{|
type 'a bad = { bad : 'a bad; u : 'a; }
|}]

type bad : float64 = #{ bad : bad ; i : int}
[%%expect{|
Line 1, characters 0-44:
1 | type bad : float64 = #{ bad : bad ; i : int}
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "bad" is recursive without boxing:
         "bad" contains "bad"
|}]

type bad = #{ a : t ; b : t }
[%%expect{|
type bad = #{ a : t; b : t; }
|}]

type 'a bad = #{ a : 'a bad }
[%%expect{|
Line 1, characters 0-29:
1 | type 'a bad = #{ a : 'a bad }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "bad" is recursive without boxing:
         "'a bad" contains "'a bad"
|}]

type bad = #( s * s )
and ('a : any) record_id2 = #{ a : 'a }
and s = #{ u : u }
and u = #(int * bad record_id2)
[%%expect{|
Line 1, characters 0-21:
1 | type bad = #( s * s )
    ^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "bad" is recursive without boxing:
         "bad" = "#(s * s)",
         "#(s * s)" contains "u",
         "u" = "#(int * bad record_id2)",
         "#(int * bad record_id2)" contains "bad"
|}]

type bad = #( s * s )
and ('a : any) record_id2 = #{ a : 'a }
and s = #{ u : u }
and u = #(int * bad alias_id record_id2)
[%%expect{|
Line 1, characters 0-21:
1 | type bad = #( s * s )
    ^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "bad" is recursive without boxing:
         "bad" = "#(s * s)",
         "#(s * s)" contains "u",
         "u" = "#(int * bad alias_id record_id2)",
         "#(int * bad alias_id record_id2)" contains "bad alias_id",
         "bad alias_id" = "bad",
         "bad" = "#(s * s)",
         "#(s * s)" contains "s"
|}]

(* We also check recursive types via modules *)
module rec Bad_rec1 : sig
  type t = #( s * s )
  and s = #{ u : Bad_rec2.u }
end = struct
  type t = #( s * s )
  and s = #{ u : Bad_rec2.u }
end
and Bad_rec2 : sig
  type u = Bad_rec1.t id
  and 'a id = 'a
end = struct
  type u = Bad_rec1.t id
  and 'a id = 'a
end
[%%expect{|
Lines 1-7, characters 0-3:
1 | module rec Bad_rec1 : sig
2 |   type t = #( s * s )
3 |   and s = #{ u : Bad_rec2.u }
4 | end = struct
5 |   type t = #( s * s )
6 |   and s = #{ u : Bad_rec2.u }
7 | end
Error: The definition of "Bad_rec1.t" is recursive without boxing:
         "Bad_rec1.t" = "#(Bad_rec1.s * Bad_rec1.s)",
         "#(Bad_rec1.s * Bad_rec1.s)" contains "Bad_rec2.u",
         "Bad_rec2.u" = "Bad_rec1.t Bad_rec2.id",
         "Bad_rec1.t Bad_rec2.id" = "Bad_rec1.t",
         "Bad_rec1.t" = "#(Bad_rec1.s * Bad_rec1.s)",
         "#(Bad_rec1.s * Bad_rec1.s)" contains "Bad_rec1.s"
|}]

(* When we allow records with elements of unrepresentable layout, this should
   still be disallowed. *)
module M : sig
  type ('a : any) opaque_id : any
end = struct
  type ('a : any) opaque_id = 'a
end
[%%expect{|
module M : sig type ('a : any) opaque_id : any end
|}]
type a = #{ b : b M.opaque_id }
and b = #{ a : a M.opaque_id }
[%%expect{|
Line 1, characters 12-29:
1 | type a = #{ b : b M.opaque_id }
                ^^^^^^^^^^^^^^^^^
Error: Unboxed record element types must have a representable layout.
       The layout of b M.opaque_id is any
         because of the definition of opaque_id at line 2, characters 2-33.
       But the layout of b M.opaque_id must be representable
         because it is the type of record field b.
|}]

(***************************************)
(* Singleton recursive unboxed records *)

(* We could allow these, as although they have unguarded recursion,
   they are finite size (thanks to the fact that we represent single-field
   records as the layout of the field rather than as a singleton product).
   However, allowing them makes checking for recursive types more difficult,
   and they are uninhabitable anyway. *)

type bad : value = #{ bad : bad }
[%%expect{|
Line 1, characters 0-33:
1 | type bad : value = #{ bad : bad }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "bad" is recursive without boxing:
         "bad" contains "bad"
|}]

type bad : float64 = #{ bad : bad }
[%%expect{|
Line 1, characters 0-35:
1 | type bad : float64 = #{ bad : bad }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "bad" is recursive without boxing:
         "bad" contains "bad"
|}]


type bad : value = #{ bad : bad }
[%%expect{|
Line 1, characters 0-33:
1 | type bad : value = #{ bad : bad }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "bad" is recursive without boxing:
         "bad" contains "bad"
|}]

type bad = #{ bad : bad }
[%%expect{|
Line 1, characters 0-25:
1 | type bad = #{ bad : bad }
    ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "bad" is recursive without boxing:
         "bad" contains "bad"
|}]

(* We actually can create singleton recursive unboxed record types,
   through recursive modules *)

module F (X : sig type t end) = struct
  type u = #{ u : X.t }
end

module rec M : sig
  type u
  type t = u
end = struct
  include F(M)
  type t = u
end
[%%expect{|
module F : functor (X : sig type t end) -> sig type u = #{ u : X.t; } end
module rec M : sig type u type t = u end
|}]

module F (X : sig
    type u
    type t = #{ u : u }
  end) = struct
  type u = X.t = #{ u : X.u }
end

module rec M : sig
  type u
  type t = #{ u : u }
end = struct
  include F(M)
  type t = #{ u : u }
  let rec u = #{ u }
end
[%%expect{|
module F :
  functor (X : sig type u type t = #{ u : u; } end) ->
    sig type u = X.t = #{ u : X.u; } end
Line 14, characters 14-20:
14 |   let rec u = #{ u }
                   ^^^^^^
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}]
