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
type t = { tl: t list } [@@unboxed]
[%%expect{|
type t = { tl : t list; } [@@unboxed]
|}]

module AbstractList : sig
  type 'a t
end = struct
  type 'a t = Cons of 'a * 'a list | Nil
end
[%%expect{|
module AbstractList : sig type 'a t end
|}]

type t = { tl: t AbstractList.t } [@@unboxed]
[%%expect{|
type t = { tl : t AbstractList.t; } [@@unboxed]
|}]

type 'a mylist = Cons of 'a * 'a list | Nil
and t = { t : t mylist } [@@unboxed]
[%%expect{|
type 'a mylist = Cons of 'a * 'a list | Nil
and t = { t : t mylist; } [@@unboxed]
|}]

(* Guarded by a function *)
type t = { f1 : t -> t } [@@unboxed]
[%%expect{|
type t = { f1 : t -> t; } [@@unboxed]
|}]

(* Guarded by a tuple *)
type a = { b : b } [@@unboxed]
and b = a * a
[%%expect{|
type a = { b : b; } [@@unboxed]
and b = a * a
|}]

(* Guarded by a function *)
type a = { b : b } [@@unboxed]
and b = { c : c } [@@unboxed]
and c = unit -> a
[%%expect{|
type a = { b : b; } [@@unboxed]
and b = { c : c; } [@@unboxed]
and c = unit -> a
|}]

(* Recursion through modules guarded by a function *)
module rec A : sig
  type t = { b1 : B.t } [@@unboxed]
end = struct
  type t = { b1 : B.t } [@@unboxed]
end
and B : sig
  type t = unit -> A.t
end = struct
  type t = unit -> A.t
end
[%%expect{|
module rec A : sig type t = { b1 : B.t; } [@@unboxed] end
and B : sig type t = unit -> A.t end
|}]

(**********************************)
(* Infinite-sized unboxed records *)

type a_bad = { b_bad : b_bad } [@@unboxed]
and b_bad = { a_bad : a_bad } [@@unboxed]
[%%expect{|
Line 1, characters 0-42:
1 | type a_bad = { b_bad : b_bad } [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "a_bad" is recursive without boxing:
         "a_bad" contains "b_bad",
         "b_bad" contains "a_bad"
|}]

type bad : any = { bad : bad } [@@unboxed]
[%%expect{|
Line 1, characters 0-42:
1 | type bad : any = { bad : bad } [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "bad" is recursive without boxing:
         "bad" contains "bad"
|}]

type bad = { x : u } [@@unboxed]
and u = T of bad [@@unboxed]
[%%expect{|
Line 1, characters 0-32:
1 | type bad = { x : u } [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "bad" is recursive without boxing:
         "bad" contains "u",
         "u" contains "bad"
|}]

type 'a record_id = { a : 'a } [@@unboxed]
type 'a alias_id = 'a
[%%expect{|
type 'a record_id = { a : 'a; } [@@unboxed]
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


type bad = #( s * s )
and ('a : any) record_id2 = { a : 'a } [@@unboxed]
and s = { u : u } [@@unboxed]
and u = #(int * bad record_id2)
[%%expect{|
Line 1, characters 0-21:
1 | type bad = #( s * s )
    ^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "bad" is recursive without boxing:
         "bad" = "#(s * s)",
         "#(s * s)" contains "s",
         "s" contains "u",
         "u" = "#(int * bad record_id2)",
         "#(int * bad record_id2)" contains "bad record_id2",
         "bad record_id2" contains "bad"
|}]

type bad = #( s * s )
and ('a : any) record_id2 = { a : 'a } [@@unboxed]
and s = { u : u } [@@unboxed]
and u = #(int * bad record_id2)
[%%expect{|
Line 1, characters 0-21:
1 | type bad = #( s * s )
    ^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "bad" is recursive without boxing:
         "bad" = "#(s * s)",
         "#(s * s)" contains "s",
         "s" contains "u",
         "u" = "#(int * bad record_id2)",
         "#(int * bad record_id2)" contains "bad record_id2",
         "bad record_id2" contains "bad"
|}]

(* We also check recursive types via modules *)
module rec Bad_rec1 : sig
  type t = #( s * s )
  and s = { u : Bad_rec2.u } [@@unboxed]
end = struct
  type t = #( s * s )
  and s = { u : Bad_rec2.u } [@@unboxed]
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
3 |   and s = { u : Bad_rec2.u } [@@unboxed]
4 | end = struct
5 |   type t = #( s * s )
6 |   and s = { u : Bad_rec2.u } [@@unboxed]
7 | end
Error: The definition of "Bad_rec1.t" is recursive without boxing:
         "Bad_rec1.t" = "#(Bad_rec1.s * Bad_rec1.s)",
         "#(Bad_rec1.s * Bad_rec1.s)" contains "Bad_rec1.s",
         "Bad_rec1.s" contains "Bad_rec2.u",
         "Bad_rec2.u" = "Bad_rec1.t Bad_rec2.id",
         "Bad_rec1.t Bad_rec2.id" = "Bad_rec1.t"
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
type a = { b : b M.opaque_id } [@@unboxed]
and b = { a : a M.opaque_id } [@@unboxed]
[%%expect{|
Line 1, characters 11-28:
1 | type a = { b : b M.opaque_id } [@@unboxed]
               ^^^^^^^^^^^^^^^^^
Error: Unboxed record element types must have a representable layout.
       The layout of b M.opaque_id is any
         because of the definition of opaque_id at line 2, characters 2-33.
       But the layout of b M.opaque_id must be representable
         because it is the type of record field b.
|}]

(* Make sure we look through [as] types *)

type 'a t = { x: ('a s as 'm) } [@@unboxed]
and 'b s = { x : 'b t } [@@unboxed]
[%%expect{|
Line 1, characters 0-43:
1 | type 'a t = { x: ('a s as 'm) } [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "t" is recursive without boxing:
         "'a t" contains "'a s",
         "'a s" contains "'a t"
|}]

(***************************************)
(* Singleton recursive unboxed records *)

type 'a safe = { a : 'a } [@@unboxed]
type x = int safe safe
[%%expect{|
type 'a safe = { a : 'a; } [@@unboxed]
type x = int safe safe
|}]

type 'a id = 'a
type x = { x : x id } [@@unboxed]
[%%expect{|
type 'a id = 'a
type x = { x : x id; } [@@unboxed]
|}]

(* CR layouts v7.2: allow bounded repetition of the same type constructor of
   unboxed records. *)
type 'a safe = { a : 'a } [@@unboxed]
and x = int safe safe
[%%expect{|
Line 2, characters 0-21:
2 | and x = int safe safe
    ^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "x" is recursive without boxing:
         "x" = "int safe safe",
         "int safe safe" contains "int safe"
|}]

(* We could allow these, as although they have unguarded recursion,
   they are finite size (thanks to the fact that we represent single-field
   records as the layout of the field rather than as a singleton product).
   However, allowing them makes checking for recursive types more difficult,
   and they are uninhabitable anyway. *)

type bad : value = { bad : bad } [@@unboxed]
[%%expect{|
Line 1, characters 0-44:
1 | type bad : value = { bad : bad } [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "bad" is recursive without boxing:
         "bad" contains "bad"
|}]

type bad : float64 = { bad : bad } [@@unboxed]
[%%expect{|
Line 1, characters 23-32:
1 | type bad : float64 = { bad : bad } [@@unboxed]
                           ^^^^^^^^^
Error: Type "bad/2" has layout "float64".
       Unboxed records may not yet contain types of this layout.
|}]


type bad : value = { bad : bad } [@@unboxed]
[%%expect{|
Line 1, characters 0-44:
1 | type bad : value = { bad : bad } [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "bad" is recursive without boxing:
         "bad" contains "bad"
|}]

type bad = { bad : bad } [@@unboxed]
[%%expect{|
Line 1, characters 0-36:
1 | type bad = { bad : bad } [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "bad" is recursive without boxing:
         "bad" contains "bad"
|}]

(* We actually can create singleton recursive unboxed record types,
   through recursive modules *)

module F (X : sig type t end) = struct
  type u = { u : X.t } [@@unboxed]
end

module rec M : sig
  type u
  type t = u
end = struct
  include F(M)
  type t = u
end
[%%expect{|
module F :
  functor (X : sig type t end) -> sig type u = { u : X.t; } [@@unboxed] end
module rec M : sig type u type t = u end
|}]

module F (X : sig
    type u
    type t = { u : u } [@@unboxed]
  end) = struct
  type u = X.t = { u : X.u } [@@unboxed]
end

module rec M : sig
  type u
  type t = { u : u } [@@unboxed]
end = struct
  include F(M)
  type t = { u : u } [@@unboxed]
  let rec u = { u } [@@unboxed]
end
[%%expect{|
module F :
  functor (X : sig type u type t = { u : u; } [@@unboxed] end) ->
    sig type u = X.t = { u : X.u; } [@@unboxed] end
Line 14, characters 14-19:
14 |   let rec u = { u } [@@unboxed]
                   ^^^^^
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}]
