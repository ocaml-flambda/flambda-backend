(* TEST
 flambda2;
 include stdlib_upstream_compatible;
 {
   expect;
 }
*)

(* NOTE: When adding tests to this file, also update
   [typing-layouts-products/recursive.ml] *)

(* We only allow recursion of unboxed product types through boxing, otherwise
   the type is uninhabitable and usually also infinite-size. *)

(***********************************************)
(* Allowed (guarded) recursive unboxed records *)

(* Guarded by `list` *)
type t = { tl: t# list }
[%%expect{|
type t = { tl : t# list; }
|}]

module AbstractList : sig
  type 'a t
end = struct
  type 'a t = Cons of 'a * 'a list | Nil
end
[%%expect{|
module AbstractList : sig type 'a t end
|}]

type t = { tl: t# AbstractList.t }
[%%expect{|
type t = { tl : t# AbstractList.t; }
|}]

type 'a mylist = Cons of 'a * 'a list | Nil
and t = { t : t# mylist }
[%%expect{|
type 'a mylist = Cons of 'a * 'a list | Nil
and t = { t : t# mylist; }
|}]

(* This passes the unboxed recursion check (as [pair] always has jkind
   [value & value], [(int, bad) pair] is indeed finite-size, but it fails the
   jkind check *)
type ('a, 'b) pair = { a : 'a ; b : 'b }
type bad = { bad : (int, bad#) pair# }
[%%expect{|
type ('a, 'b) pair = { a : 'a; b : 'b; }
Line 2, characters 0-38:
2 | type bad = { bad : (int, bad#) pair# }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error:
       The layout of bad# is value & value
         because it is an unboxed record.
       But the layout of bad# must be a sublayout of value
         because of the definition of pair at line 1, characters 0-40.
|}]

(* This fails the unboxed recursion check; we must look into [pair] since it's
   part of the same mutually recursive type decl. *)
type ('a, 'b) pair = { a : 'a ; b : 'b }
and bad = { bad : (int, bad#) pair# }
[%%expect{|
Line 2, characters 0-37:
2 | and bad = { bad : (int, bad#) pair# }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "bad#" is recursive without boxing:
         "bad#" contains "(int, bad#) pair#",
         "(int, bad#) pair#" contains "bad#"
|}]

(* Guarded by a function *)
type t = { f1 : t# -> t# ; f2 : t# -> t# }
[%%expect{|
type t = { f1 : t# -> t#; f2 : t# -> t#; }
|}]

(* Guarded by a tuple *)
type a = { b : b }
and b = a# * a#
[%%expect{|
type a = { b : b; }
and b = a# * a#
|}]

(* Guarded by a function *)
type a = { b : b# }
and b = { c : c }
and c = unit -> a#
[%%expect{|
type a = { b : b#; }
and b = { c : c; }
and c = unit -> a#
|}]

(* Recursion through modules guarded by a function *)
module rec A : sig
  type t = { b1 : B.t ; b2 : B.t }
end = struct
  type t = { b1 : B.t ; b2 : B.t }
end
and B : sig
  type t = unit -> A.t#
end = struct
  type t = unit -> A.t#
end
[%%expect{|
module rec A : sig type t = { b1 : B.t; b2 : B.t; } end
and B : sig type t = unit -> A.t# end
|}]

(**********************************)
(* Infinite-sized unboxed records *)

type bad = { bad : bad# ; i : int}
[%%expect{|
Line 1, characters 0-34:
1 | type bad = { bad : bad# ; i : int}
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "bad#" is recursive without boxing:
         "bad#" contains "bad#"
|}]

type a_bad = { b_bad : b_bad# }
and b_bad = { a_bad : a_bad# }
[%%expect{|
Line 1, characters 0-31:
1 | type a_bad = { b_bad : b_bad# }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "a_bad#" is recursive without boxing:
         "a_bad#" contains "b_bad#",
         "b_bad#" contains "a_bad#"
|}]

type bad : any = { bad : bad# }
[%%expect{|
Line 1, characters 0-31:
1 | type bad : any = { bad : bad# }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "bad#" is recursive without boxing:
         "bad#" contains "bad#"
|}]

type bad = { x : u }
and u = T of bad# [@@unboxed]
[%%expect{|
Line 1, characters 0-20:
1 | type bad = { x : u }
    ^^^^^^^^^^^^^^^^^^^^
Error: The definition of "bad#" is recursive without boxing:
         "bad#" contains "u",
         "u" contains "bad#"
|}]

type 'a record_id = { a : 'a }
type 'a alias_id = 'a
[%%expect{|
type 'a record_id = { a : 'a; }
type 'a alias_id = 'a
|}]

type bad = bad record_id#
[%%expect{|
Line 1, characters 0-25:
1 | type bad = bad record_id#
    ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type abbreviation "bad" is cyclic:
         "bad" = "bad record_id#",
         "bad record_id#" contains "bad"
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


type 'a bad = { bad : 'a bad# ; u : 'a}
[%%expect{|
Line 1, characters 0-39:
1 | type 'a bad = { bad : 'a bad# ; u : 'a}
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "bad#" is recursive without boxing:
         "'a bad#" contains "'a bad#"
|}]

type 'a bad = { bad : 'a bad# ; u : 'a}
[%%expect{|
Line 1, characters 0-39:
1 | type 'a bad = { bad : 'a bad# ; u : 'a}
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "bad#" is recursive without boxing:
         "'a bad#" contains "'a bad#"
|}]

type bad : float64 = { bad : bad# ; i : int}
[%%expect{|
Line 1, characters 0-44:
1 | type bad : float64 = { bad : bad# ; i : int}
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "bad#" is recursive without boxing:
         "bad#" contains "bad#"
|}]

type bad = { a : bad# ; b : bad# }
[%%expect{|
Line 1, characters 0-34:
1 | type bad = { a : bad# ; b : bad# }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "bad#" is recursive without boxing:
         "bad#" contains "bad#"
|}]

type 'a bad = { a : 'a bad# }
[%%expect{|
Line 1, characters 0-29:
1 | type 'a bad = { a : 'a bad# }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "bad#" is recursive without boxing:
         "'a bad#" contains "'a bad#"
|}]

type bad = #( s# * s# )
and ('a : any) record_id2 = { a : 'a }
and s = { u : u }
and u = #(int * bad record_id2#)
[%%expect{|
Line 1, characters 0-23:
1 | type bad = #( s# * s# )
    ^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "bad" is recursive without boxing:
         "bad" = "#(s# * s#)",
         "#(s# * s#)" contains "s#",
         "s#" contains "u",
         "u" = "#(int * bad record_id2#)",
         "#(int * bad record_id2#)" contains "bad record_id2#",
         "bad record_id2#" contains "bad"
|}]

type bad = #( s# * s# )
and ('a : any) record_id2 = { a : 'a }
and s = { u : u }
and u = #(int * bad record_id2#)
[%%expect{|
Line 1, characters 0-23:
1 | type bad = #( s# * s# )
    ^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "bad" is recursive without boxing:
         "bad" = "#(s# * s#)",
         "#(s# * s#)" contains "s#",
         "s#" contains "u",
         "u" = "#(int * bad record_id2#)",
         "#(int * bad record_id2#)" contains "bad record_id2#",
         "bad record_id2#" contains "bad"
|}]

(* We also check recursive types via modules *)
module rec Bad_rec1 : sig
  type t = #( s# * s# )
  and s = { u : Bad_rec2.u }
end = struct
  type t = #( s# * s# )
  and s = { u : Bad_rec2.u }
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
2 |   type t = #( s# * s# )
3 |   and s = { u : Bad_rec2.u }
4 | end = struct
5 |   type t = #( s# * s# )
6 |   and s = { u : Bad_rec2.u }
7 | end
Error: The definition of "Bad_rec1.t" is recursive without boxing:
         "Bad_rec1.t" = "#(Bad_rec1.s# * Bad_rec1.s#)",
         "#(Bad_rec1.s# * Bad_rec1.s#)" contains "Bad_rec1.s#",
         "Bad_rec1.s#" contains "Bad_rec2.u",
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
type a = { b : b# M.opaque_id }
and b = { a : a# M.opaque_id }
[%%expect{|
Line 1, characters 11-29:
1 | type a = { b : b# M.opaque_id }
               ^^^^^^^^^^^^^^^^^^
Error: Record element types must have a representable layout.
       The layout of b# M.opaque_id is any
         because of the definition of opaque_id at line 2, characters 2-33.
       But the layout of b# M.opaque_id must be representable
         because it is the type of record field b.
|}]

(* Make sure we look through [as] types *)

type 'a t = { x: ('a s# as 'm) list ; m : 'm }
and 'b s = { x : 'b t# }
[%%expect{|
Line 1, characters 0-46:
1 | type 'a t = { x: ('a s# as 'm) list ; m : 'm }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "t#" is recursive without boxing:
         "'a t#" contains "'a s#",
         "'a s#" contains "'a t#"
|}]

type 'a t = { x: ('a s# as 'm) }
and 'b s = { x : 'b t# }
[%%expect{|
Line 1, characters 0-32:
1 | type 'a t = { x: ('a s# as 'm) }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "t#" is recursive without boxing:
         "'a t#" contains "'a s#",
         "'a s#" contains "'a t#"
|}]

(***************************************)
(* Singleton recursive unboxed records *)

type 'a safe = { a : 'a }
type x = int safe# safe#
[%%expect{|
type 'a safe = { a : 'a; }
type x = int safe# safe#
|}]

type 'a id = 'a
type x = { x : x# id }
[%%expect{|
type 'a id = 'a
type x = { x : x# id; }
|}]

(* CR layouts v7.2: allow bounded repetition of the same type constructor of
   unboxed records. *)
type 'a safe = { a : 'a }
and x = int safe# safe#
[%%expect{|
Line 2, characters 0-23:
2 | and x = int safe# safe#
    ^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "x" is recursive without boxing:
         "x" = "int safe# safe#",
         "int safe# safe#" contains "int safe#"
|}]

(* We could allow these, as although they have unguarded recursion,
   they are finite size (thanks to the fact that we represent single-field
   records as the layout of the field rather than as a singleton product).
   However, allowing them makes checking for recursive types more difficult,
   and they are uninhabitable anyway. *)

type bad : value = { bad : bad# }
[%%expect{|
Line 1, characters 0-33:
1 | type bad : value = { bad : bad# }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "bad#" is recursive without boxing:
         "bad#" contains "bad#"
|}]

type bad = { bad : bad# }
[%%expect{|
Line 1, characters 0-25:
1 | type bad = { bad : bad# }
    ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The definition of "bad#" is recursive without boxing:
         "bad#" contains "bad#"
|}]

(* This should still error once unboxed records elements need not have a
   representable layout *)
module type S = sig
  type u : any
  type t = { a : u ; b : u }
end
module F (X : S) = struct
  type u = X.t# = #{ a : X.u ; b : X.u}
end

module rec M : S = struct
  include F(M)
  type t = { a : u ; b : u }
  let rec u = #{ u ; u }
end
[%%expect{|
Line 3, characters 13-20:
3 |   type t = { a : u ; b : u }
                 ^^^^^^^
Error: Record element types must have a representable layout.
       The layout of u is any
         because of the definition of u at line 2, characters 2-14.
       But the layout of u must be representable
         because it is the type of record field a.
|}]

(* CR layouts v7.2: improve this error message *)
type ('a : value & float64) t
type bad = r# t
and r = { x:int; y:bool }
[%%expect{|
type ('a : value & float64) t
Line 3, characters 0-25:
3 | and r = { x:int; y:bool }
    ^^^^^^^^^^^^^^^^^^^^^^^^^
Error:
       The kind of r# is value_or_null & float64
         because it is an unboxed record.
       But the kind of r# must be a subkind of value & float64
         because of the definition of t at line 1, characters 0-29.
|}]
