(* TEST
 flags = "-extension layouts_beta";
 expect;
*)

(*******************)
(* Type parameters *)


type bad = (int, int) t#
and 'a t = float
[%%expect{|
Line 1, characters 11-24:
1 | type bad = (int, int) t#
               ^^^^^^^^^^^^^
Error: The type constructor "t#" expects 1 argument(s),
       but is here applied to 2 argument(s)
|}]

type bad = t#
and 'a t = float
[%%expect{|
Line 1, characters 11-13:
1 | type bad = t#
               ^^
Error: The type constructor "t#" expects 1 argument(s),
       but is here applied to 0 argument(s)
|}]


type 'a r = { a : 'a }
and intr = int r
let bad : type a. a r -> intr = fun x -> x
[%%expect{|
type 'a r = { a : 'a; }
and intr = int r
Line 3, characters 41-42:
3 | let bad : type a. a r -> intr = fun x -> x
                                             ^
Error: This expression has type "a r" but an expression was expected of type
         "intr" = "int r"
       Type "a" is not compatible with type "int"
|}]

type bad = (int, int) t#
and 'a t = { i : 'a }
[%%expect{|
Line 1, characters 11-24:
1 | type bad = (int, int) t#
               ^^^^^^^^^^^^^
Error: The type constructor "t#" expects 1 argument(s),
       but is here applied to 2 argument(s)
|}]

type bad = t#
and 'a t = { i : 'a }
[%%expect{|
Line 1, characters 11-13:
1 | type bad = t#
               ^^
Error: The type constructor "t#" expects 1 argument(s),
       but is here applied to 0 argument(s)
|}]


type 'a t = { s : int }
and 'a s = 'a t#
[%%expect{|
type 'a t = { s : int; }
and 'a s = 'a t#
|}]

type 'a r = { a : int }
module type S = sig
  type 'a t = { a : 'a }
end with type 'a t = 'a r
[%%expect{|
type 'a r = { a : int; }
Line 4, characters 9-25:
4 | end with type 'a t = 'a r
             ^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type "'a r"
       Fields do not match:
         "a : int;"
       is not the same as:
         "a : 'a;"
       The type "int" is not equal to the type "'a"
|}]

(* Non-existent #-type *)
type bad = a#
[%%expect{|
Line 1, characters 11-13:
1 | type bad = a#
               ^^
Error: Unbound type constructor "a"
|}]

type t = float
[%%expect{|
type t = float
|}]

(* Alias gets its own #-type *)
type u = t#
[%%expect{|
type u = t#
|}]

(* Shadowing hides #-type *)
type float
[%%expect{|
type float
|}]

type bad = float#
[%%expect{|
Line 1, characters 11-17:
1 | type bad = float#
               ^^^^^^
Error: "float" has no unboxed version.
|}]

(* But it's still accessible via the alias *)
type u2 = t#
[%%expect{|
type u2 = t#
|}]

module M = struct
  type t = int32
end
[%%expect{|
module M : sig type t = int32 end
|}]

type int32_u = M.t#
[%%expect{|
type int32_u = M.t#
|}]

(* Restore float *)
type float = t
[%%expect{|
type float = t
|}]


type 'a t = float
type s = int t#
type v = string t#

let (_ : s -> v) = fun x -> x
[%%expect{|
type 'a t = float
type s = int t#
type v = string t#
- : s -> v = <fun>
|}]

type t = float
and s = t#
[%%expect{|
type t = float
and s = t#
|}]

type t = int
and s = t#
[%%expect{|
Line 2, characters 0-10:
2 | and s = t#
    ^^^^^^^^^^
Error: "t" has no unboxed version.
|}]

type t = s#
and s = t#
[%%expect{|
Line 1, characters 0-11:
1 | type t = s#
    ^^^^^^^^^^^
Error: "s" has no unboxed version.
|}]

type t = s#
and s = float
[%%expect{|
type t = s#
and s = float
|}]

type t = float
and s = t#
[%%expect{|
type t = float
and s = t#
|}]

module rec M : sig
  type t = M2.t#
end = struct
  type t = M2.t#
end
and M2 : sig
  type t = float
end = struct
  type t = float
end
[%%expect{|
module rec M : sig type t = M2.t# end
and M2 : sig type t = float end
|}]

module rec M : sig
  type t = M2.t#
end = struct
  type t = M2.t#
end
and M2 : sig
  type t = int
end = struct
  type t = int
end
[%%expect{|
Line 2, characters 11-16:
2 |   type t = M2.t#
               ^^^^^
Error: "M2.t" has no unboxed version.
|}]

module rec Bad1 : sig
  type t = Bad2.t#
end = struct
  type t = Bad2.t#
end
and Bad2 : sig
  type t = Bad1.t#
end = struct
  type t = Bad1.t#
end
[%%expect{|
Line 2, characters 11-18:
2 |   type t = Bad2.t#
               ^^^^^^^
Error: "Bad2.t" has no unboxed version.
|}]

type t = { i : int ; j : int }
type s = t#
[%%expect{|
type t = { i : int; j : int; }
type s = t#
|}]

type t = { i : int ; j : int }
and s = t#
[%%expect{|
type t = { i : int; j : int; }
and s = t#
|}]

let x () = #{ i = 1 ; j = 2 }
[%%expect{|
val x : unit -> t# = <fun>
|}]

type t = { s : s# }
and s = { t : t# }
[%%expect{|
Line 1, characters 0-19:
1 | type t = { s : s# }
    ^^^^^^^^^^^^^^^^^^^
Error: The definition of "t#" is recursive without boxing:
         "t#" contains "s#",
         "s#" contains "t#"
|}]



module M = struct
  type t = {x:int}
  module N = struct
    type b = t
    type u = t#
  end
end
[%%expect{|
module M :
  sig type t = { x : int; } module N : sig type b = t type u = t# end end
|}]

type a = b
and b = int
type bad = a#
[%%expect{|
type a = b
and b = int
Line 3, characters 11-13:
3 | type bad = a#
               ^^
Error: "a" has no unboxed version.
|}]

type t = a = { i : int }
and a = { i : int }
[%%expect{|
type t = a = { i : int; }
and a = { i : int; }
|}]

type a = { i : int }
and t = a = { i : int }
[%%expect{|
type a = { i : int; }
and t = a = { i : int; }
|}]


(* Recursive definitions with boxed record and its implicit version *)
module M : sig
  type t_u : value & float64
end = struct
  type t_u = t#
  and t = { s : string ; f : float# }
end
[%%expect{|
module M : sig type t_u : value & float64 end
|}]

type t = { s : s# }
and s = { t : t }
[%%expect{|
type t = { s : s#; }
and s = { t : t; }
|}]

(* Implicit unboxed record in module *)
module M = struct
  type t = { i : int }
end
[%%expect{|
module M : sig type t = { i : int; } end
|}]

let x = M.(#{ i = 1 })
[%%expect{|
val x : M.t# = #{i = 1}
|}]


(* Make sure we complain about mismatched kinds before mismatched existence of
   unboxed versions *)
type a = { i : int }
and b = a = { i : int } [@@unboxed]
[%%expect{|
Line 2, characters 0-35:
2 | and b = a = { i : int } [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type "a"
       Their internal representations differ:
       this definition uses unboxed representation.
|}]

(* Check the below have sensible errors *)
type a = { i : int }
type b = a = { i : int } [@@unboxed]
and s = b#
[%%expect{|
type a = { i : int; }
Line 3, characters 0-10:
3 | and s = b#
    ^^^^^^^^^^
Error: "b" has no unboxed version.
|}]

type a = { i : int } [@@unboxed]
type b = a = { i : int }
and s = b#
[%%expect{|
type a = { i : int; } [@@unboxed]
Line 3, characters 0-10:
3 | and s = b#
    ^^^^^^^^^^
Error: "b" has no unboxed version.
|}]

(**********************************)
(* Implicit unboxed records kinds *)

type t = { i : int }
type s = t# = #{ i : int }
[%%expect{|
type t = { i : int; }
type s = t# = #{ i : int; }
|}]

(* Implicit unboxed records get the same modalities as the boxed records, which
   are affected by field mutability. *)

(* CR layouts v7.2: once global implies unyielding, remove
   "unyielding" below *)
type 'a t = { mutable s : string ; a : 'a @@ global }
type 'a s = 'a t# = #{
  s : string @@ global many aliased unyielding;
  a : 'a @@ global
}
[%%expect{|
type 'a t = { mutable s : string; global_ a : 'a; }
type 'a s = 'a t# = #{ global_ s : string @@ many aliased; global_ a : 'a; }
|}]

(* Modality mismatch *)
type t = { mutable s : string }
type bad = t# = #{ s : string }
[%%expect{|
type t = { mutable s : string; }
Line 2, characters 0-31:
2 | type bad = t# = #{ s : string }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type "t#"
       Fields do not match:
         "global_ s : string @@ many aliased;"
       is not the same as:
         "s : string;"
       This is empty and the original is aliased.
|}]

(* This errors (as it also does for the analogous test for boxed records, see
   below) *)
type t = { i : int ; j : int }
type s = t
type u = s# = #{ i : int ; j : int }
[%%expect{|
type t = { i : int; j : int; }
type s = t
Line 3, characters 0-36:
3 | type u = s# = #{ i : int ; j : int }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type "s#"
       The original is abstract, but this is an unboxed record.
|}]

type t = { i : int ; j : int }
type s = t
type u = s = { i : int ; j : int }
[%%expect{|
type t = { i : int; j : int; }
type s = t
Line 3, characters 0-34:
3 | type u = s = { i : int ; j : int }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type "s"
       The original is abstract, but this is a record.
|}]

(******************************************************************)
(* Unboxed records can shadow labels for implicit unboxed records *)

type t = { a : int }
type u = #{ a : bool }
let f x = x.a
let g x = x.#a
[%%expect{|
type t = { a : int; }
type u = #{ a : bool; }
val f : t -> int = <fun>
val g : u -> bool = <fun>
|}]
