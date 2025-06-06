(* TEST
    expect;
*)

let use_global : 'a @ global -> unit = fun _ -> ()
let use_unique : 'a @ unique -> unit = fun _ -> ()
let use_uncontended : 'a @ uncontended -> unit = fun _ -> ()
let use_portable : 'a @ portable -> unit = fun _ -> ()
let use_many : 'a @ many -> unit = fun _ -> ()
type ('a : value mod contended) require_contended
type ('a : value mod portable) require_portable
[%%expect{|
val use_global : 'a -> unit = <fun>
val use_unique : 'a @ unique -> unit = <fun>
val use_uncontended : 'a -> unit = <fun>
val use_portable : 'a @ portable -> unit = <fun>
val use_many : 'a -> unit = <fun>
type ('a : value mod contended) require_contended
type ('a : value mod portable) require_portable
|}]

(**********************************************)
(* TEST: Mode crossing looks through [option] *)

let foo (t : int option @ contended nonportable once) =
    use_uncontended t;
    use_portable t;
    use_many t
[%%expect{|
val foo : int option @ once contended -> unit = <fun>
|}]

let foo (t : int option @ local) =
  use_global t [@nontail]

[%%expect{|
Line 2, characters 13-14:
2 |   use_global t [@nontail]
                 ^
Error: This value escapes its region.
|}]

let foo (t : int option @ aliased) =
  use_unique t;

[%%expect{|
Line 2, characters 13-14:
2 |   use_unique t;
                 ^
Error: This value is "aliased" but expected to be "unique".
|}]

(* crosses contention but not portability or linearity *)
let foo (t : ('a -> 'a) option @ contended) =
  use_uncontended t
[%%expect{|
val foo : ('a : any). ('a -> 'a) option @ contended -> unit = <fun>
|}]

let foo (t : ('a -> 'a) option @ nonportable) =
  use_portable t
[%%expect{|
Line 2, characters 15-16:
2 |   use_portable t
                   ^
Error: This value is "nonportable" but expected to be "portable".
|}]

let foo (t : ('a -> 'a) option @ once) =
  use_many t

[%%expect{|
Line 2, characters 11-12:
2 |   use_many t
               ^
Error: This value is "once" but expected to be "many".
|}]

let foo (t : ('a -> 'a) option @ local) =
  use_global t [@nontail]

[%%expect{|
Line 2, characters 13-14:
2 |   use_global t [@nontail]
                 ^
Error: This value escapes its region.
|}]

let foo (t : ('a -> 'a) option @ aliased) =
  use_unique t

[%%expect{|
Line 2, characters 13-14:
2 |   use_unique t
                 ^
Error: This value is "aliased" but expected to be "unique".
|}]

(* references crosses portability but not contention *)
let foo (t : int ref option @ contended) =
    use_uncontended t
[%%expect{|
Line 2, characters 20-21:
2 |     use_uncontended t
                        ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (t : int ref option @ nonportable once) =
    use_portable t;
    use_many t
[%%expect{|
val foo : int ref option @ once -> unit = <fun>
|}]

let foo (t : int ref option @ local) =
  use_global t [@nontail]

[%%expect{|
Line 2, characters 13-14:
2 |   use_global t [@nontail]
                 ^
Error: This value escapes its region.
|}]

let foo (t : int ref option @ aliased) =
  use_unique t

[%%expect{|
Line 2, characters 13-14:
2 |   use_unique t
                 ^
Error: This value is "aliased" but expected to be "unique".
|}]

(* shouldn't cross anything *)
let foo (t : ('a -> 'a) ref option @ contended) =
  use_uncontended t

[%%expect{|
Line 2, characters 18-19:
2 |   use_uncontended t
                      ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (t : ('a -> 'a) ref option @ nonportable) =
  use_portable t

[%%expect{|
Line 2, characters 15-16:
2 |   use_portable t
                   ^
Error: This value is "nonportable" but expected to be "portable".
|}]

let foo (t : ('a -> 'a ref) option @ once) =
  use_many t

[%%expect{|
Line 2, characters 11-12:
2 |   use_many t
               ^
Error: This value is "once" but expected to be "many".
|}]

let foo (t : ('a -> 'a) ref option @ local) =
  use_global t [@nontail]

[%%expect{|
Line 2, characters 13-14:
2 |   use_global t [@nontail]
                 ^
Error: This value escapes its region.
|}]

let foo (t : ('a -> 'a) ref option @ aliased) =
  use_unique t

[%%expect{|
Line 2, characters 13-14:
2 |   use_unique t
                 ^
Error: This value is "aliased" but expected to be "unique".
|}]

(* crosses nothing *)
let foo (t : 'a option @ contended) =
    use_uncontended t
[%%expect{|
Line 2, characters 20-21:
2 |     use_uncontended t
                        ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (t : 'a option @ nonportable) =
    use_portable t
[%%expect{|
Line 2, characters 17-18:
2 |     use_portable t
                     ^
Error: This value is "nonportable" but expected to be "portable".
|}]

let foo (t : 'a option @ once) =
  use_many t

[%%expect{|
Line 2, characters 11-12:
2 |   use_many t
               ^
Error: This value is "once" but expected to be "many".
|}]

let foo (t : 'a option @ local) =
  use_global t [@nontail]

[%%expect{|
Line 2, characters 13-14:
2 |   use_global t [@nontail]
                 ^
Error: This value escapes its region.
|}]

let foo (t : 'a option @ aliased) =
  use_unique t

[%%expect{|
Line 2, characters 13-14:
2 |   use_unique t
                 ^
Error: This value is "aliased" but expected to be "unique".
|}]

(* looks at kinds *)
let foo (type a : value mod contended portable)
      (t : a option @ contended nonportable) =
  use_uncontended t;
  use_portable t

[%%expect{|
val foo : ('a : value mod contended portable). 'a option @ contended -> unit =
  <fun>
|}]

(* CR layouts v2.8: This should be accepted *)
let foo (t : ('a : value mod contended portable) option @ contended nonportable) =
  use_uncontended t;
  use_portable t

[%%expect{|
val foo : ('a : value mod contended portable). 'a option @ contended -> unit =
  <fun>
|}, Principal{|
Line 2, characters 18-19:
2 |   use_uncontended t;
                      ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (type a : value mod contended portable) (t : a option @ once) =
  use_many t

[%%expect{|
Line 2, characters 11-12:
2 |   use_many t
               ^
Error: This value is "once" but expected to be "many".
|}]

let foo (t : ('a : value mod contended portable) option @ local) =
  use_global t [@nontail]

[%%expect{|
Line 2, characters 13-14:
2 |   use_global t [@nontail]
                 ^
Error: This value escapes its region.
|}]

let foo (type a : value mod aliased) (t : a option @ aliased) =
  use_unique t

[%%expect{|
Line 2, characters 13-14:
2 |   use_unique t
                 ^
Error: This value is "aliased" but expected to be "unique".
|}]

(********************************************)
(* TEST: Mode crossing looks through [list] *)

let foo (t : int list @ contended nonportable once) =
    use_uncontended t;
    use_portable t;
    use_many t
[%%expect{|
val foo : int list @ once contended -> unit = <fun>
|}]

let foo (t : int list @ local) =
  use_global t [@nontail]

[%%expect{|
Line 2, characters 13-14:
2 |   use_global t [@nontail]
                 ^
Error: This value escapes its region.
|}]

let foo (t : int list @ aliased) =
  use_unique t;

[%%expect{|
Line 2, characters 13-14:
2 |   use_unique t;
                 ^
Error: This value is "aliased" but expected to be "unique".
|}]

(* crosses contention but not portability or linearity *)
let foo (t : ('a -> 'a) list @ contended) =
  use_uncontended t
[%%expect{|
val foo : ('a : any). ('a -> 'a) list @ contended -> unit = <fun>
|}]

let foo (t : ('a -> 'a) list @ nonportable) =
  use_portable t
[%%expect{|
Line 2, characters 15-16:
2 |   use_portable t
                   ^
Error: This value is "nonportable" but expected to be "portable".
|}]

let foo (t : ('a -> 'a) list @ once) =
  use_many t

[%%expect{|
Line 2, characters 11-12:
2 |   use_many t
               ^
Error: This value is "once" but expected to be "many".
|}]

let foo (t : ('a -> 'a) list @ local) =
  use_global t [@nontail]

[%%expect{|
Line 2, characters 13-14:
2 |   use_global t [@nontail]
                 ^
Error: This value escapes its region.
|}]

let foo (t : ('a -> 'a) list @ aliased) =
  use_unique t

[%%expect{|
Line 2, characters 13-14:
2 |   use_unique t
                 ^
Error: This value is "aliased" but expected to be "unique".
|}]

(* references crosses portability but not contention *)
let foo (t : int ref list @ contended) =
    use_uncontended t
[%%expect{|
Line 2, characters 20-21:
2 |     use_uncontended t
                        ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (t : int ref list @ nonportable once) =
    use_portable t;
    use_many t
[%%expect{|
val foo : int ref list @ once -> unit = <fun>
|}]

let foo (t : int ref list @ local) =
  use_global t [@nontail]

[%%expect{|
Line 2, characters 13-14:
2 |   use_global t [@nontail]
                 ^
Error: This value escapes its region.
|}]

let foo (t : int ref list @ aliased) =
  use_unique t

[%%expect{|
Line 2, characters 13-14:
2 |   use_unique t
                 ^
Error: This value is "aliased" but expected to be "unique".
|}]

(* shouldn't cross anything *)
let foo (t : ('a -> 'a) ref list @ contended) =
  use_uncontended t

[%%expect{|
Line 2, characters 18-19:
2 |   use_uncontended t
                      ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (t : ('a -> 'a) ref list @ nonportable) =
  use_portable t

[%%expect{|
Line 2, characters 15-16:
2 |   use_portable t
                   ^
Error: This value is "nonportable" but expected to be "portable".
|}]

let foo (t : ('a -> 'a ref) list @ once) =
  use_many t

[%%expect{|
Line 2, characters 11-12:
2 |   use_many t
               ^
Error: This value is "once" but expected to be "many".
|}]

let foo (t : ('a -> 'a) ref list @ local) =
  use_global t [@nontail]

[%%expect{|
Line 2, characters 13-14:
2 |   use_global t [@nontail]
                 ^
Error: This value escapes its region.
|}]

let foo (t : ('a -> 'a) ref list @ aliased) =
  use_unique t

[%%expect{|
Line 2, characters 13-14:
2 |   use_unique t
                 ^
Error: This value is "aliased" but expected to be "unique".
|}]

(* crosses nothing *)
let foo (t : 'a list @ contended) =
    use_uncontended t
[%%expect{|
Line 2, characters 20-21:
2 |     use_uncontended t
                        ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (t : 'a list @ nonportable) =
    use_portable t
[%%expect{|
Line 2, characters 17-18:
2 |     use_portable t
                     ^
Error: This value is "nonportable" but expected to be "portable".
|}]

let foo (t : 'a list @ once) =
  use_many t

[%%expect{|
Line 2, characters 11-12:
2 |   use_many t
               ^
Error: This value is "once" but expected to be "many".
|}]

let foo (t : 'a list @ local) =
  use_global t [@nontail]

[%%expect{|
Line 2, characters 13-14:
2 |   use_global t [@nontail]
                 ^
Error: This value escapes its region.
|}]

let foo (t : 'a list @ aliased) =
  use_unique t

[%%expect{|
Line 2, characters 13-14:
2 |   use_unique t
                 ^
Error: This value is "aliased" but expected to be "unique".
|}]

(* looks at kinds *)
let foo (type a : value mod contended portable)
      (t : a list @ contended nonportable) =
  use_uncontended t;
  use_portable t

[%%expect{|
val foo : ('a : value mod contended portable). 'a list @ contended -> unit =
  <fun>
|}]

let foo (type a : value mod contended portable) (t : a list @ once) =
  use_many t

[%%expect{|
Line 2, characters 11-12:
2 |   use_many t
               ^
Error: This value is "once" but expected to be "many".
|}]

let foo (type a : value mod contended portable) (t : a list @ local) =
  use_global t [@nontail]

[%%expect{|
Line 2, characters 13-14:
2 |   use_global t [@nontail]
                 ^
Error: This value escapes its region.
|}]

let foo (type a : value mod contended portable) (t : a list @ aliased) =
  use_unique t

[%%expect{|
Line 2, characters 13-14:
2 |   use_unique t
                 ^
Error: This value is "aliased" but expected to be "unique".
|}]

(****************************)
(* TEST: User-written types *)

(* user syntax *)
type 'a t : immutable_data with 'a = { x : 'a }
[%%expect{|
type 'a t = { x : 'a; }
|}]

type 'a t : immutable_data with 'a = Foo of 'a
[%%expect{|
type 'a t = Foo of 'a
|}]

type t : immutable_data = { x : int }
type ('a : immutable_data) t : immutable_data = { x : 'a }
type ('a : immutable_data, 'b : immutable_data) t : immutable_data = { x : 'a; y : 'b }
type t : mutable_data = { mutable x : int }
[%%expect {|
type t = { x : int; }
type ('a : immutable_data) t = { x : 'a; }
type ('a : immutable_data, 'b : immutable_data) t = { x : 'a; y : 'b; }
type t = { mutable x : int; }
|}]

type t : immutable_data = { mutable x : int}
[%%expect {|
Line 1, characters 0-44:
1 | type t : immutable_data = { mutable x : int}
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is mutable_data
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of immutable_data
         because of the annotation on the declaration of the type t.
|}]

type ('a : mutable_data) t : immutable_data = { x : 'a }
(* CR layouts v2.8: fix error message *)
[%%expect {|
Line 1, characters 0-56:
1 | type ('a : mutable_data) t : immutable_data = { x : 'a }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data with 'a
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of immutable_data
         because of the annotation on the declaration of the type t.
|}]

(***************)
(* TEST: Loops *)

(* This requires fuel-per-type-head in [Jkind.Bound.reduce_baggage] to cut off
   *)
type 'a t = Leaf of 'a | Node of ('a * 'a) t

let rec depth : 'a. 'a t -> _ = function
  | Leaf _ -> 1
  | Node x -> 1 + depth x

[%%expect{|
type 'a t = Leaf of 'a | Node of ('a * 'a) t
val depth : 'a t -> int = <fun>
|}]

(*************************)
(* TEST: gadt refinement *)

type 'a contended_with : value mod contended with 'a
type _ t =
  | Foo : ('a : value mod contended) t
[%%expect {|
type 'a contended_with : value mod contended with 'a
type _ t = Foo : ('a : value mod contended). 'a t
|}]

let f (type a) (t : a t) (x : a contended_with @ contended) : _ @ uncontended =
  match t with
  | _ -> x
[%%expect {|
Line 3, characters 9-10:
3 |   | _ -> x
             ^
Error: This value is "contended" but expected to be "uncontended".
|}]


let f (type a) (t : a t) (x : a contended_with @ contended) : _ @ uncontended =
  match t with
  | Foo -> x
[%%expect {|
val f : 'a t -> 'a contended_with @ contended -> 'a contended_with = <fun>
|}]

(**************************************)
(* TEST: cross with functor parameter *)

module F (T : sig type t end) : sig
  type t : immutable_data with T.t
end = struct
  type t : immutable_data with T.t
end
[%%expect {|
module F :
  functor (T : sig type t end) -> sig type t : immutable_data with T.t end
|}]

module Immutable = F(struct type t : immutable_data end)
type t : immutable_data = Immutable.t
[%%expect {|
module Immutable : sig type t : immutable_data end
type t = Immutable.t
|}]

module Value = F(struct type t end)
type t : immutable_data = Value.t
[%%expect {|
module Value : sig type t : value mod non_float end
Line 2, characters 0-33:
2 | type t : immutable_data = Value.t
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "Value.t" is value mod non_float
         because of the definition of t at line 2, characters 2-34.
       But the kind of type "Value.t" must be a subkind of immutable_data
         because of the definition of t at line 2, characters 0-33.
|}]

(************************)
(* TEST: abstract types *)

(*********************)
type t : value mod contended with int
[%%expect {|
type t : value mod contended
|}]

type t_test = t require_contended
[%%expect {|
type t_test = t require_contended
|}]

type t_test = t require_portable
[%%expect {|
Line 1, characters 14-15:
1 | type t_test = t require_portable
                  ^
Error: This type "t" should be an instance of type "('a : value mod portable)"
       The kind of t is value mod contended
         because of the definition of t at line 1, characters 0-37.
       But the kind of t must be a subkind of value mod portable
         because of the definition of require_portable at line 7, characters 0-47.
|}]

let foo (t : t @ contended) = use_uncontended t
[%%expect {|
val foo : t @ contended -> unit = <fun>
|}]

let foo (t : t @ nonportable) = use_portable t
[%%expect {|
Line 1, characters 45-46:
1 | let foo (t : t @ nonportable) = use_portable t
                                                 ^
Error: This value is "nonportable" but expected to be "portable".
|}]

(*********************)
type 'a t : value mod contended with int
[%%expect {|
type 'a t : value mod contended
|}]

let foo (t : _ t @ contended) = use_uncontended t
[%%expect {|
val foo : 'a t @ contended -> unit = <fun>
|}]

let foo (t : int t @ nonportable) = use_portable t
[%%expect {|
Line 1, characters 49-50:
1 | let foo (t : int t @ nonportable) = use_portable t
                                                     ^
Error: This value is "nonportable" but expected to be "portable".
|}]

(*********************)
type 'a t : value mod contended with 'a
[%%expect {|
type 'a t : value mod contended with 'a
|}]

let foo (t : int t @ contended) = use_uncontended t
[%%expect {|
val foo : int t @ contended -> unit = <fun>
|}]

let foo (t : _ t @ contended) = use_uncontended t
[%%expect {|
Line 1, characters 48-49:
1 | let foo (t : _ t @ contended) = use_uncontended t
                                                    ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (t : int t @ nonportable) = use_portable t
[%%expect {|
Line 1, characters 49-50:
1 | let foo (t : int t @ nonportable) = use_portable t
                                                     ^
Error: This value is "nonportable" but expected to be "portable".
|}]

(*********************)
type ('a : immutable_data) t : value mod contended with 'a
[%%expect {|
type ('a : immutable_data) t : value mod contended with 'a
|}]

type 'a t_test = 'a t require_contended
(* CR layouts v2.8: fix principal case *)
[%%expect {|
type ('a : immutable_data) t_test = 'a t require_contended
|}, Principal{|
Line 1, characters 17-21:
1 | type 'a t_test = 'a t require_contended
                     ^^^^
Error: This type "'a t" should be an instance of type
         "('b : value mod contended)"
       The kind of 'a t is value mod contended with 'a
         because of the definition of t at line 1, characters 0-58.
       But the kind of 'a t must be a subkind of value mod contended
         because of the definition of require_contended at line 6, characters 0-49.
|}]

let foo (t : int t @ contended) = use_uncontended t
[%%expect {|
val foo : int t @ contended -> unit = <fun>
|}]

let foo (t : _ t @ contended) = use_uncontended t
(* CR layouts v2.8: fix principal case *)
[%%expect {|
val foo : ('a : immutable_data). 'a t @ contended -> unit = <fun>
|}, Principal{|
Line 1, characters 48-49:
1 | let foo (t : _ t @ contended) = use_uncontended t
                                                    ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (t : int t @ nonportable) = use_portable t
[%%expect {|
Line 1, characters 49-50:
1 | let foo (t : int t @ nonportable) = use_portable t
                                                     ^
Error: This value is "nonportable" but expected to be "portable".
|}]

(*********************)
type ('a, 'b) t : value mod contended with 'a with 'b
[%%expect {|
type ('a, 'b) t : value mod contended with 'a with 'b
|}]

type t_test = (int, int) t require_contended
(* CR layouts v2.8: fix principal case *)
[%%expect {|
type t_test = (int, int) t require_contended
|}, Principal{|
Line 1, characters 14-26:
1 | type t_test = (int, int) t require_contended
                  ^^^^^^^^^^^^
Error: This type "(int, int) t" should be an instance of type
         "('a : value mod contended)"
       The kind of (int, int) t is value mod contended with int
         because of the definition of t at line 1, characters 0-53.
       But the kind of (int, int) t must be a subkind of value mod contended
         because of the definition of require_contended at line 6, characters 0-49.
|}]

type ('a, 'b) t_test = ('a, 'b) t require_contended
[%%expect {|
Line 1, characters 23-33:
1 | type ('a, 'b) t_test = ('a, 'b) t require_contended
                           ^^^^^^^^^^
Error: This type "('a, 'b) t" should be an instance of type
         "('c : value mod contended)"
       The kind of ('a, 'b) t is value mod contended with 'a with 'b
         because of the definition of t at line 1, characters 0-53.
       But the kind of ('a, 'b) t must be a subkind of value mod contended
         because of the definition of require_contended at line 6, characters 0-49.
|}]

let foo (t : (int, int) t @ contended) = use_uncontended t
[%%expect {|
val foo : (int, int) t @ contended -> unit = <fun>
|}]

let foo (t : (_, _) t @ contended) = use_uncontended t
[%%expect {|
Line 1, characters 53-54:
1 | let foo (t : (_, _) t @ contended) = use_uncontended t
                                                         ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (t : (_, int) t @ contended) = use_uncontended t
[%%expect {|
Line 1, characters 55-56:
1 | let foo (t : (_, int) t @ contended) = use_uncontended t
                                                           ^
Error: This value is "contended" but expected to be "uncontended".
|}]

(********************************************************)
(* TEST: abstract types can hide mode crossing behavior *)

module T : sig
  type t : value mod contended
end = struct
  type t = { x : int }
end
[%%expect {|
module T : sig type t : value mod contended end
|}]

let foo (t : T.t @ contended) = use_uncontended t
[%%expect {|
val foo : T.t @ contended -> unit = <fun>
|}]

let foo (t : T.t @ nonportable) = use_portable t
[%%expect {|
Line 1, characters 47-48:
1 | let foo (t : T.t @ nonportable) = use_portable t
                                                   ^
Error: This value is "nonportable" but expected to be "portable".
|}]

(*********************)
module T : sig
  type 'a t : value mod contended with 'a
end = struct
  type 'a t = { x : 'a }
end
[%%expect {|
module T : sig type 'a t : value mod contended with 'a end
|}]

let foo (t : int T.t @ contended) = use_uncontended t
[%%expect {|
val foo : int T.t @ contended -> unit = <fun>
|}]

let foo (t : _ T.t @ contended) = use_uncontended t
[%%expect {|
Line 1, characters 50-51:
1 | let foo (t : _ T.t @ contended) = use_uncontended t
                                                      ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (t : int T.t @ nonportable) = use_portable t
[%%expect {|
Line 1, characters 51-52:
1 | let foo (t : int T.t @ nonportable) = use_portable t
                                                       ^
Error: This value is "nonportable" but expected to be "portable".
|}]

(*************************)
(* TEST: type equalities *)

type 'a u = { x : 'a }
type 'a t = 'a u
[%%expect {|
type 'a u = { x : 'a; }
type 'a t = 'a u
|}]

let foo (t : int t @ nonportable) = use_portable t
[%%expect {|
val foo : int t -> unit = <fun>
|}]

let foo (t : _ t @ nonportable) = use_portable t
[%%expect {|
Line 1, characters 47-48:
1 | let foo (t : _ t @ nonportable) = use_portable t
                                                   ^
Error: This value is "nonportable" but expected to be "portable".
|}]

let foo (t : int t @ aliased) = use_unique t
[%%expect {|
Line 1, characters 43-44:
1 | let foo (t : int t @ aliased) = use_unique t
                                               ^
Error: This value is "aliased" but expected to be "unique".
|}]

(*********************)

type ('a : immutable_data) u = Foo of 'a | Bar
type 'a t = 'a u
[%%expect {|
type ('a : immutable_data) u = Foo of 'a | Bar
type ('a : immutable_data) t = 'a u
|}]

let foo (t : int t @ nonportable) = use_portable t
[%%expect {|
val foo : int t -> unit = <fun>
|}]

let foo (t : _ t @ nonportable) = use_portable t
(* CR layouts v2.8: fix principal case *)
[%%expect {|
val foo : ('a : immutable_data). 'a t -> unit = <fun>
|}, Principal{|
Line 1, characters 47-48:
1 | let foo (t : _ t @ nonportable) = use_portable t
                                                   ^
Error: This value is "nonportable" but expected to be "portable".
|}]

let foo (t : int t @ aliased) = use_unique t
[%%expect {|
Line 1, characters 43-44:
1 | let foo (t : int t @ aliased) = use_unique t
                                               ^
Error: This value is "aliased" but expected to be "unique".
|}]

(***********************)
(* TEST: redefine type *)

type u = { x : int }
type t : immutable_data = u = { x : int }
let foo (t : t @ nonportable) = use_portable t
[%%expect {|
type u = { x : int; }
type t = u = { x : int; }
val foo : t -> unit = <fun>
|}]

(*********************)

type 'a u = Foo of 'a
type 'a t = 'a u = Foo of 'a
let foo (t : int t @ once) = use_many t
[%%expect {|
type 'a u = Foo of 'a
type 'a t = 'a u = Foo of 'a
val foo : int t @ once -> unit = <fun>
|}]

(*********************)

type ('a, 'b) u = Foo of 'a | Bar of 'b
type ('b, 'a) t = ('b, 'a) u = Foo of 'b | Bar of 'a
let foo (t : (int, string) t @ contended) = use_uncontended t
[%%expect {|
type ('a, 'b) u = Foo of 'a | Bar of 'b
type ('b, 'a) t = ('b, 'a) u = Foo of 'b | Bar of 'a
val foo : (int, string) t @ contended -> unit = <fun>
|}]

(*********************)

type 'a u = Foo of { x : 'a }
type 'a t : immutable_data = 'a u = Foo of { x : 'a }
[%%expect {|
type 'a u = Foo of { x : 'a; }
Line 2, characters 0-53:
2 | type 'a t : immutable_data = 'a u = Foo of { x : 'a }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data with 'a
         because it's a boxed variant type.
       But the kind of type "t" must be a subkind of immutable_data
         because of the annotation on the declaration of the type t.
|}]

(**********************************)
(* TEST: signature substitution *)

module type S = sig
  type 'a t
  val get_contended : unit -> int t @ contended
end
type 'a t = { x : 'a }
[%%expect {|
module type S =
  sig type 'a t val get_contended : unit -> int t @ contended end
type 'a t = { x : 'a; }
|}]

(************)
module type S' = S with type 'a t := 'a t

module M : S' = struct
  let get_contended () = { x = 10 }
end

let () = use_uncontended (M.get_contended ())
[%%expect {|
module type S' = sig val get_contended : unit -> int t @ contended end
module M : S'
|}]

(************)
module type S' = S with type 'a t = 'a t

module M : S' = struct
  type nonrec 'a t = 'a t
  let get_contended () = { x = 10 }
end

let () = use_uncontended (M.get_contended ())
[%%expect {|
module type S' =
  sig type 'a t = 'a t val get_contended : unit -> int t @ contended end
module M : S'
|}]

(**********************)
(* TEST: private type *)

(* a private type does not hide the kind *)
(* CR layouts v2.8: but it should be able to *)

type 'a u = { x : 'a }
type 'a t : value = private 'a u
[%%expect {|
type 'a u = { x : 'a; }
type 'a t = private 'a u
|}]

let foo (t : int t @ contended) = use_uncontended t
[%%expect {|
val foo : int t @ contended -> unit = <fun>
|}]

type 'a t : value = private Foo of 'a
[%%expect {|
type 'a t = private Foo of 'a
|}]

let foo (t : int t @ contended) = use_uncontended t
[%%expect {|
val foo : int t @ contended -> unit = <fun>
|}]

(****************)
(* TEST: tuples *)

type t : immutable_data = int * string
[%%expect {|
type t = int * string
|}]

(************)

type ('a : immutable_data, 'b : immutable_data) t : immutable_data = 'a * 'b
[%%expect {|
type ('a : immutable_data, 'b : immutable_data) t = 'a * 'b
|}]

(************)

type 'a t = int * 'a
[%%expect {|
type 'a t = int * 'a
|}]

let foo (t : int t @ contended nonportable once) =
  use_uncontended t;
  use_portable t;
  use_many t
[%%expect {|
val foo : int t @ once contended -> unit = <fun>
|}]

let foo (t : int t @ local) = use_global t [@nontail]
[%%expect {|
Line 1, characters 41-42:
1 | let foo (t : int t @ local) = use_global t [@nontail]
                                             ^
Error: This value escapes its region.
|}]

let foo (t : int t @ aliased) = use_unique t
[%%expect {|
Line 1, characters 43-44:
1 | let foo (t : int t @ aliased) = use_unique t
                                               ^
Error: This value is "aliased" but expected to be "unique".
|}]

let foo (t : _ t @ contended) = use_uncontended t
[%%expect {|
Line 1, characters 48-49:
1 | let foo (t : _ t @ contended) = use_uncontended t
                                                    ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (t : _ t @ nonportable) = use_portable t
[%%expect {|
Line 1, characters 47-48:
1 | let foo (t : _ t @ nonportable) = use_portable t
                                                   ^
Error: This value is "nonportable" but expected to be "portable".
|}]

let foo (t : _ t @ once) = use_many t
[%%expect {|
Line 1, characters 36-37:
1 | let foo (t : _ t @ once) = use_many t
                                        ^
Error: This value is "once" but expected to be "many".
|}]

(************)
type ('a, 'b, 'c, 'd) t = 'a * ('b * 'c) * 'd
[%%expect {|
type ('a, 'b, 'c, 'd) t = 'a * ('b * 'c) * 'd
|}]

let foo (t : (int, int, int, int) t @ nonportable) = use_portable t
[%%expect {|
val foo : (int, int, int, int) t -> unit = <fun>
|}]

let foo (t : (int, int, _, int) t @ nonportable) = use_portable t
[%%expect {|
Line 1, characters 64-65:
1 | let foo (t : (int, int, _, int) t @ nonportable) = use_portable t
                                                                    ^
Error: This value is "nonportable" but expected to be "portable".
|}]

(*********************************************)
(* Reduction of error seen in the tree *)
(* This requires the [is_open] technology in Ctype. *)

type 'k t1 = T of Obj.t [@@unboxed]

type 'k t2 = { x : 'k t1 }

type packed = T : _ t2 -> packed [@@unboxed]

type q = { x : packed }

module type S = sig
  type t = private q
end with type t = q

[%%expect{|
type 'k t1 = T of Obj.t [@@unboxed]
type 'k t2 = { x : 'k t1; }
type packed = T : 'a t2 -> packed [@@unboxed]
type q = { x : packed; }
module type S = sig type t = q end
|}]
