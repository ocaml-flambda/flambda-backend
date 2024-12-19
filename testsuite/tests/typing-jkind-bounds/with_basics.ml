(* TEST
    expect;
*)

let use_global : 'a @ global -> unit = fun _ -> ()
let use_unique : 'a @ unique -> unit = fun _ -> ()
let use_uncontended : 'a @ uncontended -> unit = fun _ -> ()
let use_portable : 'a @ portable -> unit = fun _ -> ()
let use_many : 'a @ many -> unit = fun _ -> ()
type ('a : value mod uncontended) require_uncontended
type ('a : value mod portable) require_portable
[%%expect{|
val use_global : 'a -> unit = <fun>
val use_unique : 'a @ unique -> unit = <fun>
val use_uncontended : 'a -> unit = <fun>
val use_portable : 'a @ portable -> unit = <fun>
val use_many : 'a -> unit = <fun>
type ('a : value mod uncontended) require_uncontended
type ('a : value mod portable) require_portable
|}]

(**********************************************)
(* TEST: Mode crossing looks through [option] *)

let foo (t : int option @@ contended nonportable once) =
    use_uncontended t;
    use_portable t;
    use_many t
[%%expect{|
val foo : int option @ once contended -> unit = <fun>
|}]

let foo (t : int option @@ local) =
  use_global t [@nontail]

[%%expect{|
Line 2, characters 13-14:
2 |   use_global t [@nontail]
                 ^
Error: This value escapes its region.
|}]

let foo (t : int option @@ aliased) =
  use_unique t;

[%%expect{|
Line 2, characters 13-14:
2 |   use_unique t;
                 ^
Error: This value is "aliased" but expected to be "unique".
|}]

(* crosses contention but not portability or linearity *)
let foo (t : ('a -> 'a) option @@ contended) =
  use_uncontended t
[%%expect{|
val foo : ('a : any). ('a -> 'a) option @ contended -> unit = <fun>
|}]

let foo (t : ('a -> 'a) option @@ nonportable) =
  use_portable t
[%%expect{|
Line 2, characters 15-16:
2 |   use_portable t
                   ^
Error: This value is "nonportable" but expected to be "portable".
|}]

let foo (t : ('a -> 'a) option @@ once) =
  use_many t

[%%expect{|
Line 2, characters 11-12:
2 |   use_many t
               ^
Error: This value is "once" but expected to be "many".
|}]

let foo (t : ('a -> 'a) option @@ local) =
  use_global t [@nontail]

[%%expect{|
Line 2, characters 13-14:
2 |   use_global t [@nontail]
                 ^
Error: This value escapes its region.
|}]

let foo (t : ('a -> 'a) option @@ aliased) =
  use_unique t

[%%expect{|
Line 2, characters 13-14:
2 |   use_unique t
                 ^
Error: This value is "aliased" but expected to be "unique".
|}]

(* references crosses portability but not contention *)
let foo (t : int ref option @@ contended) =
    use_uncontended t
[%%expect{|
Line 2, characters 20-21:
2 |     use_uncontended t
                        ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (t : int ref option @@ nonportable once) =
    use_portable t;
    use_many t
[%%expect{|
val foo : int ref option @ once -> unit = <fun>
|}]

let foo (t : int ref option @@ local) =
  use_global t [@nontail]

[%%expect{|
Line 2, characters 13-14:
2 |   use_global t [@nontail]
                 ^
Error: This value escapes its region.
|}]

let foo (t : int ref option @@ aliased) =
  use_unique t

[%%expect{|
Line 2, characters 13-14:
2 |   use_unique t
                 ^
Error: This value is "aliased" but expected to be "unique".
|}]

(* shouldn't cross anything *)
let foo (t : ('a -> 'a) ref option @@ contended) =
  use_uncontended t

[%%expect{|
Line 2, characters 18-19:
2 |   use_uncontended t
                      ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (t : ('a -> 'a) ref option @@ nonportable) =
  use_portable t

[%%expect{|
Line 2, characters 15-16:
2 |   use_portable t
                   ^
Error: This value is "nonportable" but expected to be "portable".
|}]

let foo (t : ('a -> 'a) ref option @@ once) =
  use_many t

[%%expect{|
Line 2, characters 11-12:
2 |   use_many t
               ^
Error: This value is "once" but expected to be "many".
|}]

let foo (t : ('a -> 'a) ref option @@ local) =
  use_global t [@nontail]

[%%expect{|
Line 2, characters 13-14:
2 |   use_global t [@nontail]
                 ^
Error: This value escapes its region.
|}]

let foo (t : ('a -> 'a) ref option @@ aliased) =
  use_unique t

[%%expect{|
Line 2, characters 13-14:
2 |   use_unique t
                 ^
Error: This value is "aliased" but expected to be "unique".
|}]

(* crosses nothing *)
let foo (t : 'a option @@ contended) =
    use_uncontended t
[%%expect{|
Line 2, characters 20-21:
2 |     use_uncontended t
                        ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (t : 'a option @@ nonportable) =
    use_portable t
[%%expect{|
Line 2, characters 17-18:
2 |     use_portable t
                     ^
Error: This value is "nonportable" but expected to be "portable".
|}]

let foo (t : 'a option @@ once) =
  use_many t

[%%expect{|
Line 2, characters 11-12:
2 |   use_many t
               ^
Error: This value is "once" but expected to be "many".
|}]

let foo (t : 'a option @@ local) =
  use_global t [@nontail]

[%%expect{|
Line 2, characters 13-14:
2 |   use_global t [@nontail]
                 ^
Error: This value escapes its region.
|}]

let foo (t : 'a option @@ aliased) =
  use_unique t

[%%expect{|
Line 2, characters 13-14:
2 |   use_unique t
                 ^
Error: This value is "aliased" but expected to be "unique".
|}]

(* looks at kinds *)
let foo (type a : value mod uncontended portable)
      (t : a option @@ contended nonportable) =
  use_uncontended t;
  use_portable t

[%%expect{|
val foo :
  ('a : value mod uncontended portable). 'a option @ contended -> unit =
  <fun>
|}]

(* CR layouts v2.8: This should be accepted *)
let foo (t : ('a : value mod uncontended portable) option @@ contended nonportable) =
  use_uncontended t;
  use_portable t

[%%expect{|
val foo :
  ('a : value mod uncontended portable). 'a option @ contended -> unit =
  <fun>
|}, Principal{|
Line 2, characters 18-19:
2 |   use_uncontended t;
                      ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (type a : value mod uncontended portable) (t : a option @@ once) =
  use_many t

[%%expect{|
Line 2, characters 11-12:
2 |   use_many t
               ^
Error: This value is "once" but expected to be "many".
|}]

let foo (t : ('a : value mod uncontended portable) option @@ local) =
  use_global t [@nontail]

[%%expect{|
Line 2, characters 13-14:
2 |   use_global t [@nontail]
                 ^
Error: This value escapes its region.
|}]

let foo (type a : value mod unique) (t : a option @@ aliased) =
  use_unique t

[%%expect{|
Line 2, characters 13-14:
2 |   use_unique t
                 ^
Error: This value is "aliased" but expected to be "unique".
|}]

(********************************************)
(* TEST: Mode crossing looks through [list] *)

let foo (t : int list @@ contended nonportable once) =
    use_uncontended t;
    use_portable t;
    use_many t
[%%expect{|
val foo : int list @ once contended -> unit = <fun>
|}]

let foo (t : int list @@ local) =
  use_global t [@nontail]

[%%expect{|
Line 2, characters 13-14:
2 |   use_global t [@nontail]
                 ^
Error: This value escapes its region.
|}]

let foo (t : int list @@ aliased) =
  use_unique t;

[%%expect{|
Line 2, characters 13-14:
2 |   use_unique t;
                 ^
Error: This value is "aliased" but expected to be "unique".
|}]

(* crosses contention but not portability or linearity *)
let foo (t : ('a -> 'a) list @@ contended) =
  use_uncontended t
[%%expect{|
val foo : ('a : any). ('a -> 'a) list @ contended -> unit = <fun>
|}]

let foo (t : ('a -> 'a) list @@ nonportable) =
  use_portable t
[%%expect{|
Line 2, characters 15-16:
2 |   use_portable t
                   ^
Error: This value is "nonportable" but expected to be "portable".
|}]

let foo (t : ('a -> 'a) list @@ once) =
  use_many t

[%%expect{|
Line 2, characters 11-12:
2 |   use_many t
               ^
Error: This value is "once" but expected to be "many".
|}]

let foo (t : ('a -> 'a) list @@ local) =
  use_global t [@nontail]

[%%expect{|
Line 2, characters 13-14:
2 |   use_global t [@nontail]
                 ^
Error: This value escapes its region.
|}]

let foo (t : ('a -> 'a) list @@ aliased) =
  use_unique t

[%%expect{|
Line 2, characters 13-14:
2 |   use_unique t
                 ^
Error: This value is "aliased" but expected to be "unique".
|}]

(* references crosses portability but not contention *)
let foo (t : int ref list @@ contended) =
    use_uncontended t
[%%expect{|
Line 2, characters 20-21:
2 |     use_uncontended t
                        ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (t : int ref list @@ nonportable once) =
    use_portable t;
    use_many t
[%%expect{|
val foo : int ref list @ once -> unit = <fun>
|}]

let foo (t : int ref list @@ local) =
  use_global t [@nontail]

[%%expect{|
Line 2, characters 13-14:
2 |   use_global t [@nontail]
                 ^
Error: This value escapes its region.
|}]

let foo (t : int ref list @@ aliased) =
  use_unique t

[%%expect{|
Line 2, characters 13-14:
2 |   use_unique t
                 ^
Error: This value is "aliased" but expected to be "unique".
|}]

(* shouldn't cross anything *)
let foo (t : ('a -> 'a) ref list @@ contended) =
  use_uncontended t

[%%expect{|
Line 2, characters 18-19:
2 |   use_uncontended t
                      ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (t : ('a -> 'a) ref list @@ nonportable) =
  use_portable t

[%%expect{|
Line 2, characters 15-16:
2 |   use_portable t
                   ^
Error: This value is "nonportable" but expected to be "portable".
|}]

let foo (t : ('a -> 'a) ref list @@ once) =
  use_many t

[%%expect{|
Line 2, characters 11-12:
2 |   use_many t
               ^
Error: This value is "once" but expected to be "many".
|}]

let foo (t : ('a -> 'a) ref list @@ local) =
  use_global t [@nontail]

[%%expect{|
Line 2, characters 13-14:
2 |   use_global t [@nontail]
                 ^
Error: This value escapes its region.
|}]

let foo (t : ('a -> 'a) ref list @@ aliased) =
  use_unique t

[%%expect{|
Line 2, characters 13-14:
2 |   use_unique t
                 ^
Error: This value is "aliased" but expected to be "unique".
|}]

(* crosses nothing *)
let foo (t : 'a list @@ contended) =
    use_uncontended t
[%%expect{|
Line 2, characters 20-21:
2 |     use_uncontended t
                        ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (t : 'a list @@ nonportable) =
    use_portable t
[%%expect{|
Line 2, characters 17-18:
2 |     use_portable t
                     ^
Error: This value is "nonportable" but expected to be "portable".
|}]

let foo (t : 'a list @@ once) =
  use_many t

[%%expect{|
Line 2, characters 11-12:
2 |   use_many t
               ^
Error: This value is "once" but expected to be "many".
|}]

let foo (t : 'a list @@ local) =
  use_global t [@nontail]

[%%expect{|
Line 2, characters 13-14:
2 |   use_global t [@nontail]
                 ^
Error: This value escapes its region.
|}]

let foo (t : 'a list @@ aliased) =
  use_unique t

[%%expect{|
Line 2, characters 13-14:
2 |   use_unique t
                 ^
Error: This value is "aliased" but expected to be "unique".
|}]

(* looks at kinds *)
let foo (type a : value mod uncontended portable)
      (t : a list @@ contended nonportable) =
  use_uncontended t;
  use_portable t

[%%expect{|
val foo : ('a : value mod uncontended portable). 'a list @ contended -> unit =
  <fun>
|}]

let foo (type a : value mod uncontended portable) (t : a list @@ once) =
  use_many t

[%%expect{|
Line 2, characters 11-12:
2 |   use_many t
               ^
Error: This value is "once" but expected to be "many".
|}]

let foo (type a : value mod uncontended portable) (t : a list @@ local) =
  use_global t [@nontail]

[%%expect{|
Line 2, characters 13-14:
2 |   use_global t [@nontail]
                 ^
Error: This value escapes its region.
|}]

let foo (type a : value mod uncontended portable) (t : a list @@ aliased) =
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

(* CR layouts v2.8: This should be accepted *)
(* CR reisenberg: fix! *)
[%%expect{|
Line 1, characters 0-47:
1 | type 'a t : immutable_data with 'a = { x : 'a }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data
         because it's a boxed record type.
       But the kind of type "t" must be a subkind of immutable_data
         because of the annotation on the declaration of the type t.
|}]

type 'a t : immutable_data with 'a = Foo of 'a

(* CR layouts v2.8: This should be accepted *)
(* CR reisenberg: fix! *)
[%%expect{|
Line 1, characters 0-46:
1 | type 'a t : immutable_data with 'a = Foo of 'a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is immutable_data
         because it's a boxed variant type.
       But the kind of type "t" must be a subkind of immutable_data
         because of the annotation on the declaration of the type t.
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
Error: The kind of type "t" is immutable_data
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

type 'a uncontended_with : value mod uncontended with 'a
type _ t =
  | Foo : ('a : value mod uncontended) t
[%%expect {|
type 'a uncontended_with : value mod uncontended
type _ t = Foo : ('a : value mod uncontended). 'a t
|}]

let f (type a) (t : a t) (x : a uncontended_with @@ contended) : _ @@ uncontended =
  match t with
  | _ -> x
[%%expect {|
Line 3, characters 9-10:
3 |   | _ -> x
             ^
Error: This value is "contended" but expected to be "uncontended".
|}]


let f (type a) (t : a t) (x : a uncontended_with @@ contended) : _ @@ uncontended =
  match t with
  | Foo -> x
[%%expect {|
val f : 'a t -> 'a uncontended_with @ contended -> 'a uncontended_with =
  <fun>
|}]

(**************************************)
(* TEST: cross with functor parameter *)

module F (T : sig type t end) : sig
  type t : immutable_data with T.t
end = struct
  type t : immutable_data with T.t
end
[%%expect {|
module F : functor (T : sig type t end) -> sig type t : immutable_data end
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
module Value : sig type t end
Line 2, characters 0-33:
2 | type t : immutable_data = Value.t
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "Value.t" is value
         because of the definition of t at line 2, characters 2-34.
       But the kind of type "Value.t" must be a subkind of immutable_data
         because of the definition of t at line 2, characters 0-33.
|}]

(************************)
(* TEST: abstract types *)

(*********************)
type t : value mod uncontended with int
[%%expect {|
type t : value mod uncontended
|}]

type t_test = t require_uncontended
(* CR layouts v2.8: fix principal case *)
[%%expect {|
type t_test = t require_uncontended
|}, Principal{|
Line 1, characters 14-15:
1 | type t_test = t require_uncontended
                  ^
Error: This type "t" should be an instance of type "('a : value mod uncontended)"
       The kind of t is value mod uncontended
         because of the definition of t at line 1, characters 0-39.
       But the kind of t must be a subkind of value mod uncontended
         because of the definition of require_uncontended at line 6, characters 0-53.
|}]

type t_test = t require_portable
[%%expect {|
Line 1, characters 14-15:
1 | type t_test = t require_portable
                  ^
Error: This type "t" should be an instance of type "('a : value mod portable)"
       The kind of t is value mod uncontended
         because of the definition of t at line 1, characters 0-39.
       But the kind of t must be a subkind of value mod portable
         because of the definition of require_portable at line 7, characters 0-47.
|}]

let foo (t : t @@ contended) = use_uncontended t
[%%expect {|
val foo : t @ contended -> unit = <fun>
|}]

let foo (t : t @@ nonportable) = use_portable t
[%%expect {|
Line 1, characters 46-47:
1 | let foo (t : t @@ nonportable) = use_portable t
                                                  ^
Error: This value is "nonportable" but expected to be "portable".
|}]

(*********************)
type 'a t : value mod uncontended with int
[%%expect {|
type 'a t : value mod uncontended
|}]

let foo (t : _ t @@ contended) = use_uncontended t
[%%expect {|
val foo : 'a t @ contended -> unit = <fun>
|}]

let foo (t : int t @@ nonportable) = use_portable t
[%%expect {|
Line 1, characters 50-51:
1 | let foo (t : int t @@ nonportable) = use_portable t
                                                      ^
Error: This value is "nonportable" but expected to be "portable".
|}]

(*********************)
type 'a t : value mod uncontended with 'a
[%%expect {|
type 'a t : value mod uncontended
|}]

let foo (t : int t @@ contended) = use_uncontended t
[%%expect {|
val foo : int t @ contended -> unit = <fun>
|}]

let foo (t : _ t @@ contended) = use_uncontended t
[%%expect {|
Line 1, characters 49-50:
1 | let foo (t : _ t @@ contended) = use_uncontended t
                                                     ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (t : int t @@ nonportable) = use_portable t
[%%expect {|
Line 1, characters 50-51:
1 | let foo (t : int t @@ nonportable) = use_portable t
                                                      ^
Error: This value is "nonportable" but expected to be "portable".
|}]

(*********************)
type ('a : immutable_data) t : value mod uncontended with 'a
[%%expect {|
type ('a : immutable_data) t : value mod uncontended
|}]

type 'a t_test = 'a t require_uncontended
(* CR layouts v2.8: fix principal case *)
[%%expect {|
type ('a : immutable_data) t_test = 'a t require_uncontended
|}, Principal{|
Line 1, characters 17-21:
1 | type 'a t_test = 'a t require_uncontended
                     ^^^^
Error: This type "'a t" should be an instance of type
         "('b : value mod uncontended)"
       The kind of 'a t is value mod uncontended
         because of the definition of t at line 1, characters 0-60.
       But the kind of 'a t must be a subkind of value mod uncontended
         because of the definition of require_uncontended at line 6, characters 0-53.
|}]

let foo (t : int t @@ contended) = use_uncontended t
[%%expect {|
val foo : int t @ contended -> unit = <fun>
|}]

let foo (t : _ t @@ contended) = use_uncontended t
(* CR layouts v2.8: fix principal case *)
[%%expect {|
val foo : ('a : immutable_data). 'a t @ contended -> unit = <fun>
|}, Principal{|
Line 1, characters 49-50:
1 | let foo (t : _ t @@ contended) = use_uncontended t
                                                     ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (t : int t @@ nonportable) = use_portable t
[%%expect {|
Line 1, characters 50-51:
1 | let foo (t : int t @@ nonportable) = use_portable t
                                                      ^
Error: This value is "nonportable" but expected to be "portable".
|}]

(*********************)
type ('a, 'b) t : value mod uncontended with 'a with 'b
[%%expect {|
type ('a, 'b) t : value mod uncontended
|}]

type t_test = (int, int) t require_uncontended
(* CR layouts v2.8: fix principal case *)
[%%expect {|
type t_test = (int, int) t require_uncontended
|}, Principal{|
Line 1, characters 14-26:
1 | type t_test = (int, int) t require_uncontended
                  ^^^^^^^^^^^^
Error: This type "(int, int) t" should be an instance of type
         "('a : value mod uncontended)"
       The kind of (int, int) t is value mod uncontended
         because of the definition of t at line 1, characters 0-55.
       But the kind of (int, int) t must be a subkind of
         value mod uncontended
         because of the definition of require_uncontended at line 6, characters 0-53.
|}]

type ('a, 'b) t_test = ('a, 'b) t require_uncontended
[%%expect {|
Line 1, characters 23-33:
1 | type ('a, 'b) t_test = ('a, 'b) t require_uncontended
                           ^^^^^^^^^^
Error: This type "('a, 'b) t" should be an instance of type
         "('c : value mod uncontended)"
       The kind of ('a, 'b) t is value mod uncontended
         because of the definition of t at line 1, characters 0-55.
       But the kind of ('a, 'b) t must be a subkind of value mod uncontended
         because of the definition of require_uncontended at line 6, characters 0-53.
|}]

let foo (t : (int, int) t @@ contended) = use_uncontended t
[%%expect {|
val foo : (int, int) t @ contended -> unit = <fun>
|}]

let foo (t : (_, _) t @@ contended) = use_uncontended t
[%%expect {|
Line 1, characters 54-55:
1 | let foo (t : (_, _) t @@ contended) = use_uncontended t
                                                          ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (t : (_, int) t @@ contended) = use_uncontended t
[%%expect {|
Line 1, characters 56-57:
1 | let foo (t : (_, int) t @@ contended) = use_uncontended t
                                                            ^
Error: This value is "contended" but expected to be "uncontended".
|}]

(********************************************************)
(* TEST: abstract types can hide mode crossing behavior *)

module T : sig
  type t : value mod uncontended
end = struct
  type t = { x : int }
end
[%%expect {|
module T : sig type t : value mod uncontended end
|}]

let foo (t : T.t @@ contended) = use_uncontended t
[%%expect {|
val foo : T.t @ contended -> unit = <fun>
|}]

let foo (t : T.t @@ nonportable) = use_portable t
[%%expect {|
Line 1, characters 48-49:
1 | let foo (t : T.t @@ nonportable) = use_portable t
                                                    ^
Error: This value is "nonportable" but expected to be "portable".
|}]

(*********************)
module T : sig
  type 'a t : value mod uncontended with 'a
end = struct
  type 'a t = { x : 'a }
end
(* CR layouts v2.8: fix this *)
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type 'a t = { x : 'a }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = { x : 'a; } end
       is not included in
         sig type 'a t : value mod uncontended end
       Type declarations do not match:
         type 'a t = { x : 'a; }
       is not included in
         type 'a t : value mod uncontended
       The kind of the first is immutable_data
         because of the definition of t at line 4, characters 2-24.
       But the kind of the first must be a subkind of value mod uncontended
         because of the definition of t at line 2, characters 2-43.
|}]

let foo (t : int T.t @@ contended) = use_uncontended t
[%%expect {|
Line 1, characters 13-20:
1 | let foo (t : int T.t @@ contended) = use_uncontended t
                 ^^^^^^^
Error: The type constructor "T.t" expects 0 argument(s),
       but is here applied to 1 argument(s)
|}]

let foo (t : _ T.t @@ contended) = use_uncontended t
[%%expect {|
Line 1, characters 13-18:
1 | let foo (t : _ T.t @@ contended) = use_uncontended t
                 ^^^^^
Error: The type constructor "T.t" expects 0 argument(s),
       but is here applied to 1 argument(s)
|}]

let foo (t : int T.t @@ nonportable) = use_portable t
[%%expect {|
Line 1, characters 13-20:
1 | let foo (t : int T.t @@ nonportable) = use_portable t
                 ^^^^^^^
Error: The type constructor "T.t" expects 0 argument(s),
       but is here applied to 1 argument(s)
|}]

(*************************)
(* TEST: type equalities *)

type 'a u = { x : 'a }
type 'a t = 'a u
[%%expect {|
type 'a u = { x : 'a; }
type 'a t = 'a u
|}]

let foo (t : int t @@ nonportable) = use_portable t
[%%expect {|
val foo : int t -> unit = <fun>
|}]

let foo (t : _ t @@ nonportable) = use_portable t
[%%expect {|
Line 1, characters 48-49:
1 | let foo (t : _ t @@ nonportable) = use_portable t
                                                    ^
Error: This value is "nonportable" but expected to be "portable".
|}]

let foo (t : int t @@ aliased) = use_unique t
[%%expect {|
Line 1, characters 44-45:
1 | let foo (t : int t @@ aliased) = use_unique t
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

let foo (t : int t @@ nonportable) = use_portable t
[%%expect {|
val foo : int t -> unit = <fun>
|}]

let foo (t : _ t @@ nonportable) = use_portable t
(* CR layouts v2.8: fix principal case *)
[%%expect {|
val foo : ('a : immutable_data). 'a t -> unit = <fun>
|}, Principal{|
Line 1, characters 48-49:
1 | let foo (t : _ t @@ nonportable) = use_portable t
                                                    ^
Error: This value is "nonportable" but expected to be "portable".
|}]

let foo (t : int t @@ aliased) = use_unique t
[%%expect {|
Line 1, characters 44-45:
1 | let foo (t : int t @@ aliased) = use_unique t
                                                ^
Error: This value is "aliased" but expected to be "unique".
|}]

(***********************)
(* TEST: redefine type *)

type u = { x : int }
type t : immutable_data = u = { x : int }
let foo (t : t @@ nonportable) = use_portable t
[%%expect {|
type u = { x : int; }
type t = u = { x : int; }
val foo : t -> unit = <fun>
|}]

(*********************)

type 'a u = Foo of 'a
type 'a t = 'a u = Foo of 'a
let foo (t : int t @@ once) = use_many t
[%%expect {|
type 'a u = Foo of 'a
type 'a t = 'a u = Foo of 'a
val foo : int t @ once -> unit = <fun>
|}]

(*********************)

type ('a, 'b) u = Foo of 'a | Bar of 'b
type ('b, 'a) t = ('b, 'a) u = Foo of 'b | Bar of 'a
let foo (t : (int, string) t @@ contended) = use_uncontended t
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
Error: The kind of type "t" is immutable_data
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

let foo (t : int t @@ contended) = use_uncontended t
[%%expect {|
val foo : int t @ contended -> unit = <fun>
|}]

type 'a t : value = private Foo of 'a
[%%expect {|
type 'a t = private Foo of 'a
|}]

let foo (t : int t @@ contended) = use_uncontended t
[%%expect {|
val foo : int t @ contended -> unit = <fun>
|}]

(****************)
(* TEST: tuples *)

type t : immutable_data = int * string
(* CR layouts v2.8: fix this *)
[%%expect {|
Line 1, characters 0-38:
1 | type t : immutable_data = int * string
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "int * string" is value
         because it's a tuple type.
       But the kind of type "int * string" must be a subkind of immutable_data
         because of the definition of t at line 1, characters 0-38.
|}]

(************)

type ('a : immutable_data, 'b : immutable_data) t : immutable_data = 'a * 'b
(* CR layouts v2.8: fix this *)
[%%expect {|
Line 1, characters 0-76:
1 | type ('a : immutable_data, 'b : immutable_data) t : immutable_data = 'a * 'b
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "'a * 'b" is value
         because it's a tuple type.
       But the kind of type "'a * 'b" must be a subkind of immutable_data
         because of the definition of t at line 1, characters 0-76.
|}]

(************)

type 'a t = int * 'a
[%%expect {|
type 'a t = int * 'a
|}]

let foo (t : int t @@ contended nonportable once) =
  use_uncontended t;
  use_portable t;
  use_many t
(* CR layouts v2.8: fix this *)
[%%expect {|
Line 2, characters 18-19:
2 |   use_uncontended t;
                      ^
Error: This value is "once" but expected to be "many".
|}]

let foo (t : int t @@ local) = use_global t [@nontail]
[%%expect {|
Line 1, characters 42-43:
1 | let foo (t : int t @@ local) = use_global t [@nontail]
                                              ^
Error: This value escapes its region.
|}]

let foo (t : int t @@ aliased) = use_unique t
[%%expect {|
Line 1, characters 44-45:
1 | let foo (t : int t @@ aliased) = use_unique t
                                                ^
Error: This value is "aliased" but expected to be "unique".
|}]

let foo (t : _ t @@ contended) = use_uncontended t
[%%expect {|
Line 1, characters 49-50:
1 | let foo (t : _ t @@ contended) = use_uncontended t
                                                     ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (t : _ t @@ nonportable) = use_portable t
[%%expect {|
Line 1, characters 48-49:
1 | let foo (t : _ t @@ nonportable) = use_portable t
                                                    ^
Error: This value is "nonportable" but expected to be "portable".
|}]

let foo (t : _ t @@ once) = use_many t
[%%expect {|
Line 1, characters 37-38:
1 | let foo (t : _ t @@ once) = use_many t
                                         ^
Error: This value is "once" but expected to be "many".
|}]

(************)
type ('a, 'b, 'c, 'd) t = 'a * ('b * 'c) * 'd
[%%expect {|
type ('a, 'b, 'c, 'd) t = 'a * ('b * 'c) * 'd
|}]

let foo (t : (int, int, int, int) t @@ nonportable) = use_portable t
(* CR layouts v2.8: fix this
 *)
[%%expect {|
Line 1, characters 67-68:
1 | let foo (t : (int, int, int, int) t @@ nonportable) = use_portable t
                                                                       ^
Error: This value is "nonportable" but expected to be "portable".
|}]

let foo (t : (int, int, _, int) t @@ nonportable) = use_portable t
[%%expect {|
Line 1, characters 65-66:
1 | let foo (t : (int, int, _, int) t @@ nonportable) = use_portable t
                                                                     ^
Error: This value is "nonportable" but expected to be "portable".
|}]
