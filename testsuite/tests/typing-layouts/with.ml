(* TEST
    expect;
*)

let use_global : 'a @ global -> unit = fun _ -> ()
let use_unique : 'a @ unique -> unit = fun _ -> ()
let use_uncontended : 'a @ uncontended -> unit = fun _ -> ()
let use_portable : 'a @ portable -> unit = fun _ -> ()
let use_many : 'a @ many -> unit = fun _ -> ()
[%%expect{|
val use_global : 'a -> unit = <fun>
val use_unique : 'a @ unique -> unit = <fun>
val use_uncontended : 'a -> unit = <fun>
val use_portable : 'a @ portable -> unit = <fun>
val use_many : 'a -> unit = <fun>
|}]

(************************************************)
(* TEST 1: Mode crossing looks through [option] *)

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

let foo (type a : value mod uncontended portable) (t : a option @@ once) =
  use_many t

[%%expect{|
Line 2, characters 11-12:
2 |   use_many t
               ^
Error: This value is "once" but expected to be "many".
|}]

let foo (type a : value mod uncontended portable) (t : a option @@ local) =
  use_global t [@nontail]

[%%expect{|
Line 2, characters 13-14:
2 |   use_global t [@nontail]
                 ^
Error: This value escapes its region.
|}]

let foo (type a : value mod uncontended portable) (t : a option @@ aliased) =
  use_unique t

[%%expect{|
Line 2, characters 13-14:
2 |   use_unique t
                 ^
Error: This value is "aliased" but expected to be "unique".
|}]

(**********************************************)
(* TEST 2: Mode crossing looks through [list] *)

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

(******************************)
(* TEST 3: User-written types *)

(* user syntax *)
type 'a t1 : immutable_data with 'a = { x : 'a }

(* CR layouts v2.8: This should be accepted *)
(* CR reisenberg: fix! *)
[%%expect{|
Line 1, characters 0-48:
1 | type 'a t1 : immutable_data with 'a = { x : 'a }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t1" is immutable_data
         because it's a boxed record type.
       But the kind of type "t1" must be a subkind of immutable_data
         because of the annotation on the declaration of the type t1.
|}]

type 'a t2 : immutable_data with 'a = Foo of 'a

(* CR layouts v2.8: This should be accepted *)
[%%expect{|
Line 1, characters 0-47:
1 | type 'a t2 : immutable_data with 'a = Foo of 'a
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t2" is immutable_data
         because it's a boxed variant type.
       But the kind of type "t2" must be a subkind of immutable_data
         because of the annotation on the declaration of the type t2.
|}]

(*****************)
(* TEST 4: Loops *)

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
