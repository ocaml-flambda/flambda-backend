(* TEST
   include stdlib_stable;
   flags = "-extension mode_alpha";
   expect;
*)

(** In this file, we test the dual relationship between the Determinism and Access axes,
    the Access requirements over mutable record fields,
    and the new layout kind [uncontended_data] *)

(* Access requirements over mutable record fields *)

type 'a myref = { mutable a : 'a; b : 'a }
[%%expect{|
type 'a myref = { mutable a : 'a; b : 'a; }
|}]

let foo x a = x.a <- a
[%%expect{|
val foo : 'a myref -> 'a -> unit = <fun>
|}]

let foo (x @ read_only) a = x.a <- a
[%%expect{|
Line 1, characters 28-29:
1 | let foo (x @ read_only) a = x.a <- a
                                ^
Error: This value is "read_only" but expected to be "read_write".
|}]

let foo (x @ immutable) a = x.a <- a
[%%expect{|
Line 1, characters 28-29:
1 | let foo (x @ immutable) a = x.a <- a
                                ^
Error: This value is "immutable" but expected to be "read_write".
|}]

let foo (x @ read_only) = x.a
[%%expect{|
val foo : 'a myref @ read_only -> 'a @ read_only = <fun>
|}]

let foo (x @ immutable) = x.a
[%%expect{|
Line 1, characters 26-27:
1 | let foo (x @ immutable) = x.a
                              ^
Error: This value is "immutable" but expected to be "read_only".
|}]

let foo (x @ read_only) upd = { x with a = upd }
[%%expect{|
val foo : 'a myref @ read_only -> 'a -> 'a myref @ read_only = <fun>
|}]

let foo (x @ immutable) upd = { x with a = upd }
[%%expect{|
val foo : 'a myref @ immutable -> 'a -> 'a myref @ immutable = <fun>
|}]

let foo (x @ immutable) upd = { x with b = upd }
[%%expect{|
Line 1, characters 32-33:
1 | let foo (x @ immutable) upd = { x with b = upd }
                                    ^
Error: This value is "immutable" but expected to be "read_only".
|}]

(* Errors when mutating a record field prints contention before access errors *)

let foo (x @ read_only contended) a = x.a <- a
[%%expect{|
Line 1, characters 38-39:
1 | let foo (x @ read_only contended) a = x.a <- a
                                          ^
Error: This value is "contended" but expected to be "uncontended".
  Hint: In order to write into the mutable fields,
  this record needs to be uncontended.
|}]

let foo (x @ contended read_only) a = x.a <- a
[%%expect{|
Line 1, characters 38-39:
1 | let foo (x @ contended read_only) a = x.a <- a
                                          ^
Error: This value is "contended" but expected to be "uncontended".
  Hint: In order to write into the mutable fields,
  this record needs to be uncontended.
|}]

let foo (x @ read_only shared) a = x.a <- a
[%%expect{|
Line 1, characters 35-36:
1 | let foo (x @ read_only shared) a = x.a <- a
                                       ^
Error: This value is "shared" but expected to be "uncontended".
  Hint: In order to write into the mutable fields,
  this record needs to be uncontended.
|}]

let foo (x @ immutable contended) a = x.a
[%%expect{|
Line 1, characters 38-39:
1 | let foo (x @ immutable contended) a = x.a
                                          ^
Error: This value is "contended" but expected to be "shared".
  Hint: In order to read from the mutable fields,
  this record needs to be at least shared.
|}]

(* Access requirements over refs *)

let foo (x @ immutable) = x.contents
[%%expect{|
Line 1, characters 26-27:
1 | let foo (x @ immutable) = x.contents
                              ^
Error: This value is "immutable" but expected to be "read_only".
|}]

let foo (x @ read_only) = x.contents
[%%expect{|
val foo : 'a ref @ read_only -> 'a @ read_only = <fun>
|}]

let foo (x @ read_write) = x.contents
[%%expect{|
val foo : 'a ref -> 'a = <fun>
|}]

let foo (x @ immutable) a = x := a
[%%expect{|
Line 1, characters 28-29:
1 | let foo (x @ immutable) a = x := a
                                ^
Error: This value is "immutable" but expected to be "read_write".
|}]

let foo (x @ read_only) a = x := a
[%%expect{|
Line 1, characters 28-29:
1 | let foo (x @ read_only) a = x := a
                                ^
Error: This value is "read_only" but expected to be "read_write".
|}]

let foo (x @ read_write) a = x := a
[%%expect{|
val foo : 'a ref -> 'a -> unit = <fun>
|}]

(* API that uses the [uncontended_data] layout kind *)

module Atomic : sig
  type !'a t : uncontended_data

  val make : 'a -> 'a t @@ portable deterministic
  val get : 'a t @ read_only -> 'a @@ portable deterministic
  val set : 'a t -> 'a -> unit @@ portable deterministic
end = struct
  type !'a t : uncontended_data

  external make : 'a -> 'a t @@ portable deterministic = "%makemutable"
  external get : 'a t @ read_only -> 'a @@ portable deterministic = "%atomic_load"
  external ignore : 'a -> unit @@ portable deterministic = "%ignore"
  external exchange : 'a t -> 'a -> 'a @@ portable deterministic = "%atomic_exchange"
  let set r x = ignore (exchange r x)
end
[%%expect{|
module Atomic :
  sig
    type !'a t : uncontended_data
    val make : 'a -> 'a t @@ portable deterministic
    val get : 'a t @ read_only -> 'a @@ portable deterministic
    val set : 'a t -> 'a -> unit @@ portable deterministic
  end
|}]


(* Simple checks of Atomic API *)
let foo (a @ read_only) = Atomic.set a 42
[%%expect{|
Line 1, characters 37-38:
1 | let foo (a @ read_only) = Atomic.set a 42
                                         ^
Error: This value is "read_only" but expected to be "read_write".
|}]

let foo (a @ read_write) = Atomic.set a 0
[%%expect{|
val foo : int Atomic.t -> unit = <fun>
|}]

let foo (a @ constant) = Atomic.set a 9
[%%expect{|
Line 1, characters 13-21:
1 | let foo (a @ constant) = Atomic.set a 9
                 ^^^^^^^^
Error: Unrecognized mode constant.
|}]

let foo (a @ read_only) = Atomic.get a
[%%expect{|
val foo : 'a Atomic.t @ read_only -> 'a = <fun>
|}]

let foo (a @ read_write) = Atomic.get a
[%%expect{|
val foo : 'a Atomic.t -> 'a = <fun>
|}]

let foo (a @ constant) = Atomic.get a
[%%expect{|
Line 1, characters 13-21:
1 | let foo (a @ constant) = Atomic.get a
                 ^^^^^^^^
Error: Unrecognized mode constant.
|}]

(* Closing over use of read_write gives nondeterministic *)
let foo () =
    let a = Atomic.make 42 in
    let bar () = Atomic.set a 1 in
    let _ @ deterministic = bar in
    ()
[%%expect{|
Line 4, characters 28-31:
4 |     let _ @ deterministic = bar in
                                ^^^
Error: This value is "nondeterministic" but expected to be "deterministic".
|}]

let foo : int Atomic.t @ read_write -> (unit -> unit) @ deterministic =
    fun a () -> Atomic.set a 2
[%%expect{|
Line 2, characters 4-30:
2 |     fun a () -> Atomic.set a 2
        ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This function when partially applied returns a value which is "nondeterministic",
       but expected to be "deterministic".
|}]

let a @ read_write = Atomic.make 42
[%%expect{|
val a : int Atomic.t = <abstr>
|}]

let foo @ deterministic =
    fun () -> Atomic.set a 0
[%%expect{|
Line 2, characters 25-26:
2 |     fun () -> Atomic.set a 0
                             ^
Error: The value "a" is nondeterministic, so cannot be used inside a function that is deterministic.
|}]

(* Closing over use of read_only gives observing *)
let foo () =
    let a = Atomic.make 0 in
    let bar () = Atomic.get a in
    let _ @ deterministic = bar in
    ()
[%%expect{|
Line 4, characters 28-31:
4 |     let _ @ deterministic = bar in
                                ^^^
Error: This value is "observing" but expected to be "deterministic".
|}]
