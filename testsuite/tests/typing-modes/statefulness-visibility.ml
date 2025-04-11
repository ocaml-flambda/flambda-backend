(* TEST
   expect;
*)

(** In this file, we test the dual relationship between [visibility] and [statefulness]
    axes, [visibility] requirements over mutable record fields, and the kind [sync_data]. *)

(* Visibility requirements over mutable record fields.
   [uncontended] to avoid contention errors printed first. *)

type 'a myref = { mutable a : 'a; b : 'a }
[%%expect{|
type 'a myref = { mutable a : 'a; b : 'a; }
|}]

let foo x a = x.a <- a
[%%expect{|
val foo : 'a myref -> 'a -> unit = <fun>
|}]

let foo (x @ read uncontended) a = x.a <- a
[%%expect{|
Line 1, characters 35-36:
1 | let foo (x @ read uncontended) a = x.a <- a
                                       ^
Error: This value is "read" but expected to be "read_write".
  Hint: In order to write into its mutable fields,
  this record needs to have read_write visibility.
|}]

let foo (x @ immutable uncontended) a = x.a <- a
[%%expect{|
Line 1, characters 40-41:
1 | let foo (x @ immutable uncontended) a = x.a <- a
                                            ^
Error: This value is "immutable" but expected to be "read_write".
  Hint: In order to write into its mutable fields,
  this record needs to have read_write visibility.
|}]

let foo (x @ read uncontended) = x.a
[%%expect{|
val foo : 'a myref @ uncontended read -> 'a @ uncontended read = <fun>
|}]

let foo (x @ immutable uncontended) = x.a
[%%expect{|
Line 1, characters 38-39:
1 | let foo (x @ immutable uncontended) = x.a
                                          ^
Error: This value is "immutable" but expected to be "read".
  Hint: In order to read from its mutable fields,
  this record needs to have read visibility.
|}]

let foo (x @ read uncontended) upd = { x with a = upd }
[%%expect{|
val foo : 'a myref @ uncontended read -> 'a -> 'a myref @ uncontended read =
  <fun>
|}]

let foo (x @ immutable uncontended) upd = { x with a = upd }
[%%expect{|
val foo : 'a myref @ uncontended immutable -> 'a -> 'a myref @ immutable =
  <fun>
|}]

let foo (x @ immutable uncontended) upd = { x with b = upd }
[%%expect{|
Line 1, characters 44-45:
1 | let foo (x @ immutable uncontended) upd = { x with b = upd }
                                                ^
Error: This value is "immutable" but expected to be "read".
  Hint: In order to read from its mutable fields,
  this record needs to have read visibility.
|}]

(* Errors when mutating a record field prints contention before visibility errors *)

let foo (x @ read contended) a = x.a <- a
[%%expect{|
Line 1, characters 33-34:
1 | let foo (x @ read contended) a = x.a <- a
                                     ^
Error: This value is "contended" but expected to be "uncontended".
  Hint: In order to write into its mutable fields,
  this record needs to be uncontended.
|}]

let foo (x @ contended read) a = x.a <- a
[%%expect{|
Line 1, characters 33-34:
1 | let foo (x @ contended read) a = x.a <- a
                                     ^
Error: This value is "contended" but expected to be "uncontended".
  Hint: In order to write into its mutable fields,
  this record needs to be uncontended.
|}]

let foo (x @ read shared) a = x.a <- a
[%%expect{|
Line 1, characters 30-31:
1 | let foo (x @ read shared) a = x.a <- a
                                  ^
Error: This value is "shared" but expected to be "uncontended".
  Hint: In order to write into its mutable fields,
  this record needs to be uncontended.
|}]

let foo (x @ immutable contended) a = x.a
[%%expect{|
Line 1, characters 38-39:
1 | let foo (x @ immutable contended) a = x.a
                                          ^
Error: This value is "contended" but expected to be "shared".
  Hint: In order to read from its mutable fields,
  this record needs to be at least shared.
|}]

(* visibility requirements over refs *)

let foo (x @ immutable) = x.contents
[%%expect{|
Line 1, characters 26-27:
1 | let foo (x @ immutable) = x.contents
                              ^
Error: This value is "contended" but expected to be "shared".
  Hint: In order to read from its mutable fields,
  this record needs to be at least shared.
|}]

let foo (x @ immutable shared) = x.contents
[%%expect{|
Line 1, characters 33-34:
1 | let foo (x @ immutable shared) = x.contents
                                     ^
Error: This value is "immutable" but expected to be "read".
  Hint: In order to read from its mutable fields,
  this record needs to have read visibility.
|}]

let foo (x @ immutable uncontended) = x.contents
[%%expect{|
Line 1, characters 38-39:
1 | let foo (x @ immutable uncontended) = x.contents
                                          ^
Error: This value is "immutable" but expected to be "read".
  Hint: In order to read from its mutable fields,
  this record needs to have read visibility.
|}]

let foo (x @ read) = x.contents
[%%expect{|
val foo : 'a ref @ read -> 'a @ read = <fun>
|}]

let foo (x @ read contended) = x.contents
[%%expect{|
Line 1, characters 31-32:
1 | let foo (x @ read contended) = x.contents
                                   ^
Error: This value is "contended" but expected to be "shared".
  Hint: In order to read from its mutable fields,
  this record needs to be at least shared.
|}]

let foo (x @ read uncontended) = x.contents
[%%expect{|
val foo : 'a ref @ uncontended read -> 'a @ uncontended read = <fun>
|}]

let foo (x @ read_write) = x.contents
[%%expect{|
val foo : 'a ref -> 'a = <fun>
|}]

let foo (x @ read_write contended) = x.contents
[%%expect{|
Line 1, characters 37-38:
1 | let foo (x @ read_write contended) = x.contents
                                         ^
Error: This value is "contended" but expected to be "shared".
  Hint: In order to read from its mutable fields,
  this record needs to be at least shared.
|}]

let foo (x @ read_write shared) = x.contents
[%%expect{|
val foo : 'a ref @ shared -> 'a @ shared = <fun>
|}]

let foo (x @ immutable) a = x := a
[%%expect{|
Line 1, characters 28-29:
1 | let foo (x @ immutable) a = x := a
                                ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (x @ immutable shared) a = x := a
[%%expect{|
Line 1, characters 35-36:
1 | let foo (x @ immutable shared) a = x := a
                                       ^
Error: This value is "shared" but expected to be "uncontended".
|}]

let foo (x @ immutable uncontended) a = x := a
[%%expect{|
Line 1, characters 40-41:
1 | let foo (x @ immutable uncontended) a = x := a
                                            ^
Error: This value is "immutable" but expected to be "read_write".
|}]

let foo (x @ read) a = x := a
[%%expect{|
Line 1, characters 23-24:
1 | let foo (x @ read) a = x := a
                           ^
Error: This value is "shared" but expected to be "uncontended".
|}]

let foo (x @ read contended) a = x := a
[%%expect{|
Line 1, characters 33-34:
1 | let foo (x @ read contended) a = x := a
                                     ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (x @ read uncontended) a = x := a
[%%expect{|
Line 1, characters 35-36:
1 | let foo (x @ read uncontended) a = x := a
                                       ^
Error: This value is "read" but expected to be "read_write".
|}]

let foo (x @ read_write) a = x := a
[%%expect{|
val foo : 'a ref -> 'a -> unit = <fun>
|}]

let foo (x @ read_write contended) a = x := a
[%%expect{|
Line 1, characters 39-40:
1 | let foo (x @ read_write contended) a = x := a
                                           ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (x @ read_write shared) a = x := a
[%%expect{|
Line 1, characters 36-37:
1 | let foo (x @ read_write shared) a = x := a
                                        ^
Error: This value is "shared" but expected to be "uncontended".
|}]

let foo (x @ immutable) = !x
[%%expect{|
Line 1, characters 27-28:
1 | let foo (x @ immutable) = !x
                               ^
Error: This value is "contended" but expected to be "uncontended".
|}]

(* CR dkalinichenko: update Stdlib to reflect required visibility and contention. *)

let foo (x @ immutable shared) = !x
[%%expect{|
Line 1, characters 34-35:
1 | let foo (x @ immutable shared) = !x
                                      ^
Error: This value is "shared" but expected to be "uncontended".
|}]

let foo (x @ immutable uncontended) = !x
[%%expect{|
Line 1, characters 39-40:
1 | let foo (x @ immutable uncontended) = !x
                                           ^
Error: This value is "immutable" but expected to be "read_write".
|}]

let foo (x @ read) = !x
[%%expect{|
Line 1, characters 22-23:
1 | let foo (x @ read) = !x
                          ^
Error: This value is "shared" but expected to be "uncontended".
|}]

let foo (x @ read contended) = !x
[%%expect{|
Line 1, characters 32-33:
1 | let foo (x @ read contended) = !x
                                    ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (x @ read uncontended) = !x
[%%expect{|
Line 1, characters 34-35:
1 | let foo (x @ read uncontended) = !x
                                      ^
Error: This value is "read" but expected to be "read_write".
|}]

let foo (x @ read_write) = !x
[%%expect{|
val foo : 'a ref -> 'a = <fun>
|}]

let foo (x @ read_write contended) = !x
[%%expect{|
Line 1, characters 38-39:
1 | let foo (x @ read_write contended) = !x
                                          ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let foo (x @ read_write shared) = !x
[%%expect{|
Line 1, characters 35-36:
1 | let foo (x @ read_write shared) = !x
                                       ^
Error: This value is "shared" but expected to be "uncontended".
|}]

(* API that uses the [sync_data] kind. *)

module Atomic : sig @@ stateless
  type !'a t : sync_data with 'a @@ contended

  val make : 'a -> 'a t
  val get : 'a t @ read -> 'a
  val set : 'a t -> 'a -> unit
end = struct
  type !'a t : sync_data with 'a @@ contended

  external make : 'a -> 'a t @@ stateless = "%makemutable"
  external get : 'a t @ read -> 'a @@ stateless = "%atomic_load"
  external ignore : 'a -> unit @@ stateless = "%ignore"
  external exchange : 'a t -> 'a -> 'a @@ stateless = "%atomic_exchange"
  let set r x = ignore (exchange r x)
end
[%%expect{|
module Atomic :
  sig
    type !'a t : sync_data with 'a @@ contended
    val make : 'a -> 'a t @@ stateless
    val get : 'a t @ read -> 'a @@ stateless
    val set : 'a t -> 'a -> unit @@ stateless
  end
|}]


(* Simple checks of Atomic API *)
let foo (a @ read) = Atomic.set a 42
[%%expect{|
Line 1, characters 32-33:
1 | let foo (a @ read) = Atomic.set a 42
                                    ^
Error: This value is "read" but expected to be "read_write".
|}]

let foo (a @ read_write) = Atomic.set a 0
[%%expect{|
val foo : int Atomic.t -> unit = <fun>
|}]

let foo (a @ immutable) = Atomic.set a 9
[%%expect{|
Line 1, characters 37-38:
1 | let foo (a @ immutable) = Atomic.set a 9
                                         ^
Error: This value is "immutable" but expected to be "read_write".
|}]

let foo (a @ read) = Atomic.get a
[%%expect{|
val foo : 'a Atomic.t @ read -> 'a = <fun>
|}]

let foo (a @ read_write) = Atomic.get a
[%%expect{|
val foo : 'a Atomic.t -> 'a = <fun>
|}]

let foo (a @ immutable) = Atomic.get a
[%%expect{|
Line 1, characters 37-38:
1 | let foo (a @ immutable) = Atomic.get a
                                         ^
Error: This value is "immutable" but expected to be "read".
|}]

(* Closing over use of read_write gives stateful *)
let foo () =
    let a = Atomic.make 42 in
    let bar () = Atomic.set a 1 in
    let _ @ stateless = bar in
    ()
[%%expect{|
Line 4, characters 24-27:
4 |     let _ @ stateless = bar in
                            ^^^
Error: This value is "stateful" but expected to be "stateless".
|}]

let foo : int Atomic.t @ read_write -> (unit -> unit) @ stateless =
    fun a () -> Atomic.set a 2
[%%expect{|
Line 2, characters 4-30:
2 |     fun a () -> Atomic.set a 2
        ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This function when partially applied returns a value which is "stateful",
       but expected to be "stateless".
|}]

let a @ read_write = Atomic.make 42
[%%expect{|
val a : int Atomic.t = <abstr>
|}]

let foo @ stateless =
    fun () -> Atomic.set a 0
[%%expect{|
Line 2, characters 25-26:
2 |     fun () -> Atomic.set a 0
                             ^
Error: This value is "immutable" but expected to be "read_write".
|}]

(* Closing over a stateful value also gives stateful. *)

let foo (f : (unit -> unit) @ stateful) @ stateful = fun () -> f ()
[%%expect{|
val foo : (unit -> unit) -> unit -> unit = <fun>
|}]

let foo (f : (unit -> unit) @ stateful portable) @ stateless = fun () -> f ()
[%%expect{|
Line 1, characters 73-74:
1 | let foo (f : (unit -> unit) @ stateful portable) @ stateless = fun () -> f ()
                                                                             ^
Error: The value "f" is stateful, so cannot be used inside a function that may not capture state.
|}]

(* The error for [portable] is displayed first. *)

let foo (f : (unit -> unit) @ stateful) @ stateless = fun () -> f ()
[%%expect{|
Line 1, characters 64-65:
1 | let foo (f : (unit -> unit) @ stateful) @ stateless = fun () -> f ()
                                                                    ^
Error: The value "f" is nonportable, so cannot be used inside a function that is portable.
|}]

(* CR modes: fix the error message. *)

let foo (f : (unit -> unit) @ observing portable) @ stateless = fun () -> f ()
[%%expect{|
Line 1, characters 74-75:
1 | let foo (f : (unit -> unit) @ observing portable) @ stateless = fun () -> f ()
                                                                              ^
Error: The value "f" is stateful, so cannot be used inside a function that may not capture state.
|}]

(* Closing over use of read gives observing *)
let foo () =
    let a = Atomic.make 0 in
    let bar () = Atomic.get a in
    let _ @ observing = bar in
    ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

let foo () =
  let a = Atomic.make 0 in
  let bar () = Atomic.get a in
  let _ @ stateless = bar in
  ()

[%%expect{|
Line 4, characters 22-25:
4 |   let _ @ stateless = bar in
                          ^^^
Error: This value is "observing" but expected to be "stateless".
|}]

(* Closing over a observing value also gives observing. *)

let foo (f : (unit -> unit) @ observing) @ observing = fun () -> f ()
[%%expect{|
val foo : (unit -> unit) @ observing -> (unit -> unit) @ observing = <fun>
|}]

let foo (f : (unit -> unit) @ observing) @ stateful = fun () -> f ()
[%%expect{|
val foo : (unit -> unit) @ observing -> unit -> unit = <fun>
|}]

(* CR modes: fix the error message. *)

let foo (f : (unit -> unit) @ stateful) @ observing = fun () -> f ()
[%%expect{|
Line 1, characters 64-65:
1 | let foo (f : (unit -> unit) @ stateful) @ observing = fun () -> f ()
                                                                    ^
Error: The value "f" is stateful, so cannot be used inside a function that may not capture state.
|}]

(* Testing defaulting  *)

(* [stateless] => [portable]. *)

let default : 'a @ stateless -> 'a @ portable = fun x -> x
[%%expect{|
val default : 'a @ stateless -> 'a @ portable = <fun>
|}]

let override : 'a @ stateless nonportable -> 'a @ portable = fun x -> x
[%%expect{|
Line 1, characters 70-71:
1 | let override : 'a @ stateless nonportable -> 'a @ portable = fun x -> x
                                                                          ^
Error: This value is "nonportable" but expected to be "portable".
|}]

(* [observing] or [stateful] don't change the default. *)

let fails : 'a @ observing -> 'a @ portable = fun x -> x
[%%expect{|
Line 1, characters 55-56:
1 | let fails : 'a @ observing -> 'a @ portable = fun x -> x
                                                           ^
Error: This value is "nonportable" but expected to be "portable".
|}]

let succeeds : 'a @ observing portable -> 'a @ portable = fun x -> x
[%%expect{|
val succeeds : 'a @ portable observing -> 'a @ portable = <fun>
|}]

let fails : 'a @ stateful -> 'a @ portable = fun x -> x
[%%expect{|
Line 1, characters 54-55:
1 | let fails : 'a @ stateful -> 'a @ portable = fun x -> x
                                                          ^
Error: This value is "nonportable" but expected to be "portable".
|}]

let succeeds : 'a @ stateful portable -> 'a @ portable = fun x -> x
[%%expect{|
val succeeds : 'a @ portable -> 'a @ portable = <fun>
|}]

(* Modalities. *)

type 'a t = { x : 'a @@ stateless }

let get : 'a t -> 'a @ portable = fun t -> t.x

[%%expect{|
type 'a t = { x : 'a @@ stateless; }
val get : 'a t -> 'a @ portable = <fun>
|}]

(* [immutable] => [contended]. *)

let default : 'a @ contended -> ('a @ immutable -> 'b) -> 'b = fun x f -> f x
[%%expect{|
val default : 'a @ contended -> ('a @ immutable -> 'b) -> 'b = <fun>
|}]

let override : 'a @ contended -> ('a @ immutable uncontended -> 'b) -> 'b = fun x f -> f x
[%%expect{|
Line 1, characters 89-90:
1 | let override : 'a @ contended -> ('a @ immutable uncontended -> 'b) -> 'b = fun x f -> f x
                                                                                             ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let override : 'a @ contended -> ('a @ immutable shared -> 'b) -> 'b = fun x f -> f x
[%%expect{|
Line 1, characters 84-85:
1 | let override : 'a @ contended -> ('a @ immutable shared -> 'b) -> 'b = fun x f -> f x
                                                                                        ^
Error: This value is "contended" but expected to be "shared".
|}]

(* [read] => [shared]. *)

let default : 'a @ shared -> ('a @ read -> 'b) -> 'b = fun x f -> f x
[%%expect{|
val default : 'a @ shared -> ('a @ read -> 'b) -> 'b = <fun>
|}]

let default : 'a @ contended -> ('a @ read -> 'b) -> 'b = fun x f -> f x
[%%expect{|
Line 1, characters 71-72:
1 | let default : 'a @ contended -> ('a @ read -> 'b) -> 'b = fun x f -> f x
                                                                           ^
Error: This value is "contended" but expected to be "shared".
|}]

let override : 'a @ contended -> ('a @ read uncontended -> 'b) -> 'b = fun x f -> f x
[%%expect{|
Line 1, characters 84-85:
1 | let override : 'a @ contended -> ('a @ read uncontended -> 'b) -> 'b = fun x f -> f x
                                                                                        ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let override : 'a @ contended -> ('a @ read contended -> 'b) -> 'b = fun x f -> f x
[%%expect{|
val override : 'a @ contended -> ('a @ contended read -> 'b) -> 'b = <fun>
|}]

(* [read_write] doesn't change the default. *)


let fails : 'a @ contended -> ('a @ read_write uncontended -> 'b) -> 'b = fun x f -> f x
[%%expect{|
Line 1, characters 87-88:
1 | let fails : 'a @ contended -> ('a @ read_write uncontended -> 'b) -> 'b = fun x f -> f x
                                                                                           ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let fails : 'a @ contended -> ('a @ read_write shared -> 'b) -> 'b = fun x f -> f x
[%%expect{|
Line 1, characters 82-83:
1 | let fails : 'a @ contended -> ('a @ read_write shared -> 'b) -> 'b = fun x f -> f x
                                                                                      ^
Error: This value is "contended" but expected to be "shared".
|}]

let fails : 'a @ contended -> ('a @ read_write -> 'b) -> 'b = fun x f -> f x
[%%expect{|
Line 1, characters 75-76:
1 | let fails : 'a @ contended -> ('a @ read_write -> 'b) -> 'b = fun x f -> f x
                                                                               ^
Error: This value is "contended" but expected to be "uncontended".
|}]

let fails : 'a @ shared -> ('a @ read_write -> 'b) -> 'b = fun x f -> f x
[%%expect{|
Line 1, characters 72-73:
1 | let fails : 'a @ shared -> ('a @ read_write -> 'b) -> 'b = fun x f -> f x
                                                                            ^
Error: This value is "shared" but expected to be "uncontended".
|}]

let succeeds : 'a @ contended -> ('a @ read_write contended -> 'b) -> 'b = fun x f -> f x
[%%expect{|
val succeeds : 'a @ contended -> ('a @ contended -> 'b) -> 'b = <fun>
|}]

let succeeds : 'a @ shared -> ('a @ read_write shared -> 'b) -> 'b = fun x f -> f x
[%%expect{|
val succeeds : 'a @ shared -> ('a @ shared -> 'b) -> 'b = <fun>
|}]

(* Modalities. *)

type 'a t1 = { y : 'a @@ immutable }

let get : 'a @ contended -> 'a t1 = fun y -> {y}

type 'a t2 = { z : 'a @@ read }

let get : 'a @ shared -> 'a t2 = fun z -> {z}

[%%expect{|
type 'a t1 = { y : 'a @@ immutable; }
val get : 'a @ contended -> 'a t1 = <fun>
type 'a t2 = { z : 'a @@ read; }
val get : 'a @ shared -> 'a t2 = <fun>
|}]

(* Interactions with lazy values. *)

(* [lazy_t @ stateless] capture values at [immutable]. *)
let foo (x : int ref) @ stateless = lazy (x.contents)

[%%expect{|
Line 1, characters 42-43:
1 | let foo (x : int ref) @ stateless = lazy (x.contents)
                                              ^
Error: This value is "immutable" but expected to be "read".
  Hint: In order to read from its mutable fields,
  this record needs to have read visibility.
|}]

let zap (x : int ref) @ stateless = lazy (x.contents <- 3)
[%%expect{|
Line 1, characters 42-43:
1 | let zap (x : int ref) @ stateless = lazy (x.contents <- 3)
                                              ^
Error: This value is "immutable" but expected to be "read_write".
  Hint: In order to write into its mutable fields,
  this record needs to have read_write visibility.
|}]

(* [lazy_t @ observing] capture values at [read]. *)

let bat (x : int ref) @ observing = lazy (x.contents <- 4)
[%%expect{|
Line 1, characters 42-43:
1 | let bat (x : int ref) @ observing = lazy (x.contents <- 4)
                                              ^
Error: This value is "read" but expected to be "read_write".
  Hint: In order to write into its mutable fields,
  this record needs to have read_write visibility.
|}]

let bar (x : int ref) @ observing = lazy (x.contents)

[%%expect{|
val bar : int ref -> int lazy_t @ observing = <fun>
|}]

let () =
  match bar {contents = 5} with
  | lazy 5 -> ()
  | _ -> assert false

[%%expect{|
|}]

(* [contended] lazy values can't be forced. *)
let fuz (x : int ref) @ observing immutable = lazy (x.contents)
[%%expect{|
val fuz : int ref -> int lazy_t @ observing immutable = <fun>
|}]

let () =
  match fuz {contents = -1} with
  | lazy (-1) -> ()
  | _ -> assert false

[%%expect{|
Line 3, characters 4-13:
3 |   | lazy (-1) -> ()
        ^^^^^^^^^
Error: This value is "contended" but expected to be "uncontended".
  Hint: In order to force the lazy expression,
  the lazy needs to be uncontended.
|}]

(* But [immutable] lazy values can be, by design. *)
let baz (x : int ref) @ observing immutable uncontended = lazy (x.contents)

[%%expect{|
val baz : int ref -> int lazy_t @ uncontended observing immutable = <fun>
|}]

let () =
  match baz {contents = 42} with
  | lazy 42 -> ()
  | _ -> assert false

[%%expect{|
|}]

let zab () @ immutable uncontended = lazy (ref 5)

[%%expect{|
val zab : unit -> int ref lazy_t @ uncontended immutable = <fun>
|}]

(* Forcing an [immutable] lazy returns an [immutable] value. *)
let () =
  match zab () with
  | lazy x ->
    x.contents <- 42;
    assert (x.contents = 42)

[%%expect{|
Line 4, characters 4-5:
4 |     x.contents <- 42;
        ^
Error: This value is "immutable" but expected to be "read_write".
  Hint: In order to write into its mutable fields,
  this record needs to have read_write visibility.
|}]

(* Forcing a [read] lazy returns a [read] value.*)
let zag () @ read uncontended = lazy (ref 42)

[%%expect{|
val zag : unit -> int ref lazy_t @ uncontended read = <fun>
|}]

let () =
  match zag () with
  | lazy y ->
    assert (y.contents = 42);
    y.contents <- 24

[%%expect{|
Line 5, characters 4-5:
5 |     y.contents <- 24
        ^
Error: This value is "read" but expected to be "read_write".
  Hint: In order to write into its mutable fields,
  this record needs to have read_write visibility.
|}]
