(* TEST
   include stdlib_stable;
   flags = "-extension mode_alpha";
   expect;
*)

(** In this file, we test the dual relationship between the Coordinate and Coordinated axes,
    and the new layout kind [atomically_mutable_data] *)

  module Atomic : sig
    type !'a t : atomically_mutable_data

    val make : 'a -> 'a t @@ portable coordinate_nothing
    val get : 'a t @ coordinated_read -> 'a @@ portable coordinate_nothing
    val set : 'a t -> 'a -> unit @@ portable coordinate_nothing
  end = struct
    type !'a t : atomically_mutable_data

    external make : 'a -> 'a t @@ portable coordinate_nothing = "%makemutable"
    external get : 'a t @ coordinated_read -> 'a @@ portable coordinate_nothing = "%atomic_load"
    external ignore : 'a -> unit @@ portable coordinate_nothing = "%ignore"
    external exchange : 'a t -> 'a -> 'a @@ portable coordinate_nothing = "%atomic_exchange"
    let set r x = ignore (exchange r x)
  end
[%%expect{|
module Atomic :
  sig
    type !'a t : atomically_mutable_data
    val make : 'a -> 'a t @@ portable coordinate_nothing
    val get : 'a t @ coordinated_read -> 'a @@ portable coordinate_nothing
    val set : 'a t -> 'a -> unit @@ portable coordinate_nothing
  end
|}]


(* Simple checks of dummy API *)
let foo (a @ coordinated_read) = Atomic.set a 42
[%%expect{|
Line 1, characters 44-45:
1 | let foo (a @ coordinated_read) = Atomic.set a 42
                                                ^
Error: This value is "coordinated_read" but expected to be "coordinated_write".
|}]

let foo (a @ coordinated_write) = Atomic.set a 0
[%%expect{|
val foo : int Atomic.t -> unit @@ global many = <fun>
|}]

let foo (a @ coordinated_none) = Atomic.set a 9
[%%expect{|
Line 1, characters 44-45:
1 | let foo (a @ coordinated_none) = Atomic.set a 9
                                                ^
Error: This value is "coordinated_none" but expected to be "coordinated_write".
|}]

let foo (a @ coordinated_read) = Atomic.get a
[%%expect{|
val foo : 'a Atomic.t @ coordinated_read -> 'a @@ global many = <fun>
|}]

let foo (a @ coordinated_write) = Atomic.get a
[%%expect{|
val foo : 'a Atomic.t -> 'a @@ global many = <fun>
|}]

let foo (a @ coordinated_none) = Atomic.get a
[%%expect{|
Line 1, characters 44-45:
1 | let foo (a @ coordinated_none) = Atomic.get a
                                                ^
Error: This value is "coordinated_none" but expected to be "coordinated_read".
|}]

(* Closing over use of coordinated_write gives coordinate_writing *)
let foo () =
    let a = Atomic.make 42 in
    let bar () = Atomic.set a 1 in
    let _ @ coordinate_nothing = bar in
    ()
[%%expect{|
Line 4, characters 33-36:
4 |     let _ @ coordinate_nothing = bar in
                                     ^^^
Error: This value is "coordinate_writing" but expected to be "coordinate_nothing".
|}]

let foo : int Atomic.t @ coordinated_write -> (unit -> unit) @ coordinate_nothing =
    fun a () -> Atomic.set a 2
[%%expect{|
Line 2, characters 4-30:
2 |     fun a () -> Atomic.set a 2
        ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This function when partially applied returns a value which is "coordinate_writing",
       but expected to be "coordinate_nothing".
|}]

let a @ coordinated_write = Atomic.make 42
[%%expect{|
val a : int Atomic.t @@ global many = <abstr>
|}]

let foo @ coordinate_nothing =
    fun () -> Atomic.set a 0
[%%expect{|
Line 2, characters 25-26:
2 |     fun () -> Atomic.set a 0
                             ^
Error: The value "a" is coordinate_writing, so cannot be used inside a function that is coordinate_nothing.
|}]

(* Closing over use of coordinated_read gives coordinate_reading *)
let foo () =
    let a = Atomic.make 0 in
    let bar () = Atomic.get a in
    let _ @ coordinate_nothing = bar in
    ()
[%%expect{|
Line 4, characters 33-36:
4 |     let _ @ coordinate_nothing = bar in
                                     ^^^
Error: This value is "coordinate_reading" but expected to be "coordinate_nothing".
|}]
