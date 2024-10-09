(* TEST
   include stdlib_stable;
   flags = "-extension mode_alpha";
   expect;
*)

(** In this file, we test the dual relationship between the Coordinate and Coordinated axes,
    and the new layout kind [atomically_mutable_data] *)

(* Dummy API to stand in for Atomic, here hardcoded to store unit *)
module GhostUnitAtomic : sig
    type t : atomically_mutable_data

    val make : unit -> t @ coordinated_write @@ portable coordinate_nothing

    val get : t @ coordinated_read -> unit @ contended @@ portable coordinate_nothing

    val set : t @ coordinated_write -> unit @ contended -> unit @@ portable coordinate_nothing
end = struct
    type t = unit
    let make () = ()
    let get _ = ()
    let set _ _ = ()
end
[%%expect{|
module GhostUnitAtomic :
  sig
    type t : atomically_mutable_data
    val make : unit -> t @@ portable coordinate_nothing
    val get : t @ coordinated_read -> unit @ contended @@ portable
      coordinate_nothing
    val set : t -> unit @ contended -> unit @@ portable coordinate_nothing
  end
|}]

(* Simple checks of dummy API *)
let foo (a @ coordinated_read) = GhostUnitAtomic.set a ()
[%%expect{|
Line 1, characters 53-54:
1 | let foo (a @ coordinated_read) = GhostUnitAtomic.set a ()
                                                         ^
Error: This value is "coordinated_read" but expected to be "coordinated_write".
|}]

let foo (a @ coordinated_write) = GhostUnitAtomic.set a ()
[%%expect{|
val foo : GhostUnitAtomic.t -> unit @@ global many = <fun>
|}]

let foo (a @ coordinated_none) = GhostUnitAtomic.set a ()
[%%expect{|
Line 1, characters 53-54:
1 | let foo (a @ coordinated_none) = GhostUnitAtomic.set a ()
                                                         ^
Error: This value is "coordinated_none" but expected to be "coordinated_write".
|}]

let foo (a @ coordinated_read) = GhostUnitAtomic.get a
[%%expect{|
val foo : GhostUnitAtomic.t @ coordinated_read -> unit @@ global many = <fun>
|}]

let foo (a @ coordinated_write) = GhostUnitAtomic.get a
[%%expect{|
val foo : GhostUnitAtomic.t -> unit @@ global many = <fun>
|}]

let foo (a @ coordinated_none) = GhostUnitAtomic.get a
[%%expect{|
Line 1, characters 53-54:
1 | let foo (a @ coordinated_none) = GhostUnitAtomic.get a
                                                         ^
Error: This value is "coordinated_none" but expected to be "coordinated_read".
|}]

(* Closing over use of coordinated_write gives coordinate_writing *)
let foo () =
    let a = GhostUnitAtomic.make () in
    let bar () = GhostUnitAtomic.set a () in
    let _ @ coordinate_nothing = bar in
    ()
[%%expect{|
Line 4, characters 33-36:
4 |     let _ @ coordinate_nothing = bar in
                                     ^^^
Error: This value is "coordinate_writing" but expected to be "coordinate_nothing".
|}]

let foo : GhostUnitAtomic.t @ coordinated_write -> (unit -> unit) @ coordinate_nothing =
    fun a () -> GhostUnitAtomic.set a ()
[%%expect{|
Line 2, characters 4-40:
2 |     fun a () -> GhostUnitAtomic.set a ()
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This function when partially applied returns a value which is "coordinate_writing",
       but expected to be "coordinate_nothing".
|}]

let a @ coordinated_write = GhostUnitAtomic.make ()
[%%expect{|
val a : GhostUnitAtomic.t @@ global many = <abstr>
|}]

let foo @ coordinate_nothing =
    fun () -> GhostUnitAtomic.set a ()
[%%expect{|
Line 2, characters 34-35:
2 |     fun () -> GhostUnitAtomic.set a ()
                                      ^
Error: The value "a" is coordinate_writing, so cannot be used inside a function that is coordinate_nothing.
|}]

(* Closing over use of coordinated_read gives atomicallyread *)
let foo () =
    let a = GhostUnitAtomic.make () in
    let bar () = GhostUnitAtomic.get a in
    let _ @ coordinate_nothing = bar in
    ()
[%%expect{|
Line 4, characters 33-36:
4 |     let _ @ coordinate_nothing = bar in
                                     ^^^
Error: This value is "coordinate_reading" but expected to be "coordinate_nothing".
|}]
