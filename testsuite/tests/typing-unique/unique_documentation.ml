(* TEST
   flags += "-extension unique_alpha";
   expect;
*)

(*******************************)
(* Examples from documentation *)

(*************)
(* intro.md *)

type t = Con of { field : int list }

let free : t @ unique -> unit = fun t -> ()
let free_field (unique_ i) = ()
let store : t @ aliased -> unit = fun t -> ()
let store_field i = ()
let flip_coin () = true
[%%expect{|
type t = Con of { field : int list; }
val free : t @ unique -> unit @@ global many = <fun>
val free_field : 'a @ unique -> unit @@ global many = <fun>
val store : t -> unit @@ global many = <fun>
val store_field : 'a -> unit @@ global many = <fun>
val flip_coin : unit -> bool @@ global many = <fun>
|}]

let test () =
  let dup : t -> t * t @ aliased = function t -> t, t in
  let delay_free : t @ unique -> (unit -> unit) @ once = function t -> fun () -> free t in
  let alias : 'a @ unique -> 'a @ aliased = fun x -> x in
  let linearize : 'a @ many -> 'a @ once = fun x -> x in
  ()
[%%expect{|
Line 2, characters 6-9:
2 |   let dup : t -> t * t @ aliased = function t -> t, t in
          ^^^
Warning 26 [unused-var]: unused variable dup.

Line 3, characters 6-16:
3 |   let delay_free : t @ unique -> (unit -> unit) @ once = function t -> fun () -> free t in
          ^^^^^^^^^^
Warning 26 [unused-var]: unused variable delay_free.

Line 4, characters 6-11:
4 |   let alias : 'a @ unique -> 'a @ aliased = fun x -> x in
          ^^^^^
Warning 26 [unused-var]: unused variable alias.

Line 5, characters 6-15:
5 |   let linearize : 'a @ many -> 'a @ once = fun x -> x in
          ^^^^^^^^^
Warning 26 [unused-var]: unused variable linearize.

val test : unit -> unit @@ global many = <fun>
|}]

type 'a aliased = { a : 'a @@ aliased } [@@unboxed]

let cons : 'a @ aliased -> 'a aliased list @ unique -> 'a aliased list @ unique =
  fun x xs -> { a = x } :: xs
[%%expect{|
type 'a aliased = { a : 'a @@ aliased; } [@@unboxed]
val cons : 'a -> 'a aliased list @ unique -> 'a aliased list @ unique @@
  global many = <fun>
|}]

type delayed_free = { id : int; callback : unit -> unit }

let get_id : delayed_free @ once -> int @ many = fun d -> d.id
[%%expect{|
type delayed_free = { id : int; callback : unit -> unit; }
val get_id : delayed_free @ once -> int @@ global many = <fun>
|}]

type delayed_free = { ids : int list; callback : unit -> unit }

let get_ids : delayed_free @ once -> int list @ many = fun d -> d.ids
[%%expect{|
type delayed_free = { ids : int list; callback : unit -> unit; }
val get_ids : delayed_free @ once -> int list = <fun>
|}]

let okay t =
  match t with
  | Con { field } -> free t
[%%expect{|
val okay : t @ unique -> unit @@ global many = <fun>
|}]

let bad t =
  match t with
  | Con { field } ->
    free_field field;
    free t
[%%expect{|
Line 5, characters 9-10:
5 |     free t
             ^
Error: This value is used here,
       but part of it has already been used as unique:
Line 4, characters 15-20:
4 |     free_field field;
                   ^^^^^

|}]

let okay t =
  match t with
  | Con { field } ->
    if flip_coin ()
    then free_field field
    else free t
[%%expect{|
val okay : t @ unique -> unit @@ global many = <fun>
|}]

let okay t =
  match t with
  | Con { field } ->
    store_field field;
    store t
[%%expect{|
val okay : t -> unit @@ global many = <fun>
|}]

(****************)
(* pitfalls.md *)

let module_ret_unique =
  let mk () = Con { field = [1] } in
  let use () = free (mk ()) in
  ()
[%%expect{|
Line 3, characters 6-9:
3 |   let use () = free (mk ()) in
          ^^^
Warning 26 [unused-var]: unused variable use.

val module_ret_unique : unit @@ global many = ()
|}]

module Mk = struct
  let mk () = Con { field = [1] }
end

let module_ret_unique =
  let use () = free (Mk.mk ()) in
  ()
[%%expect{|
module Mk : sig val mk : unit -> t @@ global many portable end
Line 6, characters 20-30:
6 |   let use () = free (Mk.mk ()) in
                        ^^^^^^^^^^
Error: This value is "aliased" but expected to be "unique".
|}]

module Unique_array = struct
  let set : 'a @ unique -> int -> 'b -> 'a @ unique = fun arr -> fun i -> fun x -> arr
  let size arr = 10
end
[%%expect{|
module Unique_array :
  sig
    val set : 'a @ unique -> int -> 'b -> 'a @ unique @@ global many portable
    val size : 'a -> int @@ global many portable
  end
|}]

let set_all_zero arr =
  for i = 0 to Unique_array.size arr do
    Unique_array.set arr i 0
  done
[%%expect{|
Line 3, characters 21-24:
3 |     Unique_array.set arr i 0
                         ^^^
Error: This value is "aliased" but expected to be "unique".
  Hint: This identifier cannot be used uniquely,
  because it was defined outside of the for-loop.
|}]

let set_all_zero arr =
  let set = Unique_array.set arr in
  for i = 0 to Unique_array.size arr do
    set i 0
  done
[%%expect{|
Line 4, characters 4-7:
4 |     set i 0
        ^^^
Error: The value "set" is once, so cannot be used inside a for loop
|}]

let set_all_zero arr =
  let size (unique_ arr) = 10, arr in
  let rec loop idx arr =
    if idx == 0 then arr
    else loop (idx - 1) (Unique_array.set arr idx 0)
  in
  let size, arr = size arr in
  loop size arr
[%%expect{|
val set_all_zero : 'a @ unique -> 'a @@ global many = <fun>
|}]

(****************)
(* reference.md *)

type t = { field1 : t; field2 : t }

let free : t @ unique -> unit = fun t -> ()
let free_field (unique_ i) = ()
let store : t @ aliased -> unit = fun t -> ()
let store_field i = ()
let flip_coin () = true
[%%expect{|
type t = { field1 : t; field2 : t; }
val free : t @ unique -> unit @@ global many = <fun>
val free_field : 'a @ unique -> unit @@ global many = <fun>
val store : t -> unit @@ global many = <fun>
val store_field : 'a -> unit @@ global many = <fun>
val flip_coin : unit -> bool @@ global many = <fun>
|}]

let okay t =
  if flip_coin ()
  then free t
  else (store t; store t)
[%%expect{|
val okay : t @ unique -> unit @@ global many = <fun>
|}]

let okay r =
  free r.field1;
  match r with
  | { field2; _ } -> free field2
[%%expect{|
val okay : t @ unique -> unit @@ global many = <fun>
|}]

let bad r =
  free r.field1;
  match r with
  | { field2; _ } -> free r
[%%expect{|
Line 4, characters 26-27:
4 |   | { field2; _ } -> free r
                              ^
Error: This value is used here,
       but part of it has already been used as unique:
Line 2, characters 7-15:
2 |   free r.field1;
           ^^^^^^^^

|}]

let okay r =
  let x = r in
  free x.field1;
  match r with
  | { field2; _ } -> free field2
[%%expect{|
val okay : t @ unique -> unit @@ global many = <fun>
|}]

let id : 'a @ unique -> 'a @ unique = fun t -> t
[%%expect{|
val id : 'a @ unique -> 'a @ unique @@ global many = <fun>
|}]

let bad r =
  let x = id r in
  free x.field1;
  match r with
  | { field2; _ } -> free field2
[%%expect{|
Line 5, characters 4-17:
5 |   | { field2; _ } -> free field2
        ^^^^^^^^^^^^^
Error: This value is read from here, but it has already been used as unique:
Line 2, characters 13-14:
2 |   let x = id r in
                 ^

|}]

let bad r =
  let x = { field1 = r.field1; field2 = r.field2 } in
  free x.field1;
  match r with
  | { field2; _ } -> free field2
[%%expect{|
Line 5, characters 26-32:
5 |   | { field2; _ } -> free field2
                              ^^^^^^
Error: This value is used here, but it has already been used as unique:
Line 2, characters 40-48:
2 |   let x = { field1 = r.field1; field2 = r.field2 } in
                                            ^^^^^^^^

|}]

let check_tuple x y z =
  let m =
    match x, y, z with
    | p, q, r -> free p
  in m, y, y
[%%expect{|
val check_tuple : t @ unique -> 'a -> 'b -> unit * 'a * 'a @@ global many =
  <fun>
|}]

let okay x y z =
  match x, y, z with
  | p, q, r -> free x.field1; free p.field2
[%%expect{|
val okay : t @ unique -> 'a -> 'b -> unit @@ global many = <fun>
|}]

let bad x y z =
  match x, y, z with
  | p, q, r as t -> free x.field1; free p.field2
[%%expect{|
Line 3, characters 25-26:
3 |   | p, q, r as t -> free x.field1; free p.field2
                             ^
Error: This value is read from here, but it has already been used as unique:
Line 2, characters 8-9:
2 |   match x, y, z with
            ^

|}]
