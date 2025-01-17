(* TEST
 flambda2;
 include stdlib_upstream_compatible;
 flags = "-extension unique";
 {
   expect;
 }
*)

(* Based on [testsuite/tests/typing-layouts-products/unique.ml] *)

(* Uniqueness tests *)

let unique_use : 'a @ unique -> unit = fun _ -> ()
let unique_use2 : ('a : value & value) @ unique -> unit = fun _ -> ()

type t = { x : string ; y : string }
let mk : unit -> t @ unique = fun () -> #{ x = "hi"; y = "hi" }
[%%expect{|
val unique_use : 'a @ unique -> unit = <fun>
val unique_use2 : ('a : value & value). 'a @ unique -> unit = <fun>
type t = #{ x : string; y : string; }
val mk : unit -> t @ unique = <fun>
|}]

(* Can access different fields *)
let () =
  let t = mk () in
  unique_use t.#x;
  unique_use t.#y
[%%expect{|
|}]

let () =
  let #{ x ; y } = mk () in
  unique_use x;
  unique_use y
[%%expect{|
|}]

(* Cannot access the same field twice *)
let () =
  let t = mk () in
  unique_use t.#x;
  unique_use t.#x
[%%expect{|
Line 4, characters 13-17:
4 |   unique_use t.#x
                 ^^^^
Error: This value is used here, but it has already been used as unique:
Line 3, characters 13-17:
3 |   unique_use t.#x;
                 ^^^^

|}]

let () =
  let #{ x ; y = _ } = mk () in
  unique_use x;
  unique_use x
[%%expect{|
Line 4, characters 13-14:
4 |   unique_use x
                 ^
Error: This value is used here, but it has already been used as unique:
Line 3, characters 13-14:
3 |   unique_use x;
                 ^

|}]

(* A functional update to a field allows a field to be reused *)
let () =
  let t = mk () in
  unique_use t.#x;
  let t = #{ t with x = "fresh" } in
  unique_use t.#x
[%%expect{|
|}]

(* But not a functional update to a different field *)
let () =
  let t = mk () in
  unique_use t.#x;
  let t = #{ t with y = "fresh" } in
  unique_use t.#x
[%%expect{|
Line 4, characters 13-14:
4 |   let t = #{ t with y = "fresh" } in
                 ^
Error: This value is used here, but it has already been used as unique:
Line 3, characters 13-17:
3 |   unique_use t.#x;
                 ^^^^

|}]

let () =
  let t = mk () in
  unique_use2 t;
  let _ = #{ t with y = "fresh" } in
  ()
[%%expect{|
Line 4, characters 13-14:
4 |   let _ = #{ t with y = "fresh" } in
                 ^
Error: This value is used here,
       but it is part of a value that has already been used as unique:
Line 3, characters 14-15:
3 |   unique_use2 t;
                  ^

|}]

(* Functional updates to unboxed records don't implicitly borrow the record
   (unlike boxed records). *)
let [@warning "-23"] () =
  let t = mk () in
  unique_use2 t;
  let t = #{ t with x = "fresh"; y = "fresh" } in
  unique_use t.#x
[%%expect{|
|}]

type t = #{ x : unit ; y : string }
let mk : unit -> t @ unique = fun () -> #{ x = () ; y = "fresh" }
[%%expect{|
type t = #{ x : unit; y : string; }
val mk : unit -> t @ unique = <fun>
|}]

let [@warning "-23"] () =
  let t = mk () in
  unique_use2 t;
  let t = #{ t with x = (); y = "fresh" } in
  unique_use t.#x
[%%expect{|
|}]

(* CR uniqueness: this test should succeed since unboxed records have no memory
   address and the first field is of type unit and thus mode-crosses uniqueness.
   We should fix this by allowing multiple unique uses for values that
   mode-cross uniqueness. *)
let () =
  let t = mk () in
  unique_use2 t;
  let t = #{ t with y = "fresh" } in
  unique_use2 t
[%%expect{|
Line 4, characters 13-14:
4 |   let t = #{ t with y = "fresh" } in
                 ^
Error: This value is used here,
       but it is part of a value that has already been used as unique:
Line 3, characters 14-15:
3 |   unique_use2 t;
                  ^

|}]
