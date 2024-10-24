(* TEST
 flambda2;
 include stdlib_upstream_compatible;
 flags = "-extension layouts_alpha -extension unique";
 {
   expect;
 }
*)

(* Uniqueness tests *)

type t = #{ x : string ; y : string }
let use_string (_s @ unique) = ()
let mk : unit -> t @ unique = fun () -> #{ x = "hi"; y = "hi" }
[%%expect{|
type t = #{ x : string; y : string; }
val use_string : ('a : value_or_null). 'a @ unique -> unit = <fun>
val mk : unit -> t @ unique = <fun>
|}]

(* Can access different fields *)
let () =
  let t = mk () in
  use_string t.#x;
  use_string t.#y
[%%expect{|
|}]

let () =
  let #{ x ; y } = mk () in
  use_string x;
  use_string y
[%%expect{|
|}]

(* Cannot access the same field twice *)
let () =
  let t = mk () in
  use_string t.#x;
  use_string t.#x
[%%expect{|
Line 4, characters 13-17:
4 |   use_string t.#x
                 ^^^^
Error: This value is used here, but it has already been used as unique:
Line 3, characters 13-17:
3 |   use_string t.#x;
                 ^^^^

|}]

let () =
  let #{ x ; y = _ } = mk () in
  use_string x;
  use_string x
[%%expect{|
Line 4, characters 13-14:
4 |   use_string x
                 ^
Error: This value is used here, but it has already been used as unique:
Line 3, characters 13-14:
3 |   use_string x;
                 ^

|}]

(* A functional update to a field allows a field to be reused *)
let () =
  let t = mk () in
  use_string t.#x;
  let t = #{ t with x = "fresh" } in
  use_string t.#x
[%%expect{|
|}]

(* But not a functional update to a different field *)
let () =
  let t = mk () in
  use_string t.#x;
  let t = #{ t with y = "fresh" } in
  use_string t.#x
[%%expect{|
Line 4, characters 13-14:
4 |   let t = #{ t with y = "fresh" } in
                 ^
Error: This value is used here, but it has already been used as unique:
Line 3, characters 13-17:
3 |   use_string t.#x;
                 ^^^^

|}]

