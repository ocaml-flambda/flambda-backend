(* TEST
   flags += "-extension unique ";
   flags += "-extension overwriting ";
   expect;
*)

type record_update = { x : string }
[%%expect{|
type record_update = { x : string; }
|}]


let update (unique_ r : record_update) =
  let x = overwrite_ r with { x = "foo" }
  in x.x
[%%expect{|
Line 2, characters 10-41:
2 |   let x = overwrite_ r with { x = "foo" }
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert : Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1112, characters 2-8: Assertion failed

|}]

let update2 = update { x = "bar" }
[%%expect{|
Line 1, characters 14-20:
1 | let update2 = update { x = "bar" }
                  ^^^^^^
Error: Unbound value update
|}]

(* Only global values may be written during overwrites,
   since the GC does not allow heap-to-stack pointers. *)

let gc_soundness_bug (local_ unique_ r) (local_ x) =
  exclave_ (overwrite_ r with { x })
[%%expect{|
Line 2, characters 11-36:
2 |   exclave_ (overwrite_ r with { x })
               ^^^^^^^^^^^^^^^^^^^^^^^^^
Alert : Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1112, characters 2-8: Assertion failed

|}]

let gc_soundness_nobug (local_ unique_ r) x =
  exclave_ (overwrite_ r with { x })
[%%expect{|
Line 2, characters 11-36:
2 |   exclave_ (overwrite_ r with { x })
               ^^^^^^^^^^^^^^^^^^^^^^^^^
Alert : Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1112, characters 2-8: Assertion failed

|}]

let gc_soundness_nobug (local_ unique_ r) (local_ x) =
  exclave_ ({ r with x })
[%%expect{|
Line 2, characters 11-25:
2 |   exclave_ ({ r with x })
               ^^^^^^^^^^^^^^
Warning 23 [useless-record-with]: all the fields are explicitly listed in this record:
the 'with' clause is useless.

val gc_soundness_nobug :
  local_ unique_ record_update -> local_ string -> local_ record_update =
  <fun>
|}]
