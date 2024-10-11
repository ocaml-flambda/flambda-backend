(* TEST
   flags += "-extension-universe alpha";
   expect;
*)

type record_update = { x : string; y : string }
[%%expect{|
type record_update = { x : string; y : string; }
|}]


let update (unique_ r : record_update) =
  let x = overwrite_ r with { x = "foo" }
  in x.x
[%%expect{|
Line 2, characters 10-41:
2 |   let x = overwrite_ r with { x = "foo" }
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert : Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1106, characters 2-8: Assertion failed

|}]

let update2 = update { x = "bar" }
[%%expect{|
Line 1, characters 14-20:
1 | let update2 = update { x = "bar" }
                  ^^^^^^
Error: Unbound value "update"
|}]

(* Only global values may be written during overwrites,
   since the GC does not allow heap-to-stack pointers.
   However, it is fine if there are local values (like y here)
   that are not overwritten. We test 2^3 configurations:
   - the overwritten value can be local/global
   - the resulting value can be local/global
   - the value written in the record can be local/global *)

let gc_soundness_bug (local_ unique_ r) (local_ x) =
  exclave_ overwrite_ r with { x }
[%%expect{|
Line 2, characters 11-34:
2 |   exclave_ overwrite_ r with { x }
               ^^^^^^^^^^^^^^^^^^^^^^^
Alert : Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1106, characters 2-8: Assertion failed

|}]

let disallowed_by_locality (local_ unique_ r) (local_ x) =
  overwrite_ r with { x }
[%%expect{|
Line 2, characters 2-25:
2 |   overwrite_ r with { x }
      ^^^^^^^^^^^^^^^^^^^^^^^
Alert : Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1106, characters 2-8: Assertion failed

|}]

let gc_soundness_bug (unique_ r) (local_ x) =
  exclave_ overwrite_ r with { x }
[%%expect{|
Line 2, characters 11-34:
2 |   exclave_ overwrite_ r with { x }
               ^^^^^^^^^^^^^^^^^^^^^^^
Alert : Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1106, characters 2-8: Assertion failed

|}]

let disallowed_by_locality (unique_ r) (local_ x) =
  overwrite_ r with { x }
[%%expect{|
Line 2, characters 2-25:
2 |   overwrite_ r with { x }
      ^^^^^^^^^^^^^^^^^^^^^^^
Alert : Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1106, characters 2-8: Assertion failed

|}]

let gc_soundness_no_bug (local_ unique_ r) x =
  exclave_ overwrite_ r with { x }
[%%expect{|
Line 2, characters 11-34:
2 |   exclave_ overwrite_ r with { x }
               ^^^^^^^^^^^^^^^^^^^^^^^
Alert : Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1106, characters 2-8: Assertion failed

|}]

let disallowed_by_locality (local_ unique_ r) x =
  overwrite_ r with { x }
[%%expect{|
Line 2, characters 2-25:
2 |   overwrite_ r with { x }
      ^^^^^^^^^^^^^^^^^^^^^^^
Alert : Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1106, characters 2-8: Assertion failed

|}]

let gc_soundness_no_bug (unique_ r) x =
  exclave_ overwrite_ r with { x }
[%%expect{|
Line 2, characters 11-34:
2 |   exclave_ overwrite_ r with { x }
               ^^^^^^^^^^^^^^^^^^^^^^^
Alert : Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1106, characters 2-8: Assertion failed

|}]

let gc_soundness_no_bug (unique_ r) x =
  overwrite_ r with { x }
[%%expect{|
Line 2, characters 2-25:
2 |   overwrite_ r with { x }
      ^^^^^^^^^^^^^^^^^^^^^^^
Alert : Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1106, characters 2-8: Assertion failed

|}]
