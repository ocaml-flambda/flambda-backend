(* TEST
   flags += "-extension-universe alpha";
   expect;
*)

(* CR uniqueness: More tests to consider adding here:
   - overwriting tuples
   - overwriting labeled tuples
   - overwriting labeled tuples where only some labels are given
   - overwriting tuples (and labeled ones) with the new .. syntax
   - overwriting inline records (binding a variable to the whole constructor application,
       like | (K { ... }) as v -> ...)
   - overwriting inline records (binding a variable just to the inline record,
       like | K r -> ...)
   - overwriting constructor fields
   - overwriting mutable fields (yes, this is a bit silly, but we should test it)
   - overwriting immutable fields of records with mutable fields
   - local variants of (some of) the above
   - overwriting into a local record with a freshly-constructed bit of memory
       (to make sure that inference does not locally allocate the new memory)
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
Alert Translcore: Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1106, characters 2-8: Assertion failed

|}]

let update2 = update { x = "bar" }
[%%expect{|
Line 1, characters 14-20:
1 | let update2 = update { x = "bar" }
                  ^^^^^^
Error: Unbound value "update"
|}]

let id = function x -> x

let overwrite_shared (r : record_update) =
  let r = id r in
  let x = overwrite_ r with { x = "foo" }
  in x.x
[%%expect{|
val id : ('a : value_or_null). 'a -> 'a @@ global many = <fun>
Line 5, characters 10-41:
5 |   let x = overwrite_ r with { x = "foo" }
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value is "aliased" but expected to be "unique".
|}]

let overwrite_shared (r : record_update) =
  let x = overwrite_ r with { x = "foo" }
  in (x.x, r.x)
[%%expect{|
Line 3, characters 11-12:
3 |   in (x.x, r.x)
               ^
Error: This value is read from here, but it has already been used as unique:
Line 2, characters 10-41:
2 |   let x = overwrite_ r with { x = "foo" }
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

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
Line 2, characters 31-32:
2 |   exclave_ overwrite_ r with { x }
                                   ^
Error: This value escapes its region.
|}]

let disallowed_by_locality (local_ unique_ r) (local_ x) =
  overwrite_ r with { x }
[%%expect{|
Line 2, characters 22-23:
2 |   overwrite_ r with { x }
                          ^
Error: This value escapes its region.
|}]

let gc_soundness_bug (unique_ r) (local_ x) =
  exclave_ overwrite_ r with { x }
[%%expect{|
Line 2, characters 31-32:
2 |   exclave_ overwrite_ r with { x }
                                   ^
Error: This value escapes its region.
|}]

let disallowed_by_locality (unique_ r) (local_ x) =
  overwrite_ r with { x }
[%%expect{|
Line 2, characters 22-23:
2 |   overwrite_ r with { x }
                          ^
Error: This value escapes its region.
|}]

let gc_soundness_no_bug (local_ unique_ r) x =
  exclave_ overwrite_ r with { x }
[%%expect{|
Line 2, characters 11-34:
2 |   exclave_ overwrite_ r with { x }
               ^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1106, characters 2-8: Assertion failed

|}]

(* This code should fail if we used a real allocation { r with x } here.
   But we don't: the overwritten record is regional in this case, since
   no allocation takes place. We check two related cases below. *)
let returning_regional (local_ unique_ r) x =
  overwrite_ r with { x }
[%%expect{|
Line 2, characters 2-25:
2 |   overwrite_ r with { x }
      ^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1106, characters 2-8: Assertion failed

|}]

let disallowed_by_locality (local_ unique_ r) x =
  let r = stack_ { x = ""; y = "" } in
  overwrite_ r with { x }
[%%expect{|
Line 3, characters 2-25:
3 |   overwrite_ r with { x }
      ^^^^^^^^^^^^^^^^^^^^^^^
Error: This value escapes its region.
|}]

let disallowed_by_regionality (local_ unique_ r) x =
  let r = overwrite_ r with { x } in
  let ref = ref r in
  ref
[%%expect{|
Line 3, characters 16-17:
3 |   let ref = ref r in
                    ^
Error: This value escapes its region.
|}]

let gc_soundness_no_bug (unique_ r) x =
  exclave_ overwrite_ r with { x }
[%%expect{|
Line 2, characters 11-34:
2 |   exclave_ overwrite_ r with { x }
               ^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1106, characters 2-8: Assertion failed

|}]

let gc_soundness_no_bug (unique_ r) x =
  overwrite_ r with { x }
[%%expect{|
Line 2, characters 2-25:
2 |   overwrite_ r with { x }
      ^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1106, characters 2-8: Assertion failed

|}]
