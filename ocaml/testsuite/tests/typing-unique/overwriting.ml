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

(*************************************)
(* Checking uniqueness of overwrites *)

let id = function x -> x

let overwrite_shared (r : record_update) =
  let r = id r in
  let x = overwrite_ r with { x = "foo" }
  in x.x
[%%expect{|
val id : ('a : value_or_null). 'a -> 'a @@ global many = <fun>
Line 5, characters 21-22:
5 |   let x = overwrite_ r with { x = "foo" }
                         ^
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
Line 2, characters 21-22:
2 |   let x = overwrite_ r with { x = "foo" }
                         ^

|}]

(**************************************)
(* Checking regionality of overwrites *)

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
   But we don't: the overwritten record may be regional in this case since
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
Line 3, characters 13-14:
3 |   overwrite_ r with { x }
                 ^
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

(*******************************)
(* Type-checking strong update *)

type 'a eq = { eq0 : 'a; eq1 : 'a }
type 'a pair = 'a * 'a
[%%expect{|
type 'a eq = { eq0 : 'a; eq1 : 'a; }
type 'a pair = 'a * 'a
|}]

let update eq =
  overwrite_ eq with { eq0 = "foo" }
[%%expect{|
Line 2, characters 2-36:
2 |   overwrite_ eq with { eq0 = "foo" }
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1106, characters 2-8: Assertion failed

|}]

let update eq =
  overwrite_ eq with { eq0 = "foo"; eq1 = 1 }
[%%expect{|
Line 2, characters 42-43:
2 |   overwrite_ eq with { eq0 = "foo"; eq1 = 1 }
                                              ^
Error: This expression has type "int" but an expression was expected of type
         "string"
|}]

let update () =
  let eq = { eq0 = "foo" ; eq1 = "bar" } in
  { eq with eq0 = 1 }
[%%expect{|
Line 3, characters 2-21:
3 |   { eq with eq0 = 1 }
      ^^^^^^^^^^^^^^^^^^^
Error: This expression has type "int eq" but an expression was expected of type
         "string eq"
       Type "int" is not compatible with type "string"
|}]

let update () =
  let eq = { eq0 = "foo" ; eq1 = "bar" } in
  overwrite_ eq with { eq0 = 1 }
[%%expect{|
Line 3, characters 21-32:
3 |   overwrite_ eq with { eq0 = 1 }
                         ^^^^^^^^^^^
Error: This expression has type "int eq" but an expression was expected of type
         "string eq"
       Type "int" is not compatible with type "string"
|}]

let update () =
  let eq = { eq0 = "foo" ; eq1 = "bar" } in
  overwrite_ eq with { eq0 = 1; eq1 = 2 }
[%%expect{|
Line 3, characters 2-41:
3 |   overwrite_ eq with { eq0 = 1; eq1 = 2 }
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1106, characters 2-8: Assertion failed

|}]

let update : _ pair @ unique -> _ pair = function eq ->
  overwrite_ eq with ("foo", _)
[%%expect{|
Line 2, characters 2-31:
2 |   overwrite_ eq with ("foo", _)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1106, characters 2-8: Assertion failed

|}]

let update : _ pair @ unique -> _ pair = function eq ->
  overwrite_ eq with ("foo", 1)
[%%expect{|
Line 2, characters 29-30:
2 |   overwrite_ eq with ("foo", 1)
                                 ^
Error: This expression has type "int" but an expression was expected of type
         "string"
|}]

let update : unit -> _ pair = function eq ->
  let eq : string pair = ("foo", "bar") in
  overwrite_ eq with (1, _)
[%%expect{|
Line 3, characters 25-26:
3 |   overwrite_ eq with (1, _)
                             ^
Error: This expression has type "string" but an expression was expected of type
         "int"
|}]

let update : unit -> _ pair = function eq ->
  let eq : string pair = ("foo", "bar") in
  overwrite_ eq with (1, 2)
[%%expect{|
Line 3, characters 2-27:
3 |   overwrite_ eq with (1, 2)
      ^^^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1106, characters 2-8: Assertion failed

|}]

(*************************************)
(* Other edge-cases of type checking *)

type constr = Constr1 of { x : string } | Constr2 of { x : int }

let update c =
  match c with
  | Constr1 _ -> overwrite_ c with Constr1 { x = "" }
  | Constr2 _ -> overwrite_ c with Constr2 { x = 2 }
[%%expect{|
type constr = Constr1 of { x : string; } | Constr2 of { x : int; }
Line 5, characters 17-53:
5 |   | Constr1 _ -> overwrite_ c with Constr1 { x = "" }
                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1106, characters 2-8: Assertion failed

|}]

(***********************************)
(* Checking that tags don't change *)

type options = OptionA of string | OptionB of string
[%%expect{|
type options = OptionA of string | OptionB of string
|}]

let id = function
  | OptionA s as v -> overwrite_ v with OptionA s
  | OptionB s as v -> overwrite_ v with OptionB s
[%%expect{|
Line 2, characters 22-49:
2 |   | OptionA s as v -> overwrite_ v with OptionA s
                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1106, characters 2-8: Assertion failed

|}]

let id v =
  match v with
  | OptionA s -> overwrite_ v with OptionA s
  | OptionB s -> overwrite_ v with OptionB s
[%%expect{|
Line 3, characters 17-44:
3 |   | OptionA s -> overwrite_ v with OptionA s
                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1106, characters 2-8: Assertion failed

|}]

let swap = function
  | OptionA s as v -> overwrite_ v with OptionB s
  | OptionB s as v -> overwrite_ v with OptionA s
[%%expect{|
Line 2, characters 40-47:
2 |   | OptionA s as v -> overwrite_ v with OptionB s
                                            ^^^^^^^
Error: Overwrite may not change the tag to OptionB.
Hint: The old tag of this allocation is OptionA.
|}]

let swap v =
  match v with
  | OptionA s -> overwrite_ v with OptionB s
  | OptionB s -> overwrite_ v with OptionA s
[%%expect{|
Line 3, characters 35-42:
3 |   | OptionA s -> overwrite_ v with OptionB s
                                       ^^^^^^^
Error: Overwrite may not change the tag to OptionB.
Hint: The old tag of this allocation is OptionA.
|}]

let choose_in_branch v =
  match v with
  | OptionA s ->
      if String.equal s ""
      then overwrite_ v with OptionA s
      else overwrite_ v with OptionB s
  | OptionB _ -> v
[%%expect{|
Line 6, characters 29-36:
6 |       else overwrite_ v with OptionB s
                                 ^^^^^^^
Error: The same allocation gets overwritten using different tags OptionA and OptionB.
Hint: overwrites may not change the tag; did you forget a pattern-match?
Line 5, characters 29-36:
5 |       then overwrite_ v with OptionA s
                                 ^^^^^^^

|}]

type options_record = { x : options }

let nested_path_correct r =
  match r with
  | { x = OptionA s } -> overwrite_ r.x with OptionA s
  | _ -> OptionB ""
[%%expect{|
type options_record = { x : options; }
Line 5, characters 25-54:
5 |   | { x = OptionA s } -> overwrite_ r.x with OptionA s
                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1106, characters 2-8: Assertion failed

|}]

let nested_path_wrong r =
  match r with
  | { x = OptionA s } -> overwrite_ r.x with OptionB s
  | _ -> OptionB ""
[%%expect{|
Line 3, characters 45-52:
3 |   | { x = OptionA s } -> overwrite_ r.x with OptionB s
                                                 ^^^^^^^
Error: Overwrite may not change the tag to OptionB.
Hint: The old tag of this allocation is OptionA.
|}]

(* Unsupported *)
let let_bound_path () =
  let r = { x = OptionA "foo" } in
  overwrite_ r.x with OptionA "bar"
[%%expect{|
Line 3, characters 22-29:
3 |   overwrite_ r.x with OptionA "bar"
                          ^^^^^^^
Error: Overwrite may not change the tag to OptionA.
Hint: The old tag of this allocation is unknown.
|}]
