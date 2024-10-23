(* TEST
   flags += "-extension-universe alpha";
   expect;
*)

type record_update = { x : string; y : string }
[%%expect{|
type record_update = { x : string; y : string; }
|}]

let update (unique_ r : record_update) =
  let x = overwrite_ r with { x = "foo" } in
  x.x
[%%expect{|
Line 2, characters 10-41:
2 |   let x = overwrite_ r with { x = "foo" } in
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1106, characters 2-8: Assertion failed

|}]

let update (unique_ r : record_update) =
  let x = overwrite_ r with ({ x = "foo" } : record_update) in
  x.x
[%%expect{|
Line 2, characters 10-59:
2 |   let x = overwrite_ r with ({ x = "foo" } : record_update) in
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1106, characters 2-8: Assertion failed

|}]

(*************************************)
(* Checking uniqueness of overwrites *)

let id = function x -> x

let overwrite_shared (r : record_update) =
  let r = id r in
  let x = overwrite_ r with { x = "foo" } in
  x.x
[%%expect{|
val id : ('a : value_or_null). 'a -> 'a @@ global many = <fun>
Line 5, characters 21-22:
5 |   let x = overwrite_ r with { x = "foo" } in
                         ^
Error: This value is "aliased" but expected to be "unique".
|}]

let overwrite_shared (r : record_update) =
  let x = overwrite_ r with { x = "foo" } in
  x.x, r.x
[%%expect{|
Line 3, characters 7-8:
3 |   x.x, r.x
           ^
Error: This value is read from here, but it has already been used as unique:
Line 2, characters 21-22:
2 |   let x = overwrite_ r with { x = "foo" } in
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
   no allocation takes place. We check four related cases below. *)
let returning_regional (local_ unique_ r) x =
  overwrite_ r with { x }
[%%expect{|
Line 2, characters 2-25:
2 |   overwrite_ r with { x }
      ^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1106, characters 2-8: Assertion failed

|}]

let disallowed_by_locality () x =
  let r = stack_ { x = ""; y = "" } in
  overwrite_ r with { x }
[%%expect{|
Line 3, characters 13-14:
3 |   overwrite_ r with { x }
                 ^
Error: This value escapes its region.
|}]

let returning_regional () x =
  exclave_
    let r = stack_ { x = ""; y = "" } in
    overwrite_ r with { x }
[%%expect{|
Line 4, characters 4-27:
4 |     overwrite_ r with { x }
        ^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1106, characters 2-8: Assertion failed

|}]

let disallowed_by_locality () x =
  let r = stack_ { x = ""; y = "" } in
  exclave_ overwrite_ r with { x }
[%%expect{|
Line 3, characters 22-23:
3 |   exclave_ overwrite_ r with { x }
                          ^
Error: The value "r" is local, so it cannot be used inside an exclave_
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

let update =
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

let update =
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

let update =
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

(*******************************)
(* Mode-checking strong update *)

type moded_record = { a : (int -> int) option; b : int -> int @@ portable }
[%%expect{|
type moded_record = { a : (int -> int) option; b : int -> int @@ portable; }
|}]

let update : moded_record @ unique once -> moded_record @ many =
  function mr ->
    let many_fun : int -> int @@ many = function x -> x in
    overwrite_ mr with { a = None; b = many_fun }
[%%expect{|
Line 4, characters 4-49:
4 |     overwrite_ mr with { a = None; b = many_fun }
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1106, characters 2-8: Assertion failed

|}]

let update : moded_record @ unique once -> moded_record @ many =
  function mr ->
    let once_fun : int -> int @@ once = function x -> x in
    overwrite_ mr with { a = None; b = once_fun }
[%%expect{|
Line 4, characters 39-47:
4 |     overwrite_ mr with { a = None; b = once_fun }
                                           ^^^^^^^^
Error: This value is "once" but expected to be "many".
|}]

let update : moded_record @ unique once -> moded_record @ many =
  function mr ->
    overwrite_ mr with { a = None; b = _ }
[%%expect{|
Line 3, characters 39-40:
3 |     overwrite_ mr with { a = None; b = _ }
                                           ^
Error: This value is "once" but expected to be "many".
|}]

(* Same as above, but omitting the [b = _]. *)
let update : moded_record @ unique once -> moded_record @ many =
  function mr ->
    overwrite_ mr with { a = None }
[%%expect{|
Line 3, characters 23-35:
3 |     overwrite_ mr with { a = None }
                           ^^^^^^^^^^^^
Error: This value is "once" but expected to be "many".
|}]

let update : moded_record @ unique nonportable -> moded_record @ portable =
  function mr ->
    let portable_fun : int -> int @@ portable = function x -> x in
    overwrite_ mr with { a = None; b = portable_fun }
[%%expect{|
Line 4, characters 4-53:
4 |     overwrite_ mr with { a = None; b = portable_fun }
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1106, characters 2-8: Assertion failed

|}]

let update : moded_record @ unique nonportable -> moded_record @ portable =
  function mr ->
    let nonportable_fun : int -> int @@ nonportable = function x -> x in
    overwrite_ mr with { a = None; b = nonportable_fun }
[%%expect{|
Line 4, characters 39-54:
4 |     overwrite_ mr with { a = None; b = nonportable_fun }
                                           ^^^^^^^^^^^^^^^
Error: This value is "nonportable" but expected to be "portable".
|}]

let update : moded_record @ unique nonportable -> moded_record @ portable =
  function mr ->
    let portable_fun : int -> int @@ portable = function x -> x in
    let nonportable_fun : int -> int @@ nonportable = function x -> x in
    overwrite_ mr with { a = Some nonportable_fun; b = portable_fun }
[%%expect{|
Line 5, characters 34-49:
5 |     overwrite_ mr with { a = Some nonportable_fun; b = portable_fun }
                                      ^^^^^^^^^^^^^^^
Error: This value is "nonportable" but expected to be "portable".
|}]

(* This works since the kept field has the portable modality: *)
let update : moded_record @ unique nonportable -> moded_record @ portable =
  function mr ->
    overwrite_ mr with { a = None; b = _ }
[%%expect{|
Line 3, characters 4-42:
3 |     overwrite_ mr with { a = None; b = _ }
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1106, characters 2-8: Assertion failed

|}]

(* Same as above, but omitting the [b = _]. *)
let update : moded_record @ unique nonportable -> moded_record @ portable =
  function mr ->
    overwrite_ mr with { a = None }
[%%expect{|
Line 3, characters 4-35:
3 |     overwrite_ mr with { a = None }
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
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

type unboxed_record = { x : int } [@@unboxed]
[%%expect{|
type unboxed_record = { x : int; } [@@unboxed]
|}]

let update (r : unboxed_record) =
  overwrite_ r with { x = 4 }
[%%expect{|
Line 2, characters 20-29:
2 |   overwrite_ r with { x = 4 }
                        ^^^^^^^^^
Error: Overwriting is only supported on tuples, constructors and boxed records.
|}]

type nested_record = Nested of { x : int }
[%%expect{|
type nested_record = Nested of { x : int; }
|}]

let nested_update (t : int * (string * int)) =
  overwrite_ t with (3, ("", _))
[%%expect{|
Line 2, characters 29-30:
2 |   overwrite_ t with (3, ("", _))
                                 ^
Error: Syntax error: "wildcard "_"" not expected.
|}]

let nested_update (t : int * record_update) =
  overwrite_ t with (3, {x = _; y = _})
[%%expect{|
Line 2, characters 29-30:
2 |   overwrite_ t with (3, {x = _; y = _})
                                 ^
Error: Syntax error: "wildcard "_"" not expected.
|}]

let nested_update t =
  overwrite_ t with Nested (_)
[%%expect{|
Line 2, characters 20-30:
2 |   overwrite_ t with Nested (_)
                        ^^^^^^^^^^
Error: This constructor expects an inlined record argument.
|}]

let update_hole (t : int * (string * int)) =
  overwrite_ t with _
[%%expect{|
Line 2, characters 20-21:
2 |   overwrite_ t with _
                        ^
Error: Overwriting is only supported on tuples, constructors and boxed records.
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
let let_bound_path =
  let r = { x = OptionA "foo" } in
  overwrite_ r.x with OptionA "bar"
[%%expect{|
Line 3, characters 22-29:
3 |   overwrite_ r.x with OptionA "bar"
                          ^^^^^^^
Error: Overwrite may not change the tag to OptionA.
Hint: The old tag of this allocation is unknown.
|}]

(********************************)
(* Overwriting (labeled) tuples *)

type tuple_unlabeled = string * string
[%%expect{|
type tuple_unlabeled = string * string
|}]

let update (unique_ r : tuple_unlabeled) : tuple_unlabeled =
  let x = overwrite_ r with (_, _) in
  x
[%%expect{|
Line 2, characters 10-34:
2 |   let x = overwrite_ r with (_, _) in
              ^^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1106, characters 2-8: Assertion failed

|}]

let update (unique_ r : tuple_unlabeled) : tuple_unlabeled =
  let x = overwrite_ r with ("foo", _) in
  x
[%%expect{|
Line 2, characters 10-38:
2 |   let x = overwrite_ r with ("foo", _) in
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1106, characters 2-8: Assertion failed

|}]

let update (unique_ r : tuple_unlabeled) : tuple_unlabeled =
  let x = overwrite_ r with ("foo", "bar") in
  x
[%%expect{|
Line 2, characters 10-42:
2 |   let x = overwrite_ r with ("foo", "bar") in
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1106, characters 2-8: Assertion failed

|}]

let update (unique_ r : tuple_unlabeled) : tuple_unlabeled =
  let x = overwrite_ r with ("foo", "bar", "baz") in
x
[%%expect{|
Line 2, characters 28-49:
2 |   let x = overwrite_ r with ("foo", "bar", "baz") in
                                ^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "'a * 'b * 'c"
       but an expression was expected of type
         "tuple_unlabeled" = "string * string"
|}]

type tuple_labeled = x:string * y:string
[%%expect{|
type tuple_labeled = x:string * y:string
|}]

let update (unique_ r : tuple_labeled) : tuple_labeled =
  let x = overwrite_ r with (~x:"foo", _) in
  x
[%%expect{|
Line 2, characters 28-41:
2 |   let x = overwrite_ r with (~x:"foo", _) in
                                ^^^^^^^^^^^^^
Error: This expression has type "x:string * 'a"
       but an expression was expected of type
         "tuple_labeled" = "x:string * y:string"
|}]

(* CR uniqueness: Would be good to support [~y:_], without the parentheses, if possible *)
let update (unique_ r : tuple_labeled) : tuple_labeled =
  let x = overwrite_ r with (~x:(_), ~y:(_)) in
  x
[%%expect{|
Line 2, characters 10-44:
2 |   let x = overwrite_ r with (~x:(_), ~y:(_)) in
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1106, characters 2-8: Assertion failed

|}]

let update (unique_ r : tuple_labeled) : tuple_labeled =
  let x = overwrite_ r with (~x:"foo", ~y:(_)) in
  x
[%%expect{|
Line 2, characters 10-46:
2 |   let x = overwrite_ r with (~x:"foo", ~y:(_)) in
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1106, characters 2-8: Assertion failed

|}]

let update (unique_ r : tuple_labeled) : tuple_labeled =
  let x = overwrite_ r with (~x:"foo", ~y:"bar") in
  x
[%%expect{|
Line 2, characters 10-48:
2 |   let x = overwrite_ r with (~x:"foo", ~y:"bar") in
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1106, characters 2-8: Assertion failed

|}]

(***********************************)
(* Syntax to be supported later on *)

(* CR uniqueness: It would be nice for one of these two pieces of syntax to work.
   Currently these are syntax errors. *)

(*
let update (unique_ r : tuple_labeled) : tuple_labeled =
  let x = overwrite_ r with (~x:"foo", ..) in
  x
[%%expect{|
|}]

let update (unique_ r : tuple_labeled) : tuple_labeled =
  let x = overwrite_ r with (~x:"foo") in
  x
[%%expect{|
|}]
*)

(*******************************)
(* Overwriting inlined records *)

type constructor_update = Con of { x : string; y : string }
[%%expect{|
type constructor_update = Con of { x : string; y : string; }
|}]

(* CR uniqueness: It would be nice if the analysis could figure out the tag
   of a constructor_update from its type (which only has one possible tag). *)

let update = function
  | (Con _ as c) ->
    let x = overwrite_ c with Con { x = "foo" } in
    x
[%%expect{|
Line 3, characters 12-47:
3 |     let x = overwrite_ c with Con { x = "foo" } in
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1106, characters 2-8: Assertion failed

|}]

let update = function
  | (Con c1 as c) ->
    let x = overwrite_ c with Con { c1 with x = "foo" } in
    x
[%%expect{|
Line 3, characters 12-55:
3 |     let x = overwrite_ c with Con { c1 with x = "foo" } in
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1106, characters 2-8: Assertion failed

|}]

let update = function
  | (Con c) ->
    let x = overwrite_ c with { x = "foo" } in
    x
[%%expect{|
Line 3, characters 23-24:
3 |     let x = overwrite_ c with { x = "foo" } in
                           ^
Error: This form is not allowed as the type of the inlined record could escape.
|}]

let update = function
  | (Con c) ->
    let x = overwrite_ c with { x = "foo" } in
    Con x
[%%expect{|
Line 3, characters 23-24:
3 |     let x = overwrite_ c with { x = "foo" } in
                           ^
Error: This form is not allowed as the type of the inlined record could escape.
|}]

let update = function
  | (Con c) ->
    let x = Con (overwrite_ c with { x = "foo" }) in
    x
[%%expect{|
Line 3, characters 16-49:
3 |     let x = Con (overwrite_ c with { x = "foo" }) in
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1106, characters 2-8: Assertion failed

|}]

let update = function
  | (Con c) ->
    let x = Con (overwrite_ c with { c with x = "foo" }) in
    x
[%%expect{|
Line 3, characters 16-56:
3 |     let x = Con (overwrite_ c with { c with x = "foo" }) in
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1106, characters 2-8: Assertion failed

|}]
