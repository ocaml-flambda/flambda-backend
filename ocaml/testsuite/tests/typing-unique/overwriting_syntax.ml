(* TEST
   flags += "-extension unique ";
   flags += "-extension overwriting";
   expect;
*)

(******************)
(* Usage examples *)

let overwrite_tuple = function
  (a, b) as t -> overwrite_ t with (b, _)
[%%expect{|
Line 2, characters 17-41:
2 |   (a, b) as t -> overwrite_ t with (b, _)
                     ^^^^^^^^^^^^^^^^^^^^^^^^
Alert : Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1112, characters 2-8: Assertion failed

|}]

type record = { a : int; b : int }

let overwrite_record = function
  { a; b } as t -> overwrite_ t with { b = a; a = _ }
[%%expect{|
type record = { a : int; b : int; }
Line 4, characters 19-53:
4 |   { a; b } as t -> overwrite_ t with { b = a; a = _ }
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert : Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1112, characters 2-8: Assertion failed

|}]

type constructor = C of { a : int; b : int }

let overwrite_constructor = function
  C { a; b } as t -> overwrite_ t with C { b = a; a = _ }
[%%expect{|
type constructor = C of { a : int; b : int; }
Line 4, characters 21-57:
4 |   C { a; b } as t -> overwrite_ t with C { b = a; a = _ }
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert : Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1112, characters 2-8: Assertion failed

|}]

let overwrite_variant = function
  `A (a, b) -> overwrite_ t with `A (b, _)
[%%expect{|
Line 2, characters 15-42:
2 |   `A (a, b) -> overwrite_ t with `A (b, _)
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert : Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1112, characters 2-8: Assertion failed

|}]

let overwrite_in_match = function
  C { a; b } as t ->
    match overwrite_ t with C { b = a; a = _ } with
    | C {a; b} -> C {a; b}
[%%expect{|
Line 3, characters 10-46:
3 |     match overwrite_ t with C { b = a; a = _ } with
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert : Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1112, characters 2-8: Assertion failed

|}]

(****************)
(* Non-examples *)

let underscore () = _
[%%expect{|
Line 1, characters 20-21:
1 | let underscore () = _
                        ^
Alert : Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1112, characters 2-8: Assertion failed

|}]

let underscore_tuple () = (_, 1)
[%%expect{|
Line 1, characters 27-28:
1 | let underscore_tuple () = (_, 1)
                               ^
Alert : Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1112, characters 2-8: Assertion failed

|}]

let underscore_record () = { a = _; b = 1 }
[%%expect{|
Line 1, characters 33-34:
1 | let underscore_record () = { a = _; b = 1 }
                                     ^
Alert : Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1112, characters 2-8: Assertion failed

|}]

let overwrite_with_let t = overwrite_ t with let x = (a, b) in x
[%%expect{|
Line 1, characters 27-64:
1 | let overwrite_with_let t = overwrite_ t with let x = (a, b) in x
                               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert : Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1112, characters 2-8: Assertion failed

|}]

let overwrite_with_match t = overwrite_ t with match t with C {a;b} -> C{a;b}
[%%expect{|
Line 1, characters 29-77:
1 | let overwrite_with_match t = overwrite_ t with match t with C {a;b} -> C{a;b}
                                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert : Overwrite not implemented.
Uncaught exception: File "ocaml/parsing/location.ml", line 1112, characters 2-8: Assertion failed

|}]
