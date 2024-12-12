(* TEST
   flags += "-extension-universe alpha";
   expect;
*)

(* CR uniqueness: It would be nice to also support the following syntactic forms:
     overwrite_ e with (~x:5, ..)
     overwrite_ e with { x = 4; _ }

   This would require us to allow general patterns as the top-level second argument to
   overwrite_ with. However, all overwritten fields should continue to be expressions.
*)

(******************)
(* Usage examples *)

let overwrite_tuple = function
  (a, b) as t -> overwrite_ t with (b, _)
[%%expect{|
val overwrite_tuple :
  ('a : value_or_null) ('b : value_or_null). 'a * 'b @ unique -> 'b * 'b @@
  global many = <fun>
|}]

type record = { a : int; b : int }

let overwrite_record = function
  { a; b } as t -> overwrite_ t with { b = a; a = _ }
[%%expect{|
type record = { a : int; b : int; }
val overwrite_record : record @ unique -> record @@ global many = <fun>
|}]

let with_record = function
    { a; b } as t -> { t with b = a }
[%%expect{|
val with_record : record -> record @@ global many = <fun>
|}]

let overwrite_record = function
    { a; b } as t -> overwrite_ t with { b = a }
[%%expect{|
val overwrite_record : record @ unique -> record @@ global many = <fun>
|}]

type constructor = C of { a : int; b : int }

let overwrite_constructor = function
  C { a; b } as t -> overwrite_ t with C { b = a; a = _ }
[%%expect{|
type constructor = C of { a : int; b : int; }
val overwrite_constructor : constructor @ unique -> constructor @@ global
  many = <fun>
|}]

let overwrite_constructor = function
    C { a; b } as t -> overwrite_ t with C { b = a }
[%%expect{|
val overwrite_constructor : constructor @ unique -> constructor @@ global
  many = <fun>
|}]

let overwrite_variant = function
  `A (a, b) as t -> overwrite_ t with `A (b, _)
[%%expect{|
Line 2, characters 38-47:
2 |   `A (a, b) as t -> overwrite_ t with `A (b, _)
                                          ^^^^^^^^^
Error: Overwriting is only supported on tuples, constructors and boxed records.
|}]

let overwrite_in_match = function
  C { a; b } as t ->
    match overwrite_ t with C { b = a; a = _ } with
    | C {a; b} -> C {a; b}
[%%expect{|
val overwrite_in_match : constructor @ unique -> constructor @@ global many =
  <fun>
|}]

(****************)
(* Non-examples *)

let underscore = _
[%%expect{|
Line 1, characters 17-18:
1 | let underscore = _
                     ^
Error: wildcard "_" not expected.
|}]

let underscore_tuple = (_, 1)
[%%expect{|
Line 1, characters 24-25:
1 | let underscore_tuple = (_, 1)
                            ^
Error: wildcard "_" not expected.
|}]

let underscore_record = { a = _; b = 1 }
[%%expect{|
Line 1, characters 30-31:
1 | let underscore_record = { a = _; b = 1 }
                                  ^
Error: wildcard "_" not expected.
|}]

let overwrite_with_let t = overwrite_ t with let x = (1, 2) in x
[%%expect{|
Line 1, characters 45-64:
1 | let overwrite_with_let t = overwrite_ t with let x = (1, 2) in x
                                                 ^^^^^^^^^^^^^^^^^^^
Error: Overwriting is only supported on tuples, constructors and boxed records.
|}]

let overwrite_with_match t = overwrite_ t with match t with C {a;b} -> C{a;b}
[%%expect{|
Line 1, characters 47-77:
1 | let overwrite_with_match t = overwrite_ t with match t with C {a;b} -> C{a;b}
                                                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Overwriting is only supported on tuples, constructors and boxed records.
|}]

let overwrite_with_overwrite = function
    { a; b } as t -> overwrite_ t with (overwrite_ t with { b = a })
[%%expect{|
Line 2, characters 39-68:
2 |     { a; b } as t -> overwrite_ t with (overwrite_ t with { b = a })
                                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Overwriting is only supported on tuples, constructors and boxed records.
|}]
