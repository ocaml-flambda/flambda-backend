(* TEST
 expect;
*)

type 'a pair = Pair of 'a * 'a

[%%expect{|
type 'a pair = Pair of 'a * 'a
|}]

let string_escape_l (local_ y) = let Pair (x, _) = Pair (y, "hello") in x

[%%expect{|
Line 1, characters 72-73:
1 | let string_escape_l (local_ y) = let Pair (x, _) = Pair (y, "hello") in x
                                                                            ^
Error: This value escapes its region.
  Hint: Cannot return a local value without an "exclave_" annotation.
|}]

let string_escape_r (local_ y) = let Pair (x, _) = Pair ("hello", y) in x

[%%expect{|
Line 1, characters 72-73:
1 | let string_escape_r (local_ y) = let Pair (x, _) = Pair ("hello", y) in x
                                                                            ^
Error: This value escapes its region.
  Hint: Cannot return a local value without an "exclave_" annotation.
|}]

let int_escape_l (local_ y) = let Pair (x, _) = Pair (y, 5) in x

[%%expect{|
val int_escape_l : local_ int -> int = <fun>
|}, Principal{|
Line 1, characters 63-64:
1 | let int_escape_l (local_ y) = let Pair (x, _) = Pair (y, 5) in x
                                                                   ^
Error: This value escapes its region.
  Hint: Cannot return a local value without an "exclave_" annotation.
|}]

let int_escape_r (local_ y) = let Pair (x, _) = Pair (5, y) in x

[%%expect{|
val int_escape_r : local_ int -> int = <fun>
|}, Principal{|
Line 1, characters 63-64:
1 | let int_escape_r (local_ y) = let Pair (x, _) = Pair (5, y) in x
                                                                   ^
Error: This value escapes its region.
  Hint: Cannot return a local value without an "exclave_" annotation.
|}]

let string_escape_expected_l : local_ _ -> _ pair = fun x -> Pair (x, "hello")

[%%expect{|
Line 1, characters 67-68:
1 | let string_escape_expected_l : local_ _ -> _ pair = fun x -> Pair (x, "hello")
                                                                       ^
Error: This value escapes its region.
|}]

let string_escape_expected_r : local_ _ -> _ pair = fun x -> Pair ("hello", x)

[%%expect{|
Line 1, characters 76-77:
1 | let string_escape_expected_r : local_ _ -> _ pair = fun x -> Pair ("hello", x)
                                                                                ^
Error: This value escapes its region.
|}]


(* If this one ends up being accepted under non-principal mode, that's fine. *)
let int_escape_expected_l : local_ _ -> _ pair = fun x -> Pair (x, 5)

[%%expect{|
Line 1, characters 64-65:
1 | let int_escape_expected_l : local_ _ -> _ pair = fun x -> Pair (x, 5)
                                                                    ^
Error: This value escapes its region.
|}]

let int_escape_expected_r : local_ _ -> _ pair = fun x -> Pair (5, x)

[%%expect{|
val int_escape_expected_r : local_ int -> int pair = <fun>
|}, Principal{|
Line 1, characters 67-68:
1 | let int_escape_expected_r : local_ _ -> _ pair = fun x -> Pair (5, x)
                                                                       ^
Error: This value escapes its region.
|}]

let escape : 'a -> unit = fun _ -> ()

[%%expect{|
val escape : 'a -> unit = <fun>
|}]

let pattern_l (local_ x) =
  match x with
  | Pair (y, 0) -> escape y
  | _ -> ()

[%%expect{|
val pattern_l : local_ int pair -> unit = <fun>
|}, Principal{|
Line 3, characters 26-27:
3 |   | Pair (y, 0) -> escape y
                              ^
Error: This value escapes its region.
  Hint: This argument cannot be local,
  because it is an argument in a tail call.
|}]

let pattern_r (local_ x) =
  match x with
  | Pair (0, y) -> escape y
  | _ -> ()

[%%expect{|
val pattern_r : local_ int pair -> unit = <fun>
|}, Principal{|
Line 3, characters 26-27:
3 |   | Pair (0, y) -> escape y
                              ^
Error: This value escapes its region.
  Hint: This argument cannot be local,
  because it is an argument in a tail call.
|}]
