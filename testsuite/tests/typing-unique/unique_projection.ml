(* TEST
   flags += "-extension unique_beta";
   expect;
*)

let unique_id : unique_ 'a -> unique_ 'a = fun x -> x
let aliased_id : 'a -> 'a = fun x -> x
type 'a aliased = { a : 'a @@ aliased many }
[%%expect{|
val unique_id : 'a @ unique -> 'a @ unique = <fun>
val aliased_id : 'a -> 'a = <fun>
type 'a aliased = { a : 'a @@ many aliased; }
|}]

let proj_alias x =
  match x with
  | { a } -> a, aliased_id x
[%%expect{|
val proj_alias : 'a aliased -> 'a * 'a aliased = <fun>
|}]

let proj_unique x =
  match x with
  | { a } -> a, unique_id x
[%%expect{|
Line 3, characters 26-27:
3 |   | { a } -> a, unique_id x
                              ^
Error: This value is used uniquely, but you tried to project out of it.
Hint: This is currently not supported, please consult the documentation.
|}]
