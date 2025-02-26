(* TEST
 flags = "-extension-universe stable";
 expect;
*)

(* CR dkalinichenko: [or_null] is not available in [stable]. *)
let x : int or_null = This 3

[%%expect{|
Line 1, characters 12-19:
1 | let x : int or_null = This 3
                ^^^^^^^
Error: Unbound type constructor "or_null"
|}]

(* CR dkalinichenko: [Null] and [This] are not available in [stable]. *)

let y = Null

[%%expect{|
Line 1, characters 8-12:
1 | let y = Null
            ^^^^
Error: Unbound constructor "Null"
|}]

let z = This 5

[%%expect{|
Line 1, characters 8-12:
1 | let z = This 5
            ^^^^
Error: Unbound constructor "This"
|}]

(* CR dkalinichenko: [or_null_reexport] is not available in [stable]. *)

type 'a my_or_null : value_or_null = 'a or_null [@@or_null_reexport]
[%%expect{|
Line 1, characters 40-47:
1 | type 'a my_or_null : value_or_null = 'a or_null [@@or_null_reexport]
                                            ^^^^^^^
Error: Unbound type constructor "or_null"
Hint: Did you mean "my_or_null"?
|}]
