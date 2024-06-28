(* TEST
 flags = "-extension layouts_beta";
 expect;
*)

type t_any_non_null : any_non_null

[%%expect{|
type t_any_non_null : any_non_null
|}]

type t_value_or_null : value_or_null

[%%expect{|
Line 1, characters 23-36:
1 | type t_value_or_null : value_or_null
                           ^^^^^^^^^^^^^
Error: Layout value_or_null is more experimental than allowed by the enabled layouts extension.
       You must enable -extension layouts_alpha to use this feature.
|}]

type 'a t = 'a or_null

[%%expect{|
Line 1, characters 15-22:
1 | type 'a t = 'a or_null
                   ^^^^^^^
Error: Unbound type constructor or_null
|}]

let x = This 3.14

[%%expect{|
Line 1, characters 8-12:
1 | let x = This 3.14
            ^^^^
Error: Unbound constructor This
|}]

let y = Null

[%%expect{|
Line 1, characters 8-12:
1 | let y = Null
            ^^^^
Error: Unbound constructor Null
|}]
