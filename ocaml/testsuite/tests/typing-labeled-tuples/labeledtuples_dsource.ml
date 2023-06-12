
(* TEST
   * expect
   flags = "-dsource"
*)
let x = ~~(~x:1, ~y:2)
[%%expect{|

let x = (~x:1, ~y:2);;
Line 1, characters 8-22:
1 | let x = ~~(~x:1, ~y:2)
            ^^^^^^^^^^^^^^
Error: Labeled tuples are not yet supported
|}]

let z = 5
let y = ~~(~z:z, ~z, ~z:(z [@attr]))
[%%expect{|

let z = 5;;
val z : int = 5

let y = (~z, ~z, ~z:((z)[@attr ]));;
Line 2, characters 8-36:
2 | let y = ~~(~z:z, ~z, ~z:(z [@attr]))
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Labeled tuples are not yet supported
|}]
