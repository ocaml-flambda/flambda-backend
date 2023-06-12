(* TEST
   * expect
   flags = "-dsource"
*)
let x = ~~(~x:1, ~y:2)
[%%expect{|

let x = (~x:1, ~y:2);;
val x : x: int * y: int = (~x: 1, ~y: 2)
|}]

(* Attribute should prevent punning *)
let z = 5
let y = ~~(~z:z, ~z, ~z:(z [@attr]))
[%%expect{|

let z = 5;;
val z : int = 5

let y = (~z, ~z, ~z:((z)[@attr ]));;
val y : z: int * z: int * z: int = (~z: 5, ~z: 5, ~z: 5)
|}]
