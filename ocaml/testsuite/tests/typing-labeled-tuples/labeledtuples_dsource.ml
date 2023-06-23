(* TEST
   * expect
   flags = "-dsource"
*)
let x = ~~(~x:1, ~y:2)
[%%expect{|

let x = (~x:1, ~y:2);;
val x : x:int * y:int = (~x:1, ~y:2)
|}]

(* Attribute should prevent punning *)
let z = 5
let y = ~~(~z:z, ~z, ~z:(z [@attr]))
[%%expect{|

let z = 5;;
val z : int = 5

let y = (~z, ~z, ~z:((z)[@attr ]));;
val y : z:int * z:int * z:int = (~z:5, ~z:5, ~z:5)
|}]

let (~~(~x=x0; ~s; ~(y:int); _)) : ~~(x:int * s:string * y:int * string) =
   ~~(~x: 1, ~s: "a", ~y: 2, "ignore me")
[%%expect{|

let ((~x:x0, ~s:s, ~y:(y : int), _) : (x:int * s:string * y:int * string)) =
  (~x:1, ~s:"a", ~y:2, "ignore me");;
Line 1, characters 4-32:
1 | let (~~(~x=x0; ~s; ~(y:int); _)) : ~~(x:int * s:string * y:int * string) =
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Labeled tuple patterns are not yet supported
|}]
