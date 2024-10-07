(* TEST
 flags = "-extension labeled_tuples -dsource";
 expect;
*)
let x = ~x:1, ~y:2
[%%expect{|

let x = (~x:1, ~y:2);;
val x : x:int * y:int @@ global many = (~x:1, ~y:2)
|}]

(* Attribute should prevent punning *)
let z = 5
let y = ~z:z, ~z, ~z:(z [@attr])
[%%expect{|

let z = 5;;
val z : int @@ global many = 5

let y = (~z, ~z, ~z:((z)[@attr ]));;
val y : z:int * z:int * z:int @@ global many = (~z:5, ~z:5, ~z:5)
|}]

let (~x:x0, ~s, ~(y:int), ..) : x:int * s:string * y:int * string =
   ~x: 1, ~s: "a", ~y: 2, "ignore me"
[%%expect{|

let (~x:x0, ~s, ~y:(y : int), ..) : (x:int * s:string * y:int * string) =
  (~x:1, ~s:"a", ~y:2, "ignore me");;
val x0 : int @@ global many portable = 1
val s : string @@ global many portable = "a"
val y : int @@ global many portable = 2
|}]
