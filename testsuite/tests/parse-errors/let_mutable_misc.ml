(* TEST
   flags = "-extension let_mutable";
   expect; *)

(* Test 1: let mutable ... and ... is illegal *)
let foo_1 () =
  let mutable x = []
  and z = 3
  in
  x <- z :: x;
  match x with
  | [] -> 0
  | z :: _ -> z

[%%expect{|
Line 2, characters 14-15:
2 |   let mutable x = []
                  ^
Error: Mutable let bindings are not allowed as part of a `let .. and ..' group
|}]

(* Test 2: mutable and rec don't mix *)
let foo_2_1 () =
  let mutable rec x = 1 :: x in
  match x with
  | [] -> 0
  | _ :: _ -> 1

[%%expect{|
Line 2, characters 18-19:
2 |   let mutable rec x = 1 :: x in
                      ^
Error: Mutable let bindings are not allowed to be recursive
|}]

(* Test 3: only variable patterns may be mutable *)
let foo_3_1 y =
  let mutable (x1,x2) = (y,y+1) in
  x1 <- x1 + 10;
  x2 <- x2 + 20;
  (x1,x2)

[%%expect {|
Line 2, characters 14-21:
2 |   let mutable (x1,x2) = (y,y+1) in
                  ^^^^^^^
Error: Only variables are allowed as the left-hand side of "let mutable"
|}]

type t3_2 = {x_3_2 : int}
let foo_3_2 y =
  let mutable {x_3_2} = {x_3_2 = y + 1} in
  x_3_2 <- x_3_2 + 10;
  x_3_2


[%%expect{|
type t3_2 = { x_3_2 : int; }
Line 3, characters 14-21:
3 |   let mutable {x_3_2} = {x_3_2 = y + 1} in
                  ^^^^^^^
Error: Only variables are allowed as the left-hand side of "let mutable"
|}]
