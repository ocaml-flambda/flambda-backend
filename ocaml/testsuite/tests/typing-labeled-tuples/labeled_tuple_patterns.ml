(* TEST
   * expect
*)

(* Test match statements with exception patterns *)

exception Odd

let x_must_be_even (~~(~x; y)) =
   if x mod 2 = 1 then
      raise Odd
   else
      (~~(~x, y))

let foo xy k_good k_bad = 
   match x_must_be_even xy with
   | ~~(~x; y) -> k_good ()
   | exception Odd -> k_bad ()
[%%expect{|
exception Odd
val x_must_be_even : x:int * 'a -> x:int * 'a = <fun>
val foo : x:int * 'a -> (unit -> 'b) -> (unit -> 'b) -> 'b = <fun>
|}]

(* Test correctness *)
let _ = foo (~~(~x:2, 5)) (fun () -> true) (fun () -> false)
let _ = foo (~~(~x:3, 5)) (fun () -> false) (fun () -> true)
[%%expect{|
- : bool = true
- : bool = true
|}]

(* Test that the actions occur outside of the exception handler *)
let _ =
   try
      foo (~~(~x:2, 5)) (fun () -> raise Odd) (fun () -> false)
   with Odd -> true
let _ =
   try
      foo (~~(~x:3, 5)) (fun () -> false) (fun () -> raise Odd)
   with Odd -> true
[%%expect{|
- : bool = true
- : bool = true
|}]

(* Labeled tuple pattern *)
let ~~(~x=x0; ~y=y0; _) = ~~(~x: 1, ~y: 2, "ignore me")
[%%expect{|
val x0 : int = 1
val y0 : int = 2
|}]

(* Pattern with punning and type annotation *)
let ~~(~(x:int); ~y; _) = ~~(~x: 1, ~y: 2, "ignore me")
[%%expect{|
val x : int = 1
val y : int = 2
|}]

(* Patterns in functions *)
let f = fun (~~(~foo; ~bar=bar)) -> foo * 10 + bar
let bar = 5
let _ = f (~~(~foo:1, ~bar))
[%%expect{|
val f : foo:int * bar:int -> int = <fun>
val bar : int = 5
- : int = 15
|}]

(* Correct annotation *)
let f : ~~(foo:int * bar:int) -> int =
   fun (~~(~foo; ~bar=bar)) -> foo * 10 + bar
[%%expect{|
val f : foo:int * bar:int -> int = <fun>
|}]

let f = fun (~~(~foo; ~bar=bar) : ~~(foo:int * bar:int)) -> foo * 10 + bar
[%%expect{|
val f : foo:int * bar:int -> int = <fun>
|}]

(* Missing label *)
let f : ~~(int * bar:int) -> int = fun (~~(~foo; ~bar=bar)) -> foo * 10 + bar
[%%expect{|
Line 1, characters 39-59:
1 | let f : ~~(int * bar:int) -> int = fun (~~(~foo; ~bar=bar)) -> foo * 10 + bar
                                           ^^^^^^^^^^^^^^^^^^^^
Error: This pattern was expected to match values of type int * bar:int,
       but it is missing a unlabeled component.
       Hint: use .. to ignore some components.
|}]

let f = fun (~~(~foo; ~bar=bar) : ~~(foo:int * int)) -> foo * 10 + bar
[%%expect{|
Line 1, characters 13-31:
1 | let f = fun (~~(~foo; ~bar=bar) : ~~(foo:int * int)) -> foo * 10 + bar
                 ^^^^^^^^^^^^^^^^^^
Error: This pattern was expected to match values of type foo:int * int,
       but it is missing a unlabeled component.
       Hint: use .. to ignore some components.
|}]

(* Wrong label *)
let f : ~~(foo:int * foo:int) -> int =
   fun (~~(~foo; ~bar=bar)) -> foo * 10 + bar
[%%expect{|
Line 2, characters 7-27:
2 |    fun (~~(~foo; ~bar=bar)) -> foo * 10 + bar
           ^^^^^^^^^^^^^^^^^^^^
Error: This pattern was expected to match values of type foo:int * foo:int,
       but it is missing a component with label foo.
       Hint: use .. to ignore some components.
|}]

(* Wrong type *)
let f : ~~(foo:float * foo:int) -> int =
   fun (~~(~foo; ~bar=bar)) -> foo * 10 + bar
[%%expect{|
Line 2, characters 7-27:
2 |    fun (~~(~foo; ~bar=bar)) -> foo * 10 + bar
           ^^^^^^^^^^^^^^^^^^^^
Error: This pattern was expected to match values of type foo:float * foo:int,
       but it is missing a component with label foo.
       Hint: use .. to ignore some components.
|}]

(* Annotated pattern *)
let f ((~~(~x;y)) : ~~(x:int * int)) : int = x + y
[%%expect{|
val f : x:int * int -> int = <fun>
|}]

(* Misannotated pattern *)
let f ((~~(~x;y)) : ~~(int * int)) : int = x + y
[%%expect{|
Line 1, characters 7-17:
1 | let f ((~~(~x;y)) : ~~(int * int)) : int = x + y
           ^^^^^^^^^^
Error: This pattern was expected to match values of type int * int,
       but it is missing a unlabeled component.
       Hint: use .. to ignore some components.
|}]

let f ((~~(~x;y)) : ~~(int * x:int)) : int = x + y
[%%expect{|
val f : int * x:int -> int = <fun>
|}]

(* Annotation within pattern *)
let f ((~~(~(x:int);y)) : ~~(x:int * int)) : int = x + y
[%%expect{|
val f : x:int * int -> int = <fun>
|}]

let f (~~(~(x:int);y)) = x + y
[%%expect{|
val f : x:int * int -> int = <fun>
|}]

let f (~~(~x=(x0:int);y)) = x0 + y
[%%expect{|
val f : x:int * int -> int = <fun>
|}]

(* Misannotation within pattern *)
let f (~~(~(x:float);y)) = x + y
[%%expect{|
Line 1, characters 27-28:
1 | let f (~~(~(x:float);y)) = x + y
                               ^
Error: This expression has type float but an expression was expected of type
         int
|}]
(* Reordering in functions *)
type xy = ~~(x:int * y:int)
type yx = ~~(y:int * x:int)
let xy_id (pt : xy) = pt
let yx_id (pt : yx) = pt
[%%expect{|
type xy = x:int * y:int
type yx = y:int * x:int
val xy_id : xy -> xy = <fun>
val yx_id : yx -> yx = <fun>
|}]

let xy_id (~~(~y; ~x) : xy) = ~~(~x, ~y)
[%%expect{|
val xy_id : xy -> x:int * y:int = <fun>
|}]


let swap (~~(~x; ~y)) = ~~(~y, ~x)
[%%expect{|
val swap : x:'a * y:'b -> y:'b * x:'a = <fun>
|}]

let swap (~~(~y; ~x) : xy) = ~~(~y, ~x)
[%%expect{|
val swap : xy -> y:int * x:int = <fun>
|}]

let swap (~~(~x; ~y)) = (~~(~x, ~y) : yx)
[%%expect{|
Line 1, characters 25-35:
1 | let swap (~~(~x; ~y)) = (~~(~x, ~y) : yx)
                             ^^^^^^^^^^
Error: This expression has type x:'a * y:'b
       but an expression was expected of type yx = y:int * x:int
|}]

let swap (pt : xy) : yx = pt
[%%expect{|
Line 1, characters 26-28:
1 | let swap (pt : xy) : yx = pt
                              ^^
Error: This expression has type xy = x:int * y:int
       but an expression was expected of type yx = y:int * x:int
|}]

let swap : xy -> yx = Fun.id  
[%%expect{|
Line 1, characters 22-28:
1 | let swap : xy -> yx = Fun.id
                          ^^^^^^
Error: This expression has type xy -> xy
       but an expression was expected of type xy -> yx
       Type xy = x:int * y:int is not compatible with type yx = y:int * x:int
|}]

let swap : xy -> yx = xy_id  
[%%expect{|
Line 1, characters 22-27:
1 | let swap : xy -> yx = xy_id
                          ^^^^^
Error: This expression has type xy -> x:int * y:int
       but an expression was expected of type xy -> yx
       Type x:int * y:int is not compatible with type yx = y:int * x:int
|}]

let swap : xy -> yx = yx_id  
[%%expect{|
Line 1, characters 22-27:
1 | let swap : xy -> yx = yx_id
                          ^^^^^
Error: This expression has type yx -> yx
       but an expression was expected of type xy -> yx
       Type yx = y:int * x:int is not compatible with type xy = x:int * y:int
|}]

(* Reordering and partial matches *)
let lt = (~~(~x:1, ~y:2, ~x:3, 4)) 

(* Full match, in order *)
let matches =
  let (~~(~x; ~y; ~x=x2; z)) = lt in
  (x, y, x2, z)
[%%expect{|
val lt : x:int * y:int * x:int * int = (~x:1, ~y:2, ~x:3, 4)
val matches : int * int * int * int = (1, 2, 3, 4)
|}]

(* Full match, over-bound *)
let matches =
  let (~~(~x; ~y; ~x; z)) = lt in
  (x, y, z)
[%%expect{|
Line 2, characters 19-20:
2 |   let (~~(~x; ~y; ~x; z)) = lt in
                       ^
Error: Variable x is bound several times in this matching
|}]

(* Full match, missing label *)
let matches =
  let (~~(~x; ~y; z)) = lt in
  (x, y, z)
[%%expect{|
Line 2, characters 6-21:
2 |   let (~~(~x; ~y; z)) = lt in
          ^^^^^^^^^^^^^^^
Error: This pattern was expected to match values of type
       x:int * y:int * x:int * int,
       but it is missing a component with label x.
       Hint: use .. to ignore some components.
|}]

(* Full match, wrong label *)
let matches =
  let (~~(~x; ~y; ~w; z)) = lt in
  (x, y, z)
[%%expect{|
Line 2, characters 6-25:
2 |   let (~~(~x; ~y; ~w; z)) = lt in
          ^^^^^^^^^^^^^^^^^^^
Error: This pattern was expected to match values of type
       x:int * y:int * x:int * int,
       but it is missing a component with label x.
       Hint: use .. to ignore some components.
|}]

(* Full match, extra label *)
let matches =
  let (~~(~x; ~y; ~x; ~y; z)) = lt in
  (x, y, z)
[%%expect{|
Line 2, characters 6-29:
2 |   let (~~(~x; ~y; ~x; ~y; z)) = lt in
          ^^^^^^^^^^^^^^^^^^^^^^^
Error: This pattern was expected to match values of type
       x:int * y:int * x:int * int,
       but it contains an extra component with label y.
|}]

(* Full match, extra unlabeled label *)
let matches =
  let (~~(~x; ~y; ~x; z; w)) = lt in
  (x, y, z)
[%%expect{|
Line 2, characters 6-28:
2 |   let (~~(~x; ~y; ~x; z; w)) = lt in
          ^^^^^^^^^^^^^^^^^^^^^^
Error: This pattern was expected to match values of type
       x:int * y:int * x:int * int,
       but it contains an extra unlabeled component.
|}]


(* Partial match *)
let matches =
  let (~~(~x; ~y; .. )) = lt in
  (x, y)
[%%expect{|
val matches : int * int = (1, 2)
|}]

(* Partial match, reordered *)
let matches =
  let (~~(~y; ~x; .. )) = lt in
  (x, y)
[%%expect{|
val matches : int * int = (1, 2)
|}]

(* Partial match, reordered, over-bound *)
let matches =
  let (~~(~y=x; ~x; .. )) = lt in
  x
[%%expect{|
Line 2, characters 13-14:
2 |   let (~~(~y=x; ~x; .. )) = lt in
                 ^
Error: Variable x is bound several times in this matching
|}]

(* Partial match one *)
let matches =
  let (~~(~x; .. )) = lt in
  x
[%%expect{|
val matches : int = 1
|}]
 
(* Partial match all *)
let matches =
   let (~~( ~x; ~y; ~x=x2; z; .. )) = lt in
   (x, y, x2, z)
[%%expect{|
Line 2, characters 7-35:
2 |    let (~~( ~x; ~y; ~x=x2; z; .. )) = lt in
           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 189 [unnecessarily-partial-tuple-pattern]: This tuple pattern unnecessarily ends in '..',
as it explicitly matches all components of its expected type.
val matches : int * int * int * int = (1, 2, 3, 4)
|}]
 
(* Partial match too many of a name *)
let matches =
   let (~~( ~y; ~y=y2; ~x; .. )) = lt in
   (x, y)
[%%expect{|
Line 2, characters 7-32:
2 |    let (~~( ~y; ~y=y2; ~x; .. )) = lt in
           ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This pattern was expected to match values of type
       x:int * y:int * x:int * int,
       but it contains an extra component with label y.
|}]

(* Partial match bad name *)
let matches =
   let (~~( ~w; ~y; ~x; .. )) = lt in
   (x, y, x2, z)
[%%expect{|
Line 2, characters 7-29:
2 |    let (~~( ~w; ~y; ~x; .. )) = lt in
           ^^^^^^^^^^^^^^^^^^^^^^
Error: This pattern was expected to match values of type
       x:int * y:int * x:int * int,
       but it contains an extra component with label w.
|}]

(* Nested pattern *)
let f (z, (~~(~y; ~x))) = x, y, z
[%%expect{|
val f : 'a * (y:'b * x:'c) -> 'c * 'b * 'a = <fun>
|}]

(* Non-principally known patterns *)

let f (z, (~~(~y; ~x; ..))) = x, y, z
[%%expect{|
Line 1, characters 10-26:
1 | let f (z, (~~(~y; ~x; ..))) = x, y, z
              ^^^^^^^^^^^^^^^^
Error: Could not determine the type of this partial tuple pattern.
|}]

let f (~~(~x; ~y; ..)) = x, y
[%%expect{|
Line 1, characters 6-22:
1 | let f (~~(~x; ~y; ..)) = x, y
          ^^^^^^^^^^^^^^^^
Error: Could not determine the type of this partial tuple pattern.
|}]

(* CR labeled tuples: test all of the above with top-level lets *)

(* Labeled tuples nested in records *)

let x = ref (~~(~x:1, ~y:2, ~x:3, 4))

(* Good match *)
let _1234 = match x with
| { contents = ~~(~x=x0; ~y; ~x ; z)} -> (x0, y, x, z)
[%%expect{|
val x : (x:int * y:int * x:int * int) ref =
  {contents = (~x:1, ~y:2, ~x:3, 4)}
val _1234 : int * int * int * int = (1, 2, 3, 4)
|}]

(* Good partial match *)
let _1  = match x with
| { contents = ~~(~x; ..)} -> x
[%%expect{|
val _1 : int = 1
|}]

(* Wrong label *)
let () = match x with
| { contents = ~~(~w ; ..) } -> w 
[%%expect{|
Line 2, characters 15-26:
2 | | { contents = ~~(~w ; ..) } -> w
                   ^^^^^^^^^^^
Error: This pattern was expected to match values of type
       x:int * y:int * x:int * int,
       but it contains an extra component with label w.
|}]

(* Missing unordered label *)
let () = match x with
| { contents = ~~(~x=x0; ~y ; ~x) } -> y 
[%%expect{|
Line 2, characters 15-33:
2 | | { contents = ~~(~x=x0; ~y ; ~x) } -> y
                   ^^^^^^^^^^^^^^^^^^
Error: This pattern was expected to match values of type
       x:int * y:int * x:int * int, but it is missing a unlabeled component.
       Hint: use .. to ignore some components.
|}]

(* Extra unordered label *)
let () = match x with
| { contents = ~~(~x=x0; ~y; ~x; w1; w2) } -> y 
[%%expect{|
Line 2, characters 15-40:
2 | | { contents = ~~(~x=x0; ~y; ~x; w1; w2) } -> y
                   ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This pattern was expected to match values of type
       x:int * y:int * x:int * int,
       but it contains an extra unlabeled component.
|}]

(* Extra unordered label, open *)
let () = match x with
| { contents = ~~(~x=x0; ~y; ~x; w1; w2; ..) } -> y 
[%%expect{|
Line 2, characters 15-44:
2 | | { contents = ~~(~x=x0; ~y; ~x; w1; w2; ..) } -> y
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This pattern was expected to match values of type
       x:int * y:int * x:int * int,
       but it contains an extra unlabeled component.
|}]

(* Missing label *)
let () = match x with
| { contents = ~~(~x=x0; ~y; x) } -> y 
[%%expect{|
Line 2, characters 15-31:
2 | | { contents = ~~(~x=x0; ~y; x) } -> y
                   ^^^^^^^^^^^^^^^^
Error: This pattern was expected to match values of type
       x:int * y:int * x:int * int,
       but it is missing a component with label x.
       Hint: use .. to ignore some components.
|}]

(* Extra label *)
let () = match x with
| { contents = ~~(~y=y0; ~y; ~x) } -> y 
[%%expect{|
Line 2, characters 15-32:
2 | | { contents = ~~(~y=y0; ~y; ~x) } -> y
                   ^^^^^^^^^^^^^^^^^
Error: This pattern was expected to match values of type
       x:int * y:int * x:int * int,
       but it is missing a component with label x.
       Hint: use .. to ignore some components.
|}]

(* Behavior w.r.t whether types are principally known *)

let f (z : ~~(x:_ * y:_)) =
  match z with
  | (~~(~y; ~x)) -> x + y
[%%expect{|
val f : x:int * y:int -> int = <fun>
|}]

let f = function (~~(~x; ~y)) -> x + y

let g z =
  (f z, match z with (~~(~y; ~x)) -> x + y)
[%%expect{|
val f : x:int * y:int -> int = <fun>
val g : x:int * y:int -> int * int = <fun>
|}, Principal{|
val f : x:int * y:int -> int = <fun>
Line 4, characters 21-33:
4 |   (f z, match z with (~~(~y; ~x)) -> x + y)
                         ^^^^^^^^^^^^
Error: This pattern matches values of type y:'a * x:'b
       but a pattern was expected which matches values of type x:int * y:int
|}]

let f = function (~~(~x; ~y)) -> x + y

let g z =
  (match z with (~~(~y; ~x)) -> x + y, f z)
[%%expect{|
val f : x:int * y:int -> int = <fun>
Line 4, characters 41-42:
4 |   (match z with (~~(~y; ~x)) -> x + y, f z)
                                             ^
Error: This expression has type y:int * x:int
       but an expression was expected of type x:int * y:int
|}]
