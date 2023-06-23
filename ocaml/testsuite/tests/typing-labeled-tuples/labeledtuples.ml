(* TEST
   * expect
*)

let x = ~~(~x:1, ~y:2)

[%%expect{|
val x : x:int * y:int = (~x:1, ~y:2)
|}];;

let z = 5
let punned = 2
let _ = ~~( ~x: 5, 2, ~z, ~(punned:int))
[%%expect{|
val z : int = 5
val punned : int = 2
- : x:int * int * z:int * punned:int = (~x:5, 2, ~z:5, ~punned:2)
|}]

type ('a, 'b) pair = Pair of 'a * 'b
let x = Pair (~~(~x: 5, 2))

[%%expect{|
type ('a, 'b) pair = Pair of 'a * 'b
Line 2, characters 8-27:
2 | let x = Pair (~~(~x: 5, 2))
            ^^^^^^^^^^^^^^^^^^^
Error: Constructors cannot have labeled arguments. Consider using an inline record instead.
|}]

(* Happy case *)
let foo b = if b then
   ~~(~a: "s", 10, ~c: "hi")
else
   ~~(~a: "5", 10, ~c: "hi")
[%%expect{|
val foo : bool -> a:string * int * c:string = <fun>
|}]

(* Missing label (the type vars in the error aren't ideal, but the same thing
   happens when unifying normal tuples of different lengths) *)
let foo b = if b then
   ~~(~a: "s", 10, "hi")
else
   ~~(~a: "5", 10, ~c: "hi")
[%%expect{|
Line 4, characters 3-28:
4 |    ~~(~a: "5", 10, ~c: "hi")
       ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type a:string * int * c:'a
       but an expression was expected of type a:string * int * string
|}]

(* Missing labeled component *)
let foo b = if b then
   ~~(~a: "s", 10)
else
   ~~(~a: "5", 10, ~c: "hi")
[%%expect{|
Line 4, characters 3-28:
4 |    ~~(~a: "5", 10, ~c: "hi")
       ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type a:'a * 'b * c:'c
       but an expression was expected of type a:string * int
|}]

(* Wrong label *)
let foo b = if b then
   ~~(~a: "s", 10, ~a: "hi")
else
   ~~(~a: "5", 10, ~c: "hi")
[%%expect{|
Line 4, characters 3-28:
4 |    ~~(~a: "5", 10, ~c: "hi")
       ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type a:string * int * c:'a
       but an expression was expected of type a:string * int * a:string
|}]

(* Types in function argument/return *)
let default = ~~(~x: 1, ~y: 2)
let choose_pt replace_with_default pt =
   if replace_with_default then
      default
   else
      pt
[%%expect{|
val default : x:int * y:int = (~x:1, ~y:2)
val choose_pt : bool -> x:int * y:int -> x:int * y:int = <fun>
|}]

(* Application happy case *)
let a = choose_pt true (~~(~x: 5, ~y: 6))
[%%expect{|
val a : x:int * y:int = (~x:1, ~y:2)
|}]

(* CR labeled tuples: reordering should eventually work *)
let a = choose_pt true (~~(~y: 6, ~x: 5))
[%%expect{|
Line 1, characters 23-41:
1 | let a = choose_pt true (~~(~y: 6, ~x: 5))
                           ^^^^^^^^^^^^^^^^^^
Error: This expression has type y:'a * x:'b
       but an expression was expected of type x:int * y:int
|}]

(* Mutually-recursive definitions *)
let rec a = ~~(1, ~lbl:b)
and b = ~~(2, ~lbl:a)
[%%expect{|
Line 2, characters 19-20:
2 | and b = ~~(2, ~lbl:a)
                       ^
Error: This expression has type int * lbl:(int * lbl:'a)
       but an expression was expected of type 'a
       The type variable 'a occurs inside int * lbl:(int * lbl:'a)
|}]

let rec l = ~~(~lbl: 5, ~lbl2: 10) :: l
[%%expect{|
val l : (lbl:int * lbl2:int) list = [(~lbl:5, ~lbl2:10); <cycle>]
|}]

(* Tuple containing labeled tuples *)
let tup = (~~(~a:1, ~b:2), ~~(~b:3, ~a:4), 5)
[%%expect{|
val tup : (a:int * b:int) * (b:int * a:int) * int =
  ((~a:1, ~b:2), (~b:3, ~a:4), 5)
|}]

(* Polymorphic variant containing labeled tuple *)
let a = `Some (~~(~a: 1, ~b:2, 3))
[%%expect{|
val a : [> `Some of a:int * b:int * int ] = `Some (~a:1, ~b:2, 3)
|}]

(* List of labeled tuples *)
let lst = (~~(~a: 1, ~b: 2)) :: []
[%%expect{|
val lst : (a:int * b:int) list = [(~a:1, ~b:2)]
|}]

(* Ref of labeled tuple *)
let x = ref (~~(~x:"hello", 5))
[%%expect{|
val x : (x:string * int) ref = {contents = (~x:"hello", 5)}
|}]

(* Polymorphic record containing a labeled tuple *)
type 'a box = {thing: 'a}
let boxed = {thing = ~~("hello", ~x:5)}
[%%expect{|
type 'a box = { thing : 'a; }
val boxed : (string * x:int) box = {thing = ("hello", ~x:5)}
|}]

(* CR labeled tuples: Add tests with labeled tuples in:
   - non-polymorphic records (where the labeled type must be written out)
   - functors
   - module inclusion
   - recursive modules
*)

(* CR labeled-tuples: test a mutually recursive function with labeled tuple
   params, such as the following:

(* Take a [a:'a * b:'a] and an int, and returns a
   [swapped:[a:'a * b:'a] * same:bool].
   The swapped component is the input with the [a] and [b] components swapped
   as many times as the input int. The second component is whether the first
   equals the input. *)
let rec swap ~~(~a, ~b) =
   function
   | 0 -> ~~(~swapped:~~(~a, ~b), ~same:true)
   | n -> swap' (~~(~a:b, ~b:a)) (n-1)
and swap' ~~(~a, ~b) =
   function
   | 0 -> ~~(~swapped:~~(~a, ~b), ~same:false)
   | n -> swap (~~(~a:b, ~b:a)) (n-1)
*)

(* CR labeled-tuples: test evaluation order w.r.t. reordering, such as in:

type t = { x : unit; y : unit }
let t1 = { x = Printf.printf "x\n"; y = Printf.printf "y\n" }
let t2 = { y = Printf.printf "y\n"; x = Printf.printf "x\n" }
*)

(* Labeled tuple pattern *)
let ~~(~x=x0; ~y=y0; _) = ~~(~x: 1, ~y: 2, "ignore me")
[%%expect{|
Line 1, characters 4-23:
1 | let ~~(~x=x0; ~y=y0; _) = ~~(~x: 1, ~y: 2, "ignore me")
        ^^^^^^^^^^^^^^^^^^^
Error: Labeled tuple patterns are not yet supported
|}]

(* Pattern with punning and type annotation *)
let ~~(~(x:int); ~y; _) = ~~(~x: 1, ~y: 2, "ignore me")
[%%expect{|
Line 1, characters 4-23:
1 | let ~~(~(x:int); ~y; _) = ~~(~x: 1, ~y: 2, "ignore me")
        ^^^^^^^^^^^^^^^^^^^
Error: Labeled tuple patterns are not yet supported
|}]

(* Labeled tuple pattern in constructor pattern, with the same arity as the
   constructor. This is intentionally disallowed. *)
let f = function
| Pair (~~(~x=5; 2)) -> true
| _ -> false
[%%expect{|
Line 2, characters 2-20:
2 | | Pair (~~(~x=5; 2)) -> true
      ^^^^^^^^^^^^^^^^^^
Error: Constructors cannot have labeled arguments. Consider using an inline record instead.
|}]

(* Labeled tuple patterns in constructor patterns with that can union with the
   constructor pattern type.
   
   CR labeled tuples: these should eventually work. *)
let f = function
| Some (~~(~x=5; 2)) -> true
| _ -> false
[%%expect{|
Line 2, characters 7-20:
2 | | Some (~~(~x=5; 2)) -> true
           ^^^^^^^^^^^^^
Error: Labeled tuple patterns are not yet supported
|}]


type t = Foo of (~~(x:int * int))
let f = function
| Foo (~~(~x=5; 2)) -> true
| _ -> false
[%%expect{|
type t = Foo of (x:int * int)
Line 3, characters 6-19:
3 | | Foo (~~(~x=5; 2)) -> true
          ^^^^^^^^^^^^^
Error: Labeled tuple patterns are not yet supported
|}]

(* CR labeled tuples: test constructor special cases thoroughly once patterns
   are typed. *)

(* Labeled tuple type annotations *)
(* Bad type *)
let x: ~~(string * a:int * int) = ~~(~lbl:5, "hi")
[%%expect{|
Line 1, characters 34-50:
1 | let x: ~~(string * a:int * int) = ~~(~lbl:5, "hi")
                                      ^^^^^^^^^^^^^^^^
Error: This expression has type lbl:'a * 'b
       but an expression was expected of type string * a:int * int
|}]

(* Well-typed *)
let x: ~~(string * a:int * int) = ~~("hi", ~a:1, 2)
[%%expect{|
val x : string * a:int * int = ("hi", ~a:1, 2)
|}]

(* Function type *)
let mk_x : ~~(foo:unit * bar:unit) -> ~~(string * a:int * int) = fun _ -> x
[%%expect{|
val mk_x : foo:unit * bar:unit -> string * a:int * int = <fun>
|}]

let x = mk_x (~~(~foo:(), ~bar:()))
[%%expect{|
val x : string * a:int * int = ("hi", ~a:1, 2)
|}]
