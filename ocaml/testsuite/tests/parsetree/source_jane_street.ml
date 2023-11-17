(* Confirm that Pprintast round-trips with Jane Street experimental syntax.
   test.ml is the driver that runs this test. test.ml also checks that
   Pprintast doesn't print the attribute encoding.
*)

(***********)
(* Layouts *)

let f (type a : immediate) (x : a) = x;;
let f (type (a : immediate)) (x : a) = x;;
let f (type (a : immediate) (b : immediate)) (x : a) = x;;

let f y (type a : immediate) (x : a) = x;;
let f y (type (a : immediate)) (x : a) = x;;
let f y (type (a : immediate) (b : immediate)) (x : a) = x;;

let f y (type a : immediate) = y;;
let f y (type (a : immediate)) = y;;
let f y (type (a : immediate) (b : immediate)) = y;;

(* Just newtypes, no value parameters *)
let f (type a : immediate) (type b : immediate)
      (type (c : immediate) (d : immediate))
  = ();;

(******************)
(* Comprehensions *)

(* simple range *)
[: x for x = 0 to 100 :];;
[| x for x = 0 to 100 |];;
[  x for x = 0 to 100   ];;

(* simple in-comprehension *)
[: x for x in [: 1; 2; 3 :] :];;
[| x for x in [| 1; 2; 3 |] |];;
[  x for x in [  1; 2; 3  ]  ];;

(* complex comprehension *)
[: x + y
    for x in [: 1; 2; 3 :]
    for y in [: 4; 5; 6 :]
    when x + y > 6
:];;
[| x + y
    for x in [| 1; 2; 3 |]
    for y in [| 4; 5; 6 |]
    when x + y > 6
|];;
[  x + y
    for x in [ 1; 2; 3 ]
    for y in [ 4; 5; 6 ]
    when x + y > 6
];;


(* User-written attributes *)
([(x[@test.attr1]) for (x[@test.attr2]) in ([][@test.attr3])] [@test.attr4]);;

(*********)
(* Local *)

(* parameters *)
let f (local_ x) ~(local_ y) ~z:(local_ z) ?foo:(local_ w = 1) = x + y + z + w;;

(* bindings *)
let g () =
  let local_ f = () in
  let local_ f : 'a . 'a -> 'a = fun x -> x in
  let local_ f x y = x + y in
  let local_ f : int -> int = fun z -> z + z in
  (* nroberts: we should reenable this test when we fix
   * pprint_ast to put the (int -> int) annotation back in
   * the correct position. *)
  (*let local_ f x y : int -> int = fun z -> x + y + z in*)
  ();;

(* expressions *)
let g () = local_
  let f = local_ () in
  let f x y = local_ (x + y) in
  local_ ();;

(* types *)
type record =
  { global_ field : int
  ; normal_field : int
  };;

type 'a parameterized_record = {
  mutable a: 'a ;
  global_ c: 'a };;

type fn = local_ int -> local_ int;;
type nested_fn = (local_ int -> local_ int) -> local_ int;;
type ('a, 'b) labeled_fn =
  a:local_ 'a -> ?b:local_ b -> local_ 'a -> (int -> local_ 'b);;

(*******************)
(* Include functor *)

module T = struct
  include functor F
end;;

module type S = sig
  include functor F
end;;

(********************)
(* Immutable arrays *)

let f x =
  match x with
  | [::] -> [::]
  | ([:x:] [@test.attr1]) -> (([:x:])[@test.attr1])
  | ([:x;y:] [@test.attr2][@test.attr3]) ->
      ([:x;y:] [@test.attr2][@test.attr3]);;

(******************)
(* Labeled tuples *)
let x = (~x:1, ~y:2)
let x = ((~x:1, ~y:2) [@test.attr])
let _ = ( ~x: 5, 2, ~z, ~(punned:int))
let (x : (x:int * y:int)) = (~x:1, ~y:2)
let (x : ((x:int * y:int) [@test.attr])) = (~x:1, ~y:2)

let (~x:x0, ~s, ~(y:int), ..) : (x:int * s:string * y:int * string) =
   (~x: 1, ~s: "a", ~y: 2, "ignore me")

module M : sig
  val f : (x:int * string) -> (x:int * string)
  val mk : unit -> (x:bool * y:string)
end = struct
  let f x = x
  let mk () = (~x:false, ~y:"hi")
end

module X_int_int = struct
   type t = (x:int * int)
end

let foo xy k_good k_bad =
   match x_must_be_even xy with
   | (~x, y) -> k_good ()
   | exception Odd -> k_bad ()

let (~(x:int), ~y, _) = (~x: 1, ~y: 2, "ignore me")
let ((~(x:int), ~y, _) [@test.attr]) = (~x: 1, ~y: 2, "ignore me")
let f = fun (~foo, ~bar:bar) -> foo * 10 + bar
let f ((~(x:int),y) : (x:int * int)) : int = x + y

type xy = (x:int * y:int)

(* Reordering and partial matches *)
let lt = (~x:1, ~y:2, ~x:3, 4)

let matches =
  let (~x, .. ) = lt in
  x

let matches =
  let (~y, ~x, .. ) = lt in
  (x, y)
