(* Confirm that Pprintast round-trips with Jane Street experimental syntax.
   test.ml is the driver that runs this test. test.ml also checks that
   Pprintast doesn't print the attribute encoding.
*)

(***********)
(* Layouts *)

let f (type a : immediate) (x : a) = x;;
let f (type (a : immediate)) (x : a) = x;;
let f (type (a : immediate) (b : immediate)) (x : a) = x;;
let f (type (a : immediate) (b : immediate) c) (x : a) = x;;

let f y (type a : immediate) (x : a) = x;;
let f y (type (a : immediate)) (x : a) = x;;
let f y (type (a : immediate) (b : immediate)) (x : a) = x;;

let f y (type a : immediate) = y;;
let f y (type (a : immediate)) = y;;
let f y (type (a : immediate) (b : immediate)) = y;;
let f y (type (a : immediate) (b : immediate) c) = y;;


(* Just newtypes, no value parameters *)
let f (type a : immediate) (type b : immediate)
      (type (c : immediate) (d : immediate))
  = ();;

module type S_for_layouts = sig
  type t : float64

  type variant = A : ('a : immediate). 'a -> variant

  val f1: ('a : float64) ('b : immediate) 'c . 'a -> 'b -> 'c
  val f2: ('a : float64) 'b ('c : bits64) . 'a -> 'b -> 'c
  val f3: 'a 'b ('c : word) . 'a -> 'b -> 'c
end;;

type ('a : immediate) for_layouts = 'a;;

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
(* Modes *)

(* parameters *)
let f (local_ unique_ x) ~(local_ once_ y) ~z:(unique_ once_ z)
  ?foo:(local_ unique_ once_ w = 1) () = ();;

(* bindings *)
let g () =
  let local_ unique_ f = () in
  let unique_ once_ f : 'a . 'a -> 'a = fun x -> x in
  let once_ local_ f x y = x + y in
  let local_ unique_ once_ f : int -> int = fun z -> z + z in
  let local_ f x: int -> int = x in
  (* nroberts: we should reenable this test when we fix
   * pprint_ast to put the (int -> int) annotation back in
   * the correct position. *)
  (*let local_ f x y : int -> int = fun z -> x + y + z in*)
  ();;

(* expressions *)
let g () = local_ unique_
  let f = unique_ once_ () in
  let f x y = once_ local_ (x + y) in
  local_ unique_ once_ ();;

(* exclaves *)
let f () = exclave_
  let f x y = exclave_ (x + y) in
  ()

(* types *)
type record =
  { global_ field : int
  ; normal_field : int
  };;

type 'a parameterized_record = {
  mutable a: 'a ;
  global_ c: 'a };;

type fn = local_ unique_ int -> local_ once_ int;;
type nested_fn = (local_ unique_ int -> local_ once_ int) -> local_ unique_ once_ int;;
type ('a, 'b) labeled_fn =
  a:local_ unique_ 'a -> ?b:local_ once_ 'b -> unique_ once_ 'a -> (int -> once_ unique_ 'b);;

(*******************)
(* Include functor *)

module F_struct (_ : sig end) = struct
end

module type F_sig = functor (_ : sig end) -> sig end

module T = struct
  include functor F_struct
end;;

module type S = sig
  include functor F_sig
end;;

(********************)
(* Immutable arrays *)

let f x =
  match x with
  | [::] -> [::]
  | ([:x:] [@test.attr1]) -> (([:x:])[@test.attr1])
  | ([:x;y:] [@test.attr2][@test.attr3]) ->
      ([:x;y:] [@test.attr2][@test.attr3])
  | _ -> assert false;;

(******************)
(* Labeled tuples *)
let z, punned = 4, 5
let x_must_be_even _ = assert false
exception Odd

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

(********************)
(* Unboxed literals *)

open Stable

let test_float s f =
  Format.printf "%s: %f\n" s (Float_u.to_float f); Format.print_flush ()
let test_int32 s f =
  Format.printf "%s: %ld\n" s (Int32_u.to_int32 f); Format.print_flush ()
let test_int64 s f =
  Format.printf "%s: %Ld\n" s (Int64_u.to_int64 f); Format.print_flush ()
let test_nativeint s f =
  Format.printf "%s: %s\n" s (Nativeint_u.to_string f); Format.print_flush ()

(* Expressions *)

let () = test_float "e" #2.718281828459045
let () = test_float "negative_one_half" (-#0.5)
let () = test_float "negative_one_half" (- #0.5)
let () = test_float "negative_one_half" (-.#0.5)
let () = test_float "negative_one_half" (-. #0.5)
let () = test_float "positive_one_dot" (+#1.)
let () = test_float "positive_one_dot" (+ #1.)
let () = test_float "positive_one_dot" (+.#1.)
let () = test_float "positive_one_dot" (+. #1.)
let () = test_float "one_billion" (#1e9)
let () = test_float "one_twenty_seven_point_two_five_in_floating_hex" (#0x7f.4)
let () = test_float "five_point_three_seven_five_in_floating_hexponent" (#0xa.cp-1)

let () = test_nativeint "zero" (#0n)
let () = test_int32 "positive_one" (+#1l)
let () = test_int32 "positive_one" (+ #1l)
let () = test_int64 "negative_one" (-#1L)
let () = test_int64 "negative_one" (- #1L)
let () = test_nativeint "two_fifty_five_in_hex" (#0xFFn)
let () = test_int32 "twenty_five_in_octal" (#0o31l)
let () = test_int64 "forty_two_in_binary" (#0b101010L)

(* Patterns *)

let f x =
  match x with
  | #4. -> `Four
  | #5. -> `Five
  | _ -> `Other
;;

let () =
  match f #5. with
  | `Five -> ()
  | _ -> assert false;;

let f x =
  match x with
  | #4. -> #0.
  | #5. -> #1.
  | x ->  x
;;

test_float "result" (f #7.);;

let f x =
  match x with
  | #4. -> #0.
  | #5. -> #1.
  | #6. -> #2.
  | #7. -> #3.
  | #8. -> #4.
  | #9. -> #5.
  | #10. -> #6.
  | #11. -> #7.
  | x ->  x
;;

test_float "larger match result" (f #7.);;


let f x =
  match x with
  | #4L -> `Four
  | #5L -> `Five
  | _ -> `Other
;;

let () =
  match f #4L with
  | `Four -> ()
  | _ -> assert false;;

let f x =
  match x with
  | #4L -> #0L
  | #5L -> #1L
  | x ->  x
;;

test_int64 "result" (f #7L);;
