(* Confirm that pprintast round-trips with Jane Street experimental syntax. *)

(***********)
(* Layouts *)

let f (type a : immediate) (x : a) = x
let f (type (a : immediate)) (x : a) = x
let f (type (a : immediate) (b : immediate)) (x : a) = x

let f y (type a : immediate) (x : a) = x
let f y (type (a : immediate)) (x : a) = x
let f y (type (a : immediate) (b : immediate)) (x : a) = x

let f y (type a : immediate) = y
let f y (type (a : immediate)) = y
let f y (type (a : immediate) (b : immediate)) = y

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

(*********)
(* Local *)

(* parameters *)
let f (local_ x) ~(local_ y) ~z:(local_ z) = x + y + z;;

(* bindings *)
let g () =
  let local_ f = () in
  let local_ f x y = x + y in
  (* nroberts: we should reenable this test when we fix
   * pprint_ast to put the (int -> int) annotation back in
   * the correct position. *)
  (*let local_ f x y : int -> int = fun z -> x + y in*)
  ();;

(* expressions *)
let g () = local_
  let f = local_ () in
  let f x y = local_ (x + y) in
  local_ ();;

let g () = exclave_
  let f = local_ () in
  let f x y = local_ (x + y) in
  ();;

(* types *)
type record =
  { global_ field : int
  ; normal_field : int
  };;

type fn = local_ int -> local_ int;;
type nested_fn = (local_ int -> local_ int) -> local_ int;;

(*******************)
(* Include functor *)

module T = struct
  include functor F
end

module type S = sig
  include functor F
end
