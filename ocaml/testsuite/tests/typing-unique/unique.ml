(* TEST
   flags += "-extension unique"
 * expect
*)

(* unique means the value is the only usage *)
let dup x = unique_ (x, x)
[%%expect{|
Line 1, characters 24-25:
1 | let dup x = unique_ (x, x)
                            ^
Error: This value is used here, but it has already been used as unique:
Line 1, characters 21-22:
1 | let dup x = unique_ (x, x)
                         ^

|}]

(* unique value can be used more than once *)
let dup (unique_ x) = (x, x)
[%%expect{|
val dup : unique_ 'a -> 'a * 'a = <fun>
|}]

(* once value can be used only once*)
let dup (once_ x) = (x, x)
[%%expect{|
Line 1, characters 24-25:
1 | let dup (once_ x) = (x, x)
                            ^
Error: This value is used here,
       but it is defined as once and has already been used:
Line 1, characters 21-22:
1 | let dup (once_ x) = (x, x)
                         ^

|}]

let dup (unique_ x) = (unique_ x, x, x)
[%%expect{|
Line 1, characters 34-35:
1 | let dup (unique_ x) = (unique_ x, x, x)
                                      ^
Error: This value is used here, but it has already been used as unique:
Line 1, characters 31-32:
1 | let dup (unique_ x) = (unique_ x, x, x)
                                   ^

|}]

let dup (unique_ x) = (x, (unique_ x), x)
[%%expect{|
Line 1, characters 26-37:
1 | let dup (unique_ x) = (x, (unique_ x), x)
                              ^^^^^^^^^^^
Error: This value is used here as unique, but it has already been used:
Line 1, characters 23-24:
1 | let dup (unique_ x) = (x, (unique_ x), x)
                           ^

|}]


let dup (unique_ x) = ((unique_ x), x)
[%%expect{|
Line 1, characters 36-37:
1 | let dup (unique_ x) = ((unique_ x), x)
                                        ^
Error: This value is used here, but it has already been used as unique:
Line 1, characters 23-34:
1 | let dup (unique_ x) = ((unique_ x), x)
                           ^^^^^^^^^^^

|}]

(* below we define a tuple that can be used multiple times,
  but manually relax it to once *)
let dup x = once_ (x, x)
[%%expect{|
val dup : 'a -> once_ 'a * 'a = <fun>
|}]

(* closing over unique values gives once closure  *)
let f () =
  let unique_ k = "foo" in
  let g () = (unique_ k) ^ (unique_ k) in
  g () ^ g ()
[%%expect{|
Line 3, characters 27-38:
3 |   let g () = (unique_ k) ^ (unique_ k) in
                               ^^^^^^^^^^^
Error: This value is used here, but it has already been used as unique:
Line 3, characters 13-24:
3 |   let g () = (unique_ k) ^ (unique_ k) in
                 ^^^^^^^^^^^

|}]

(* but if the closure doesn't utilize the uniqueness,
  then it's not once *)
let f () =
  let unique_ k = "foo" in
  let g () = k ^ k in
  g () ^ g ()
[%%expect{|
val f : unit -> string = <fun>
|}]

(* variables inside loops will be made both shared and many *)
(* the following is fine, because k inside loop is shared *)
let f () =
  let unique_ k = "foo" in
  for i = 1 to 5 do
    ignore k
  done
[%%expect{|
val f : unit -> unit = <fun>
|}]


(* The following is bad, because k is once and cannot be used more than once*)
let f () =
  let once_ k = "foo" in
  for i = 1 to 5 do
    k
  done
[%%expect{|
Line 4, characters 4-5:
4 |     k
        ^
Error: The value k is once, so cannot be used inside a for loop
|}]

(* The following is bad, because k is used uniquely *)
let f () =
  let unique_ k = "foo" in
  for i = 1 to 5 do
    unique_ k
  done
[%%expect{|
Line 4, characters 12-13:
4 |     unique_ k
                ^
Error: Found a shared value where a unique value was expected
  Hint: This identifier cannot be used uniquely,
  because it was defined outside of the for-loop.
|}]

let f =
  let unique_ a = "hello" in
  let g (unique_ a) = a in
  for i = 0 to 5 do
    let _ = g a in ()
  done;
  ()
[%%expect{|
Line 5, characters 14-15:
5 |     let _ = g a in ()
                  ^
Error: Found a shared value where a unique value was expected
  Hint: This identifier cannot be used uniquely,
  because it was defined outside of the for-loop.
|}]

let f =
  let g (unique_ a) = a in
  for i = 0 to 5 do
    let unique_ a = 3 in
    let _ = g a in ()
  done;
  ()
[%%expect{|
val f : unit = ()
|}]

(* the following is howerver fine, because g doesn't use the uniqueness of k;
in fact, the k inside g is just shared.
    *)
let f () =
  let unique_ k = "foo" in
  let g () = k ^ k in
  (* k is unique, and thus g is once *)
  g () ^ g ()
[%%expect{|
val f : unit -> string = <fun>
|}]

(* closing over once values gives once closure *)
(* note that in g we don't annotate k; becaue once_ is already the most relaxed mode
    *)
let f () =
  let once_ k = "foo" in
  let g () = k in
  (g (), g () )
[%%expect{|
Line 4, characters 9-10:
4 |   (g (), g () )
             ^
Error: This value is used here,
       but it is defined as once and has already been used:
Line 4, characters 3-4:
4 |   (g (), g () )
       ^

|}]

let x = "foo"
[%%expect{|
val x : string = "foo"
|}]

(* Top-level must be many *)
let once_ foo = "foo"
[%%expect{|
Line 1, characters 4-21:
1 | let once_ foo = "foo"
        ^^^^^^^^^^^^^^^^^
Error: Found a once value where a many value was expected
|}]

(* the following is fine - we relax many to once *)
let foo y = once_ x
[%%expect{|
val foo : 'a -> once_ string = <fun>
|}]

(* top-level must be shared; the following unique is weakened to shared *)
let unique_ foo = "foo"
[%%expect{|
val foo : string = "foo"
|}]


(* the following is bad - trying to tighten shared to unique *)
let foo y = unique_ x
[%%expect{|
Line 1, characters 20-21:
1 | let foo y = unique_ x
                        ^
Error: Found a shared value where a unique value was expected
|}]


(* global modality entails shared modality;
this is crucial once we introduce borrowing whose scope is controlled
by locality *)
type 'a glob = { global_ glob: 'a } [@@unboxed]
[%%expect{|
type 'a glob = { global_ glob : 'a; } [@@unboxed]
|}]
let dup (glob : 'a) : 'a glob * 'a glob = unique_ ({glob}, {glob})
[%%expect{|
val dup : 'a -> 'a glob * 'a glob = <fun>
|}]

(* For strict type/mode match we need module *)
module M : sig
  val drop : unique_ 'a -> unique_ unit
  end = struct
  let drop (unique_ x) = unique_ ()
end
[%%expect{|
module M : sig val drop : unique_ 'a -> unique_ unit end
|}]

(* In the following we won't use module *)
(* printed modes are imprecise *)
let unique_id : 'a. unique_ 'a -> unique_ 'a = fun x -> x
[%%expect{|
val unique_id : unique_ 'a -> unique_ 'a = <fun>
|}]

let shared_id : 'a -> 'a = fun x -> x
[%%expect{|
val shared_id : 'a -> 'a = <fun>
|}]

let tail_unique _x =
  let unique_ y = "foo" in unique_id y
[%%expect{|
val tail_unique : 'a -> string = <fun>
|}]

let tail_unique : unique_ 'a list -> unique_ 'a list = function
  | [] -> []
  | _ :: xx -> xx
[%%expect{|
val tail_unique : unique_ 'a list -> unique_ 'a list = <fun>
|}]

let higher_order (f : unique_ 'a -> unique_ 'b) (unique_ x : 'a) = unique_ f x
[%%expect{|
val higher_order : (unique_ 'a -> unique_ 'b) -> unique_ 'a -> 'b = <fun>
|}]

let higher_order2 (f : 'a -> unique_ 'b) (x : 'a) = unique_ f x
[%%expect{|
val higher_order2 : ('a -> unique_ 'b) -> 'a -> 'b = <fun>
|}]

let higher_order3 (f : 'a -> 'b) (unique_ x : 'a) = unique_ f x
[%%expect{|
Line 1, characters 60-63:
1 | let higher_order3 (f : 'a -> 'b) (unique_ x : 'a) = unique_ f x
                                                                ^^^
Error: Found a shared value where a unique value was expected
|}]

let higher_order4 (f : unique_ 'a -> 'b) (x : 'a) = f (shared_id x)
[%%expect{|
Line 1, characters 54-67:
1 | let higher_order4 (f : unique_ 'a -> 'b) (x : 'a) = f (shared_id x)
                                                          ^^^^^^^^^^^^^
Error: Found a shared value where a unique value was expected
|}]

let higher_order5 (unique_ x) = let f (unique_ x) = unique_ x in higher_order f x
[%%expect{|
val higher_order5 : unique_ 'a -> 'a = <fun>
|}]

let higher_order6 (unique_ x) = let f (unique_ x) = unique_ x in higher_order2 f x
[%%expect{|
Line 1, characters 79-80:
1 | let higher_order6 (unique_ x) = let f (unique_ x) = unique_ x in higher_order2 f x
                                                                                   ^
Error: This expression has type unique_ 'a -> 'a
       but an expression was expected of type 'b -> unique_ 'c
|}]

type record_update = { x : string }
[%%expect{|
type record_update = { x : string; }
|}]

let update2 = update { x = "bar" }
[%%expect{|
Line 1, characters 14-20:
1 | let update2 = update { x = "bar" }
                  ^^^^^^
Error: Unbound value update
|}]

let inf1 (unique_ x : float) = unique_ let y = x in y
[%%expect{|
val inf1 : unique_ float -> float = <fun>
|}]

let inf2 (b : bool) (unique_ x : float) = unique_ let y = if b then x else 1.0 in y
[%%expect{|
val inf2 : bool -> unique_ float -> float = <fun>
|}]

let inf3 : bool -> float -> unique_ float -> float = fun b y x ->
  let _ = shared_id y in let unique_ z = if b then x else y in z
[%%expect{|
Line 2, characters 58-59:
2 |   let _ = shared_id y in let unique_ z = if b then x else y in z
                                                              ^
Error: Found a shared value where a unique value was expected
|}]

let inf4 (b : bool) (y : float) (unique_ x : float) =
  let _ = shared_id y in let unique_ z = if b then x else y in z
[%%expect{|
Line 2, characters 58-59:
2 |   let _ = shared_id y in let unique_ z = if b then x else y in z
                                                              ^
Error: This value is used here as unique, but it has already been used:
Line 2, characters 20-21:
2 |   let _ = shared_id y in let unique_ z = if b then x else y in z
                        ^

|}]


let inf5 (b : bool) (y : float) (unique_ x : float) =
  let z = if b then x else y in unique_ z
[%%expect{|
val inf5 : bool -> unique_ float -> unique_ float -> float = <fun>
|}]

let inf6 (unique_ x) = let f x = x in higher_order f x
[%%expect{|
val inf6 : unique_ 'a -> 'a = <fun>
|}]

let unique_default_args ?(unique_ x = 1.0) () = x
[%%expect{|
val unique_default_args : ?x:unique_ float -> unit -> float = <fun>
|}]

(* Unique Local *)

let ul (unique_ local_ x) = x
[%%expect{|
val ul : local_ unique_ 'a -> local_ 'a = <fun>
|}]

let ul_ret x = unique_ local_ x
[%%expect{|
val ul_ret : unique_ 'a -> local_ 'a = <fun>
|}]

type point = { x : float; y : float }
[%%expect{|
type point = { x : float; y : float; }
|}]

let overwrite_point t =
  unique_ ({t with y = 0.5}, {t with x = 0.5})
[%%expect{|
val overwrite_point : unique_ point -> point * point = <fun>
|}]

let gc_soundness_nobug (local_ unique_ p) (local_ f) =
  local_ { p with x = f }
[%%expect{|
val gc_soundness_nobug : local_ unique_ point -> local_ float -> local_ point =
  <fun>
|}]

let rec foo =
  fun (local_ o) ->
  match (unique_ o) with
  | Some () -> foo None
  | None -> ()
[%%expect{|
val foo : local_ unique_ unit option -> unit = <fun>
|}]

let rec bar =
  fun (unique_ o) ->
  match o with
  | Some () -> ()
  | None -> bar (local_ Some ()) [@nontail]
[%%expect{|
val bar : local_ unique_ unit option -> unit = <fun>
|}]

let foo : local_ unique_ string -> unit = fun (local_ s) -> ()
[%%expect{|
val foo : local_ unique_ string -> unit = <fun>
|}]

let bar : local_ unique_ string -> unit = fun (unique_ s) -> ()
[%%expect{|
val bar : local_ unique_ string -> unit = <fun>
|}]

(* Currying *)

let curry =
  let foo ~a ~b ~c ~d = (a, b, c, (unique_ d)) in
  foo ~a:3 ~c:4
[%%expect{|
val curry : b:'_weak1 -> d:unique_ '_weak2 -> int * '_weak1 * int * '_weak2 =
  <fun>
|}]

(* the following two failed because top-level must be many *)
(* TODO: maybe a particular error for that? *)
let curry =
  let foo ~a ~b ~c ~d = (a, b, (unique_ c), (unique_ d)) in
  foo ~a:3 ~c:4
[%%expect{|
Line 3, characters 2-15:
3 |   foo ~a:3 ~c:4
      ^^^^^^^^^^^^^
Error: Found a once value where a many value was expected
|}]

let curry =
  let foo ~a ~b ~c ~d = ((unique_ a), b, c, d) in
  foo ~a:3 ~c:4
[%%expect{|
Line 3, characters 2-15:
3 |   foo ~a:3 ~c:4
      ^^^^^^^^^^^^^
Error: Found a once value where a many value was expected
|}]

let curry =
  let foo ~a ~b ~c ~d = (a, (unique_ b), c, unique_ d) in
  foo ~a:3 ~c:4
[%%expect{|
val curry :
  b:unique_ '_weak3 -> d:unique_ '_weak4 -> int * '_weak3 * int * '_weak4 =
  <fun>
|}]

let curry =
  let foo ~a ~b ~c ~d = (a, b, (unique_ c), unique_ d) in
  let bar = foo ~a:3 ~b:2 ~c:4 in
  (bar ~d:3, bar ~d:5)
[%%expect{|
Line 4, characters 13-16:
4 |   (bar ~d:3, bar ~d:5)
                 ^^^
Error: This value is used here,
       but it is defined as once and has already been used:
Line 4, characters 3-6:
4 |   (bar ~d:3, bar ~d:5)
       ^^^

|}]

let curry =
  let foo ~a ~b ~c ~d = (a, b, (unique_ c), unique_ d) in
  let bar = foo ~a:3 ~c:4 in
  let baz = bar ~b:4 in (baz ~d:3, baz ~d:5)
[%%expect{|
Line 4, characters 35-38:
4 |   let baz = bar ~b:4 in (baz ~d:3, baz ~d:5)
                                       ^^^
Error: This value is used here,
       but it is defined as once and has already been used:
Line 4, characters 25-28:
4 |   let baz = bar ~b:4 in (baz ~d:3, baz ~d:5)
                             ^^^

|}]

let curry =
  let unique_ x = "foo" in
  let foo y = unique_ x in
  (foo 1, foo 2)
[%%expect{|
Line 4, characters 10-13:
4 |   (foo 1, foo 2)
              ^^^
Error: This value is used here,
       but it is defined as once and has already been used:
Line 4, characters 3-6:
4 |   (foo 1, foo 2)
       ^^^

|}]

type box = { x : int }
[%%expect{|
type box = { x : int; }
|}]

let curry (unique_ b1 : box) (unique_ b2 : box) = ()
[%%expect{|
val curry : unique_ box -> unique_ box -> unit = <fun>
|}]

let curry : unique_ box -> unique_ box -> unit = fun b1 b2 -> ()
[%%expect{|
val curry : unique_ box -> unique_ box -> unit = <fun>
|}]

let curry : unique_ box -> (unique_ box -> unit) = fun b1 b2 -> ()
[%%expect{|
Line 1, characters 51-66:
1 | let curry : unique_ box -> (unique_ box -> unit) = fun b1 b2 -> ()
                                                       ^^^^^^^^^^^^^^^
Error: This function when partially applied returns a once value,
       but expected to be many.
|}]

let curry : unique_ box -> (unique_ box -> unit) = fun b1 -> function | b2 -> ()
[%%expect{|
Line 1, characters 51-80:
1 | let curry : unique_ box -> (unique_ box -> unit) = fun b1 -> function | b2 -> ()
                                                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This function when partially applied returns a once value,
       but expected to be many.
|}]

(* For nested functions, inner functions are not constrained *)
let no_curry : unique_ box -> (unique_ box -> unit) = fun b1 -> fun b2 -> ()
[%%expect{|
val no_curry : unique_ box -> (unique_ box -> unit) = <fun>
|}]

(* If both type and mode are wrong, complain about type *)
let f () =
  let id2 (x : string) = shared_id x in
  let unique_ r = 42 in
  id2 r
[%%expect{|
Line 4, characters 6-7:
4 |   id2 r
          ^
Error: This expression has type int but an expression was expected of type
         string
|}]


let foo () =
  let unique_ _bar : int -> int -> int =
    ((fun y z -> z) : int -> unique_ (int -> int)) in
  ()
[%%expect{|
Line 3, characters 4-50:
3 |     ((fun y z -> z) : int -> unique_ (int -> int)) in
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type int -> unique_ (int -> int)
       but an expression was expected of type int -> int -> int
|}]


let return_local : local_ 'a -> local_ 'a = fun x -> x
let return_global : local_ 'a -> int = fun x -> 0
[%%expect{|
val return_local : local_ 'a -> local_ 'a = <fun>
val return_global : local_ 'a -> int = <fun>
|}]


(* recursive function *)
(* the following error, because make_tree must return unique
    (which is needed for x to be unique), and therefore
    cannot return (x, x) *)
type tree = Leaf | Node of tree * tree
let rec make_tree = fun n ->
  if n <= 0 then Leaf
  else let unique_ x = make_tree (n - 1)
       in Node (x, x)
[%%expect{|
type tree = Leaf | Node of tree * tree
Line 5, characters 19-20:
5 |        in Node (x, x)
                       ^
Error: This value is used here, but it has already been used as unique:
Line 5, characters 16-17:
5 |        in Node (x, x)
                    ^

|}]
