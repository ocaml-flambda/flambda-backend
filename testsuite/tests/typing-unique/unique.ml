(* TEST
 expect;
*)

(* unique means the value is the only usage *)
let dup x = unique_ (x, x)
[%%expect{|
Line 1, characters 24-25:
1 | let dup x = unique_ (x, x)
                            ^
Error: This value is used here, but it is already being used as unique:
Line 1, characters 21-22:
1 | let dup x = unique_ (x, x)
                         ^

|}]

(* unique value can be used more than once *)
let dup (unique_ x) = (x, x)
[%%expect{|
val dup : 'a @ unique -> 'a * 'a = <fun>
|}]

(* once value can be used only once*)
let dup (once_ x) = (x, x)
[%%expect{|
Line 1, characters 24-25:
1 | let dup (once_ x) = (x, x)
                            ^
Error: This value is used here,
       but it is defined as once and is already being used:
Line 1, characters 21-22:
1 | let dup (once_ x) = (x, x)
                         ^

|}]

let dup (unique_ x) = (unique_ x, x, x)
[%%expect{|
Line 1, characters 34-35:
1 | let dup (unique_ x) = (unique_ x, x, x)
                                      ^
Error: This value is used here, but it is already being used as unique:
Line 1, characters 31-32:
1 | let dup (unique_ x) = (unique_ x, x, x)
                                   ^

|}]

let dup (unique_ x) = (x, (unique_ x), x)
[%%expect{|
Line 1, characters 26-37:
1 | let dup (unique_ x) = (x, (unique_ x), x)
                              ^^^^^^^^^^^
Error: This value is used here as unique, but it is already being used:
Line 1, characters 23-24:
1 | let dup (unique_ x) = (x, (unique_ x), x)
                           ^

|}]


let dup (unique_ x) = ((unique_ x), x)
[%%expect{|
Line 1, characters 36-37:
1 | let dup (unique_ x) = ((unique_ x), x)
                                        ^
Error: This value is used here, but it is already being used as unique:
Line 1, characters 23-34:
1 | let dup (unique_ x) = ((unique_ x), x)
                           ^^^^^^^^^^^

|}]

(* below we define a tuple that can be used multiple times,
  but manually relax it to once *)
let dup x = once_ (x, x)
[%%expect{|
val dup : 'a -> 'a * 'a @ once = <fun>
|}]

(* closing over unique values gives once closure  *)
let f () =
  let unique_ k = [1;2;3] in
  let g () = (unique_ k) @ [1;2;3] in
  g () @ g ()
[%%expect{|
Line 4, characters 9-10:
4 |   g () @ g ()
             ^
Error: This value is used here,
       but it is defined as once and is already being used:
Line 4, characters 2-3:
4 |   g () @ g ()
      ^

|}]

(* but if the closure doesn't utilize the uniqueness,
  then it's not once *)
let f () =
  let unique_ k = [1;2;3] in
  let g () = k @ [1;2;3] in
  g () @ g ()
[%%expect{|
val f : unit -> int list = <fun>
|}]

(* closing over once values gives once closure *)
(* note that in g we don't annotate k; because once_ is already the most relaxed mode *)
let f () =
  let once_ k = [(fun x -> x)] in
  let g () = k @ [(fun x -> x)] in
  g () @ g ()
[%%expect{|
Line 3, characters 13-14:
3 |   let g () = k @ [(fun x -> x)] in
                 ^
Error: This value is "once" but expected to be "many".
|}]

(* variables inside loops will be made both aliased and many *)
(* the following is fine, because k inside loop is aliased *)
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
  let once_ k = fun x -> x in
  for i = 1 to 5 do
    k
  done
[%%expect{|
Line 4, characters 4-5:
4 |     k
        ^
Error: The value "k" is once, so cannot be used inside a for loop
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
Error: This value is "aliased" but expected to be "unique".
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
Error: This value is "aliased" but expected to be "unique".
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
Error: This value is "once" but expected to be "many".
|}]

(* the following is fine - we relax many to once *)
let foo () = once_ x
[%%expect{|
val foo : unit -> string @ once = <fun>
|}]

(* top-level must be aliased; the following unique is weakened to aliased *)
let unique_ foo = "foo"
[%%expect{|
val foo : string = "foo"
|}]


(* the following is bad - trying to tighten aliased to unique *)
let foo () = unique_ x
[%%expect{|
Line 1, characters 21-22:
1 | let foo () = unique_ x
                         ^
Error: This value is "aliased" but expected to be "unique".
|}]


(* CR zqian: [global] should imply [aliased]/[many], once we introduce borrowing whose
scope is controlled by locality *)
type 'a glob = { glob: 'a @@ aliased many } [@@unboxed]
[%%expect{|
type 'a glob = { glob : 'a @@ many aliased; } [@@unboxed]
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
module M : sig val drop : 'a @ unique -> unit @ unique end
|}]

(* In the following we won't use module *)
(* printed modes are imprecise *)
let unique_id : unique_ 'a -> unique_ 'a = fun x -> x
[%%expect{|
val unique_id : 'a @ unique -> 'a @ unique = <fun>
|}]

let aliased_id : 'a -> 'a = fun x -> x
[%%expect{|
val aliased_id : 'a -> 'a = <fun>
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
val tail_unique : 'a list @ unique -> 'a list @ unique = <fun>
|}]

let higher_order (f : unique_ 'a -> unique_ 'b) (unique_ x : 'a) = unique_ f x
[%%expect{|
val higher_order : ('a @ unique -> 'b @ unique) -> 'a @ unique -> 'b = <fun>
|}]

let higher_order2 (f : 'a -> unique_ 'b) (x : 'a) = unique_ f x
[%%expect{|
val higher_order2 : ('a -> 'b @ unique) -> 'a -> 'b = <fun>
|}]

let higher_order3 (f : 'a -> 'b) (unique_ x : 'a) = unique_ f x
[%%expect{|
Line 1, characters 60-63:
1 | let higher_order3 (f : 'a -> 'b) (unique_ x : 'a) = unique_ f x
                                                                ^^^
Error: This value is "aliased" but expected to be "unique".
|}]

let higher_order4 (f : unique_ 'a -> 'b) (x : 'a) = f (aliased_id x)
[%%expect{|
Line 1, characters 54-68:
1 | let higher_order4 (f : unique_ 'a -> 'b) (x : 'a) = f (aliased_id x)
                                                          ^^^^^^^^^^^^^^
Error: This value is "aliased" but expected to be "unique".
|}]

let higher_order5 (unique_ x) = let f (unique_ x) = unique_ x in higher_order f x
[%%expect{|
val higher_order5 : 'a @ unique -> 'a = <fun>
|}]

let higher_order6 (unique_ x) = let f (unique_ x) = unique_ x in higher_order2 f x
[%%expect{|
Line 1, characters 79-80:
1 | let higher_order6 (unique_ x) = let f (unique_ x) = unique_ x in higher_order2 f x
                                                                                   ^
Error: This expression has type "'a @ unique -> 'a"
       but an expression was expected of type "'b -> 'c @ unique"
|}]

let inf1 (unique_ x : float) = unique_ let y = x in y
[%%expect{|
val inf1 : float @ unique -> float = <fun>
|}]

let inf2 (b : bool) (unique_ x : float) = unique_ let y = if b then x else 1.0 in y
[%%expect{|
val inf2 : bool -> float @ unique -> float = <fun>
|}]

let inf3 : bool -> float -> unique_ float -> float = fun b y x ->
  let _ = aliased_id y in let unique_ z = if b then x else y in z
[%%expect{|
Line 2, characters 59-60:
2 |   let _ = aliased_id y in let unique_ z = if b then x else y in z
                                                               ^
Error: This value is "aliased" but expected to be "unique".
|}]

let inf4 (b : bool) (y : float) (unique_ x : float) =
  let _ = aliased_id y in let unique_ z = if b then x else y in z
[%%expect{|
Line 2, characters 59-60:
2 |   let _ = aliased_id y in let unique_ z = if b then x else y in z
                                                               ^
Error: This value is used here as unique, but it has already been used:
Line 2, characters 21-22:
2 |   let _ = aliased_id y in let unique_ z = if b then x else y in z
                         ^

|}]


let inf5 (b : bool) (y : float) (unique_ x : float) =
  let z = if b then x else y in unique_ z
[%%expect{|
val inf5 : bool -> float @ unique -> float @ unique -> float = <fun>
|}]

let inf6 (unique_ x) = let f x = x in higher_order f x
[%%expect{|
val inf6 : 'a @ unique -> 'a = <fun>
|}]

let unique_default_args ?(unique_ x = 1.0) () = x
[%%expect{|
val unique_default_args : ?x:float @ unique -> unit -> float = <fun>
|}]

(* Unique Local *)

let ul (unique_ local_ x) = x
[%%expect{|
val ul : 'a @ local unique -> local_ 'a = <fun>
|}]

let ul_ret x = exclave_ unique_ x
[%%expect{|
val ul_ret : 'a @ unique -> local_ 'a = <fun>
|}]

let rec foo =
  fun (local_ o) ->
  match (unique_ o) with
  | Some () -> foo None
  | None -> ()
[%%expect{|
val foo : unit option @ local unique -> unit = <fun>
|}]

let rec bar =
  fun (unique_ o) ->
  match o with
  | Some () -> ()
  | None -> bar (local_ Some ()) [@nontail]
[%%expect{|
val bar : unit option @ local unique -> unit = <fun>
|}]

let foo : local_ unique_ string -> unit = fun (local_ s) -> ()
[%%expect{|
val foo : string @ local unique -> unit = <fun>
|}]

let bar : local_ unique_ string -> unit = fun (unique_ s) -> ()
[%%expect{|
val bar : string @ local unique -> unit = <fun>
|}]

(* Currying *)

let curry =
  let foo ~a ~b ~c ~d = (a, b, c, (unique_ d)) in
  foo ~a:3 ~c:4
[%%expect{|
val curry : b:'_weak1 -> d:'_weak2 @ unique -> int * '_weak1 * int * '_weak2 =
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
Error: This value is "once" but expected to be "many".
|}]

let curry =
  let foo ~a ~b ~c ~d = ((unique_ a), b, c, d) in
  foo ~a:3 ~c:4
[%%expect{|
Line 3, characters 2-15:
3 |   foo ~a:3 ~c:4
      ^^^^^^^^^^^^^
Error: This value is "once" but expected to be "many".
|}]

let curry =
  let foo ~a ~b ~c ~d = (a, (unique_ b), c, unique_ d) in
  foo ~a:3 ~c:4
[%%expect{|
val curry :
  b:'_weak3 @ unique -> d:'_weak4 @ unique -> int * '_weak3 * int * '_weak4 =
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
       but it is defined as once and is already being used:
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
       but it is defined as once and is already being used:
Line 4, characters 25-28:
4 |   let baz = bar ~b:4 in (baz ~d:3, baz ~d:5)
                             ^^^

|}]

let curry =
  let unique_ x = "foo" in
  let foo () = unique_ x in
  (foo (), foo ())
[%%expect{|
Line 4, characters 11-14:
4 |   (foo (), foo ())
               ^^^
Error: This value is used here,
       but it is defined as once and is already being used:
Line 4, characters 3-6:
4 |   (foo (), foo ())
       ^^^

|}]

type box = { x : int }
[%%expect{|
type box = { x : int; }
|}]

let curry (unique_ b1 : box) (unique_ b2 : box) = ()
[%%expect{|
val curry : box @ unique -> box @ unique -> unit = <fun>
|}]

let curry : unique_ box -> unique_ box -> unit = fun b1 b2 -> ()
[%%expect{|
val curry : box @ unique -> box @ unique -> unit = <fun>
|}]

let curry : unique_ box -> (unique_ box -> unit) = fun b1 b2 -> ()
[%%expect{|
Line 1, characters 51-66:
1 | let curry : unique_ box -> (unique_ box -> unit) = fun b1 b2 -> ()
                                                       ^^^^^^^^^^^^^^^
Error: This function when partially applied returns a value which is "once",
       but expected to be "many".
|}]

let curry : unique_ box -> (unique_ box -> unit) = fun b1 -> function | b2 -> ()
[%%expect{|
Line 1, characters 51-80:
1 | let curry : unique_ box -> (unique_ box -> unit) = fun b1 -> function | b2 -> ()
                                                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This function when partially applied returns a value which is "once",
       but expected to be "many".
|}]

(* For nested functions, inner functions are not constrained *)
let no_curry : unique_ box -> (unique_ box -> unit) = fun b1 -> fun b2 -> ()
[%%expect{|
val no_curry : box @ unique -> (box @ unique -> unit) = <fun>
|}]

(* If both type and mode are wrong, complain about type *)
let f () =
  let id2 (x : string) = aliased_id x in
  let unique_ r = 42 in
  id2 r
[%%expect{|
Line 4, characters 6-7:
4 |   id2 r
          ^
Error: This expression has type "int" but an expression was expected of type
         "string"
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
Error: This value is used here, but it is already being used as unique:
Line 5, characters 16-17:
5 |        in Node (x, x)
                    ^

|}]

(* Uniqueness is unbroken by the implicit positional argument. *)
let f ~(call_pos : [%call_pos]) () =
  let unique_ x = call_pos in
  (x, x)
;;
[%%expect{|
val f : call_pos:[%call_pos] -> unit -> lexing_position * lexing_position =
  <fun>
|}]

let f ~(call_pos : [%call_pos]) () =
  unique_ (call_pos, call_pos)
;;
[%%expect{|
Line 2, characters 21-29:
2 |   unique_ (call_pos, call_pos)
                         ^^^^^^^^
Error: This value is used here, but it is already being used as unique:
Line 2, characters 11-19:
2 |   unique_ (call_pos, call_pos)
               ^^^^^^^^

|}]

let array_pats (arr : int option array) =
  match arr with
  | [| o |] -> let _ = unique_id arr in aliased_id o
  | _ -> None
[%%expect{|
Line 3, characters 33-36:
3 |   | [| o |] -> let _ = unique_id arr in aliased_id o
                                     ^^^
Error: This value is used here as unique,
       but it has already been used in an array pattern:
Line 3, characters 4-11:
3 |   | [| o |] -> let _ = unique_id arr in aliased_id o
        ^^^^^^^

|}]

let iarray_pats (arr : int option iarray) =
  match arr with
  | [: o :] -> let _ = unique_id arr in unique_id o
  | _ -> None
[%%expect{|
Line 3, characters 50-51:
3 |   | [: o :] -> let _ = unique_id arr in unique_id o
                                                      ^
Error: This value is used here,
       but it is part of a value that has already been used as unique:
Line 3, characters 33-36:
3 |   | [: o :] -> let _ = unique_id arr in unique_id o
                                     ^^^

|}]

(* Shadowing of unique variables *)
let shadow x =
  x, (let x = (1, 2) in x)
[%%expect{|
val shadow : 'a -> 'a * (int * int) = <fun>
|}]

let array_pat_barrier (arr : int option array) =
  match arr with
  | [| _ |] -> unique_id arr
  | _ -> [| None |]
[%%expect{|
Line 3, characters 25-28:
3 |   | [| _ |] -> unique_id arr
                             ^^^
Error: This value is used here as unique,
       but it has already been used in an array pattern:
Line 3, characters 4-11:
3 |   | [| _ |] -> unique_id arr
        ^^^^^^^

|}]

let iarray_pat_barrier (arr : int option iarray) =
  match arr with
  | [: _ :] -> unique_id arr
  | _ -> [: None :]
[%%expect{|
Line 3, characters 25-28:
3 |   | [: _ :] -> unique_id arr
                             ^^^
Error: This value is used here as unique,
       but it has already been used in an array pattern:
Line 3, characters 4-11:
3 |   | [: _ :] -> unique_id arr
        ^^^^^^^

|}]

let constant_pat_barrier (opt : int option) =
  match opt with
  | Some 1 -> unique_id opt
  | _ -> None
[%%expect{|
Line 3, characters 24-27:
3 |   | Some 1 -> unique_id opt
                            ^^^
Error: This value is used here as unique,
       but part of it has already been used in a constant pattern:
Line 3, characters 9-10:
3 |   | Some 1 -> unique_id opt
             ^

|}]

let lazy_pat_barrier (l : int Lazy.t) =
  match l with
  | lazy 1 -> unique_id l
  | _ -> lazy 2
[%%expect{|
Line 3, characters 24-25:
3 |   | lazy 1 -> unique_id l
                            ^
Error: This value is used here as unique,
       but it has already been used in a lazy pattern:
Line 3, characters 4-10:
3 |   | lazy 1 -> unique_id l
        ^^^^^^

|}]
