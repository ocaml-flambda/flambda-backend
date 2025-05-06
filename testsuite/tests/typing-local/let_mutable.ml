(* TEST
   flags = "-extension let_mutable";
   expect; *)

(* Test 1: basic usage in a for loop *)
let foo1 y =
  let mutable x = y in
  for i = 1 to 10 do
    x <- x + i
  done;
  x

let () = assert (Int.equal (foo1 0) 55)
let () = assert (Int.equal (foo1 42) 97)

[%%expect{|
val foo1 : int -> int = <fun>
|}]

(* Test 2: Reject use of mutable in closure. *)
let foo2 y =
  let mutable x = y in
  let add_55 () =
    for i = 1 to 10 do
      x <- x + i
    done;
    x
  in
  add_55

[%%expect{|
Line 5, characters 6-16:
5 |       x <- x + i
          ^^^^^^^^^^
Error: The variable x is mutable, so cannot be used inside a closure that might escape
|}]

(* Test 3: Rejected for same reason as test 2, but this one is actually safe and
   could be allowed with more sophisticated analysis in the future. *)
let foo3 y =
  let mutable x = y in
  let rec add_55 z =
    match z with
    | 0 -> x
    | z -> x <- x + z; add_55 (z-1)
  in
  add_55 10
[%%expect{|
Line 5, characters 11-12:
5 |     | 0 -> x
               ^
Error: The variable x is mutable, so cannot be used inside a closure that might escape
|}]

(* Test 4: Disallowed interactions with locals *)
let foo4_1 y =
  let mutable x = [] in
  for i = 1 to y do
    x <- local_ (i :: x)
  done;
  match x with
  | [] -> assert false
  | (x :: xs) -> x

[%%expect{|
Line 4, characters 9-24:
4 |     x <- local_ (i :: x)
             ^^^^^^^^^^^^^^^
Error: This value escapes its region.
|}]


let foo4_2 y = (* Can't sneak local out of non-local for loop body region *)
  let mutable x = [] in
  let build_loop () =
    for i = 1 to y do local_
      x <- local_ (i :: x)
    done;
    match x with
    | [] -> assert false
    | (x :: xs) -> x
  in
  build_loop ()

[%%expect{|
Line 5, characters 6-26:
5 |       x <- local_ (i :: x)
          ^^^^^^^^^^^^^^^^^^^^
Error: The variable x is mutable, so cannot be used inside a closure that might escape
|}]


let foo4_3 y = (* Can't sneak local out of non-local while loop body region *)
  let mutable x = y in
  let i = ref 1 in
  while !i <= 10 do
    x <- (local_ (x + !i));
    i := !i + 1;
  done; x

[%%expect{|
Line 5, characters 9-26:
5 |     x <- (local_ (x + !i));
             ^^^^^^^^^^^^^^^^^
Error: This value escapes its region.
|}]

let foo4_4 y = (* Can't sneak local out of non-local while cond region *)
  let mutable x = y in
  while x <- (local_ (x + 1)); x <= 100 do
    x <- x + x
  done; x

[%%expect{|
Line 3, characters 13-29:
3 |   while x <- (local_ (x + 1)); x <= 100 do
                 ^^^^^^^^^^^^^^^^
Error: This value escapes its region.
|}]

let foo4_5 y =
  let mutable x = [] in
  for i = 1 to y do
    for j = 1 to y do exclave_
      x <- local_ ((i*j) :: x)
    done
  done;
  x
;;
[%%expect{|
Line 5, characters 11-30:
5 |       x <- local_ ((i*j) :: x)
               ^^^^^^^^^^^^^^^^^^^
Error: This value escapes its region.
|}]

let foo4_6 y =
  let mutable x = [] in
  for i = 1 to y do exclave_
    for j = 1 to y do
      x <- local_ ((i*j) :: x)
    done
  done;
  x
;;
[%%expect{|
Line 5, characters 11-30:
5 |       x <- local_ ((i*j) :: x)
               ^^^^^^^^^^^^^^^^^^^
Error: This value escapes its region.
|}]


(* Test 5: Allowed interactions with locals. *)
let foo5_1 y =  (* Assignment of local allowed in same scope *)
  let mutable x = [] in
  x <- (local_ (y :: x));
  x <- (local_ (y :: x));
  match x with
  | [] -> assert false
  | (x :: xs) -> x

let () = assert Int.(equal 42 (foo5_1 42))

let foo5_2 y =  (* Assignment of local works in _local_ for loop body region *)
  let mutable x = [] in
  for i = 1 to y do exclave_
    x <- local_ (i :: x)
  done;
  match x with
  | [] -> assert false
  | (x :: xs) -> x

let () = assert Int.(equal 42 (foo5_2 42))

let foo5_3 y = (* Assignment of local works in _local_ while body region *)
  let mutable x = y in
  let i = ref 1 in
  while !i <= 10 do exclave_
    x <- (local_ (x + !i));
    i := !i + 1;
  done; x

let foo5_4 y = (* Assign of local works in _local_ while cond region *)
  let mutable x = y in
  while exclave_ x <- (local_ (x + 1)); x <= 100 do
    x <- x + x
  done; x

[%%expect{|
val foo5_1 : 'a -> 'a = <fun>
val foo5_2 : int -> int = <fun>
val foo5_3 : int -> int = <fun>
val foo5_4 : int -> int = <fun>
|}]

(* Test 6: let mutable ... and ... is illegal *)
let foo_6 () =
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

(* Test 7: mutable and rec don't mix *)
let foo_7_1 () =
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

(* Test 8: only variable patterns may be mutable *)
let foo_8_1 y =
  let mutable (x1,x2) = (y,y+1) in
  x1 <- x1 + 10;
  x2 <- x2 + 20;
  (x1,x2)

[%%expect {|
Line 2, characters 14-21:
2 |   let mutable (x1,x2) = (y,y+1) in
                  ^^^^^^^
Error: Only variables are allowed as left-hand side of "let mutable"
|}]

type t8_2 = {x_8_2 : int}
let foo_8_2 y =
  let mutable {x_8_2} = {x_8_2 = y + 1} in
  x_8_2 <- x_8_2 + 10;
  x_8_2


[%%expect{|
type t8_2 = { x_8_2 : int; }
Line 3, characters 14-21:
3 |   let mutable {x_8_2} = {x_8_2 = y + 1} in
                  ^^^^^^^
Error: Only variables are allowed as left-hand side of "let mutable"
|}]

(* Test 11: binding a mutable variable shouldn't be simplified away *)
let f_11 () =
  let mutable x = 10 in
  let y = x in
  x <- x + 10;
  (y,x)

let () = assert (f_11 () = (10,20))
[%%expect{|
val f_11 : unit -> int * int = <fun>
|}]

(* Test 12: like Test 11, but with a constructor *)
type t_12 = Foo_12 of int

let y_12 =
  let mutable x = 42 in
  let y = Foo_12 x in
    x <- 84; y
;;
[%%expect{|
type t_12 = Foo_12 of int
val y_12 : t_12 = Foo_12 42
|}]

(* Test 13: disallow modes? *)
let u_13 y = let x @ unique = y in x;;

let f_13 y z = let mutable x @ unique = y in
  x <- z;
  u_13 x
;;
[%%expect{|
|}]
