(* TEST
   flags = "-extension let_mutable";
   include stdlib_upstream_compatible;
   expect; *)

(* Test 1.1: basic usage in a for loop *)
let foo1 y =
  let mutable x = y in
  for i = 1 to 10 do
    x <- x + i
  done;
  (x : int)

let () = assert (Int.equal (foo1 0) 55)
let () = assert (Int.equal (foo1 42) 97)

[%%expect{|
val foo1 : int -> int = <fun>
|}]

(* Test 1.2: basic usage with a nested record returning string *)
type t_1_2 = { str_1_2 : string ref }
let x_1_2 =
  let mutable x = { str_1_2 = ref "Hi" } in
  x <- { str_1_2 = ref "Bye" };
  (x.str_1_2.contents : string)
[%%expect{|
type t_1_2 = { str_1_2 : string ref; }
val x_1_2 : string = "Bye"
|}]

(* Test 1.3: returning an immutable record *)
type t_1_3 = { str_1_3 : string }
let x_1_3 =
  let mutable x = { str_1_3 = "Hi" } in
  x <- { str_1_3 = "Bye" };
  (x : t_1_3)
[%%expect{|
type t_1_3 = { str_1_3 : string; }
val x_1_3 : t_1_3 = {str_1_3 = "Bye"}
|}]

(* Test 1.4: returning a mutable nested record *)
type t_1_4 = { str_1_4 : string ref }
let x_1_4 =
  let mutable x = { str_1_4 = ref "Hi" } in
  x <- { str_1_4 = ref "Bye" };
  (x : t_1_4)
[%%expect{|
type t_1_4 = { str_1_4 : string ref; }
val x_1_4 : t_1_4 = {str_1_4 = {contents = "Bye"}}
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
Error: Mutable variable cannot be used inside closure.
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
Error: Mutable variable cannot be used inside closure.
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
    for i = 1 to y do exclave_
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
Error: Mutable variable cannot be used inside closure.
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

(* exclave_ closes one region, not two *)
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
[%%expect{|
val foo5_1 : 'a -> 'a = <fun>
|}]

let foo5_2 y =  (* Assignment of local works in _local_ for loop body region *)
  let mutable x = [] in
  for i = 1 to y do exclave_
    x <- local_ (i :: x)
  done;
  match x with
  | [] -> assert false
  | (x :: xs) -> x

let () = assert Int.(equal 42 (foo5_2 42))
[%%expect{|
val foo5_2 : int -> int = <fun>
|}]

let foo5_3 y = (* Assignment of local works in _local_ while body region *)
  let mutable x = y in
  let i = ref 1 in
  while !i <= 10 do exclave_
    x <- (local_ (x + !i));
    i := !i + 1;
  done; x
[%%expect{|
val foo5_3 : int -> int = <fun>
|}]

let foo5_4 y = (* Assign of local works in _local_ while cond region *)
  let mutable x = y in
  while exclave_ x <- (local_ (x + 1)); x <= 100 do
    x <- x + x
  done; x

[%%expect{|
val foo5_4 : int -> int = <fun>
|}]

(* Test 11: binding a mutable variable shouldn't be simplified away *)
let f_11 () =
  let mutable x = 10 in
  let y = x in
  x <- x + 10;
  ((y : int), (x : int))

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

(* Test 13.1: Can't put aliased in unique mutable variable *)
let reset_ref (x @ unique) = x := 0;;
let x_13_1 =
  let y = ref 3 in
  let mutable x @ unique = { contents = 1 } in
  x <- y;
  reset_ref x;
  !y
;;
[%%expect{|
val reset_ref : int ref @ unique -> unit = <fun>
Line 6, characters 12-13:
6 |   reset_ref x;
                ^
Error: This value is "aliased" but expected to be "unique".
|}]

(* Test 13.2: Unique mutable variable *)
let x_13_2 =
  let mutable x @ unique = { contents = 1 } in
  reset_ref x;
  !x
;;
[%%expect{|
Line 3, characters 12-13:
3 |   reset_ref x;
                ^
Error: This value is "aliased" but expected to be "unique".
|}]

(* Test 13.3: Can't put a global in a local record *)
let x_13_3 = ref 0
let y_13_3 =
  let mutable x @ local = ref (ref 0) in
  x := x_13_3;
  x <- ref x_13_3;
  !x
[%%expect{|
val x_13_3 : int ref = {contents = 0}
Line 3, characters 14-37:
3 |   let mutable x @ local = ref (ref 0) in
                  ^^^^^^^^^^^^^^^^^^^^^^^
Error: This value escapes its region.
|}]

(* Test 14: mutable functions *)
let x_14 =
  let mutable f = fun x -> 2*x in
  let y = f 1 in
  f <- (fun x -> 3*x);
  let z = f 10 in
  y + z
;;
[%%expect{|
val x_14 : int = 32
|}]

(* Test 15: mutable unboxed floats *)
let r_15 =
  let open Stdlib_upstream_compatible.Float_u in
  let mutable r = #256.0 in
  for i = 1 to 10 do
    r <- div r #2.0
  done;
  to_float r
;;
(* 2^8 / 2^10 = 2^-2 *)
[%%expect{|
val r_15 : float = 0.25
|}]

(* Test 16: mutable variables must be representable *)
type t_16 : any;;
let f_16 () = let mutable x = (assert false : t_16) in ();;
[%%expect{|
type t_16 : any
Line 2, characters 30-51:
2 | let f_16 () = let mutable x = (assert false : t_16) in ();;
                                  ^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "t_16" but an expression was expected of type
         "('a : '_representable_layout_1)"
       The layout of t_16 is any
         because of the definition of t_16 at line 1, characters 0-15.
       But the layout of t_16 must be representable
         because it's the type of a variable bound by a `let`.
|}, Principal{|
type t_16 : any
Line 2, characters 26-27:
2 | let f_16 () = let mutable x = (assert false : t_16) in ();;
                              ^
Error: This pattern matches values of type "t_16"
       but a pattern was expected which matches values of type
         "('a : '_representable_layout_1)"
       The layout of t_16 is any
         because of the definition of t_16 at line 1, characters 0-15.
       But the layout of t_16 must be representable
         because it's the type of a variable bound by a `let`.
|}]

(* Test 17: mutable variables can't change type *)
let x_17 =
  let mutable x = 3.0 in
  x <- 3;
  x
;;
[%%expect{|
Line 3, characters 7-8:
3 |   x <- 3;
           ^
Error: This expression has type "int" but an expression was expected of type
         "float"
  Hint: Did you mean "3."?
|}]

(* some mode crossing *)
let f () =
  let mutable x : int = 42 in
  x <- (local_ 24);
  x
[%%expect{|
val f : unit -> int = <fun>
|}]
