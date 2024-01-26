(* TEST
 * expect *)

type ('a, 'b) bar0 = Bar0 of 'a * 'b
type bar = (int, string) bar0

type ('a, 'b) foo0 = {
  x : 'a;
  y : 'b;
}
type foo = (int, string) foo0
[%%expect{|
type ('a, 'b) bar0 = Bar0 of 'a * 'b
type bar = (int, string) bar0
type ('a, 'b) foo0 = { x : 'a; y : 'b; }
type foo = (int, string) foo0
|}]

(* mode crosing is implemented at several points
   in the type checker corresponding to the
   following tests *)

(* We are very stingy in the following when giving
   type annotation because we want to control type
   information precisely, so that the examples behave
   in expected way only for supposed reasons. *)

(* A single modification of `type_argument`
   enables mode crossing at the following points,
   all depending on the expected type *)

(* 1. function argument crosses mode at application   *)
let f' x = x + 1
[%%expect{|
val f' : int -> int = <fun>
|}]

let f : local_ _ -> _ =
  fun n -> f' n
[%%expect{|
val f : int@local -> int = <fun>
|}]

(* As comparison, string won't cross modes *)
let f' x = x ^ "hello"
[%%expect{|
val f' : string -> string = <fun>
|}]

let f : local_ _ -> _ =
  fun x -> f' x
[%%expect{|
Line 2, characters 14-15:
2 |   fun x -> f' x
                  ^
Error: This value escapes its region
|}]

(* 2. constructor argument crosses mode at construction *)
let f : local_ _ -> bar =
    fun n -> Bar0 (n, "hello")
[%%expect{|
val f : int@local -> bar = <fun>
|}]

let f : local_ _ -> bar =
  fun n -> Bar0 (42, n)
[%%expect{|
Line 2, characters 21-22:
2 |   fun n -> Bar0 (42, n)
                         ^
Error: This value escapes its region
|}]

(* 3. record field crosses mode at construction *)
let f : local_ _ -> foo =
  fun n -> {x = n; y = "hello"}
[%%expect{|
val f : int@local -> foo = <fun>
|}]

let f : local_ _ -> foo =
  fun n -> {x = 42; y = n}
[%%expect{|
Line 2, characters 24-25:
2 |   fun n -> {x = 42; y = n}
                            ^
Error: This value escapes its region
|}]

(* 4. expression crosses mode when being constrained *)
let f : local_ _ -> _ =
  fun n -> (n : int)
[%%expect{|
val f : int@local -> int = <fun>
|}]

let f : local_ _ -> _ =
  fun n -> (n : string)
[%%expect{|
Line 2, characters 12-13:
2 |   fun n -> (n : string)
                ^
Error: This value escapes its region
|}]

(* 5. polymorphic variant arguments crosses mode on construction*)
let f : local_ _ -> [> `Number of int] =
  fun n -> `Number n
[%%expect{|
val f : int@local -> [> `Number of int ] = <fun>
|}]

let f : local_ _ -> [> `Text of string] =
  fun n -> `Text n
[%%expect{|
Line 2, characters 17-18:
2 |   fun n -> `Text n
                     ^
Error: This value escapes its region
|}]

(* tuple elements crosses mode at construction *)
let f : local_ _ -> int * int =
  fun n -> (n, n)
[%%expect{|
val f : int@local -> int * int = <fun>
|}]

let f : local_ _ -> string * string =
  fun n -> (n, n)
[%%expect{|
Line 2, characters 12-13:
2 |   fun n -> (n, n)
                ^
Error: This value escapes its region
|}]

(* array elements crosses mode at construction *)
let f : local_ _ -> int array =
  fun n -> [|n; n|]
[%%expect{|
val f : int@local -> int array = <fun>
|}]

let f: local_ _ -> string array =
  fun n -> [|n; n|]
[%%expect{|
Line 2, characters 13-14:
2 |   fun n -> [|n; n|]
                 ^
Error: This value escapes its region
|}]

(* after discussion with sdolan, we agree that
  the following cannot type check because of lock;
  lazy is not commonly used anyway. *)
let f: local_ _ -> int lazy_t =
  fun n -> lazy n
[%%expect{|
Line 2, characters 16-17:
2 |   fun n -> lazy n
                    ^
Error: The value n is local, so cannot be used inside a lazy expression.
|}]

(* record field crosses mode at projection  *)
let f : local_ foo -> _  =
  fun r -> r.x
[%%expect{|
val f : foo@local -> int = <fun>
|}]

let f : local_ foo -> _ =
  fun r -> r.y
[%%expect{|
Line 2, characters 11-14:
2 |   fun r -> r.y
               ^^^
Error: This value escapes its region
|}]

(* the expected type is not considered when mode crossing the result of
pexp_field. However, upon function definition, the expected type of
the body will be used to mode cross *)
let f : local_ _ -> int =
  fun r -> r.x
[%%expect{|
val f : (int, 'a) foo0@local -> int = <fun>
|}]

(* expression crosses mode when prefixed with local_ *)
let g : int -> int
  = fun x -> x + 42

let f : _ -> int =
  fun () ->
     g (local_ 42)
[%%expect{|
val g : int -> int = <fun>
val f : unit -> int = <fun>
|}]

let g : string -> string
  = fun y -> y ^ "hello"

let f : _ -> string =
  fun () ->
    g (local_ "world")

[%%expect{|
val g : string -> string = <fun>
Line 6, characters 6-22:
6 |     g (local_ "world")
          ^^^^^^^^^^^^^^^^
Error: This value escapes its region
|}]

(* the result of function application crosses mode *)
let f : _ -> local_ _ =
  fun () -> local_ 42
[%%expect{|
val f : unit -> int@local = <fun>
|}]

let g : _ -> _ =
  fun () -> let x = f () in x
[%%expect{|
val g : unit -> int = <fun>
|}]

let f : _ -> local_ _ =
  fun () -> local_ "hello"
[%%expect{|
val f : unit -> string@local = <fun>
|}]

let g : _ -> _ =
  fun () -> let x = f () in x
[%%expect{|
Line 2, characters 28-29:
2 |   fun () -> let x = f () in x
                                ^
Error: This value escapes its region
|}]

(* constructor argument crosses modes upon pattern matching *)
let f : local_ bar -> _ =
  fun b ->
    match b with
    | Bar0 (x, _) -> x
[%%expect{|
val f : bar@local -> int = <fun>
|}]

(* This example is identical to the last one,
   except the type annotation. *)
(* This example works because function body
   crosses modes based on its expected type *)
let f : local_ _ -> int =
  fun b ->
    match b with
    | Bar0 (x, _) -> x
[%%expect{|
val f : (int, 'a) bar0@local -> int = <fun>
|}]

let f : local_ bar -> _ =
  fun b ->
    match b with
    | Bar0 (_, y) -> y
[%%expect{|
Line 4, characters 21-22:
4 |     | Bar0 (_, y) -> y
                         ^
Error: This value escapes its region
|}]

(* record fields crosses modes upon pattern matching *)
let f : local_ foo -> _ =
  fun r ->
  match r with
  | {x; _} -> x
[%%expect{|
val f : foo@local -> int = <fun>
|}]

(* this example works again because function body crosses modes
   based on its expected type *)
let f : local_ _ -> int =
  fun r ->
  match r with
  | {x; _} -> x
[%%expect{|
val f : (int, 'a) foo0@local -> int = <fun>
|}]

let f : local_ foo -> _ =
  fun r ->
    match r with
    | {y; _} -> y
[%%expect{|
Line 4, characters 16-17:
4 |     | {y; _} -> y
                    ^
Error: This value escapes its region
|}]

(* constraint crosses modes upon pattern matching  *)
let f : local_ _ -> _ =
  fun (x : int) -> x
[%%expect{|
val f : int@local -> int = <fun>
|}]

let f : local_ _ -> _ =
  fun (x : string) -> x
[%%expect{|
Line 2, characters 22-23:
2 |   fun (x : string) -> x
                          ^
Error: This value escapes its region
|}]


(* Following tests immediacy detection,
   given by goldfirere *)
module M : sig
  type t [@@immediate]
end = struct
  type t = int
end

type t2 = { x : int } [@@unboxed]

let f : local_ _ -> M.t =
  fun x -> x

let f : local_ _ -> t2 =
  fun x -> x
[%%expect{|
module M : sig type t : immediate end
type t2 = { x : int; } [@@unboxed]
val f : M.t@local -> M.t = <fun>
val f : t2@local -> t2 = <fun>
|}]

(* This test needs the snapshotting in [is_immediate] to prevent a type error
   from the use of the gadt equation in the inner scope. *)
type _ t_gadt = Int : int t_gadt
type 'a t_rec = { fld : 'a }

let f (type a) (x : a t_gadt) (y : a) =
  match x with
    Int -> { fld = y }.fld
[%%expect{|
type _ t_gadt = Int : int t_gadt
type 'a t_rec = { fld : 'a; }
val f : 'a t_gadt -> 'a -> 'a = <fun>
|}]

(* Mode crossing in coercing arrow types *)
let foo : int -> int = fun x -> x
[%%expect{|
val foo : int -> int = <fun>
|}]

let foo' : int -> local_ int = fun x -> local_ x
[%%expect{|
val foo' : int -> int@local = <fun>
|}]



let bar (f : local_ int -> int) = f 42
[%%expect{|
val bar : (int@local -> int) -> int = <fun>
|}]

(* Implicit mode crossing is not good enough *)
let _ = bar foo
[%%expect{|
Line 1, characters 12-15:
1 | let _ = bar foo
                ^^^
Error: This expression has type int -> int
       but an expression was expected of type int@local -> int
|}]

let _ = bar (foo :> local_ int -> int)
[%%expect{|
- : int = 42
|}]

let _ = bar (foo : int -> int :> local_ int -> int)
[%%expect{|
- : int = 42
|}]

(* Only the RHS type of :> is looked at for mode crossing *)
let _ = bar (foo : int -> int :> local_ _ -> _)
[%%expect{|
Line 1, characters 12-47:
1 | let _ = bar (foo : int -> int :> local_ _ -> _)
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type int -> int is not a subtype of 'a@local -> 'b
|}]


(* An example: the RHS allows mode crossing but the LHS doesn't *)
let foo = function
  | `A -> ()
  | `B (s : string) -> ()
[%%expect{|
val foo : [< `A | `B of string ] -> unit = <fun>
|}]

let foo_ = (foo : [`A | `B of string] -> unit :> local_ [`A] -> unit)
[%%expect{|
val foo_ : [ `A ]@local -> unit = <fun>
|}]

let foo_ = (foo : [`A | `B of string] -> unit :> local_ [`B of string] -> unit)
[%%expect{|
Line 1, characters 11-79:
1 | let foo_ = (foo : [`A | `B of string] -> unit :> local_ [`B of string] -> unit)
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type [ `A | `B of string ] -> unit is not a subtype of
         [ `B of string ]@local -> unit
|}]

(* You can't erase the info that a function might allocate in parent region *)
let _ = bar (foo' :> local_ int -> int)
[%%expect{|
Line 1, characters 12-39:
1 | let _ = bar (foo' :> local_ int -> int)
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type int -> int@local is not a subtype of int@local -> int
|}]

(* Mode crossing at identifiers - in the following, x and y are added to the
environment at mode local, but they cross to global when they are refered to
again. Note that ref is polymorphic and thus doesn't trigger crossing. *)
let foo () =
  let x, y = local_ (42, 24) in
  let _ = ref x, ref y in
  ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]