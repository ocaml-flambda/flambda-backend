(* TEST
   * expect
*)

let leak n =
  let[@stack] r = ref n in
  r
[%%expect{|
Line 3, characters 2-3:
3 |   r
      ^
Error: The value r is local, so cannot be used here as it might escape
|}]

external idint : (int[@stack]) -> int = "%identity"
[%%expect{|
external idint : (int [@stack]) -> int = "%identity"
|}]

let noleak n =
  let[@stack] r = ref n in
  idint (r.contents)
[%%expect{|
val noleak : int -> int = <fun>
|}]  


(* This will be typeable eventually, once label mutability affects mode *)
let (!) : ('a ref[@stack]) -> 'a =
  fun r -> r.contents
[%%expect{|
val ( ! ) : ('a ref [@stack]) -> 'a = <fun>
|}]



(*
 * Type equalities of function types
 *)

module Equ = struct
  type ('a, 'b) fn = 'a -> 'b
  type ('a, 'b) fn_sarg = ('a[@stack]) -> 'b
  type ('a, 'b) fn_sret = 'a -> ('b[@stackret])
  type ('a, 'b) fn_sargret = ('a[@stack]) -> ('b[@stackret])

  (* When a [@stack] argument appears in a function type with multiple arguments,
     return modes are implicitly [@stackret] until the final argument. *)
  type ('a, 'b, 'c, 'd, 'e) fn5 =
    'a -> ('b[@stack]) -> 'c -> 'd -> 'e

  type ('a,'b,'c,'d,'e) equ_fn5 = unit
    constraint
      ('a, 'b, 'c, 'd, 'e) fn5
      =
      ('a, ('b, ('c, ('d, 'e) fn) fn_sret) fn_sargret) fn
end
[%%expect{|
module Equ :
  sig
    type ('a, 'b) fn = 'a -> 'b
    type ('a, 'b) fn_sarg = ('a [@stack]) -> 'b
    type ('a, 'b) fn_sret = 'a -> ('b [@stackret])
    type ('a, 'b) fn_sargret = ('a [@stack]) -> ('b [@stackret])
    type ('a, 'b, 'c, 'd, 'e) fn5 =
        'a -> ('b [@stack]) -> ('c -> ('d -> 'e [@stackret]) [@stackret])
    type ('a, 'b, 'c, 'd, 'e) equ_fn5 = unit
  end
|}]

type distinct_sarg = unit constraint (int,int) Equ.fn_sarg = int -> int
[%%expect{|
Line 1, characters 37-71:
1 | type distinct_sarg = unit constraint (int,int) Equ.fn_sarg = int -> int
                                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type constraints are not consistent.
Type (int, int) Equ.fn_sarg = (int [@stack]) -> int
is not compatible with type int -> int
|}]
type distinct_sret = unit constraint (int,int) Equ.fn_sret = int -> int
[%%expect{|
Line 1, characters 37-71:
1 | type distinct_sret = unit constraint (int,int) Equ.fn_sret = int -> int
                                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type constraints are not consistent.
Type (int, int) Equ.fn_sret = int -> (int [@stackret])
is not compatible with type int -> int
|}]
type distinct_sarg_sret = unit constraint (int,int) Equ.fn_sarg = (int, int) Equ.fn_sargret
[%%expect{|
Line 1, characters 42-91:
1 | type distinct_sarg_sret = unit constraint (int,int) Equ.fn_sarg = (int, int) Equ.fn_sargret
                                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type constraints are not consistent.
Type (int, int) Equ.fn_sarg = (int [@stack]) -> int
is not compatible with type
  (int, int) Equ.fn_sargret = (int [@stack]) -> (int [@stackret])
|}]



(*
 * Curried functions and partial application
 *)

(* f4 results in a local value if it is partially applied to two or
   three arguments, because it closes over the locally-allocated
   second argument. Applications to 1 or 4 arguments are not local. *)
(* FIXME: the printed types, while correct, are unnecessarily ugly *)
let f4 : int -> ('a[@stack]) -> int -> int -> int =
  fun a _ b c -> a + b + c
[%%expect{|
val f4 :
  int -> ('a [@stack]) -> (int -> (int -> int [@stackret]) [@stackret]) =
  <fun>
|}]

let apply1 x = f4 x
[%%expect{|
val apply1 :
  int -> ('a [@stack]) -> (int -> (int -> int [@stackret]) [@stackret]) =
  <fun>
|}]
let apply2 x = f4 x x
[%%expect{|
Line 1, characters 15-21:
1 | let apply2 x = f4 x x
                   ^^^^^^
Error: This locally-allocated return value escapes
|}]
let apply3 x = f4 x x x
[%%expect{|
Line 1, characters 15-23:
1 | let apply3 x = f4 x x x
                   ^^^^^^^^
Error: This locally-allocated return value escapes
|}]
let apply4 x =
  f4 x x x x
[%%expect{|
val apply4 : int -> int = <fun>
|}]

(* Partial applications of two or three arguments are OK if bound locally *)
let apply2_stack x =
  let[@stack] g = f4 x x in
  g x x
let apply3_stack x =
  let[@stack] g = f4 x x x in
  g x
[%%expect{|
val apply2_stack : int -> int = <fun>
val apply3_stack : int -> int = <fun>
|}]

(*
 * Overapplication (functions that return functions)
 *)

let f : ('a[@stack]) -> int -> int = fun _ x -> x
let g : ('a[@stack]) -> int -> _ = fun _ x -> f
[%%expect{|
val f : ('a [@stack]) -> (int -> int [@stackret]) = <fun>
val g :
  ('a [@stack]) ->
  (int -> ('b [@stack]) -> (int -> int [@stackret]) [@stackret]) = <fun>
|}]
let apply1 x = g x
[%%expect{|
Line 1, characters 15-18:
1 | let apply1 x = g x
                   ^^^
Error: This locally-allocated return value escapes
|}]
let apply2 x = g x x
[%%expect{|
val apply2 : int -> ('a [@stack]) -> (int -> int [@stackret]) = <fun>
|}]
let apply3 x = g x x x
[%%expect{|
Line 1, characters 15-22:
1 | let apply3 x = g x x x
                   ^^^^^^^
Error: This locally-allocated return value escapes
|}]
let apply4 x = g x x x x
[%%expect{|
val apply4 : int -> int = <fun>
|}]

(*
 * Labels and reordering
 *)

let app1 (f : a:int -> b:((int ref)[@stack]) -> unit -> unit) = f ~b:(ref 42) ()
[%%expect{|
Line 1, characters 69-77:
1 | let app1 (f : a:int -> b:((int ref)[@stack]) -> unit -> unit) = f ~b:(ref 42) ()
                                                                         ^^^^^^^^
Error: This locally-allocated argument is captured by a partial application
|}]
let app2 (f : a:int -> b:((int ref)[@stack]) -> unit -> unit) = f ~b:(ref 42)
[%%expect{|
Line 1, characters 69-77:
1 | let app2 (f : a:int -> b:((int ref)[@stack]) -> unit -> unit) = f ~b:(ref 42)
                                                                         ^^^^^^^^
Error: This locally-allocated argument is captured by a partial application
|}]
let app3 (f : a:int -> b:((int ref)[@stack]) -> unit) = f ~b:(ref 42)
[%%expect{|
Line 1, characters 61-69:
1 | let app3 (f : a:int -> b:((int ref)[@stack]) -> unit) = f ~b:(ref 42)
                                                                 ^^^^^^^^
Error: This locally-allocated argument is captured by a partial application
|}]
let app4 (f : b:((int ref)[@stack]) -> a:int -> unit) = f ~b:(ref 42)
[%%expect{|
Line 1, characters 56-69:
1 | let app4 (f : b:((int ref)[@stack]) -> a:int -> unit) = f ~b:(ref 42)
                                                            ^^^^^^^^^^^^^
Error: This locally-allocated return value escapes
|}]
let app42 (f : a:((int ref)[@stack]) -> (int, b:(int ref[@stack]) -> c:int -> unit) Equ.fn) =
  f ~a:(ref 1) 2 ~c:4
[%%expect{|
val app42 :
  (a:(int ref [@stack]) ->
   (int, b:(int ref [@stack]) -> (c:int -> unit [@stackret])) Equ.fn) ->
  b:(int ref [@stack]) -> unit = <fun>
|}]
let app43 (f : a:((int ref)[@stack]) -> (int, b:(int ref[@stack]) -> c:int -> unit) Equ.fn) =
  f ~a:(ref 1) 2
[%%expect{|
val app43 :
  (a:(int ref [@stack]) ->
   (int, b:(int ref [@stack]) -> (c:int -> unit [@stackret])) Equ.fn) ->
  b:(int ref [@stack]) -> (c:int -> unit [@stackret]) = <fun>
|}]
let app5 (f : b:((int ref)[@stack]) -> a:int -> unit) = f ~a:42
[%%expect{|
val app5 :
  (b:(int ref [@stack]) -> (a:int -> unit [@stackret])) ->
  b:(int ref [@stack]) -> unit = <fun>
|}]
let app6 (f : a:((int ref)[@stack]) -> b:((int ref)[@stack]) -> c:int -> unit) = f ~c:42
[%%expect{|
val app6 :
  (a:(int ref [@stack]) ->
   (b:(int ref [@stack]) -> (c:int -> unit [@stackret]) [@stackret])) ->
  a:(int ref [@stack]) -> (b:(int ref [@stack]) -> unit [@stackret]) = <fun>
|}]


let rapp1 (f : a:int -> unit -> (int ref [@stackret])) = f ()
[%%expect{|
val rapp1 :
  (a:int -> unit -> (int ref [@stackret])) -> a:int -> (int ref [@stackret]) =
  <fun>
|}]
let rapp2 (f : a:int -> unit -> (int ref [@stackret])) = f ~a:1
[%%expect{|
val rapp2 :
  (a:int -> unit -> (int ref [@stackret])) -> unit -> (int ref [@stackret]) =
  <fun>
|}]
let rapp3 (f : a:int -> unit -> (int ref [@stackret])) = f ~a:1 ()
[%%expect{|
Line 1, characters 57-66:
1 | let rapp3 (f : a:int -> unit -> (int ref [@stackret])) = f ~a:1 ()
                                                             ^^^^^^^^^
Error: This locally-allocated return value escapes
|}]

let bug1 () =
  let foo : a:(string[@stack]) -> b:(string[@stack]) -> c:int -> unit = fun ~a ~b ~c -> () in
  let[@stack] bar = foo ~b:"hello" in
  bar ~a:"world"
[%%expect{|
Line 4, characters 2-16:
4 |   bar ~a:"world"
      ^^^^^^^^^^^^^^
Error: This locally-allocated return value escapes
|}]
type ('a, 'b) b_sfn = b:('a[@stack]) -> 'b
let bug2 () =
  let foo : a:(string[@stack]) -> (string, c:int -> unit) b_sfn = fun ~a ~b -> fun ~c -> () in
  let[@stack] bar = foo ~b:"hello" in
  bar ~a:"world"
[%%expect{|
type ('a, 'b) b_sfn = b:('a [@stack]) -> 'b
val bug2 : unit -> c:int -> unit = <fun>
|}]
let bug3 () =
  let foo : a:(string[@stack]) -> (string, c:int -> unit) b_sfn = fun ~a ~b -> fun ~c -> print_string a in
  let[@stack] bar = foo ~b:"hello" in
  bar ~a:"world"
[%%expect{|
Line 2, characters 102-103:
2 |   let foo : a:(string[@stack]) -> (string, c:int -> unit) b_sfn = fun ~a ~b -> fun ~c -> print_string a in
                                                                                                          ^
Error: The value a is local, so cannot be used here as it might escape
|}]


(*
 * Optional arguments
 *)
let appopt1 (f : ?a:((int ref)[@stack]) -> unit -> unit) =
  f ~a:(let[@stack] x = ref 42 in x) ()
[%%expect{|
val appopt1 : (?a:(int ref [@stack]) -> (unit -> unit [@stackret])) -> unit =
  <fun>
|}]
let appopt2 (f : ?a:((int ref)[@stack]) -> unit -> unit) =
  f ~a:(let[@stack] x = ref 42 in x)
[%%expect{|
Line 2, characters 2-36:
2 |   f ~a:(let[@stack] x = ref 42 in x)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This locally-allocated return value escapes
|}]

(* In principle. it would be sound to allow this one:
   we close over a value in Alloc_local mode, but it is known to be immediate *)
let appopt3 (f : ?a:((int ref)[@stack]) -> int -> int -> unit) =
  f 42
[%%expect{|
Line 2, characters 2-6:
2 |   f 42
      ^^^^
Error: This locally-allocated return value escapes
|}]

type yfn = y:unit -> unit -> int
let optret1 (f : ?x:int -> (yfn[@stackret])) = f ()
[%%expect{|
type yfn = y:unit -> unit -> int
Line 2, characters 47-51:
2 | let optret1 (f : ?x:int -> (yfn[@stackret])) = f ()
                                                   ^^^^
Error: This locally-allocated return value escapes
|}]


(*
 * Closures and context locks
 *)

let heap_closure () =
  let[@stack] foo = ref 1 in
  let fn () =
    let[@stack] fn2 () =
      let[@stack] _baz = foo in
      () in
    fn2 () in
  let _force_heap = ref fn in
  fn ()

[%%expect{|
Line 8, characters 24-26:
8 |   let _force_heap = ref fn in
                            ^^
Error: The value fn is local, so cannot be used here as it might escape
|}]

let local_closure () =
  let[@stack] foo = ref 1 in
  let[@stack] fn () =
    let[@stack] fn2 () =
      let[@stack] _baz = foo in
      () in
    fn2 () in
  fn ()

[%%expect{|
val local_closure : unit -> unit = <fun>
|}]

(*
 * Always-nonlocal things
 *)
let[@stack] toplevel_stack = {contents=42}
[%%expect{|
Line 1, characters 12-26:
1 | let[@stack] toplevel_stack = {contents=42}
                ^^^^^^^^^^^^^^
Error: This locally-allocated value escapes
|}]

module type T = sig val x : int option end
let first_class_module () =
  let[@stack] thing = Some 1 in
  let[@stack] _m : (module T) = (module struct let x = thing end) in
  ()
[%%expect{|
module type T = sig val x : int option end
Line 4, characters 55-60:
4 |   let[@stack] _m : (module T) = (module struct let x = thing end) in
                                                           ^^^^^
Error: The value thing is local, so cannot be used here as it might escape
|}]
let local_module () =
  let[@stack] thing = Some 1 in
  let[@stack] _ =
    let module M = struct let x = thing end in
    ()
  in ()
[%%expect{|
Line 4, characters 34-39:
4 |     let module M = struct let x = thing end in
                                      ^^^^^
Error: The value thing is local, so cannot be used here as it might escape
|}]
let obj () =
  let[@stack] thing = Some 1 in
  let _obj = object method foo = thing end in
  ()
[%%expect{|
Line 3, characters 33-38:
3 |   let _obj = object method foo = thing end in
                                     ^^^^^
Error: The value thing is local, so cannot be used here as it might escape
|}]


(*
 * Higher order functions, with arguments that promise not to leak
 *)

let use_locally (f : ('a[@stack]) -> 'a) (x : 'a) = f x
(* This version also promises not to leak the closure *)
let use_locally' : (('a [@stack]) -> 'a [@stack]) -> 'a -> 'a = fun f x -> f x
[%%expect{|
val use_locally : (('a [@stack]) -> 'a) -> 'a -> 'a = <fun>
val use_locally' : (('a [@stack]) -> 'a [@stack]) -> ('a -> 'a [@stackret]) =
  <fun>
|}]

let no_leak = use_locally (fun x -> 1) 42
let no_leak' = use_locally' (fun x -> 1) 42
[%%expect{|
val no_leak : int = 1
val no_leak' : int = 1
|}]

let leak_id =
  use_locally (fun x -> x) 42
[%%expect{|
Line 2, characters 24-25:
2 |   use_locally (fun x -> x) 42
                            ^
Error: The value x is local, so cannot be used here as it might escape
|}]

let leak_ref =
  let r = ref None in
  use_locally (fun x -> r.contents <- Some x; x) 42

[%%expect{|
Line 3, characters 43-44:
3 |   use_locally (fun x -> r.contents <- Some x; x) 42
                                               ^
Error: The value x is local, so cannot be used here as it might escape
|}]

let leak_ref_2 =
  let[@stack] r = ref None in
  use_locally (fun x -> let[@stack] _ = r in r.contents <- Some x; x) 42
[%%expect{|
Line 3, characters 40-41:
3 |   use_locally (fun x -> let[@stack] _ = r in r.contents <- Some x; x) 42
                                            ^
Error: The value r is local, so cannot be used inside a closure that might escape
|}]

let leak_ref_3 =
  let[@stack] r = ref None in
  use_locally' (fun x -> let[@stack] _ = r in r.contents <- Some x; x) 42
[%%expect{|
Line 3, characters 65-66:
3 |   use_locally' (fun x -> let[@stack] _ = r in r.contents <- Some x; x) 42
                                                                     ^
Error: The value x is local, so cannot be used here as it might escape
|}]



let no_leak_exn =
  use_locally (fun x -> let[@stack] _exn = Invalid_argument x in "bluh") "blah"
[%%expect{|
val no_leak_exn : string = "bluh"
|}]
let do_leak_exn =
  use_locally (fun x -> let[@stack] _exn = raise (Invalid_argument x) in "bluh") "blah"

[%%expect{|
Line 2, characters 67-68:
2 |   use_locally (fun x -> let[@stack] _exn = raise (Invalid_argument x) in "bluh") "blah"
                                                                       ^
Error: The value x is local, so cannot be used here as it might escape
|}]

(* same, but this time the function is allowed to return its argument *)
let use_locally (f : ('a[@stack]) -> ('a[@stackret])) : ('a[@stack]) -> ('a[@stackret]) = f
[%%expect{|
val use_locally :
  (('a [@stack]) -> ('a [@stackret])) -> ('a [@stack]) -> ('a [@stackret]) =
  <fun>
|}]

let loc = ((fun x -> x) : (int[@stack]) -> (int[@stackret]))

let no_leak_id = let[@stack] _ = use_locally ((fun x -> x) : (int[@stack]) -> (int[@stackret])) 42 in ()

[%%expect{|
Line 1, characters 11-23:
1 | let loc = ((fun x -> x) : (int[@stack]) -> (int[@stackret]))
               ^^^^^^^^^^^^
Error: This locally-allocated return value escapes
|}]
