(* TEST
   * expect *)

let leak n =
  let r = local_ ref n in
  r
[%%expect{|
Line 3, characters 2-3:
3 |   r
      ^
Error: This local value escapes its region
  Hint: Cannot return local value without an explicit "local_" annotation
|}]

external idint : local_ int -> int = "%identity"
[%%expect{|
external idint : local_ int -> int = "%identity"
|}]

let noleak n =
  let r = local_ ref n in
  idint (r.contents)
[%%expect{|
val noleak : int -> int = <fun>
|}]


let (!) = fun (local_ r) -> r.contents
[%%expect{|
val ( ! ) : local_ 'a ref -> 'a = <fun>
|}]

(* Local lets *)

let leak n =
  let local_ r = ref n in
  r
[%%expect{|
Line 3, characters 2-3:
3 |   r
      ^
Error: This local value escapes its region
  Hint: Cannot return local value without an explicit "local_" annotation
|}]

let leak n =
  let local_ r : int ref = ref n in
  r
[%%expect{|
Line 3, characters 2-3:
3 |   r
      ^
Error: This local value escapes its region
  Hint: Cannot return local value without an explicit "local_" annotation
|}]

let leak n =
  let local_ f : 'a. 'a -> 'a = fun x -> x in
  f
[%%expect{|
Line 3, characters 2-3:
3 |   f
      ^
Error: This local value escapes its region
  Hint: Cannot return local value without an explicit "local_" annotation
|}]

let leak n =
  let local_ f x : int = x in
  f
[%%expect{|
Line 3, characters 2-3:
3 |   f
      ^
Error: This local value escapes its region
  Hint: Cannot return local value without an explicit "local_" annotation
|}]

(* If both type and mode are wrong, complain about type *)
let f () =
  let local_ r = ref 42 in
  print_endline r
[%%expect{|
Line 3, characters 16-17:
3 |   print_endline r
                    ^
Error: This expression has type int ref
       but an expression was expected of type string
|}]

(*
 * Type equalities of function types
 *)

  (* When a [local_] argument appears in a function type with multiple arguments,
     return modes are implicitly stack until the final argument. *)
type equ_fn = unit
  constraint
    'a -> local_ 'b -> 'c -> 'd -> 'e
    = 'a -> local_ 'b -> local_ ('c -> local_ ('d -> 'e))
[%%expect{|
type equ_fn = unit
|}]

type distinct_sarg = unit constraint local_ int -> int = int -> int
[%%expect{|
Line 1, characters 37-67:
1 | type distinct_sarg = unit constraint local_ int -> int = int -> int
                                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type constraints are not consistent.
       Type local_ int -> int is not compatible with type int -> int
|}]
type distinct_sret = unit constraint int -> local_ int = int -> int
[%%expect{|
Line 1, characters 37-67:
1 | type distinct_sret = unit constraint int -> local_ int = int -> int
                                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type constraints are not consistent.
       Type int -> local_ int is not compatible with type int -> int
|}]
type distinct_sarg_sret = unit constraint local_ int -> int = local_ int -> local_ int
[%%expect{|
Line 1, characters 42-86:
1 | type distinct_sarg_sret = unit constraint local_ int -> int = local_ int -> local_ int
                                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type constraints are not consistent.
       Type local_ int -> int is not compatible with type
         local_ int -> local_ int
|}]

type local_higher_order = unit constraint
  local_ (int -> int -> int) -> int = local_ (int -> local_ (int -> int)) -> int
[%%expect{|
type local_higher_order = unit
|}]

type nonlocal_higher_order = unit constraint
  (int -> int -> int) -> int = (int -> local_ (int -> int)) -> int
[%%expect{|
Line 2, characters 2-66:
2 |   (int -> int -> int) -> int = (int -> local_ (int -> int)) -> int
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type constraints are not consistent.
       Type (int -> int -> int) -> int is not compatible with type
         (int -> local_ (int -> int)) -> int
       Type int -> int -> int is not compatible with type
         int -> local_ (int -> int)
|}]

type local_higher_order = unit constraint
  int -> local_ (int -> int -> int) = int -> local_ (int -> local_ (int -> int))
[%%expect{|
type local_higher_order = unit
|}]

type nonlocal_higher_order = unit constraint
  int -> (int -> int -> int) = int -> (int -> local_ (int -> int))
[%%expect{|
Line 2, characters 2-66:
2 |   int -> (int -> int -> int) = int -> (int -> local_ (int -> int))
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type constraints are not consistent.
       Type int -> int -> int -> int is not compatible with type
         int -> int -> local_ (int -> int)
       Type int -> int -> int is not compatible with type
         int -> local_ (int -> int)
|}]

let foo () =
  let local_ _bar : int -> int -> int =
    ((fun y z -> z) : int -> local_ (int -> int)) in
  ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

let foo () =
  let _bar : int -> int -> int =
    ((fun y z -> z) : int -> local_ (int -> int)) in
  ()
[%%expect{|
Line 3, characters 4-49:
3 |     ((fun y z -> z) : int -> local_ (int -> int)) in
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type int -> local_ (int -> int)
       but an expression was expected of type int -> int -> int
|}]

let foo () =
  let local_ _bar : 'a. 'a -> 'a -> 'a =
    ((fun y z -> z) : _ -> local_ (_ -> _)) in
  ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

let foo () =
  let _bar : 'a. 'a -> 'a -> 'a =
    ((fun y z -> z) : _ -> local_ (_ -> _)) in
  ()
[%%expect{|
Line 3, characters 4-43:
3 |     ((fun y z -> z) : _ -> local_ (_ -> _)) in
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type 'b -> local_ ('c -> 'c)
       but an expression was expected of type 'a -> 'a -> 'a
|}]

let foo () =
  let local_ _bar x : int -> int -> int =
    ((fun y z -> z) : int -> local_ (int -> int)) in
  ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

let foo () =
  let _bar x : int -> int -> int =
    ((fun y z -> z) : int -> local_ (int -> int)) in
  ()
[%%expect{|
Line 3, characters 4-49:
3 |     ((fun y z -> z) : int -> local_ (int -> int)) in
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type int -> local_ (int -> int)
       but an expression was expected of type int -> int -> int
|}]

let foo (local_ bar : int -> int -> int) =
  let _ = (bar : int -> local_ (int -> int)) in
  ()
[%%expect{|
val foo : local_ (int -> int -> int) -> unit = <fun>
|}]

let foo (bar : int -> local_ (int -> int)) =
  let _ = (bar : int -> int -> int) in
  ()
[%%expect{|
Line 2, characters 11-14:
2 |   let _ = (bar : int -> int -> int) in
               ^^^
Error: This expression has type int -> local_ (int -> int)
       but an expression was expected of type int -> int -> int
|}]


(*
 * Curried functions and partial application
 *)

(* f4 results in a local value if it is partially applied to two or
   three arguments, because it closes over the locally-allocated
   second argument. Applications to 1 or 4 arguments are not local. *)
let f4 : int -> local_ 'a -> int -> int -> int =
  fun a _ b c -> a + b + c
[%%expect{|
val f4 : int -> local_ 'a -> int -> int -> int = <fun>
|}]

let apply1 x = f4 x
[%%expect{|
val apply1 : int -> local_ 'a -> int -> int -> int = <fun>
|}]
let apply2 x = f4 x x
[%%expect{|
Line 1, characters 15-21:
1 | let apply2 x = f4 x x
                   ^^^^^^
Error: This local value escapes its region
  Hint: Cannot return local value without an explicit "local_" annotation
|}]
let apply3 x = f4 x x x
[%%expect{|
Line 1, characters 15-23:
1 | let apply3 x = f4 x x x
                   ^^^^^^^^
Error: This local value escapes its region
  Hint: Cannot return local value without an explicit "local_" annotation
|}]
let apply4 x =
  f4 x x x x
[%%expect{|
val apply4 : int -> int = <fun>
|}]

(* Partial applications of two or three arguments are OK if bound locally *)
let apply2_stack x =
  let g = f4 x x in
  let res = g x x in
  res
let apply3_stack x =
  let g = f4 x x x in
  let res = g x in
  res
[%%expect{|
val apply2_stack : int -> int = <fun>
val apply3_stack : int -> int = <fun>
|}]

(*
 * Overapplication (functions that return functions)
 *)

let g : local_ 'a -> int -> _ = fun _ _ -> (fun[@curry] (local_ _) (x : int) -> x)
[%%expect{|
val g : local_ 'a -> int -> (local_ 'b -> int -> int) = <fun>
|}]
let apply1 x = g x
[%%expect{|
Line 1, characters 15-18:
1 | let apply1 x = g x
                   ^^^
Error: This local value escapes its region
  Hint: Cannot return local value without an explicit "local_" annotation
|}]
let apply2 x = g x x
[%%expect{|
val apply2 : int -> local_ 'a -> int -> int = <fun>
|}]
let apply3 x = g x x x
[%%expect{|
Line 1, characters 15-22:
1 | let apply3 x = g x x x
                   ^^^^^^^
Error: This local value escapes its region
  Hint: Cannot return local value without an explicit "local_" annotation
|}]
let apply4 x = g x x x x
[%%expect{|
val apply4 : int -> int = <fun>
|}]

(*
 * Labels and reordering
 *)

let app1 (f : a:int -> b:local_ int ref -> unit -> unit) = f ~b:(local_ ref 42) ()
[%%expect{|
Line 1, characters 64-79:
1 | let app1 (f : a:int -> b:local_ int ref -> unit -> unit) = f ~b:(local_ ref 42) ()
                                                                    ^^^^^^^^^^^^^^^
Error: This value escapes its region
  Hint: It is captured by a partial application
|}]
let app2 (f : a:int -> b:local_ int ref -> unit -> unit) = f ~b:(local_ ref 42)
[%%expect{|
Line 1, characters 64-79:
1 | let app2 (f : a:int -> b:local_ int ref -> unit -> unit) = f ~b:(local_ ref 42)
                                                                    ^^^^^^^^^^^^^^^
Error: This value escapes its region
  Hint: It is captured by a partial application
|}]
let app3 (f : a:int -> b:local_ int ref -> unit) = f ~b:(local_ ref 42)
[%%expect{|
Line 1, characters 56-71:
1 | let app3 (f : a:int -> b:local_ int ref -> unit) = f ~b:(local_ ref 42)
                                                            ^^^^^^^^^^^^^^^
Error: This value escapes its region
  Hint: It is captured by a partial application
|}]
let app4 (f : b:local_ int ref -> a:int -> unit) = f ~b:(local_ ref 42)
[%%expect{|
Line 1, characters 56-71:
1 | let app4 (f : b:local_ int ref -> a:int -> unit) = f ~b:(local_ ref 42)
                                                            ^^^^^^^^^^^^^^^
Error: This local value escapes its region
  Hint: This argument cannot be local, because this is a tail call
|}]
let app42 (f : a:local_ int ref -> (int -> b:local_ int ref -> c:int -> unit)) =
  f ~a:(local_ ref 1) 2 ~c:4
[%%expect{|
val app42 :
  (a:local_ int ref -> (int -> b:local_ int ref -> c:int -> unit)) ->
  b:local_ int ref -> unit = <fun>
|}]
let app43 (f : a:local_ int ref -> (int -> b:local_ int ref -> c:int -> unit)) =
  f ~a:(local_ ref 1) 2
[%%expect{|
Line 2, characters 7-21:
2 |   f ~a:(local_ ref 1) 2
           ^^^^^^^^^^^^^^
Error: This local value escapes its region
  Hint: This argument cannot be local, because this is a tail call
|}]
let app5 (f : b:local_ int ref -> a:int -> unit) = f ~a:42
[%%expect{|
val app5 : (b:local_ int ref -> a:int -> unit) -> b:local_ int ref -> unit =
  <fun>
|}]
let app6 (f : a:local_ int ref -> b:local_ int ref -> c:int -> unit) = f ~c:42
[%%expect{|
val app6 :
  (a:local_ int ref -> b:local_ int ref -> c:int -> unit) ->
  a:local_ int ref -> b:local_ int ref -> unit = <fun>
|}]

let app1' (f : a:int -> b:local_ int ref -> unit -> unit) = f ~b:(ref 42) ()
[%%expect{|
val app1' : (a:int -> b:local_ int ref -> unit -> unit) -> a:int -> unit =
  <fun>
|}]
let app2' (f : a:int -> b:local_ int ref -> unit -> unit) = f ~b:(ref 42)
[%%expect{|
val app2' :
  (a:int -> b:local_ int ref -> unit -> unit) ->
  a:int -> local_ (unit -> unit) = <fun>
|}]
let app3' (f : a:int -> b:local_ int ref -> unit) = f ~b:(ref 42)
[%%expect{|
val app3' : (a:int -> b:local_ int ref -> unit) -> a:int -> unit = <fun>
|}]
let app4' (f : b:local_ int ref -> a:int -> unit) = f ~b:(ref 42)
[%%expect{|
Line 1, characters 52-65:
1 | let app4' (f : b:local_ int ref -> a:int -> unit) = f ~b:(ref 42)
                                                        ^^^^^^^^^^^^^
Error: This local value escapes its region
  Hint: Cannot return local value without an explicit "local_" annotation
|}]
let app42' (f : a:local_ int ref -> (int -> b:local_ int ref -> c:int -> unit)) =
  f ~a:(ref 1) 2 ~c:4
[%%expect{|
val app42' :
  (a:local_ int ref -> (int -> b:local_ int ref -> c:int -> unit)) ->
  b:local_ int ref -> unit = <fun>
|}]
let app43' (f : a:local_ int ref -> (int -> b:local_ int ref -> c:int -> unit)) =
  f ~a:(ref 1) 2
[%%expect{|
val app43' :
  (a:local_ int ref -> (int -> b:local_ int ref -> c:int -> unit)) ->
  b:local_ int ref -> c:int -> unit = <fun>
|}]

let rapp1 (f : a:int -> unit -> local_ int ref) = f ()
[%%expect{|
val rapp1 : (a:int -> unit -> local_ int ref) -> a:int -> local_ int ref =
  <fun>
|}]
let rapp2 (f : a:int -> unit -> local_ int ref) = f ~a:1
[%%expect{|
val rapp2 : (a:int -> unit -> local_ int ref) -> unit -> local_ int ref =
  <fun>
|}]
let rapp3 (f : a:int -> unit -> local_ int ref) = f ~a:1 ()
[%%expect{|
Line 1, characters 50-59:
1 | let rapp3 (f : a:int -> unit -> local_ int ref) = f ~a:1 ()
                                                      ^^^^^^^^^
Error: This local value escapes its region
  Hint: Cannot return local value without an explicit "local_" annotation
|}]

let bug1 () =
  let foo : a:local_ string -> b:local_ string -> c:int -> unit =
    fun ~a ~b ~c -> ()
  in
  let bar = local_ foo ~b:"hello" in
  let res = bar ~a:"world" in
  res
[%%expect{|
Line 7, characters 2-5:
7 |   res
      ^^^
Error: This local value escapes its region
  Hint: Cannot return local value without an explicit "local_" annotation
|}]
let bug2 () =
  let foo : a:local_ string -> (b:local_ string -> (c:int -> unit)) =
    fun ~a -> fun[@curry] ~b -> fun[@curry] ~c -> ()
  in
  let bar = local_ foo ~b:"hello" in
  let res = bar ~a:"world" in
  res
[%%expect{|
val bug2 : unit -> c:int -> unit = <fun>
|}]
let bug3 () =
  let foo : a:local_ string -> (b:local_ string -> (c:int -> unit)) =
    fun ~a -> fun[@curry] ~b -> fun[@curry] ~c -> print_string a
  in
  let[@stack] bar = foo ~b:"hello" in
  let res = bar ~a:"world" in
  res
[%%expect{|
Line 3, characters 63-64:
3 |     fun ~a -> fun[@curry] ~b -> fun[@curry] ~c -> print_string a
                                                                   ^
Error: The value a is local, so cannot be used inside a closure that might escape
|}]

(* Regression test for bug with mishandled regional function modes *)
let bug4 : local_ (string -> foo:string -> unit) -> (string -> unit) =
  fun f -> f ~foo:"hello"
[%%expect{|
Line 2, characters 11-25:
2 |   fun f -> f ~foo:"hello"
               ^^^^^^^^^^^^^^
Error: This value escapes its region
|}]

let bug4' () =
  let local_ f arg ~foo = () in
  let local_ perm ~foo = f ~foo in
  perm ~foo:"foo" "Optional"
[%%expect{|
Line 3, characters 25-31:
3 |   let local_ perm ~foo = f ~foo in
                             ^^^^^^
Error: This local value escapes its region
  Hint: Cannot return local value without an explicit "local_" annotation
|}]

(*
 * Optional arguments
 *)
let appopt1 (f : ?a:local_ int ref -> unit -> unit) =
  let res = f ~a:(let x = local_ ref 42 in x) () in
  res
[%%expect{|
val appopt1 : (?a:local_ int ref -> unit -> unit) -> unit = <fun>
|}]
let appopt2 (f : ?a:local_ int ref -> unit -> unit) =
  let res = f ~a:(let x = local_ ref 42 in x) in
  res
[%%expect{|
Line 3, characters 2-5:
3 |   res
      ^^^
Error: This local value escapes its region
  Hint: Cannot return local value without an explicit "local_" annotation
|}]

(* In principle. it would be sound to allow this one:
   we close over a value in Alloc_local mode, but it is known to be immediate *)
let appopt3 (f : ?a:local_ int ref -> int -> int -> unit) =
  let res = f 42 in
  res
[%%expect{|
Line 3, characters 2-5:
3 |   res
      ^^^
Error: This local value escapes its region
  Hint: Cannot return local value without an explicit "local_" annotation
|}]

let optret1 (f : ?x:int -> local_ (y:unit -> unit -> int)) = f ()
[%%expect{|
Line 1, characters 61-65:
1 | let optret1 (f : ?x:int -> local_ (y:unit -> unit -> int)) = f ()
                                                                 ^^^^
Error: This local value escapes its region
  Hint: Cannot return local value without an explicit "local_" annotation
|}]

(* Optional argument elimination eta-expands and therefore allocates *)
let no_eta (local_ f : unit -> int) = (f : unit -> int)
[%%expect{|
val no_eta : local_ (unit -> int) -> unit -> int = <fun>
|}]

let eta (local_ f : ?a:bool -> unit -> int) = (f : unit -> int)
[%%expect{|
Line 1, characters 47-48:
1 | let eta (local_ f : ?a:bool -> unit -> int) = (f : unit -> int)
                                                   ^
Error: This value escapes its region
|}]

let etajoin p (f : ?b:bool -> unit -> int) (local_ g : unit -> int) =
  if p then (f : unit -> int) else g
[%%expect{|
val etajoin :
  bool -> (?b:bool -> unit -> int) -> local_ (unit -> int) -> unit -> int =
  <fun>
|}]

(* Default arguments *)

let foo ?(local_ x) () = x;;
[%%expect{|
val foo : ?x:local_ 'a -> unit -> local_ 'a option = <fun>
|}]

let foo ?(local_ x = "hello") () = x;;
[%%expect{|
val foo : ?x:local_ string -> unit -> local_ string = <fun>
|}]

let foo ?(local_ x = local_ "hello") () = x;;
[%%expect{|
Line 1, characters 21-35:
1 | let foo ?(local_ x = local_ "hello") () = x;;
                         ^^^^^^^^^^^^^^
Error: This value escapes its region
|}]

let foo ?(local_ x = local_ "hello") () = local_ x;;
[%%expect{|
Line 1, characters 21-35:
1 | let foo ?(local_ x = local_ "hello") () = local_ x;;
                         ^^^^^^^^^^^^^^
Error: This value escapes its region
|}]

(*
 * Modification of return modes in argument position
 *)

let use (local_ f : _ -> _ -> _) x y =
  f x y
let result = use (+) 1 2
[%%expect{|
val use : local_ ('a -> 'b -> 'c) -> 'a -> 'b -> 'c = <fun>
val result : int = 3
|}]

let baduse (f : _ -> _ -> _) x y = lazy (f x y)
let result = baduse (fun a b -> local_ (a,b)) 1 2
[%%expect{|
val baduse : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c lazy_t = <fun>
Line 2, characters 32-44:
2 | let result = baduse (fun a b -> local_ (a,b)) 1 2
                                    ^^^^^^^^^^^^
Error: This value escapes its region
|}]


(*
 * Closures and context locks
 *)

let heap_closure () =
  let foo = local_ ref 1 in
  let fn () =
    let[@stack] fn2 () =
      let[@stack] _baz = foo in
      () in
    let res = fn2 () in
    res
  in
  let _force_heap = ref fn in
  let res = fn () in
  res

[%%expect{|
Line 10, characters 24-26:
10 |   let _force_heap = ref fn in
                             ^^
Error: This value escapes its region
|}]

let local_closure () =
  let foo = local_ ref 1 in
  let local_ fn () =
    let local_ fn2 () =
      let _baz = local_ foo in
      ()
    in
    let res = fn2 () in
    res
  in
  let res = fn () in
  res

[%%expect{|
val local_closure : unit -> unit = <fun>
|}]

(*
 * Always-nonlocal things
 *)
let toplevel_stack = local_ {contents=42}
[%%expect{|
Line 1, characters 4-18:
1 | let toplevel_stack = local_ {contents=42}
        ^^^^^^^^^^^^^^
Error: This value escapes its region
|}]

module M = struct
  let _ = local_ {contents=42}
end
[%%expect{|
module M : sig end
|}]

let _ = local_ {contents=42}
[%%expect{|
Line 1, characters 4-5:
1 | let _ = local_ {contents=42}
        ^
Error: This value escapes its region
|}]


module type T = sig val x : int option end
let first_class_module () =
  let thing = local_ Some 1 in
  let _m : (module T) = local_ (module struct let x = thing end) in
  ()
[%%expect{|
module type T = sig val x : int option end
Line 4, characters 50-51:
4 |   let _m : (module T) = local_ (module struct let x = thing end) in
                                                      ^
Error: This value escapes its region
|}]
let local_module () =
  let thing = local_ Some 1 in
  let _ =
    let module M = struct let x = thing end in
    local_ ()
  in ()
[%%expect{|
Line 4, characters 30-31:
4 |     let module M = struct let x = thing end in
                                  ^
Error: This value escapes its region
|}]
let obj () =
  let thing = local_ Some 1 in
  let _obj = object method foo = thing end in
  ()
[%%expect{|
Line 3, characters 33-38:
3 |   let _obj = object method foo = thing end in
                                     ^^^^^
Error: The value thing is local, so cannot be used inside a closure that might escape
|}]


(*
 * Higher order functions, with arguments that promise not to leak
 *)

let use_locally (f : local_ 'a -> 'a) (x : 'a) = f x
(* This version also promises not to leak the closure *)
let use_locally' (local_ f : local_ 'a -> 'a) (x : 'a) =
  let res = f x in
  res
[%%expect{|
val use_locally : (local_ 'a -> 'a) -> 'a -> 'a = <fun>
val use_locally' : local_ (local_ 'a -> 'a) -> 'a -> 'a = <fun>
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
Error: This value escapes its region
|}]

let leak_ref =
  let r = ref None in
  use_locally (fun x -> r.contents <- Some x; x) 42

[%%expect{|
Line 3, characters 43-44:
3 |   use_locally (fun x -> r.contents <- Some x; x) 42
                                               ^
Error: This value escapes its region
|}]

let leak_ref_2 =
  let r = local_ ref None in
  use_locally (fun x -> let _ = local_ r in r.contents <- Some x; x) 42
[%%expect{|
Line 3, characters 39-40:
3 |   use_locally (fun x -> let _ = local_ r in r.contents <- Some x; x) 42
                                           ^
Error: The value r is local, so cannot be used inside a closure that might escape
|}]

let leak_ref_3 =
  let r = local_ ref None in
  use_locally' (fun x -> let _ = local_ r in r.contents <- Some x; x) 42
[%%expect{|
Line 3, characters 64-65:
3 |   use_locally' (fun x -> let _ = local_ r in r.contents <- Some x; x) 42
                                                                    ^
Error: This value escapes its region
|}]


(* raised exceptions must be global *)
let no_leak_exn =
  use_locally (fun x -> let _exn = local_ Invalid_argument x in "bluh") "blah"
[%%expect{|
val no_leak_exn : string = "bluh"
|}]
let do_leak_exn =
  use_locally (fun x -> let _exn = local_ raise (Invalid_argument x) in "bluh") "blah"

[%%expect{|
Line 2, characters 66-67:
2 |   use_locally (fun x -> let _exn = local_ raise (Invalid_argument x) in "bluh") "blah"
                                                                      ^
Error: This value escapes its region
|}]

(* handled exceptions are known to be global *)
let catch (f : unit -> local_ string) =
  let a =
    match f () with
    | _ -> "hello"
    | exception (Invalid_argument x) -> x
  in
  let b =
    try let _ = f () in "hello" with
    | Invalid_argument x -> x
  in
  (a, b)
[%%expect{|
val catch : (unit -> local_ string) -> string * string = <fun>
|}]


(* same, but this time the function is allowed to return its argument *)
let use_locally (f : local_ 'a -> local_ 'a) : local_ 'a -> local_ 'a = f
[%%expect{|
val use_locally : (local_ 'a -> local_ 'a) -> local_ 'a -> local_ 'a = <fun>
|}]

let loc = ((fun x -> local_ x) : local_ int -> local_ int)

let no_leak_id =
  let _ =
    local_ use_locally ((fun x -> local_ x) : local_ int -> local_ int) 42
  in ()

[%%expect{|
val loc : local_ int -> local_ int = <fun>
val no_leak_id : unit = ()
|}]

module type S = sig val s : string end

(* Don't escape through being unpacked as a module *)

let bar (local_ (m : (module S))) =
  let (module _) = m in
  ()
[%%expect{|
module type S = sig val s : string end
val bar : local_ (module S) -> unit = <fun>
|}]

let bar (local_ (m : (module S))) =
  let (module M) = m in
  M.s
[%%expect{|
Line 2, characters 19-20:
2 |   let (module M) = m in
                       ^
Error: This value escapes its region
|}]

let bar (local_ m) =
  let module M = (val m : S) in
  M.s
[%%expect{|
Line 2, characters 22-23:
2 |   let module M = (val m : S) in
                          ^
Error: This value escapes its region
|}]

(* Don't escape through a lazy value *)

let foo (local_ x) =
  let _ = lazy (print_string !x) in
  ()
[%%expect{|
Line 2, characters 30-31:
2 |   let _ = lazy (print_string !x) in
                                  ^
Error: The value x is local, so cannot be used inside a closure that might escape
|}]

(* Don't escape through a functor *)

let foo (local_ x) =
  let module Foo (X : sig end) = struct
    let () = print_string !x
  end in
  let module _ = Foo(struct end) in
  ()
[%%expect{|
Line 3, characters 27-28:
3 |     let () = print_string !x
                               ^
Error: The value x is local, so cannot be used inside a closure that might escape
|}]

(* Don't escape through a class method *)

let foo (local_ x) =
  let module M = struct
    class c = object
      method m = !x
    end
  end in new c
[%%expect{|
Line 4, characters 18-19:
4 |       method m = !x
                      ^
Error: The value x is local, so cannot be used inside a closure that might escape
|}]

(* Don't escape through an object method *)

let foo (local_ x) =
  let o = object
    method m = !x
  end in
  o#m

[%%expect{|
Line 3, characters 16-17:
3 |     method m = !x
                    ^
Error: The value x is local, so cannot be used inside a closure that might escape
|}]

(* Don't escape through a class instance variable *)

let foo (local_ x) =
  let module M = struct
    class c = object
      val m = !x
    end
  end in new c
[%%expect{|
Line 4, characters 15-16:
4 |       val m = !x
                   ^
Error: The value x is local, so cannot be used inside a closure that might escape
|}]

(* Don't escape through a class instance variable *)

let foo (local_ x) =
  let o = object
    val m = !x
  end in o
[%%expect{|
Line 3, characters 13-14:
3 |     val m = !x
                 ^
Error: The value x is local, so cannot be used inside a closure that might escape
|}]

(* Don't escape through a class local variable *)

let foo (local_ x) =
  let module M = struct
    class c =
      let y = x in
      object end
  end in new M.c
[%%expect{|
Line 4, characters 10-11:
4 |       let y = x in
              ^
Error: This value escapes its region
|}]

let foo (local_ x) =
  let module M = struct
    class c =
      let _ = x in
      object end
  end in new M.c
[%%expect{|
val foo : local_ 'a -> <  > = <fun>
|}]

let foo (local_ x : string ref) =
  let module M = struct
    class c =
      let y = !x in
      object method m = y
    end
  end in new M.c
[%%expect{|
val foo : local_ string ref -> < m : string > = <fun>
|}]

(* Don't escape under a class parameter variable *)

let foo (local_ x : string ref) =
  let module M = struct
    class c =
      fun () ->
      let y = !x in
      object method m = y end
  end in new M.c
[%%expect{|
Line 5, characters 15-16:
5 |       let y = !x in
                   ^
Error: The value x is local, so cannot be used inside a closure that might escape
|}]

let foo (local_ x : string ref) =
  let module M = struct
    class c =
      let y = !x in
      fun () ->
      object method m = y end
  end in new M.c
[%%expect{|
val foo : local_ string ref -> (unit -> < m : string >) = <fun>
|}]

(* Don't escape in inherit expressions *)

class d (p : string) = object method m = p end

let foo (local_ x : string ref) =
  let module M = struct
    class c = object
      inherit d !x
      method n = 42
    end
  end in new M.c
[%%expect{|
class d : string -> object method m : string end
Line 6, characters 17-18:
6 |       inherit d !x
                     ^
Error: The value x is local, so cannot be used inside a closure that might escape
|}]

(* Don't escape in initializers *)

let foo (local_ x) =
  let o = object
    initializer (print_string !x)
  end in
  o#m

[%%expect{|
Line 3, characters 31-32:
3 |     initializer (print_string !x)
                                   ^
Error: The value x is local, so cannot be used inside a closure that might escape
|}]

(* Don't escape in non-function 'let rec' bindings *)
let foo (local_ x) =
  (* fine, local recursive function *)
  let rec g () = let _ = x in h (); () and h () = g (); () in
  g (); ()
[%%expect {|
val foo : local_ 'a -> unit = <fun>
|}]

let foo (local_ x) =
  (* fine, local non-recursive binding *)
  let _ = (x, 1) in
  1
[%%expect {|
val foo : local_ 'a -> int = <fun>
|}]

let foo (local_ x) =
  (* not fine, local recursive non-function (needs caml_alloc_dummy) *)
  let rec g = x :: g in
  let _ = g in ()
[%%expect {|
Line 3, characters 14-15:
3 |   let rec g = x :: g in
                  ^
Error: This value escapes its region
|}]

(* Cannot pass local values to tail calls *)

let print (local_ x) = print_string !x

let foo x =
  let r = local_ { contents = x } in
  print r
[%%expect{|
val print : local_ string ref -> unit = <fun>
Line 5, characters 8-9:
5 |   print r
            ^
Error: This local value escapes its region
  Hint: This argument cannot be local, because this is a tail call
|}]

let local_cb (local_ f) = f ()
let foo (local_ x) = local_cb (fun () -> x := 17; 42)
[%%expect{|
val local_cb : local_ (unit -> 'a) -> 'a = <fun>
Line 2, characters 41-42:
2 | let foo (local_ x) = local_cb (fun () -> x := 17; 42)
                                             ^
Error: The value x is local, so cannot be used inside a closure that might escape
Hint: The closure might escape because it is an argument to a tail call
|}]

let foo x =
  let r = local_ { contents = x } in
  print r;
  ()
[%%expect{|
val foo : string -> unit = <fun>
|}]

let foo x =
  let r = local_ { contents = x } in
  local_ print r
[%%expect{|
val foo : string -> local_ unit = <fun>
|}]

(* Can pass local values to calls explicitly marked as nontail *)
let foo x =
  let local_ r = ref x in
  print r [@nontail]
[%%expect{|
val foo : string -> unit = <fun>
|}]

let foo x =
  let local_ f () = 42 in
  f () [@nontail]
[%%expect{|
val foo : 'a -> int = <fun>
|}]

(* Cannot call local values in tail calls *)

let foo x =
  let r = local_ { contents = x } in
  let local_ foo () = r.contents in
  foo ()
[%%expect{|
Line 4, characters 2-5:
4 |   foo ()
      ^^^
Error: This local value escapes its region
  Hint: This function cannot be local, because this is a tail call
|}]

let foo x =
  let r = local_ { contents = x } in
  let local_ foo () = r.contents in
  let res = foo () in
  res
[%%expect{|
val foo : 'a -> 'a = <fun>
|}]

let foo x =
  let r = local_ { contents = x } in
  let local_ foo () = r.contents in
  local_ foo ()
[%%expect{|
val foo : 'a -> local_ 'a = <fun>
|}]

(* Cannot return local values without annotations on all exits *)

let foo x =
  let r = local_ { contents = x } in
  r
[%%expect{|
Line 3, characters 2-3:
3 |   r
      ^
Error: This local value escapes its region
  Hint: Cannot return local value without an explicit "local_" annotation
|}]

let foo x =
  let r = local_ { contents = x } in
  local_ r
[%%expect{|
val foo : 'a -> local_ 'a ref = <fun>
|}]

let foo p x =
  let r = local_ { contents = x } in
  if p then local_ r
  else r
[%%expect{|
Line 4, characters 7-8:
4 |   else r
           ^
Error: This function return is not annotated with "local_"
       whilst other returns were.
|}]

let foo p x =
  let r = local_ { contents = x } in
  if p then local_ r
  else local_ r
[%%expect{|
val foo : bool -> 'a -> local_ 'a ref = <fun>
|}]

let foo p x = local_
  let r = local_ { contents = x } in
  if p then r
  else r
[%%expect{|
val foo : bool -> 'a -> local_ 'a ref = <fun>
|}]

(* Non-local regional values can be passed to tail calls *)
let rec length acc (local_ xl) =
  match xl with
  | [] -> 0
  | x :: xs -> length (acc + 1) xs
[%%expect{|
val length : int -> local_ 'a list -> int = <fun>
|}]

let foo () =
  let r = local_ ref 5 in
  let bar x = !x in
  let baz () =
    bar r
  in
  let x = baz () in
  x
[%%expect{|
val foo : unit -> int = <fun>
|}]

(* Parameter modes must be matched by the type *)

let foo : 'a -> unit = fun (local_ x) -> ()
[%%expect{|
Line 1, characters 23-43:
1 | let foo : 'a -> unit = fun (local_ x) -> ()
                           ^^^^^^^^^^^^^^^^^^^^
Error: This function has a local parameter, but was expected to have type:
       'a -> unit
|}]

(* Return mode must be greater than the type *)

let foo : unit -> local_ string = fun () -> "hello"
[%%expect{|
val foo : unit -> local_ string = <fun>
|}]

let foo : unit -> string = fun () -> local_ "hello"
[%%expect{|
Line 1, characters 37-51:
1 | let foo : unit -> string = fun () -> local_ "hello"
                                         ^^^^^^^^^^^^^^
Error: This value escapes its region
|}]

(* Unboxed type constructors do not affect regionality *)
type 'a unb1 = A of 'a [@@unboxed]
type 'a unb2 = { foo : 'a } [@@unboxed]
type 'a unb3 = B of { bar : 'a } [@@unboxed]
let f (local_ x) = B { bar = { foo = A x } }
[%%expect{|
type 'a unb1 = A of 'a [@@unboxed]
type 'a unb2 = { foo : 'a; } [@@unboxed]
type 'a unb3 = B of { bar : 'a; } [@@unboxed]
val f : local_ 'a -> local_ 'a unb1 unb2 unb3 = <fun>
|}]


(* Fields have the same mode unless they are nonlocal or mutable *)

type 'a imm = { imm : 'a }
type 'a mut = { mutable mut : 'a }
type 'a gbl = { global_ gbl : 'a }
type 'a nlcl = { nonlocal_ nlcl : 'a }
[%%expect{|
type 'a imm = { imm : 'a; }
type 'a mut = { mutable mut : 'a; }
type 'a gbl = { global_ gbl : 'a; }
type 'a nlcl = { nonlocal_ nlcl : 'a; }
|}]

let foo (local_ x) = x.imm
[%%expect{|
val foo : local_ 'a imm -> local_ 'a = <fun>
|}]
let foo y =
  let x = local_ { imm = y } in
  x.imm
[%%expect{|
Line 3, characters 2-7:
3 |   x.imm
      ^^^^^
Error: This local value escapes its region
  Hint: Cannot return local value without an explicit "local_" annotation
|}]
let foo (local_ x) = x.mut
[%%expect{|
val foo : local_ 'a mut -> 'a = <fun>
|}]
let foo y =
  let x = local_ { mut = y } in
  x.mut
[%%expect{|
val foo : 'a -> 'a = <fun>
|}]
let foo (local_ x) = x.gbl
[%%expect{|
val foo : local_ 'a gbl -> 'a = <fun>
|}]
let foo y =
  let x = local_ { gbl = y } in
  x.gbl
[%%expect{|
val foo : 'a -> 'a = <fun>
|}]
let foo (local_ x) = x.nlcl
[%%expect{|
val foo : local_ 'a nlcl -> local_ 'a = <fun>
|}]
let foo (local_ y) =
  let x = local_ { nlcl = y } in
  x.nlcl
[%%expect{|
val foo : local_ 'a -> local_ 'a = <fun>
|}]

let foo (local_ { imm }) = imm
[%%expect{|
val foo : local_ 'a imm -> local_ 'a = <fun>
|}]
let foo y =
  let { imm } = local_ { imm = y } in
  imm
[%%expect{|
Line 3, characters 2-5:
3 |   imm
      ^^^
Error: This local value escapes its region
  Hint: Cannot return local value without an explicit "local_" annotation
|}]
let foo (local_ { mut }) = mut
[%%expect{|
val foo : local_ 'a mut -> 'a = <fun>
|}]
let foo y =
  let { mut } = local_ { mut = y } in
  mut
[%%expect{|
val foo : 'a -> 'a = <fun>
|}]
let foo (local_ { gbl }) = gbl
[%%expect{|
val foo : local_ 'a gbl -> 'a = <fun>
|}]
let foo y =
  let { gbl } = local_ { gbl = y } in
  gbl
[%%expect{|
val foo : 'a -> 'a = <fun>
|}]
let foo (local_ { nlcl }) = nlcl
[%%expect{|
val foo : local_ 'a nlcl -> local_ 'a = <fun>
|}]
let foo (local_ y) =
  let { nlcl } = local_ { nlcl = y } in
  nlcl
[%%expect{|
val foo : local_ 'a -> local_ 'a = <fun>
|}]

let foo (local_ imm) =
  let _ = { imm } in
  ()
[%%expect{|
val foo : local_ 'a -> unit = <fun>
|}]
let foo () =
  let imm = local_ ref 5 in
  let _ = { imm } in
  ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]
let foo (local_ mut) =
  let _ = { mut } in
  ()
[%%expect{|
Line 2, characters 12-15:
2 |   let _ = { mut } in
                ^^^
Error: This value escapes its region
|}]
let foo () =
  let mut = local_ ref 5 in
  let _ = { mut } in
  ()
[%%expect{|
Line 3, characters 12-15:
3 |   let _ = { mut } in
                ^^^
Error: This value escapes its region
|}]
let foo (local_ gbl) =
  let _ = { gbl } in
  ()
[%%expect{|
Line 2, characters 12-15:
2 |   let _ = { gbl } in
                ^^^
Error: This value escapes its region
|}]
let foo () =
  let gbl = local_ ref 5 in
  let _ = { gbl } in
  ()
[%%expect{|
Line 3, characters 12-15:
3 |   let _ = { gbl } in
                ^^^
Error: This value escapes its region
|}]
let foo (local_ nlcl) =
  let _ = { nlcl } in
  ()
[%%expect{|
val foo : local_ 'a -> unit = <fun>
|}]
let foo () =
  let nlcl = local_ ref 5 in
  let _ = { nlcl } in
  ()
[%%expect{|
Line 3, characters 12-16:
3 |   let _ = { nlcl } in
                ^^^^
Error: This local value escapes its region
|}]

(* Global and nonlocal fields are preserved in module inclusion *)
module M : sig
  type t = { nonlocal_ foo : string }
end = struct
  type t = { foo : string }
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = { foo : string }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = { foo : string; } end
       is not included in
         sig type t = { nonlocal_ foo : string; } end
       Type declarations do not match:
         type t = { foo : string; }
       is not included in
         type t = { nonlocal_ foo : string; }
       Fields do not match:
         foo : string;
       is not the same as:
         nonlocal_ foo : string;
       The second is nonlocal and the first is not.
|}]

module M : sig
  type t = { foo : string }
end = struct
  type t = { nonlocal_ foo : string }
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = { nonlocal_ foo : string }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = { nonlocal_ foo : string; } end
       is not included in
         sig type t = { foo : string; } end
       Type declarations do not match:
         type t = { nonlocal_ foo : string; }
       is not included in
         type t = { foo : string; }
       Fields do not match:
         nonlocal_ foo : string;
       is not the same as:
         foo : string;
       The first is nonlocal and the second is not.
|}]

module M : sig
  type t = { global_ foo : string }
end = struct
  type t = { foo : string }
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = { foo : string }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = { foo : string; } end
       is not included in
         sig type t = { global_ foo : string; } end
       Type declarations do not match:
         type t = { foo : string; }
       is not included in
         type t = { global_ foo : string; }
       Fields do not match:
         foo : string;
       is not the same as:
         global_ foo : string;
       The second is global and the first is not.
|}]

module M : sig
  type t = { foo : string }
end = struct
  type t = { global_ foo : string }
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = { global_ foo : string }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = { global_ foo : string; } end
       is not included in
         sig type t = { foo : string; } end
       Type declarations do not match:
         type t = { global_ foo : string; }
       is not included in
         type t = { foo : string; }
       Fields do not match:
         global_ foo : string;
       is not the same as:
         foo : string;
       The first is global and the second is not.
|}]

(* Special handling of tuples in matches and let bindings *)
let escape : 'a -> unit = fun x -> ()

let foo (local_ x) y =
  match x, y with
  | Some _, Some b -> escape b
  | None, _ -> ()
  | pr  -> let _, _ = pr in ();;
[%%expect{|
val escape : 'a -> unit = <fun>
val foo : local_ 'a option -> 'b option -> unit = <fun>
|}]

let foo (local_ x) y =
  let pr = x, y in
  match pr with
  | Some _, Some b -> escape b
  | None, _ -> ()
  | _  -> ();;
[%%expect{|
Line 4, characters 29-30:
4 |   | Some _, Some b -> escape b
                                 ^
Error: This value escapes its region
|}]

let foo (local_ x) y =
  match x, y with
  | pr ->
    let _, b = pr in
    escape b
  | _  -> ();;
[%%expect{|
Line 5, characters 11-12:
5 |     escape b
               ^
Error: This value escapes its region
|}]

let foo p (local_ x) y z =
  let (_, b) as pr =
    if p then x, y else z
  in
  let _, _ = pr in
  escape b;;
[%%expect{|
val foo : bool -> local_ 'a -> 'b -> 'a * 'b -> unit = <fun>
|}]

let foo p (local_ x) y (local_ z) =
  let _, b =
    if p then x, y else z
  in
  escape b;;
[%%expect{|
Line 5, characters 9-10:
5 |   escape b;;
             ^
Error: This value escapes its region
|}]

let foo p (local_ x) y z =
  let a, _ =
    if p then x, y else z
  in
  escape a;;
[%%expect{|
Line 5, characters 9-10:
5 |   escape a;;
             ^
Error: This value escapes its region
|}]

let foo p (local_ x) y z =
  let pr =
    if p then x, y else z
  in
  let _, b = pr in
  escape b;;
[%%expect{|
Line 6, characters 9-10:
6 |   escape b;;
             ^
Error: This value escapes its region
|}]

(* [as] patterns *)

let foo (local_ x) =
  match x with
  | None as y -> escape y
  | Some _ -> ()
[%%expect{|
val foo : local_ 'a option -> unit = <fun>
|}]

let foo (local_ x) =
  match x with
  | None -> ()
  | Some _ as y -> escape y
[%%expect{|
Line 4, characters 26-27:
4 |   | Some _ as y -> escape y
                              ^
Error: This value escapes its region
|}]

let foo (local_ x) =
  match x with
  | 0 as y -> escape y
  | _ -> ()
[%%expect{|
val foo : local_ int -> unit = <fun>
|}]

let foo (local_ x) =
  match x with
  | 'a'..'e' as y -> escape y
  | _ -> ()
[%%expect{|
val foo : local_ char -> unit = <fun>
|}]

let foo (local_ x) =
  match x with
  | 1.1 as y -> escape y
  | _ -> ()
[%%expect{|
Line 3, characters 23-24:
3 |   | 1.1 as y -> escape y
                           ^
Error: This value escapes its region
|}]

let foo (local_ x) =
  match x with
  | `Foo as y -> escape y
  | _ -> ()
[%%expect{|
val foo : local_ [> `Foo ] -> unit = <fun>
|}]

let foo (local_ x) =
  match x with
  | (`Foo _) as y -> escape y
  | _ -> ()
[%%expect{|
Line 3, characters 28-29:
3 |   | (`Foo _) as y -> escape y
                                ^
Error: This value escapes its region
|}]

let foo (local_ x) =
  match x with
  | (None | Some _) as y -> escape y
[%%expect{|
Line 3, characters 35-36:
3 |   | (None | Some _) as y -> escape y
                                       ^
Error: This value escapes its region
|}]

let foo (local_ x) =
  match x with
  | (Some _|None) as y -> escape y
[%%expect{|
Line 3, characters 33-34:
3 |   | (Some _|None) as y -> escape y
                                     ^
Error: This value escapes its region
|}]

type foo = [`Foo | `Bar]

let foo (local_ x) =
  match x with
  | #foo as y -> escape y
[%%expect{|
type foo = [ `Bar | `Foo ]
val foo : local_ [< foo ] -> unit = <fun>
|}]

type foo = [`Foo | `Bar of int]

let foo (local_ x) =
  match x with
  | #foo as y -> escape y
[%%expect{|
type foo = [ `Bar of int | `Foo ]
Line 5, characters 24-25:
5 |   | #foo as y -> escape y
                            ^
Error: This value escapes its region
|}]

(* Primitives *)

(* Poly-moded eta expansion *)
module Heap32 : sig val add : int32 -> int32 -> int32 end = Int32
module Heap32E : sig external add : int32 -> int32 -> int32 = "%int32_add" end = Int32
module Local32 : sig val add : local_ int32 -> local_ int32 -> local_ int32 end = Int32
module Local32E : sig external add : local_ int32 -> local_ int32 -> local_ int32 = "%int32_add" end = Int32
[%%expect{|
module Heap32 : sig val add : int32 -> int32 -> int32 end
module Heap32E :
  sig external add : int32 -> int32 -> int32 = "%int32_add" end
module Local32 :
  sig val add : local_ int32 -> local_ int32 -> local_ int32 end
module Local32E :
  sig
    external add : local_ int32 -> local_ int32 -> local_ int32
      = "%int32_add"
  end
|}]
module Bad32 : sig val add : local_ int32 -> local_ int32 -> int32 end =
  struct let add = Int32.add end
[%%expect{|
Line 2, characters 2-32:
2 |   struct let add = Int32.add end
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig val add : local_ int32 -> local_ int32 -> local_ int32 end
       is not included in
         sig val add : local_ int32 -> local_ int32 -> int32 end
       Values do not match:
         val add : local_ int32 -> local_ int32 -> local_ int32
       is not included in
         val add : local_ int32 -> local_ int32 -> int32
       The type local_ int32 -> local_ int32 -> local_ int32
       is not compatible with the type local_ int32 -> local_ int32 -> int32
       Type local_ int32 -> local_ int32 is not compatible with type
         local_ int32 -> int32
|}]
module Opt32 : sig external add : (int32[@local_opt]) -> (int32[@local_opt]) -> (int32[@local_opt]) = "%int32_add" end = Int32
module Bad32_2 : sig val add : local_ int32 -> local_ int32 -> int32 end =
  Opt32
[%%expect{|
module Opt32 :
  sig
    external add :
      (int32 [@local_opt]) -> (int32 [@local_opt]) -> (int32 [@local_opt])
      = "%int32_add"
  end
Line 3, characters 2-7:
3 |   Opt32
      ^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig
           external add :
             (int32 [@local_opt]) ->
             (int32 [@local_opt]) -> (int32 [@local_opt]) = "%int32_add"
         end
       is not included in
         sig val add : local_ int32 -> local_ int32 -> int32 end
       Values do not match:
         external add :
           (int32 [@local_opt]) ->
           (int32 [@local_opt]) -> (int32 [@local_opt]) = "%int32_add"
       is not included in
         val add : local_ int32 -> local_ int32 -> int32
       The type local_ int32 -> local_ int32 -> local_ int32
       is not compatible with the type local_ int32 -> local_ int32 -> int32
       Type local_ int32 -> local_ int32 is not compatible with type
         local_ int32 -> int32
|}]

module Contravariant_instantiation : sig
  external to_int_trunc : Int64.t -> int = "%int64_to_int"
end = struct
  external to_int_trunc : (Int64.t [@local_opt]) -> int = "%int64_to_int"
end
[%%expect{|
module Contravariant_instantiation :
  sig external to_int_trunc : Int64.t -> int = "%int64_to_int" end
|}]

(* Return modes *)
let zx : int ref -> (int -> unit) = (:=)
let zz : local_ (int ref) -> int -> unit = (:=)
let zy : local_ (int ref) -> (int -> unit) = (:=)
[%%expect{|
val zx : int ref -> int -> unit = <fun>
val zz : local_ int ref -> int -> unit = <fun>
Line 3, characters 45-49:
3 | let zy : local_ (int ref) -> (int -> unit) = (:=)
                                                 ^^^^
Error: This expression has type local_ 'a ref -> 'a -> unit
       but an expression was expected of type local_ int ref -> (int -> unit)
|}]

let int32 (local_ x) (local_ y) = local_
  Int32.(div (logxor (mul x y) (sub x y)) (shift_right y 10))
let int64 (local_ x) (local_ y) = local_
  Int64.(div (logxor (mul x y) (sub x y)) (shift_right y 10))
let nativeint (local_ x) (local_ y) = local_
  Nativeint.(div (logxor (mul x y) (sub x y)) (shift_right y 10))
let float (local_ x) (local_ y) = local_
  (x +. y *. x -. 42.)
[%%expect{|
val int32 : local_ int32 -> local_ int32 -> local_ int32 = <fun>
val int64 : local_ int64 -> local_ int64 -> local_ int64 = <fun>
val nativeint : local_ nativeint -> local_ nativeint -> local_ nativeint =
  <fun>
val float : local_ float -> local_ float -> local_ float = <fun>
|}]

let etapair (local_ x) = local_ (fst x, snd x)
[%%expect{|
val etapair : local_ 'a * 'b -> local_ 'a * 'b = <fun>
|}]

(* Arity checking on primitives *)
external goodadd : int32 -> int32 -> int32 = "%int32_add"
[%%expect{|
external goodadd : int32 -> int32 -> int32 = "%int32_add"
|}]
external badadd : int32 -> (int32 -> int32) = "%int32_add"
[%%expect{|
Line 1, characters 0-58:
1 | external badadd : int32 -> (int32 -> int32) = "%int32_add"
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Wrong arity for builtin primitive "%int32_add"
|}]

let compare (local_ x) (local_ y) =
  [x = y; x <> y; x < y; x > y; x <= y; x >= y; compare x y = 0; x == y; x != y]
[%%expect{|
val compare : local_ 'a -> local_ 'a -> bool list = <fun>
|}]

(* integer primitives accept local args *)
let intf (local_ x) = x |> Int.succ |> Int.add 42 |> pred |> (/) 100 |> (+) 1
[%%expect{|
val intf : local_ int -> int = <fun>
|}]

(* primitives don't count as tail calls, so you can pass them locals *)
let primloc x = let local_ y = Int32.add x 1l in Int32.to_int y
[%%expect{|
val primloc : int32 -> int = <fun>
|}]

(* (&&) and (||) tail call on the right *)
let testbool1 f = let local_ r = ref 42 in (f r || false) && true 

let testbool2 f = let local_ r = ref 42 in true && (false || f r)
[%%expect{|
val testbool1 : (local_ int ref -> bool) -> bool = <fun>
Line 3, characters 63-64:
3 | let testbool2 f = let local_ r = ref 42 in true && (false || f r)
                                                                   ^
Error: This local value escapes its region
  Hint: This argument cannot be local, because this is a tail call
|}]

(* mode-crossing using unary + *)
let promote (local_ x) = +x
[%%expect{|
val promote : local_ int -> int = <fun>
|}]

(* Or-patterns *)
let foo (local_ x) y =
  match y, x with
  | Some z, None | None, Some z -> z
  | None, None | Some _, Some _ -> assert false
[%%expect{|
val foo : local_ 'a option -> 'a option -> local_ 'a = <fun>
|}]

let foo (local_ x) y =
  match x, y with
  | Some z, None | None, Some z -> z
  | None, None | Some _, Some _ -> assert false
[%%expect{|
val foo : local_ 'a option -> 'a option -> local_ 'a = <fun>
|}]

module M = struct
  let (Some z, _, _) | (None, Some z, _)
      | (None, None, z) = (Some (ref 0), (local_ (Some (ref 0))), (ref 0))
end
[%%expect{|
Line 2, characters 35-36:
2 |   let (Some z, _, _) | (None, Some z, _)
                                       ^
Error: This value escapes its region
|}]

module M = struct
  let (Some z, _, _) | (None, Some z, _)
      | (None, None, z) = ((local_ Some (ref 0)), (Some (ref 0)), (ref 0))
end
[%%expect{|
Line 2, characters 12-13:
2 |   let (Some z, _, _) | (None, Some z, _)
                ^
Error: This value escapes its region
|}]

(* Example of backtracking after mode error *)
let f g n =
  let a = local_ [n+1] in
  let () = g a in
  ()
let z : (int list -> unit) -> int -> unit = f
[%%expect{|
val f : (local_ int list -> unit) -> int -> unit = <fun>
Line 5, characters 44-45:
5 | let z : (int list -> unit) -> int -> unit = f
                                                ^
Error: This expression has type (local_ int list -> unit) -> int -> unit
       but an expression was expected of type
         (int list -> unit) -> int -> unit
       Type local_ int list -> unit is not compatible with type
         int list -> unit
|}]

module M = struct
  let f g n =
    let a = local_ [n+1] in
    let () = g a in
    ()
  let z : (int list -> unit) -> int -> unit = f
end
[%%expect{|
Line 6, characters 46-47:
6 |   let z : (int list -> unit) -> int -> unit = f
                                                  ^
Error: This expression has type (local_ int list -> unit) -> int -> unit
       but an expression was expected of type
         (int list -> unit) -> int -> unit
       Type local_ int list -> unit is not compatible with type
         int list -> unit
|}]

(* Subtyping *)

let foo f = (f : local_ string -> float :> string -> float)
[%%expect{|
val foo : (local_ string -> float) -> string -> float = <fun>
|}]

let foo f = (f : string -> float :> string -> local_ float)
[%%expect{|
val foo : (string -> float) -> string -> local_ float = <fun>
|}]

let foo f = (f : string -> local_ float :> string -> float)
[%%expect{|
Line 1, characters 12-59:
1 | let foo f = (f : string -> local_ float :> string -> float)
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type string -> local_ float is not a subtype of string -> float
|}]

let foo f = (f : string -> float :> local_ string -> local_ float)
[%%expect{|
Line 1, characters 12-66:
1 | let foo f = (f : string -> float :> local_ string -> local_ float)
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type string -> float is not a subtype of local_ string -> local_ float
|}]

let foo f = ignore (f :> string -> float); ()
[%%expect{|
val foo : (string -> float) -> unit = <fun>
|}]

let local_to_global (local_ _s : string) = 42.0

let foo f = ignore (f :> string -> float); [f; local_to_global]
[%%expect{|
val local_to_global : local_ string -> float = <fun>
val foo : (local_ string -> float) -> (local_ string -> float) list = <fun>
|}]

let global_to_local (_s : string) = local_ 42.0

let foo f = ignore (f :> string -> float); [f; global_to_local]
[%%expect{|
val global_to_local : string -> local_ float = <fun>
Line 3, characters 47-62:
3 | let foo f = ignore (f :> string -> float); [f; global_to_local]
                                                   ^^^^^^^^^^^^^^^
Error: This expression has type string -> local_ float
       but an expression was expected of type string -> float
|}]

(* Submoding during module inclusion *)

module F (X : sig val foo : local_ float -> string end) : sig
  val foo : float -> string
end = X;;
[%%expect{|
module F :
  functor (X : sig val foo : local_ float -> string end) ->
    sig val foo : float -> string end
|}]

module F (X : sig val foo : float -> string end) : sig
  val foo : float -> local_ string
end = X;;
[%%expect{|
module F :
  functor (X : sig val foo : float -> string end) ->
    sig val foo : float -> local_ string end
|}]

module F (X : sig val foo : float -> string end) : sig
  val foo : local_ float -> string
end = X;;
[%%expect{|
Line 3, characters 6-7:
3 | end = X;;
          ^
Error: Signature mismatch:
       Modules do not match:
         sig val foo : float -> string end
       is not included in
         sig val foo : local_ float -> string end
       Values do not match:
         val foo : float -> string
       is not included in
         val foo : local_ float -> string
       The type float -> string is not compatible with the type
         local_ float -> string
|}]

module F (X : sig val foo : float -> local_ string end) : sig
  val foo : float -> string
end = X;;
[%%expect{|
Line 3, characters 6-7:
3 | end = X;;
          ^
Error: Signature mismatch:
       Modules do not match:
         sig val foo : float -> local_ string end
       is not included in
         sig val foo : float -> string end
       Values do not match:
         val foo : float -> local_ string
       is not included in
         val foo : float -> string
       The type float -> local_ string is not compatible with the type
         float -> string
|}]

module F (X : sig val foo : local_ float -> float -> string end) : sig
  val foo : float -> float -> string
end = X;;
[%%expect{|
Line 3, characters 6-7:
3 | end = X;;
          ^
Error: Signature mismatch:
       Modules do not match:
         sig val foo : local_ float -> float -> string end
       is not included in
         sig val foo : float -> float -> string end
       Values do not match:
         val foo : local_ float -> float -> string
       is not included in
         val foo : float -> float -> string
       The type local_ float -> float -> string
       is not compatible with the type float -> float -> string
|}]

module F (X : sig val foo : local_ float -> float -> string end) : sig
  val foo : float -> local_ (float -> string)
end = X;;
[%%expect{|
module F :
  functor (X : sig val foo : local_ float -> float -> string end) ->
    sig val foo : float -> local_ (float -> string) end
|}]

module F (X : sig val foo : float -> float -> string end) : sig
  val foo : float -> local_ (float -> string)
end = X;;
[%%expect{|
module F :
  functor (X : sig val foo : float -> float -> string end) ->
    sig val foo : float -> local_ (float -> string) end
|}]

type 'a inv = Inv of ('a -> 'a)
type 'a co = Co of 'a
type 'a contra = Contra of ('a -> int)
type 'a bi = Bi

module F (X : sig val foo : (float -> string) inv end) : sig
  val foo : (float -> local_ string) inv
end = X;;
[%%expect{|
type 'a inv = Inv of ('a -> 'a)
type 'a co = Co of 'a
type 'a contra = Contra of ('a -> int)
type 'a bi = Bi
Line 8, characters 6-7:
8 | end = X;;
          ^
Error: Signature mismatch:
       Modules do not match:
         sig val foo : (float -> string) inv end
       is not included in
         sig val foo : (float -> local_ string) inv end
       Values do not match:
         val foo : (float -> string) inv
       is not included in
         val foo : (float -> local_ string) inv
       The type (float -> string) inv is not compatible with the type
         (float -> local_ string) inv
       Type float -> string is not compatible with type
         float -> local_ string
|}]

module F (X : sig val foo : (float -> string) co end) : sig
  val foo : (float -> local_ string) co
end = X;;
[%%expect{|
module F :
  functor (X : sig val foo : (float -> string) co end) ->
    sig val foo : (float -> local_ string) co end
|}]

module F (X : sig val foo : (float -> string) contra end) : sig
  val foo : (float -> local_ string) contra
end = X;;
[%%expect{|
Line 3, characters 6-7:
3 | end = X;;
          ^
Error: Signature mismatch:
       Modules do not match:
         sig val foo : (float -> string) contra end
       is not included in
         sig val foo : (float -> local_ string) contra end
       Values do not match:
         val foo : (float -> string) contra
       is not included in
         val foo : (float -> local_ string) contra
       The type (float -> string) contra is not compatible with the type
         (float -> local_ string) contra
       Type float -> string is not compatible with type
         float -> local_ string
|}]

module F (X : sig val foo : (float -> string) bi end) : sig
  val foo : (float -> local_ string) bi
end = X;;
[%%expect{|
module F :
  functor (X : sig val foo : (float -> string) bi end) ->
    sig val foo : (float -> local_ string) bi end
|}]

module F (X : sig val foo : (float -> local_ string) inv end) : sig
  val foo : (float -> string) inv
end = X;;
[%%expect{|
Line 3, characters 6-7:
3 | end = X;;
          ^
Error: Signature mismatch:
       Modules do not match:
         sig val foo : (float -> local_ string) inv end
       is not included in
         sig val foo : (float -> string) inv end
       Values do not match:
         val foo : (float -> local_ string) inv
       is not included in
         val foo : (float -> string) inv
       The type (float -> local_ string) inv is not compatible with the type
         (float -> string) inv
       Type float -> local_ string is not compatible with type
         float -> string
|}]

module F (X : sig val foo : (float -> local_ string) co end) : sig
  val foo : (float -> string) co
end = X;;
[%%expect{|
Line 3, characters 6-7:
3 | end = X;;
          ^
Error: Signature mismatch:
       Modules do not match:
         sig val foo : (float -> local_ string) co end
       is not included in
         sig val foo : (float -> string) co end
       Values do not match:
         val foo : (float -> local_ string) co
       is not included in
         val foo : (float -> string) co
       The type (float -> local_ string) co is not compatible with the type
         (float -> string) co
       Type float -> local_ string is not compatible with type
         float -> string
|}]

module F (X : sig val foo : (float -> local_ string) contra end) : sig
  val foo : (float -> string) contra
end = X;;
[%%expect{|
module F :
  functor (X : sig val foo : (float -> local_ string) contra end) ->
    sig val foo : (float -> string) contra end
|}]

module F (X : sig val foo : (float -> local_ string) bi end) : sig
  val foo : (float -> string) bi
end = X;;
[%%expect{|
module F :
  functor (X : sig val foo : (float -> local_ string) bi end) ->
    sig val foo : (float -> string) bi end
|}]


(*
 * constructor arguments global/nonlocal
 *)

(* Global argument are preserved in module inclusion *)
module M : sig
  type t = Bar of int * global_ string
end = struct
  type t = Bar of int * string
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = Bar of int * string
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = Bar of int * string end
       is not included in
         sig type t = Bar of int * global_ string end
       Type declarations do not match:
         type t = Bar of int * string
       is not included in
         type t = Bar of int * global_ string
       Constructors do not match:
         Bar of int * string
       is not the same as:
         Bar of int * global_ string
       Locality mismatch at argument position 2 : The second is global and the first is not.
|}]


module M : sig
  type t = Bar of int * string
end = struct
  type t = Bar of int * global_ string
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = Bar of int * global_ string
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = Bar of int * global_ string end
       is not included in
         sig type t = Bar of int * string end
       Type declarations do not match:
         type t = Bar of int * global_ string
       is not included in
         type t = Bar of int * string
       Constructors do not match:
         Bar of int * global_ string
       is not the same as:
         Bar of int * string
       Locality mismatch at argument position 2 : The first is global and the second is not.
|}]

(* Nonlocal argument are preserved in module inclusion *)
module M : sig
  type t = Bar of int * nonlocal_ string
end = struct
  type t = Bar of int * string
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = Bar of int * string
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = Bar of int * string end
       is not included in
         sig type t = Bar of int * nonlocal_ string end
       Type declarations do not match:
         type t = Bar of int * string
       is not included in
         type t = Bar of int * nonlocal_ string
       Constructors do not match:
         Bar of int * string
       is not the same as:
         Bar of int * nonlocal_ string
       Locality mismatch at argument position 2 : The second is nonlocal and the first is not.
|}]


module M : sig
  type t = Bar of int * string
end = struct
  type t = Bar of int * nonlocal_ string
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = Bar of int * nonlocal_ string
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = Bar of int * nonlocal_ string end
       is not included in
         sig type t = Bar of int * string end
       Type declarations do not match:
         type t = Bar of int * nonlocal_ string
       is not included in
         type t = Bar of int * string
       Constructors do not match:
         Bar of int * nonlocal_ string
       is not the same as:
         Bar of int * string
       Locality mismatch at argument position 2 : The first is nonlocal and the second is not.
|}]

(* global_ and nonlocal_ bind closer than star *)
type gfoo = GFoo of global_ string * string
type rfoo = RFoo of nonlocal_ string * string
[%%expect{|
type gfoo = GFoo of global_ string * string
type rfoo = RFoo of nonlocal_ string * string
|}]

(* TESTING OF GLOBAL_ *)

(* global arguments must be global when constructing
   cannot be regional or local
*)
let f (local_ s : string) =
  GFoo (s, "bar")
[%%expect{|
Line 2, characters 8-9:
2 |   GFoo (s, "bar")
            ^
Error: This value escapes its region
|}]

let f =
  let local_ s = "foo" in
  GFoo (s, "bar")
[%%expect{|
Line 3, characters 8-9:
3 |   GFoo (s, "bar")
            ^
Error: This value escapes its region
|}]

(* s' extracted from x as global *)
(* despite x is local or regional*)
let f (s : string) =
  let local_ x = GFoo (s, "bar") in
  match x with
  | GFoo (s', _) -> ref s'

[%%expect{|
val f : string -> string ref = <fun>
|}]

let f (local_ x : gfoo) =
  match x with
  | GFoo (s', _) -> ref s'

[%%expect{|
val f : local_ gfoo -> string ref = <fun>
|}]

(* the argument not marked global remains contingent on construction  *)
(* local gives local *)
let f (s : string) =
  let local_ x = GFoo ("bar", s) in
  match x with
  | GFoo (_, s') -> s'

[%%expect{|
Line 4, characters 20-22:
4 |   | GFoo (_, s') -> s'
                        ^^
Error: This local value escapes its region
  Hint: Cannot return local value without an explicit "local_" annotation
|}]

(* and regional gives regional *)
let f (local_ x : gfoo) =
  match x with
  | GFoo (_, s') -> ref s'

[%%expect{|
Line 3, characters 24-26:
3 |   | GFoo (_, s') -> ref s'
                            ^^
Error: This value escapes its region
|}]

(* TESTING NONLOCAL_ *)
(* nonlocal argument must be outer than the construction*)
(* below, local is not outer than local*)
let f (local_ s : string) =
  let local_ s' = "foo" in
  let local_ x = RFoo(s', "bar") in
  "bar"
[%%expect{|
Line 3, characters 22-24:
3 |   let local_ x = RFoo(s', "bar") in
                          ^^
Error: This local value escapes its region
|}]

(* local is not outer than global either *)
let f =
  let local_ s = "foo" in
  let x = RFoo(s, "bar") in
  "bar"
[%%expect{|
Line 3, characters 15-16:
3 |   let x = RFoo(s, "bar") in
                   ^
Error: This local value escapes its region
|}]


(* but global is outer than local *)
let f (s : string) =  (* s is global *)
  let local_ _x = RFoo (s, "bar") in  (* x is local *)
  "foo"
[%%expect{|
val f : string -> string = <fun>
|}]

(* and regional is outer than local *)
let f (local_ s : string) =  (* s is regional *)
  let local_ _x = RFoo (s, "bar") in  (* x is local *)
  "foo"
[%%expect{|
val f : local_ string -> string = <fun>
|}]

(* s' extracted from x is not local  *)
(* even though x is local *)
let f (local_ s : string) =
  let local_ x = RFoo (s, "bar") in
  match x with
  | RFoo (s', _) -> s'

[%%expect{|
val f : local_ string -> local_ string = <fun>
|}]

(* Moreover, it is not global *)
let f (local_ s : string) =
  let local_ x = RFoo(s, "bar") in
  match x with
  | RFoo (s', _) -> GFoo (s', "bar")
[%%expect{|
Line 4, characters 26-28:
4 |   | RFoo (s', _) -> GFoo (s', "bar")
                              ^^
Error: This value escapes its region
|}]

(* x is already global *)
(* regional s' extracted from x is still global *)
let f (s : string) =
  let x = RFoo (s, "bar") in
  match x with
  | RFoo (s', _) -> GFoo(s', "bar")
[%%expect{|
val f : string -> gfoo = <fun>
|}]

(* regional s extracted from regional x *)
(* still regional *)
(* first it is not local *)
let f (local_ x : rfoo) =
  match x with
  | RFoo (s, _) -> s
[%%expect{|
val f : local_ rfoo -> local_ string = <fun>
|}]

(* second, it is not global *)
let f (local_ x : rfoo) =
  match x with
  | RFoo (s, _) -> ref s
[%%expect{|
Line 3, characters 23-24:
3 |   | RFoo (s, _) -> ref s
                           ^
Error: This value escapes its region
|}]


(* test of arrays *)
(* as elements of arrays are mutable *)
(* it is only safe for them to be at global mode *)
(* cf: similarly reference cell can contain only global values *)

(* on construction of array, we ensure elements are global *)

let f (local_ x : string) = 
  [|x; "foo"|]
[%%expect{|
Line 2, characters 4-5:
2 |   [|x; "foo"|]
        ^
Error: This value escapes its region
|}]

let f (x : string) = 
  [|x; "foo"|]
[%%expect{|
val f : string -> string array = <fun>
|}]  


(* on pattern matching of array, 
   elements are strengthened to global 
  even if array itself is local *)
let f (local_ a : string array) = 
  match a with
  | [| x; _ |] -> ref x
  | _ -> ref "foo"

[%%expect{|
val f : local_ string array -> string ref = <fun>
|}]  