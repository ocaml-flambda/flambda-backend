(* TEST
   * expect
*)

let leak n =
  let r = local_ ref n in
  r
[%%expect{|
Line 3, characters 2-3:
3 |   r
      ^
Error: Cannot return locally-allocated value without explicit "local_" annotation
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


(* This will be typeable eventually, once label mutability affects mode *)
let (!) = fun (local_ r) -> r.contents
[%%expect{|
val ( ! ) : local_ 'a ref -> 'a = <fun>
|}]

(*
 * Type equalities of function types
 *)

module Equ = struct
  (* When a [local_] argument appears in a function type with multiple arguments,
     return modes are implicitly stack until the final argument. *)
  type ('a, 'b, 'c, 'd, 'e) fn5 =
    'a -> local_ 'b -> 'c -> 'd -> 'e

  type ('a,'b,'c,'d,'e) equ_fn5 = unit
    constraint
      ('a, 'b, 'c, 'd, 'e) fn5
      =
      'a -> local_ 'b -> local_ ('c -> local_ ('d -> 'e))
end
[%%expect{|
module Equ :
  sig
    type ('a, 'b, 'c, 'd, 'e) fn5 = 'a -> local_ 'b -> 'c -> 'd -> 'e
    type ('a, 'b, 'c, 'd, 'e) equ_fn5 = unit
  end
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
Type local_ int -> int is not compatible with type local_ int -> local_ int
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

let f : local_ 'a -> int -> int = fun _ x -> x
let g : local_ 'a -> int -> _ = fun _ x -> f
[%%expect{|
val f : local_ 'a -> int -> int = <fun>
val g : local_ 'a -> int -> (local_ 'b -> int -> int) = <fun>
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
val apply2 : int -> local_ 'a -> int -> int = <fun>
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

let app1 (f : a:int -> b:local_ int ref -> unit -> unit) = f ~b:(ref 42) ()
[%%expect{|
Line 1, characters 64-72:
1 | let app1 (f : a:int -> b:local_ int ref -> unit -> unit) = f ~b:(ref 42) ()
                                                                    ^^^^^^^^
Error: This locally-allocated argument is captured by a partial application
|}]
let app2 (f : a:int -> b:local_ int ref -> unit -> unit) = f ~b:(ref 42)
[%%expect{|
Line 1, characters 64-72:
1 | let app2 (f : a:int -> b:local_ int ref -> unit -> unit) = f ~b:(ref 42)
                                                                    ^^^^^^^^
Error: This locally-allocated argument is captured by a partial application
|}]
let app3 (f : a:int -> b:local_ int ref -> unit) = f ~b:(ref 42)
[%%expect{|
Line 1, characters 56-64:
1 | let app3 (f : a:int -> b:local_ int ref -> unit) = f ~b:(ref 42)
                                                            ^^^^^^^^
Error: This locally-allocated argument is captured by a partial application
|}]
let app4 (f : b:local_ int ref -> a:int -> unit) = f ~b:(ref 42)
[%%expect{|
Line 1, characters 51-64:
1 | let app4 (f : b:local_ int ref -> a:int -> unit) = f ~b:(ref 42)
                                                       ^^^^^^^^^^^^^
Error: This locally-allocated return value escapes
|}]
let app42 (f : a:local_ int ref -> (int -> b:local_ int ref -> c:int -> unit)) =
  f ~a:(ref 1) 2 ~c:4
[%%expect{|
val app42 :
  (a:local_ int ref -> (int -> b:local_ int ref -> c:int -> unit)) ->
  b:local_ int ref -> unit = <fun>
|}]
let app43 (f : a:local_ int ref -> (int -> b:local_ int ref -> c:int -> unit)) =
  f ~a:(ref 1) 2
[%%expect{|
val app43 :
  (a:local_ int ref -> (int -> b:local_ int ref -> c:int -> unit)) ->
  b:local_ int ref -> c:int -> unit = <fun>
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
Error: This locally-allocated return value escapes
|}]

let bug1 () =
  let foo : a:local_ string -> b:local_ string -> c:int -> unit = fun ~a ~b ~c -> () in
  let bar = local_ foo ~b:"hello" in
  let res = bar ~a:"world" in
  res
[%%expect{|
Line 5, characters 2-5:
5 |   res
      ^^^
Error: Cannot return locally-allocated value without explicit "local_" annotation
|}]
let bug2 () =
  let foo : a:local_ string -> (b:local_ string -> (c:int -> unit)) =
    fun ~a -> fun ~b -> fun ~c -> ()
  in
  let bar = local_ foo ~b:"hello" in
  let res = bar ~a:"world" in
  res
[%%expect{|
val bug2 : unit -> c:int -> unit = <fun>
|}]
let bug3 () =
  let foo : a:local_ string -> (b:local_ string -> (c:int -> unit)) =
    fun ~a -> fun ~b -> fun ~c -> print_string a
  in
  let[@stack] bar = foo ~b:"hello" in
  let res = bar ~a:"world" in
  res
[%%expect{|
Line 3, characters 47-48:
3 |     fun ~a -> fun ~b -> fun ~c -> print_string a
                                                   ^
Error: The value a is local, so cannot be used here as it might escape
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
Error: Cannot return locally-allocated value without explicit "local_" annotation
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
Error: Cannot return locally-allocated value without explicit "local_" annotation
|}]

let optret1 (f : ?x:int -> local_ (y:unit -> unit -> int)) = f ()
[%%expect{|
Line 1, characters 61-65:
1 | let optret1 (f : ?x:int -> local_ (y:unit -> unit -> int)) = f ()
                                                                 ^^^^
Error: This locally-allocated return value escapes
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
Error: The value fn is local, so cannot be used here as it might escape
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
Error: This locally-allocated value escapes
|}]

let _ = local_ {contents=42}
[%%expect{|
- : int ref = {contents = 42}
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
Error: This locally-allocated value escapes
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
Error: This locally-allocated value escapes
|}]
let obj () =
  let thing = local_ Some 1 in
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
Error: The value x is local, so cannot be used here as it might escape
|}]



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
Error: The value x is local, so cannot be used here as it might escape
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
Error: The value m is local, so cannot be used here as it might escape
|}]

let bar (local_ m) =
  let module M = (val m : S) in
  M.s
[%%expect{|
Line 2, characters 22-23:
2 |   let module M = (val m : S) in
                          ^
Error: The value m is local, so cannot be used here as it might escape
|}]

(* Don't escape through a lazy value *)

let foo (local_ x) =
  lazy (print_string !x)
[%%expect{|
Line 2, characters 22-23:
2 |   lazy (print_string !x)
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
Error: The value x is local, so cannot be used here as it might escape
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
Error: The value x is local, so cannot be used here as it might escape
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
Error: This locally-allocated value escapes
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
      object method m = y  end
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
Error: The value r is local, so cannot be used here as it might escape
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

(* Cannot call local values in tail calls *)

let foo x =
  let r = local_ { contents = x } in
  let local_ foo () = r.contents in
  foo ()
[%%expect{|
Line 4, characters 2-5:
4 |   foo ()
      ^^^
Error: The value foo is local, so cannot be used here as it might escape
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
Error: Cannot return locally-allocated value without explicit "local_" annotation
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
Error: Cannot return locally-allocated value without explicit "local_" annotation
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

(* Parameter modes must be matched by the type *)

let foo : 'a -> unit = fun (local_ x) -> ()
[%%expect{|
Line 1, characters 23-43:
1 | let foo : 'a -> unit = fun (local_ x) -> ()
                           ^^^^^^^^^^^^^^^^^^^^
Error: This function has a local parameter, but was expected to have type:
       'a -> unit
|}]

(* Return mode must be matched by the type *)

let foo : unit -> local_ string = fun () -> "hello"
[%%expect{|
Line 1, characters 44-51:
1 | let foo : unit -> local_ string = fun () -> "hello"
                                                ^^^^^^^
Error: Cannot return locally-allocated value without explicit "local_" annotation
|}]

let foo : unit -> string = fun () -> local_ "hello"
[%%expect{|
Line 1, characters 37-51:
1 | let foo : unit -> string = fun () -> local_ "hello"
                                         ^^^^^^^^^^^^^^
Error: This locally-allocated value escapes
|}]

(* Fields have the same mode unless they are nonlocal or mutable *)

type 'a imm = { imm : 'a }
type 'a mut = { mutable mut : 'a }
type 'a nlcl = { nonlocal_ nlcl : 'a }
[%%expect{|
type 'a imm = { imm : 'a; }
type 'a mut = { mutable mut : 'a; }
type 'a nlcl = { nonlocal_ nlcl : 'a; }
|}]

let foo (local_ x) = x.imm
[%%expect{|
Line 1, characters 21-26:
1 | let foo (local_ x) = x.imm
                         ^^^^^
Error: This locally-allocated value escapes
|}]
let foo (local_ x) = x.mut
[%%expect{|
val foo : local_ 'a mut -> 'a = <fun>
|}]
let foo (local_ x) = x.nlcl
[%%expect{|
val foo : local_ 'a nlcl -> 'a = <fun>
|}]

let foo (local_ { imm }) = imm
[%%expect{|
Line 1, characters 27-30:
1 | let foo (local_ { imm }) = imm
                               ^^^
Error: Cannot return locally-allocated value without explicit "local_" annotation
|}]
let foo (local_ { mut }) = mut
[%%expect{|
val foo : local_ 'a mut -> 'a = <fun>
|}]
let foo (local_ { nlcl }) = nlcl
[%%expect{|
val foo : local_ 'a nlcl -> 'a = <fun>
|}]

let foo (local_ imm) =
  let _ = { imm } in
  ()
[%%expect{|
val foo : local_ 'a -> unit = <fun>
|}]
let foo (local_ mut) =
  let _ = { mut } in
  ()
[%%expect{|
Line 2, characters 12-15:
2 |   let _ = { mut } in
                ^^^
Error: The value mut is local, so cannot be used here as it might escape
|}]
let foo (local_ nlcl) =
  let _ = { nlcl } in
  ()
[%%expect{|
Line 2, characters 12-16:
2 |   let _ = { nlcl } in
                ^^^^
Error: The value nlcl is local, so cannot be used here as it might escape
|}]

(* Nonlocality is preserved in module inclusion *)
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
       is not compatible with:
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
       is not compatible with:
         foo : string;
       The first is nonlocal and the second is not.
|}]
