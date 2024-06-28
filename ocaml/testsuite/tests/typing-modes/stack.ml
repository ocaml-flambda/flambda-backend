(* TEST
flags += "-extension comprehensions";
expect;
*)

let ignore_local : 'a @ local -> unit = fun _ -> ()
[%%expect{|
val ignore_local : local_ 'a -> unit = <fun>
|}]

let f = ref (stack_ (fun x -> x))
[%%expect{|
Line 1, characters 20-32:
1 | let f = ref (stack_ (fun x -> x))
                        ^^^^^^^^^^^^
Error: This allocation cannot be on the stack.
|}]

let f = ref (stack_ (2, 3))
[%%expect{|
Line 1, characters 20-26:
1 | let f = ref (stack_ (2, 3))
                        ^^^^^^
Error: This allocation cannot be on the stack.
|}]

let f = ignore_local (stack_ (2, 3))
[%%expect{|
val f : unit = ()
|}]

type t = Foo | Bar of int

let f = ref (stack_ Foo)
[%%expect{|
type t = Foo | Bar of int
Line 3, characters 20-23:
3 | let f = ref (stack_ Foo)
                        ^^^
Error: This expression is not an allocation site.
|}]

let f = ref (stack_ (Bar 42))
[%%expect{|
Line 1, characters 20-28:
1 | let f = ref (stack_ (Bar 42))
                        ^^^^^^^^
Error: This allocation cannot be on the stack.
|}]

let f = ignore_local (stack_ (Bar 42))
[%%expect{|
val f : unit = ()
|}]

let f = ref (stack_ `Foo)
[%%expect{|
Line 1, characters 20-24:
1 | let f = ref (stack_ `Foo)
                        ^^^^
Error: This expression is not an allocation site.
|}]

let f = ref (stack_ (`Bar 42))
[%%expect{|
Line 1, characters 20-29:
1 | let f = ref (stack_ (`Bar 42))
                        ^^^^^^^^^
Error: This allocation cannot be on the stack.
|}]

let f = ignore_local (stack_ (`Bar 42))
[%%expect{|
val f : unit = ()
|}]

type r = {x : string} [@@unboxed]

let f = ref (stack_ {x = "hello"})
[%%expect{|
type r = { x : string; } [@@unboxed]
Line 3, characters 20-33:
3 | let f = ref (stack_ {x = "hello"})
                        ^^^^^^^^^^^^^
Error: This expression is not an allocation site.
|}]

type r = {x : string}

let f = ref (stack_ {x = "hello"})
[%%expect{|
type r = { x : string; }
Line 3, characters 20-33:
3 | let f = ref (stack_ {x = "hello"})
                        ^^^^^^^^^^^^^
Error: This allocation cannot be on the stack.
|}]

let f = ignore_local (stack_ {x = "hello"})
[%%expect{|
val f : unit = ()
|}]

type r = {x : float; y : string}

let f (r : r) = ref (stack_ r.x)
[%%expect{|
type r = { x : float; y : string; }
Line 3, characters 28-31:
3 | let f (r : r) = ref (stack_ r.x)
                                ^^^
Error: This expression is not an allocation site.
|}]

type r = {x : float; y : float}
let f (r : r) = ref (stack_ r.x)
[%%expect{|
type r = { x : float; y : float; }
Line 2, characters 28-31:
2 | let f (r : r) = ref (stack_ r.x)
                                ^^^
Error: This allocation cannot be on the stack.
|}]

let f (r : r) = ignore_local (stack_ r.x) [@nontail]
[%%expect{|
val f : r -> unit = <fun>
|}]

let f = ref (stack_ [| 42; 56 |])
[%%expect{|
Line 1, characters 20-32:
1 | let f = ref (stack_ [| 42; 56 |])
                        ^^^^^^^^^^^^
Error: This allocation cannot be on the stack.
|}]

let f = ignore_local (stack_ [| 42; 56 |])
[%%expect{|
val f : unit = ()
|}]

(* tail-position stack_ does not indicate local-returning *)
let f () = stack_ (3, 5)
[%%expect{|
Line 1, characters 18-24:
1 | let f () = stack_ (3, 5)
                      ^^^^^^
Error: This allocation cannot be on the stack.
|}]

let f () = exclave_ stack_ (3, 5)
[%%expect{|
val f : unit -> local_ int * int = <fun>
|}]

let f () =
    let g = stack_ (fun x -> x) in
    g 42
[%%expect{|
Line 3, characters 4-5:
3 |     g 42
        ^
Error: This value escapes its region.
  Hint: This function cannot be local,
  because it is the function in a tail call.
|}]

let f () =
    (stack_ (fun x -> x)) 42
[%%expect{|
Line 2, characters 12-24:
2 |     (stack_ (fun x -> x)) 42
                ^^^^^^^^^^^^
Error: This allocation cannot be on the stack.
  Hint: This function cannot be stack-allocated,
  because it is the function in a tail call.
|}]

let f () =
    List.length (stack_ [1; 2; 3])
[%%expect{|
Line 2, characters 24-33:
2 |     List.length (stack_ [1; 2; 3])
                            ^^^^^^^^^
Error: This allocation cannot be on the stack.
  Hint: This argument cannot be stack-allocated,
  because it is an argument in a tail call.
|}]

(* Allocations that are not supported for stack *)
let f () = stack_ [i for i = 0 to 9]
[%%expect{|
Line 1, characters 18-36:
1 | let f () = stack_ [i for i = 0 to 9]
                      ^^^^^^^^^^^^^^^^^^
Error: Stack allocating list comprehensions is unsupported yet.
|}]

let f () = stack_ [|i for i = 0 to 9|]
[%%expect{|
Line 1, characters 18-38:
1 | let f () = stack_ [|i for i = 0 to 9|]
                      ^^^^^^^^^^^^^^^^^^^^
Error: Stack allocating array comprehensions is unsupported yet.
|}]

class foo cla = object end

let f () = stack_ (new cla)
[%%expect{|
class foo : 'a -> object  end
Line 3, characters 23-26:
3 | let f () = stack_ (new cla)
                           ^^^
Error: Unbound class cla
|}]

class foo cla = object method bar = stack_ {< >} end
[%%expect{|
Line 1, characters 43-48:
1 | class foo cla = object method bar = stack_ {< >} end
                                               ^^^^^
Error: Stack allocating objects is unsupported yet.
|}]

let f() = stack_ (object end)
[%%expect{|
Line 1, characters 17-29:
1 | let f() = stack_ (object end)
                     ^^^^^^^^^^^^
Error: Stack allocating objects is unsupported yet.
|}]

let f() = stack_ (lazy "hello")
[%%expect{|
Line 1, characters 17-31:
1 | let f() = stack_ (lazy "hello")
                     ^^^^^^^^^^^^^^
Error: Stack allocating lazy expressions is unsupported yet.
|}]

module M = struct end
module type S = sig end

let f() = stack_ (module M : S)
[%%expect{|
module M : sig end
module type S = sig end
Line 4, characters 17-31:
4 | let f() = stack_ (module M : S)
                     ^^^^^^^^^^^^^^
Error: Stack allocating modules is unsupported yet.
|}]

(* stack_ works shallowly *)
let f () =
  let r = ref "hello" in
  let _ = stack_ (r.contents, r.contents) in
  r.contents
[%%expect{|
val f : unit -> string = <fun>
|}]

let f () =
  let r = "hello" in
  let _ = stack_ (r, r) in
  r
[%%expect{|
val f : unit -> string = <fun>
|}]

type t = { x : int list; y : int list @@ global }

let mk () =
  let r = stack_ { x = [1;2;3]; y = [4;5;6] } in
  r.y
[%%expect{|
type t = { x : int list; global_ y : int list; }
val mk : unit -> int list = <fun>
|}]

let mk () =
  let r = stack_ { x = [1;2;3]; y = [4;5;6] } in
  r.x
[%%expect{|
Line 3, characters 2-5:
3 |   r.x
      ^^^
Error: This value escapes its region.
  Hint: Cannot return a local value without an "exclave_" annotation.
|}]
