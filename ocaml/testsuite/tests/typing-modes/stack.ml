(* TEST
expect;
*)

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

type t = Foo | Bar of int

let f = ref (stack_ Foo)
[%%expect{|
type t = Foo | Bar of int
Line 3, characters 20-23:
3 | let f = ref (stack_ Foo)
                        ^^^
Error: This expression is not an allocation.
|}]

let f = ref (stack_ (Bar 42))
[%%expect{|
Line 1, characters 20-28:
1 | let f = ref (stack_ (Bar 42))
                        ^^^^^^^^
Error: This allocation cannot be on the stack.
|}]

let f = ref (stack_ `Foo)
[%%expect{|
Line 1, characters 20-24:
1 | let f = ref (stack_ `Foo)
                        ^^^^
Error: This expression is not an allocation.
|}]

let f = ref (stack_ (`Bar 42))
[%%expect{|
Line 1, characters 20-29:
1 | let f = ref (stack_ (`Bar 42))
                        ^^^^^^^^^
Error: This allocation cannot be on the stack.
|}]

type r = {x : string} [@@unboxed]

let f = ref (stack_ {x = "hello"})
[%%expect{|
type r = { x : string; } [@@unboxed]
Line 3, characters 20-33:
3 | let f = ref (stack_ {x = "hello"})
                        ^^^^^^^^^^^^^
Error: This expression is not an allocation.
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

type r = {x : float; y : string}

let f (r : r) = ref (stack_ r.x)
[%%expect{|
type r = { x : float; y : string; }
Line 3, characters 28-31:
3 | let f (r : r) = ref (stack_ r.x)
                                ^^^
Error: This expression is not an allocation.
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

let f = ref (stack_ [| 42; 56 |])
[%%expect{|
Line 1, characters 20-32:
1 | let f = ref (stack_ [| 42; 56 |])
                        ^^^^^^^^^^^^
Error: This allocation cannot be on the stack.
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
