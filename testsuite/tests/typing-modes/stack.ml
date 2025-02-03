(* TEST
flags += "-extension comprehensions";
stack-allocation;
expect;
*)

let ignore_local : 'a @ local -> unit = fun _ -> ()
[%%expect{|
val ignore_local : local_ 'a -> unit = <fun>
|}]

let f = ref (stack_ fun x -> x)
[%%expect{|
Line 1, characters 12-31:
1 | let f = ref (stack_ fun x -> x)
                ^^^^^^^^^^^^^^^^^^^
Error: This value escapes its region.
|}]

let f = ref (stack_ (42, 42))
[%%expect{|
Line 1, characters 12-29:
1 | let f = ref (stack_ (42, 42))
                ^^^^^^^^^^^^^^^^^
Error: This value escapes its region.
|}]

let f () =
  let g = stack_ ((42, 42) : _ @@ global ) in
  ()
[%%expect{|
Line 2, characters 17-42:
2 |   let g = stack_ ((42, 42) : _ @@ global ) in
                     ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This allocation cannot be on the stack.
|}]

let f () =
  let g = ref (stack_ ((42, 42) : _ @@ global )) in
  ()
[%%expect{|
Line 2, characters 22-47:
2 |   let g = ref (stack_ ((42, 42) : _ @@ global )) in
                          ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This allocation cannot be on the stack.
|}]

let f () =
  let g = stack_ (fun x y -> x : 'a -> 'a -> 'a) in
  ()
[%%expect{|
Line 2, characters 17-48:
2 |   let g = stack_ (fun x y -> x : 'a -> 'a -> 'a) in
                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This allocation cannot be on the stack.
|}]

let f () =
  let g = ref (stack_ (fun x y -> x : 'a -> 'a -> 'a)) in
  ()
[%%expect{|
Line 2, characters 22-53:
2 |   let g = ref (stack_ (fun x y -> x : 'a -> 'a -> 'a)) in
                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This allocation cannot be on the stack.
|}]


let f = ref (stack_ (2, 3))
[%%expect{|
Line 1, characters 12-27:
1 | let f = ref (stack_ (2, 3))
                ^^^^^^^^^^^^^^^
Error: This value escapes its region.
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
Line 1, characters 12-29:
1 | let f = ref (stack_ (Bar 42))
                ^^^^^^^^^^^^^^^^^
Error: This value escapes its region.
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
Line 1, characters 12-30:
1 | let f = ref (stack_ (`Bar 42))
                ^^^^^^^^^^^^^^^^^^
Error: This value escapes its region.
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
Line 3, characters 12-34:
3 | let f = ref (stack_ {x = "hello"})
                ^^^^^^^^^^^^^^^^^^^^^^
Error: This value escapes its region.
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
Line 2, characters 20-32:
2 | let f (r : r) = ref (stack_ r.x)
                        ^^^^^^^^^^^^
Error: This value escapes its region.
|}]

let f (r : r) = ignore_local (stack_ r.x) [@nontail]
[%%expect{|
val f : r -> unit = <fun>
|}]

let f = ref (stack_ [| 42; 56 |])
[%%expect{|
Line 1, characters 12-33:
1 | let f = ref (stack_ [| 42; 56 |])
                ^^^^^^^^^^^^^^^^^^^^^
Error: This value escapes its region.
|}]

let f = ignore_local (stack_ [| 42; 56 |])
[%%expect{|
val f : unit = ()
|}]

(* tail-position stack_ does not indicate local-returning *)
let f () = stack_ (3, 5)
[%%expect{|
Line 1, characters 11-24:
1 | let f () = stack_ (3, 5)
               ^^^^^^^^^^^^^
Error: This value escapes its region.
  Hint: Cannot return a local value without an "exclave_" annotation.
|}]

let f () = exclave_ stack_ (3, 5)
[%%expect{|
val f : unit -> local_ int * int = <fun>
|}]

let f () =
    let g = stack_ fun x -> x in
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
    (stack_ fun x -> x) 42
[%%expect{|
Line 2, characters 4-23:
2 |     (stack_ fun x -> x) 42
        ^^^^^^^^^^^^^^^^^^^
Error: This value escapes its region.
  Hint: This function cannot be local,
  because it is the function in a tail call.
|}]

let f () =
    List.length (stack_ [1; 2; 3])
[%%expect{|
Line 2, characters 16-34:
2 |     List.length (stack_ [1; 2; 3])
                    ^^^^^^^^^^^^^^^^^^
Error: This value escapes its region.
  Hint: This argument cannot be local,
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
Error: Unbound class "cla"
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

(* Testing primitives *)
external fst : ('a * 'b[@local_opt]) -> ('a[@local_opt]) = "%field0_immut"
external ref : 'a -> ('a ref[@local_opt]) = "%makemutable"
external ref_heap : 'a -> 'a ref = "%makemutable"
external ref_stack : 'a -> 'a ref @ local = "%makemutable"
external id : 'a -> 'a = "%identity"
external c_func : 'a -> 'a = "foo"
[%%expect{|
external fst : ('a * 'b [@local_opt]) -> ('a [@local_opt]) = "%field0_immut"
external ref : 'a -> ('a ref [@local_opt]) = "%makemutable"
external ref_heap : 'a -> 'a ref = "%makemutable"
external ref_stack : 'a -> local_ 'a ref = "%makemutable"
external id : 'a -> 'a = "%identity"
external c_func : 'a -> 'a = "foo"
|}]

let foo () =
  let _ = stack_ (fst (42, 24)) in
  ()
[%%expect{|
Line 2, characters 17-31:
2 |   let _ = stack_ (fst (42, 24)) in
                     ^^^^^^^^^^^^^^
Error: This cannot be marked as stack_,
       because this primitive does not allocate.
|}]

let foo () =
  let _ = stack_ (c_func 52) in
  ()
[%%expect{|
Line 2, characters 17-28:
2 |   let _ = stack_ (c_func 52) in
                     ^^^^^^^^^^^
Error: This cannot be marked as stack_, because it is either not a primitive,
       or the primitive does not allocate.
|}]

let foo () =
  let _ = stack_ (ref_heap 52) in
  ()
[%%expect{|
Line 2, characters 17-30:
2 |   let _ = stack_ (ref_heap 52) in
                     ^^^^^^^^^^^^^
Error: This primitive always allocates on heap
       (maybe it should be declared with "[@local_opt]" or "local_"?)
|}]

let foo () =
  let _ = stack_ (ref_stack 52) in
  ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

let foo () =
  let _ = stack_ (ref 52) in
  ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

(* %identity, while appearing to be a primitive, doesn't translate to lambda primitive. *)
let foo () =
  let _ = stack_ (id 42) in
  ()
[%%expect{|
Line 2, characters 17-24:
2 |   let _ = stack_ (id 42) in
                     ^^^^^^^
Error: This cannot be marked as stack_, because it is either not a primitive,
       or the primitive does not allocate.
|}]
