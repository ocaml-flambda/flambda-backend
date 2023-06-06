(* TEST
 * expect *)

(* typing tests *)

let escape x =
  let _ = ref x in
  ()
[%%expect{|
val escape : 'a -> unit = <fun>
|}]  

(* Any function ending with exclave is always typed to 
   return local value. 
  This is to accomadate some code in compiler that relies 
  on the function's type to know if it allocates in caller's 
  region. *)
let foo () = 
  [%exclave] (
    let local_ y = Some 42 in 
    y
  )
[%%expect{|
val foo : unit -> local_ int option = <fun>
|}]
(* sidenote: in the above, 
   y escapes the function even though local_
   - indeed y is in the outer region!
   *)

(* Of course this applies even when the exclave returns a global value,
  because it might still allocate in outer region
    *)
let foo () = 
  [%exclave] (
    let local_ _y = Some 42 in 
    let x = Some 42 in 
    let _ = escape x in 
    x
  )
[%%expect{|
val foo : unit -> local_ int option = <fun>
|}]

(* this still applies even when the exclave doesn't allocate in outer region at all,
  because I don't know any reliable mechanisms in type checker to do that. 
  So it's better to be safe and say that "it might allocate in outer region". *)
let foo x =
  [%exclave] (
    let x = Some 42 in 
    let _ = escape x in
    x
  )
[%%expect{|
val foo : 'a -> local_ int option = <fun>
|}]


(* Below we do some usual testing  *)
let foo x =
  [%exclave] (
    let local_ y = None in
    (* y is not global *)
    ref y
  )
[%%expect{|
Line 5, characters 8-9:
5 |     ref y
            ^
Error: This value escapes its region
|}]

(* following we check error detection *)
let foo x =
  let local_ y = "foo" in
  [%exclave] (Some y)
[%%expect{|
Line 3, characters 19-20:
3 |   [%exclave] (Some y)
                       ^
Error: The value y is local, so cannot be used inside exclave
|}]

let foo x =
  let local_ y = "foo" in
  let z = [%exclave] (Some y) in
  z
[%%expect{|
Line 3, characters 10-29:
3 |   let z = [%exclave] (Some y) in
              ^^^^^^^^^^^^^^^^^^^
Error: Exclave expression should only be in tail position of the current region
|}]

(* following we test WHILE loop *)
(* exclave in loop is allowed*)
let foo () =
  while true do
    [%exclave] ()
  done

[%%expect{|
val foo : unit -> unit = <fun>
|}]

(* we also require tail position *)
let foo () =
  while true do
    [%exclave] ();
    ()
  done
[%%expect{|
Line 3, characters 4-17:
3 |     [%exclave] ();
        ^^^^^^^^^^^^^
Error: Exclave expression should only be in tail position of the current region
|}]

(* following we test FOR loop *)
let foo () =
  for i = 1 to 42 do
    [%exclave] ()
  done
[%%expect{|
val foo : unit -> unit = <fun>
|}]

let foo () =
  for i = 1 to 42 do
    [%exclave] ();
    ()
  done
[%%expect{|
Line 3, characters 4-17:
3 |     [%exclave] ();
        ^^^^^^^^^^^^^
Error: Exclave expression should only be in tail position of the current region
|}]

type t = { nonlocal_ x : int option }

let foo (local_ x) =
  let __ = { x } in
  [%exclave] x

[%%expect{|
type t = { nonlocal_ x : int option; }
val foo : local_ int option -> local_ int option = <fun>
|}]

let foo (local_ x) =
  let _ = { x } in
  [%exclave] { x }

[%%expect{|
Line 3, characters 15-16:
3 |   [%exclave] { x }
                   ^
Error: This local value escapes its region
|}]

(* semantics tests *)

type data = {mutable a : string}

let foo () =
  let local_ x : data = {a  = "foo"} in
  let local_ z = "hello" in
  x.a <- z;
  while true do
    [%exclave] (
      let local_ y = "hello" in
      x.a <- y
    )
  done

[%%expect{|
type data = { mutable a : string; }
Line 6, characters 9-10:
6 |   x.a <- z;
             ^
Error: This value escapes its region
|}]

let foo x =
  [%exclave] (
    let local_ y = Some x in
    y
  );;

let bar _ =
  match foo 5 with
  | None -> "None!"
  | Some x -> "Some of " ^ (string_of_int (x + 0)) ;;
bar ();;
[%%expect{|
val foo : 'a -> local_ 'a option = <fun>
val bar : 'a -> string = <fun>
- : string = "Some of 5"
|}]

(* Ensure that Alias bindings are not substituted by Simplif (PR1448) *)
type 'a glob = Glob of ('a[@global])

let[@inline never] return_local a = [%local] (Glob a)

let f () =
  let (Glob x) = return_local 1 in
  [%exclave]
    (let (_ : _) = return_local 99 in
     assert (x <> 99))

f ();
[%%expect{||}]

