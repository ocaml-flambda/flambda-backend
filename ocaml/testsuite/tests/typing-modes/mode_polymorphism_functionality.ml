(* TEST
 flags += "-extension unique -extension mode_polymorphism_alpha";
 expect;
*)

(*
 * This file tests that mode polymorphism works, without printing mode variables.
 * The modes printed are not always representative of the underlying modes: they have
 * been zapped to legacy
*)

(* BASIC POLYMORPHISM *)

let foo =
  let foo x = x in
  let x = ref 42 in
  let x = foo x in
  let (y @ contended) = ref !x in
  let _ = foo y in
  foo
[%%expect{|
val foo : unique_ '_weak1 @ portable -> '_weak1 @ 'm = <fun>
|}]

let id x = x
[%%expect{|
val id : 'a @ [< 'm] -> 'a @ [> 'm] = <fun>
|}]

let () =
  let x = ref 42 in
  let x = id x in
  let (y @ contended) = ref !x in
  let _ = id y in
  ()
[%%expect{|
|}]

let bar (unique_ c) (f : _ @ unique -> unit) =
  f (id c)
[%%expect{|
val bar :
  'a @ [< global many unique uncontended] ->
  (unique_ 'a -> unit) @ 'n -> unit @ 'm = <fun>
|}]

let bar (local_ c) (f : _ @ local -> unit) =
  let _ = f (id c) in
  ()
[%%expect{|
val bar :
  'a @ [< many uncontended > local aliased] ->
  (local_ 'a -> unit) @ 'n -> unit @ 'm = <fun>
|}]

let bar (x @ aliased) (f : _ @ unique -> unit) =
  let x = id x in
  let _ = f x in
  ()
[%%expect{|
Line 3, characters 12-13:
3 |   let _ = f x in
                ^
Error: This value is "aliased" but expected to be "unique".
|}]

(* mode variables used as portable imposes a portable bound on them *)
let use_portable (_ @ portable) = ()
let foo x = use_portable x
[%%expect{|
val use_portable : 'a @ [< portable] -> unit @ 'm = <fun>
val foo : 'a @ [< portable] -> unit @ 'm = <fun>
|}]

let foo x =
  let bar = fun () -> x in
  use_portable bar
[%%expect{|
val foo : 'a @ [< global portable] -> unit @ 'm = <fun>
|}]

(* REFERENCES *)

(* references are not mode polymorphic *)
let alloc x = ref x
[%%expect{|
val alloc :
  'a @ [< global many uncontended] -> 'a ref @ [> aliased nonportable] =
  <fun>
|}]

let foo (f : _ @ unique -> unit) =
  f (alloc 42)
[%%expect{|
Line 2, characters 4-14:
2 |   f (alloc 42)
        ^^^^^^^^^^
Error: This value is "aliased" but expected to be "unique".
|}]

let foo (f : _ @ uncontended -> unit) =
  f (alloc 42)
[%%expect{|
val foo : (int ref -> unit) @ 'n -> unit @ 'm = <fun>
|}]

let foo x (f : _ @ unique -> unit) =
  let y = ref x in
  let x = !y in
  f x
[%%expect{|
Line 4, characters 4-5:
4 |   f x
        ^
Error: This value is "aliased" but expected to be "unique".
|}]


(* RECORDS AND MODALITIES *)

type 'a myrec1 = { x : 'a }
[%%expect{|
type 'a myrec1 = { x : 'a; }
|}]

let foo =
  let r = { x = fun x -> x } in
  let _ = { x = local_ (fun x -> x) } in
  r.x
[%%expect{|
val foo : 'a @ [< 'm] -> 'a @ [> 'm] = <fun>
|}]

type ('a, 'b) myrec2 = { i : 'a; p : 'b @@ portable }
[%%expect{|
type ('a, 'b) myrec2 = { i : 'a; p : 'b @@ portable; }
|}]

let foo =
  let (c @ contended) = ref 42 in
  let f = fun () -> () in
  let _ = { i = c; p = f } in
  let r = { i = ref 42; p = f } in
  !(r.i)
[%%expect{|
val foo : int = 42
|}]

(* the p field is still polymorphic in the non-portability axes *)
let () =
    let r = { i = (); p = fun () -> () } in
    let _ = { i = (); p = local_ (fun x -> x) } in
    r.i
[%%expect{|
|}]

(* mutable record fields are not polymorphic *)
type 'a myrec3 = { mutable y : 'a }
let foo =
  let r = { y = fun x -> x } in
  let _ = { y = local_ (fun x -> x) } in
  r.y
[%%expect{|
type 'a myrec3 = { mutable y : 'a; }
Line 4, characters 16-35:
4 |   let _ = { y = local_ (fun x -> x) } in
                    ^^^^^^^^^^^^^^^^^^^
Error: This value escapes its region.
|}]

(* PRODUCTS *)

(* currying has an effect on the following signature -- see [CURRYING] below *)
let prod x y = (x, y)
[%%expect{|
val prod :
  'a @ [< global many uncontended > aliased] ->
  'b @ [< global many uncontended] -> 'a * 'b @ [> aliased nonportable] =
  <fun>
|}]

let dupl x = (x, x)
[%%expect{|
val dupl :
  'a @ [< global many uncontended] -> 'a * 'a @ [> aliased nonportable] =
  <fun>
|}]

let use_portable (_x @ portable contended) = ()
[%%expect{|
val use_portable : 'a @ [< portable > contended] -> unit @ 'm = <fun>
|}]

(* CR ageorges: even without currying, products appear to enforce bounds on input/output *)
let foo =
    let p : unit -> int @@ portable = fun () -> 2 in
    let x = dupl p in
    let snd (_, a) = a in
    use_portable (snd x)
[%%expect{|
Line 5, characters 17-24:
5 |     use_portable (snd x)
                     ^^^^^^^
Error: This value is "nonportable" but expected to be "portable".
|}]

(* CURRYING *)

let curry x y = x
[%%expect{|
val curry :
  'a @ [< 'm & global many > aliased] -> 'b @ 'n -> 'a @ [> 'm | aliased] =
  <fun>
|}]

(* currying imposes bounds on the first polymorphic mode variable to be
[< global many > aliased]. The following three tests display those bounds *)

let () =
  let c = local_ ref 42 in
  curry c
[%%expect{|
Line 3, characters 8-9:
3 |   curry c
            ^
Error: This value escapes its region.
|}]

let bar (once_ x) =
  curry x
[%%expect{|
Line 2, characters 8-9:
2 |   curry x
            ^
Error: This value is "once" but expected to be "many".
|}]

let bar (unique_ x) (f : _ @ unique -> unit) =
  let x = curry x () in
  f x
[%%expect{|
Line 3, characters 4-5:
3 |   f x
        ^
Error: This value is "aliased" but expected to be "unique".
|}]

(* The returned closure is nonportable *)

let () =
  let c = ref 42 in
  let f = fun () -> !c in
  let bar1 = curry f in
  let _ : unit -> unit @@ portable = fun () -> let _ = bar1 () in () in
  ()
[%%expect{|
Line 5, characters 55-59:
5 |   let _ : unit -> unit @@ portable = fun () -> let _ = bar1 () in () in
                                                           ^^^^
Error: The value "bar1" is nonportable, so cannot be used inside a function that is portable.
|}]

(* CLOSING OVER POLYMORPHIC MODE VARIABLES *)

(* if [curry] is eta-expanded, we observe different bounds:
  [< 'm & global many] -> (_ -> [> 'm | aliased]) [> nonportable]. *)

let curry x = fun y -> x
[%%expect{|
val curry :
  'a @ [< 'm & global many] ->
  ('b @ 'n -> 'a @ [> 'm | aliased]) @ [> nonportable] = <fun>
|}]

(* x is < global *)
let () =
  let c = local_ ref 42 in
  curry c
[%%expect{|
Line 3, characters 8-9:
3 |   curry c
            ^
Error: This value escapes its region.
|}]

(* x is < many *)
let bar (once_ x) =
  curry x
[%%expect{|
Line 2, characters 8-9:
2 |   curry x
            ^
Error: This value is "once" but expected to be "many".
|}]

(* return value is > aliased *)
let bar (unique_ x) (f : _ @ unique -> unit) =
  let x = curry x () in
  f x
[%%expect{|
Line 3, characters 4-5:
3 |   f x
        ^
Error: This value is "aliased" but expected to be "unique".
|}]

(* returned value matches input portability *)
let var (x @ portable) (f : _ @ portable -> unit) =
  let x = curry x () in
  f x
[%%expect{|
val var :
  'a @ [< global many portable uncontended > aliased] ->
  ('a @ portable -> unit) @ 'n -> unit @ 'm = <fun>
|}]
let var (x @ nonportable) =
  curry x ()
[%%expect{|
val var :
  'a @ [< global many uncontended > nonportable] ->
  'a @ [> aliased nonportable] = <fun>
|}]

(* returned value matches input contention *)
let var (x @ uncontended) (f : _ @ uncontended -> unit) =
  let x = curry x () in
  f x
[%%expect{|
val var :
  'a @ [< global many uncontended > aliased] ->
  ('a -> unit) @ 'n -> unit @ 'm = <fun>
|}]
let var (x @ contended) =
  curry x ()
[%%expect{|
val var :
  'a @ [< global many portable > contended] -> 'a @ [> aliased contended] =
  <fun>
|}]

(* The returned closure is nonportable *)
let () =
  let c = ref 42 in
  let f = fun () -> !c in
  let bar1 = curry f in
  let _ : unit -> unit @@ portable = fun () -> let _ = bar1 () in () in
  ()
[%%expect{|
Line 5, characters 55-59:
5 |   let _ : unit -> unit @@ portable = fun () -> let _ = bar1 () in () in
                                                           ^^^^
Error: The value "bar1" is nonportable, so cannot be used inside a function that is portable.
|}]
