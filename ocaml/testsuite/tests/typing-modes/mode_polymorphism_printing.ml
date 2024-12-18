(* TEST
 flags = "-extension unique -extension mode_polymorphism_alpha";
 expect;
*)

(*
 * This file tests printing of poymorphic mode variables
*)


let id x = x
[%%expect{|
val id : 'a @ [< 'm] -> 'a @ [> 'm] = <fun>
|}]

let foo x = 42
[%%expect{|
val foo : 'a @ 'n -> int @ 'm = <fun>
|}]

(* CR ageorges: Is there a way to explain the following? id is instantiated, but foo is
  generalized with more bounds that necessary? *)
let foo x = id x
[%%expect{|
val foo : 'a @ [< global many portable uncontended] -> 'a @ [> aliased] =
  <fun>
|}]

let foo f x = f x
[%%expect{|
val foo :
  ('a @ [> aliased] -> 'b @ [< global many uncontended]) @ [< global many] ->
  'a @ [< uncontended] -> 'b @ [> aliased nonportable] = <fun>
|}, Principal{|
val foo :
  ('a @ [> aliased] -> 'b @ [< global many uncontended]) @ [< global many > aliased] ->
  'a @ [< uncontended] -> 'b @ [> aliased nonportable] = <fun>
|}]

let foo =
  let id x = x in
  fun x -> id x
[%%expect{|
val foo : 'a @ [< global many portable uncontended] -> 'a @ [> aliased] =
  <fun>
|}]

(* CR ageorges: make the printer aware of mode crossing/jkinds *)
let foo a b = a + b
[%%expect{|
val foo : int @ 'o -> (int @ 'n -> int @ 'm) = <fun>
|}, Principal{|
val foo : int @ [< global many > aliased] -> int @ 'n -> int @ 'm = <fun>
|}]


(* records *)

type ('a,'b) mytypemod = { x : 'a; y : 'b @@ portable }

let foo t = t.x
[%%expect{|
type ('a, 'b) mytypemod = { x : 'a; y : 'b @@ portable; }
val foo : ('a, 'b) mytypemod @ [< 'm] -> 'a @ [> 'm] = <fun>
|}]

let foo t = t.y
[%%expect{|
val foo : ('a, 'b) mytypemod @ [< 'm] -> 'b @ [> 'm mod portable] = <fun>
|}]

let x =
  let foo x = x in
  let _ @ contended = foo (ref 42 : _ @@ contended ) in
  let _ @ uncontended = foo  (ref 41 : _ @@ uncontended) in
  foo
[%%expect{|
val x : '_weak1 -> '_weak1 @ [> aliased nonportable] = <fun>
|}]

type ('a,'b) mytype = { x : 'a; y : 'b }
[%%expect{|
type ('a, 'b) mytype = { x : 'a; y : 'b; }
|}]

let foo x y = { x; y }
[%%expect{|
val foo :
  'a @ [< global many uncontended > aliased] ->
  'b @ [< global many uncontended] ->
  ('a, 'b) mytype @ [> aliased nonportable] = <fun>
|}]

let foo x = fun y -> { x; y }
[%%expect{|
val foo :
  'a @ [< global many uncontended] ->
  ('b @ [< global many uncontended] ->
   ('a, 'b) mytype @ [> aliased nonportable]) @ [> nonportable] =
  <fun>
|}]

let foo x = { x; y = 42 }
[%%expect{|
val foo :
  'a @ [< global many uncontended] ->
  ('a, int) mytype @ [> aliased nonportable] = <fun>
|}]

let foo r = { r with y = 42 }
[%%expect{|
val foo :
  ('a, 'b) mytype @ [< global many uncontended] ->
  ('a, int) mytype @ [> aliased nonportable] = <fun>
|}]

type 'a myref = { mutable x : 'a }
[%%expect{|
type 'a myref = { mutable x : 'a; }
|}]

let create a = { x = a }
[%%expect{|
val create :
  'a @ [< global many uncontended] -> 'a myref @ [> aliased nonportable] =
  <fun>
|}]

let read r = r.x
[%%expect{|
val read :
  'a myref @ [< 'm & shared] -> 'a @ [> 'm mod global many | aliased] = <fun>
|}]

let store r = fun a -> r.x <- a
[%%expect{|
val store :
  'a myref @ [< global many uncontended] ->
  ('a @ [< global many uncontended] -> unit @ 'm) @ [> nonportable] = <fun>
|}]

(* products *)

let dupl x = (x, x)
[%%expect{|
val dupl :
  'a @ [< global many uncontended] -> 'a * 'a @ [> aliased nonportable] =
  <fun>
|}]

let prod x y = (x, y)
[%%expect{|
val prod :
  'a @ [< global many uncontended > aliased] ->
  'b @ [< global many uncontended] -> 'a * 'b @ [> aliased nonportable] =
  <fun>
|}]

let prod_eta x = fun y -> (x, y)
[%%expect{|
val prod_eta :
  'a @ [< global many uncontended] ->
  ('b @ [< global many uncontended] -> 'a * 'b @ [> aliased nonportable]) @ [> nonportable] =
  <fun>
|}]

let fst (a, _) = a
let snd (_, b) = b
[%%expect{|
val fst : 'a * 'b @ [< 'm] -> 'a @ [> 'm] = <fun>
val snd : 'a * 'b @ [< 'm] -> 'b @ [> 'm] = <fun>
|}]

let foo x = fun y ->
  let x' = fst (x,y) in
  let y' = snd (x,y) in
  (x', y')
[%%expect{|
val foo :
  'a @ [< global many uncontended] ->
  ('b @ [< global many uncontended] -> 'a * 'b @ [> aliased nonportable]) @ [> nonportable] =
  <fun>
|}]

(* currying *)

let foo x y = x
[%%expect{|
val foo :
  'a @ [< 'm & global many > aliased] -> 'b @ 'n -> 'a @ [> 'm | aliased] =
  <fun>
|}]

let foo x y = y
[%%expect{|
val foo : 'a @ [< global many > aliased] -> 'b @ [< 'm] -> 'b @ [> 'm] =
  <fun>
|}]

let foo f = fun x -> fun y -> f x y
[%%expect{|
val foo :
  ('a @ [> aliased] ->
   ('b @ [> aliased] -> 'c @ [< global many uncontended]) @ 'm) @ [< global many] ->
  ('a @ [< global many uncontended] ->
   ('b @ [< uncontended] -> 'c @ [> aliased nonportable]) @ [> nonportable]) @ [> nonportable] =
  <fun>
|}]

let fst x = fun y -> x
[%%expect{|
val fst :
  'a @ [< 'm & global many] ->
  ('b @ 'n -> 'a @ [> 'm | aliased]) @ [> nonportable] = <fun>
|}]
let snd x = fun y -> y
[%%expect{|
val snd : 'a @ 'o -> ('b @ [< 'm] -> 'b @ [> 'm]) @ 'n = <fun>
|}]

let foo x y = ref x
[%%expect{|
val foo :
  'a @ [< global many uncontended > aliased] ->
  'b @ 'm -> 'a ref @ [> aliased nonportable] = <fun>
|}]

let foo (x @ aliased) y = ref x
[%%expect{|
val foo :
  'a @ [< global many uncontended > aliased] ->
  'b @ 'm -> 'a ref @ [> aliased nonportable] = <fun>
|}]

let foo (x @ contended) y = x
[%%expect{|
val foo :
  'a @ [< 'm & global many > aliased contended] ->
  'b @ 'n -> 'a @ [> 'm | aliased contended] = <fun>
|}]

let foo x y z = 42
[%%expect{|
val foo :
  'a @ [< global many > aliased] ->
  'b @ [< global many > aliased] -> 'c @ 'n -> int @ 'm = <fun>
|}]

let foo x y = (x, y)
[%%expect{|
val foo :
  'a @ [< global many uncontended > aliased] ->
  'b @ [< global many uncontended] -> 'a * 'b @ [> aliased nonportable] =
  <fun>
|}]

let foo x y z = (y,z)
[%%expect{|
val foo :
  'a @ [< global many > aliased] ->
  'b @ [< global many uncontended > aliased] ->
  'c @ [< global many uncontended] -> 'b * 'c @ [> aliased nonportable] =
  <fun>
|}]

(* annotations *)

(* CR ageorges: if a mode variable is fully determined (its bounds are equal) consider
  printing it as a constant rather than variable *)
let legacy_id (x @ global many aliased nonportable uncontended) = x
[%%expect{|
val legacy_id :
  'a @ [< global many uncontended > aliased nonportable] ->
  'a @ [> aliased nonportable] = <fun>
|}]

let foo (local_ x) = x
[%%expect{|
val foo : 'a @ [< 'm > local] -> 'a @ [> 'm | local] = <fun>
|}]

let foo x = exclave_ x
[%%expect{|
val foo : 'a @ [< 'm] -> 'a @ [> 'm mod global | local] = <fun>
|}]

let foo (x @ portable) = x
[%%expect{|
val foo : 'a @ [< 'm & portable] -> 'a @ [> 'm] = <fun>
|}]

let foo : unit -> unit @@ portable = fun () -> ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

let foo (unique_ y) (z @ portable) = z
[%%expect{|
val foo : 'a @ [< global unique] -> 'b @ [< 'm & portable] -> 'b @ [> 'm] =
  <fun>
|}]

let foo (local_ x) (unique_ y) (z @ portable) = exclave_ (x, y, z)
[%%expect{|
val foo :
  'a @ [< many uncontended > local aliased] ->
  'b @ [< many unique uncontended] ->
  'c @ [< many portable uncontended] ->
  'a * 'b * 'c @ [> local aliased nonportable] = <fun>
|}]

(* if a type is annotated, mode crossing has an effect on the bounds of mode variable *)

type intref = { mutable v : int }

let foo (x : intref) (f : intref @ local -> int) = f x
[%%expect{|
type intref = { mutable v : int; }
val foo :
  intref @ [< global many uncontended > aliased] ->
  (local_ intref -> int) @ 'n -> int @ 'm = <fun>
|}]

(* CR ageorges: ideally we want to apply mode crossing reguardless of principality *)
let foo (f : int -> int) x y = f
[%%expect{|
val foo :
  (int -> int) @ [< global many] ->
  'a @ [< global many > aliased] -> 'b @ 'n -> (int -> int) @ 'm = <fun>
|}, Principal{|
val foo :
  (int -> int) @ [< global many > aliased] ->
  'a @ [< global many > aliased] -> 'b @ 'n -> (int -> int) @ 'm = <fun>
|}]

let foo (f : intref @ local -> int) (x : intref) (y : intref) = f x
[%%expect{|
val foo :
  (local_ intref -> int) @ [< global many] ->
  intref @ [< global many uncontended > aliased] -> intref @ 'n -> int @ 'm =
  <fun>
|}, Principal{|
val foo :
  (local_ intref -> int) @ [< global many > aliased] ->
  intref @ [< global many uncontended > aliased] -> intref @ 'n -> int @ 'm =
  <fun>
|}]

(* aliases of non-polymorphic functions *)

let map = List.map
[%%expect{|
val map : ('a -> 'b) -> 'a list -> 'b list = <fun>
|}]

let map f l = List.map f l
[%%expect{|
val map :
  ('a @ [> aliased nonportable] -> 'b @ [< global many uncontended]) @ [< global many] ->
  'a list @ [< global many uncontended] -> 'b list @ [> aliased nonportable] =
  <fun>
|}, Principal{|
val map :
  ('a @ [> aliased nonportable] -> 'b @ [< global many uncontended]) @ [< global many > aliased] ->
  'a list @ [< global many uncontended] -> 'b list @ [> aliased nonportable] =
  <fun>
|}]

let map_eta f = fun l -> List.map f l
[%%expect{|
val map_eta :
  ('a @ [> aliased nonportable] -> 'b @ [< global many uncontended]) @ [< global many] ->
  ('a list @ [< global many uncontended] -> 'b list @ [> aliased nonportable]) @ [> nonportable] =
  <fun>
|}]

(* modules *)

 module Counter : sig
  type t

  val incr : t -> t

  val to_int : t -> int
end = struct
  type t = int

  let incr n = n + 1

  let to_int = fun n -> n
 end
 [%%expect{|
module Counter : sig type t val incr : t -> t val to_int : t -> int end
|}]

let incr n = Counter.incr n
[%%expect{|
val incr :
  Counter.t @ [< global many uncontended] ->
  Counter.t @ [> aliased nonportable] = <fun>
|}]

let incr = Counter.incr
[%%expect{|
val incr : Counter.t -> Counter.t = <fun>
|}]

let incr n = n + 1
[%%expect{|
val incr : int @ 'n -> int @ 'm = <fun>
|}]

let id x = x
[%%expect{|
val id : 'a @ [< 'm] -> 'a @ [> 'm] = <fun>
|}]

module Foo : sig
  type t

  val id_portable : t @ portable -> t @ portable

  val id_nonportable : t -> t

  val bar : t @ portable -> t
end = struct
  type t = unit -> unit

  let id_portable = id

  let id_nonportable = id

  let bar = id
end
[%%expect{|
module Foo :
  sig
    type t
    val id_portable : t @ portable -> t @ portable
    val id_nonportable : t -> t
    val bar : t @ portable -> t
  end
|}]

(* CR ageorges: remove duplicates in [< 'm & 'm] and [> 'm | 'm] *)
module Foo : sig
  type t

  val illegal : t -> t @ portable
end = struct
  type t = unit -> unit

  let illegal = id
end
[%%expect{|
Lines 5-9, characters 6-3:
5 | ......struct
6 |   type t = unit -> unit
7 |
8 |   let illegal = id
9 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           type t = unit -> unit
           val illegal : 'a @ [< 'm] -> 'a @ [> 'm]
         end
       is not included in
         sig type t val illegal : t -> t @ portable end
       Values do not match:
         val illegal : 'a @ [< 'm] -> 'a @ [> 'm]
       is not included in
         val illegal : t -> t @ portable
       The type
         "t @ [< 'm & 'm & 'm & 'm > nonportable] ->
         t @ [> 'm | 'm | 'm | 'm | nonportable]"
       is not compatible with the type "t -> t @ portable"
|}]
