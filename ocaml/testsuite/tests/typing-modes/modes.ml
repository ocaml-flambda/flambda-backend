(* TEST
 expect;
*)

(* Let bindings *)
let local_ foo : string @@ unique = "hello"
[%%expect{|
Line 1, characters 4-43:
1 | let local_ foo : string @@ unique = "hello"
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value escapes its region.
|}]

let local_ foo @ unique = "hello"
[%%expect{|
Line 1, characters 4-33:
1 | let local_ foo @ unique = "hello"
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value escapes its region.
|}]

let local_ foo : 'a. 'a -> 'a @@ unique = fun x -> x
[%%expect{|
Line 1, characters 4-52:
1 | let local_ foo : 'a. 'a -> 'a @@ unique = fun x -> x
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value escapes its region.
|}]

let foo : type a. a -> a @@ unique = fun x -> x
[%%expect{|
val foo : 'a -> 'a = <fun>
|}]

let (x, y) @ local unique = "hello", "world"
[%%expect{|
Line 1, characters 4-44:
1 | let (x, y) @ local unique = "hello", "world"
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value escapes its region.
|}]

let (x, y) : _ @@ local unique = "hello", "world"
[%%expect{|
Line 1, characters 4-49:
1 | let (x, y) : _ @@ local unique = "hello", "world"
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value escapes its region.
|}]

let foo @ foo = "hello"
[%%expect{|
Line 1, characters 10-13:
1 | let foo @ foo = "hello"
              ^^^
Error: Unrecognized mode foo.
|}]

let foo () =
  let bar @ local local = "hello" in
  ()
[%%expect{|
Line 2, characters 18-23:
2 |   let bar @ local local = "hello" in
                      ^^^^^
Error: The locality axis has already been specified.
|}]

(* CR zqian: this should be supported *)
(* let foo a b @ local = "hello"
let foo a b : _ @@ local = "hello" *)

(* Expressions *)
let foo = ("hello" : _ @@ local)
[%%expect{|
Line 1, characters 10-32:
1 | let foo = ("hello" : _ @@ local)
              ^^^^^^^^^^^^^^^^^^^^^^
Error: This value escapes its region.
|}]

(* this is not mode annotation *)
let foo = ("hello" @ local)
[%%expect{|
Line 1, characters 11-18:
1 | let foo = ("hello" @ local)
               ^^^^^^^
Error: This expression has type "string" but an expression was expected of type
         "'a list"
|}]

(* CR zqian: Support the following in the future. Currently skipped for
  simplicity in the ocamlformat.
let foo ~bar = bar ^ "hello"

let x =
  let bar = "world" in
  foo ~(bar : _ @@ unique)
[%%expect{|
val foo : bar:string -> string = <fun>
Line 5, characters 8-11:
5 |   foo ~(bar : _ @@ unique)
            ^^^
Error: Found a aliased value where a unique value was expected
|}]

let x =
  let bar = "world" in
  foo ~(bar @ unique)
[%%expect{|
Line 3, characters 8-11:
3 |   foo ~(bar @ unique)
            ^^^
Error: Found a aliased value where a unique value was expected
|}]

type r = {a : string; b : string}

let r = {a : _ @@ unique = "hello";
         b : _ @@ once = "world"}
[%%expect{|
type r = { a : string; b : string; }
Line 4, characters 11-32:
4 |          b : _ @@ once = "world"}
               ^^^^^^^^^^^^^^^^^^^^^
Error: Found a once value where a many value was expected
|}]

let r = {a @ unique = "hello";
         b @ once = "world"}
[%%expect{|
Line 2, characters 11-27:
2 |          b @ once = "world"}
               ^^^^^^^^^^^^^^^^
Error: Found a once value where a many value was expected
|}]

let foo () =
  let bar = "hello" in
  let biz = "world" in
  ~(bar:_@@unique), ~(biz:_@@once)
[%%expect{|
Line 4, characters 4-7:
4 |   ~(bar:_@@unique), ~(biz:_@@once)
        ^^^
Error: Found a aliased value where a unique value was expected
|}]


let foo () =
  let bar = "hello" in
  let biz = "world" in
  ~(bar @ unique), ~(biz @ once)
[%%expect{|
Line 4, characters 4-7:
4 |   ~(bar @ unique), ~(biz @ once)
        ^^^
Error: Found a aliased value where a unique value was expected
|}]
*)

(* arrow types *)
type r = local_ string @ unique once -> unique_ string @ local once
[%%expect{|
type r = local_ once_ unique_ string -> local_ once_ unique_ string
|}]

type r = local_ string * y:string @ unique once -> local_ string * w:string @ once
[%%expect{|
type r =
    local_ once_ unique_ string * y:string -> local_ once_ string * w:string
|}]

type r = x:local_ string * y:string @ unique once -> local_ string * w:string @ once
[%%expect{|
type r =
    x:local_ once_ unique_ string * y:string -> local_ once_
    string * w:string
|}]


type r = local_ string @ foo -> string
[%%expect{|
Line 1, characters 25-28:
1 | type r = local_ string @ foo -> string
                             ^^^
Error: Unrecognized mode foo.
|}]

type r = local_ string @ local -> string
[%%expect{|
Line 1, characters 25-30:
1 | type r = local_ string @ local -> string
                             ^^^^^
Error: The locality axis has already been specified.
|}]

(* Mixing legacy and new modes *)
type r = local_ unique_ once_ string -> string
[%%expect{|
type r = local_ once_ unique_ string -> string
|}]

type r = local_ unique_ once_ string @ portable contended -> string
[%%expect{|
type r = local_ once_ unique_ string @ portable contended -> string
|}]

type r = string @ local unique once portable contended -> string
[%%expect{|
type r = local_ once_ unique_ string @ portable contended -> string
|}]

type r = string @ local unique once nonportable uncontended -> string
[%%expect{|
type r = local_ once_ unique_ string -> string
|}]


(* modality on constructor arguments and record fields *)

type t = Foo of string @@ global * global_ string
[%%expect{|
type t = Foo of global_ string * global_ string
|}]

type t = Foo of string @@ foo
[%%expect{|
Line 1, characters 26-29:
1 | type t = Foo of string @@ foo
                              ^^^
Error: Unrecognized modality foo.
|}]

type t = Foo of global_ string @@ global
(* CR reduced-modality: this should warn. *)
[%%expect{|
type t = Foo of global_ string
|}]

type r = {
  x : string @@ global
}
[%%expect{|
type r = { global_ x : string; }
|}]

type r = {
  x : string @@ foo
}
[%%expect{|
Line 2, characters 16-19:
2 |   x : string @@ foo
                    ^^^
Error: Unrecognized modality foo.
|}]

type r = {
  global_ x : string @@ global
}
(* CR reduced-modality: this should warn. *)
[%%expect{|
type r = { global_ x : string; }
|}]

(* Modalities don't imply each other; this will change as we add borrowing. *)
type r = {
  global_ x : string @@ aliased
}
[%%expect{|
type r = { global_ x : string @@ aliased; }
|}]

type r = {
  x : string @@ aliased global many
}
[%%expect{|
type r = { global_ x : string @@ many aliased; }
|}]

type r = {
  x : string @@ aliased global many aliased
}
(* CR reduced-modality: this should warn. *)
[%%expect{|
type r = { global_ x : string @@ many aliased; }
|}]

type r = Foo of string @@ global aliased many
[%%expect{|
type r = Foo of global_ string @@ many aliased
|}]

(* mutable implies global aliased many. No warnings are given since we imagine
   that the coupling will be removed soon. *)
type r = {
  mutable x : string @@ global aliased many
}
[%%expect{|
type r = { mutable x : string; }
|}]


(* patterns *)

let foo ?(local_ x @ unique once = 42) () = ()
[%%expect{|
val foo : ?x:local_ once_ unique_ int -> unit -> unit = <fun>
|}]

let foo ?(local_ x : _ @@ unique once = 42) () = ()
[%%expect{|
val foo : ?x:local_ once_ unique_ int -> unit -> unit = <fun>
|}]

let foo ?(local_ x : 'a. 'a -> 'a @@ unique once) = ()
[%%expect{|
Line 1, characters 10-48:
1 | let foo ?(local_ x : 'a. 'a -> 'a @@ unique once) = ()
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Optional parameters cannot be polymorphic
|}]

let foo ?x:(local_ (x,y) @ unique once = (42, 42)) () = ()
[%%expect{|
val foo : ?x:local_ once_ unique_ int * int -> unit -> unit = <fun>
|}]

let foo ?x:(local_ (x,y) : _ @@ unique once = (42, 42)) () = ()
[%%expect{|
val foo : ?x:local_ once_ unique_ int * int -> unit -> unit = <fun>
|}]

let foo ?x:(local_ (x,y) : 'a.'a->'a @@ unique once) () = ()
[%%expect{|
Line 1, characters 12-51:
1 | let foo ?x:(local_ (x,y) : 'a.'a->'a @@ unique once) () = ()
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Optional parameters cannot be polymorphic
|}]

(* let-bound function *)
(* by default, all strings following @ are parsed as modes, not argument *)
let foo () =
  let bar @ unique local once = () in
  bar
[%%expect{|
val foo : unit -> unit = <fun>
|}]

let foo () =
  let bar @ unique local foo = () in
  bar
[%%expect{|
Line 2, characters 25-28:
2 |   let bar @ unique local foo = () in
                             ^^^
Error: Unrecognized mode foo.
|}]

let foo () =
  let (bar @ local) a b = () in
  bar 42 24;
  ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

(* modalities on normal values requires [-extension mode_alpha] *)
module type S = sig
  val x : string -> string @ local @@ foo bar
end
[%%expect{|
Line 2, characters 38-41:
2 |   val x : string -> string @ local @@ foo bar
                                          ^^^
Error: The extension "mode" is disabled and cannot be used
|}]


(*
 * Modification of return modes in argument position
 *)

let use_local (f : _ -> _ -> _ @@ local) x y =
  f x y
let result = use_local (^) "hello" " world"
[%%expect{|
val use_local : local_ ('a -> 'b -> 'c) -> 'a -> 'b -> 'c = <fun>
val result : string = "hello world"
|}]

let use_local_ret (f : _ -> _ @ local) x y =
  let _ = f x in ()
let global_ret : string -> string @ global = fun x -> x
let result = use_local_ret global_ret "hello"
[%%expect{|
val use_local_ret : ('a -> local_ 'b) -> 'a -> 'c -> unit = <fun>
val global_ret : string -> string = <fun>
val result : '_weak1 -> unit = <fun>
|}]

let use_global_ret (f : _ -> _ @ global) x = lazy (f x)
let local_ret a = exclave_ (Some a)
let bad_use = use_global_ret local_ret "hello"
[%%expect{|
val use_global_ret : ('a -> 'b) -> 'a -> 'b lazy_t = <fun>
val local_ret : 'a -> local_ 'a option = <fun>
Line 3, characters 29-38:
3 | let bad_use = use_global_ret local_ret "hello"
                                 ^^^^^^^^^
Error: This expression has type 'a -> local_ 'a option
       but an expression was expected of type 'b -> 'c
|}]

let use_nonportable_ret (f : _ -> (_ -> _) @ nonportable) x y =
  f x y
let portable_ret : string -> (string -> string) @ portable =
  fun x y -> y
let result = use_nonportable_ret portable_ret "hello" " world"
[%%expect{|
val use_nonportable_ret : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c = <fun>
val portable_ret : string -> (string -> string) @ portable = <fun>
val result : string = " world"
|}]

let use_portable_ret (f : _ -> (_ -> _) @ portable) x y =
  lazy ((f x) y)
let nonportable_ret : string -> (string -> string) @ nonportable =
  fun x y -> x ^ y
let bad_use = use_portable_ret nonportable_ret "hello" " world"
[%%expect{|
val use_portable_ret : ('a -> ('b -> 'c) @ portable) -> 'a -> 'b -> 'c lazy_t =
  <fun>
val nonportable_ret : string -> string -> string = <fun>
Line 5, characters 31-46:
5 | let bad_use = use_portable_ret nonportable_ret "hello" " world"
                                   ^^^^^^^^^^^^^^^
Error: This expression has type string -> string -> string
       but an expression was expected of type 'a -> ('b -> 'c) @ portable
|}]

let use_contended_ret (f : _ -> _ @ contended) x =
  let _ = f x in ()
let uncontended_ret : string -> string @ uncontended =
  fun x -> x
let result = use_contended_ret uncontended_ret "hello"
[%%expect{|
val use_contended_ret : ('a -> 'b @ contended) -> 'a -> unit = <fun>
val uncontended_ret : string -> string = <fun>
val result : unit = ()
|}]

let use_uncontended_ret (f : _ -> _ @ uncontended) x =
  let _ = f x in ()
let contended_ret : string -> string @ contended =
  fun x -> x
let bad_use = use_uncontended_ret contended_ret "hello"
[%%expect{|
val use_uncontended_ret : ('a -> 'b) -> 'a -> unit = <fun>
val contended_ret : string -> string @ contended = <fun>
Line 5, characters 34-47:
5 | let bad_use = use_uncontended_ret contended_ret "hello"
                                      ^^^^^^^^^^^^^
Error: This expression has type string -> string @ contended
       but an expression was expected of type 'a -> 'b
|}]

(*
 * Modification of parameter modes in argument position
 *)

let use_local (local_ f : _ -> _ -> _) x y =
  f x y
let use_global (f : _ -> _ -> _) x y = f x y

let foo x y = x +. y
let bar (local_ x) (local_ y) = let _ = x +. y in ()

let result = use_local foo 1. 2.
[%%expect{|
val use_local : local_ ('a -> 'b -> 'c) -> 'a -> 'b -> 'c = <fun>
val use_global : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c = <fun>
val foo : float -> float -> float = <fun>
val bar : local_ float -> local_ float -> unit = <fun>
val result : float = 3.
|}]

let result = use_local bar 1. 2.
[%%expect{|
val result : unit = ()
|}]

let result = use_global foo 1. 2.
[%%expect{|
val result : float = 3.
|}]

let result = use_global bar 1. 2.
[%%expect{|
Line 1, characters 24-27:
1 | let result = use_global bar 1. 2.
                            ^^^
Error: This expression has type local_ float -> local_ float -> unit
       but an expression was expected of type local_ 'a -> ('b -> 'c)
|}]

let use_portable_arg (f : (_ -> _) @ portable -> _) g = f g
let nonportable_arg (f @ nonportable) = f ()
let result = use_portable_arg nonportable_arg (fun () -> ())
[%%expect{|
val use_portable_arg :
  ('a : any) ('b : any) 'c.
    (('a -> 'b) @ portable -> 'c) -> ('a -> 'b) @ portable -> 'c =
  <fun>
val nonportable_arg : (unit -> 'a) -> 'a = <fun>
val result : unit = ()
|}]

let use_nonportable_arg (f : (_ -> _) @ nonportable -> _) g = f g
let portable_arg (f @ portable) = f ()
let bad_use = use_nonportable_arg portable_arg (fun () -> ())
[%%expect{|
val use_nonportable_arg :
  ('a : any) ('b : any) 'c. (('a -> 'b) -> 'c) -> ('a -> 'b) -> 'c = <fun>
val portable_arg : (unit -> 'a) @ portable -> 'a = <fun>
Line 3, characters 34-46:
3 | let bad_use = use_nonportable_arg portable_arg (fun () -> ())
                                      ^^^^^^^^^^^^
Error: This expression has type (unit -> 'a) @ portable -> 'a
       but an expression was expected of type ('b -> 'c) -> 'd
|}]

let use_uncontended_arg (f : _ @ uncontended -> _) x = f x
let contended_arg (x @ contended) = ()
let result = use_uncontended_arg contended_arg ()
[%%expect{|
val use_uncontended_arg : ('a -> 'b) -> 'a -> 'b = <fun>
val contended_arg : 'a @ contended -> unit = <fun>
val result : unit = ()
|}]

let use_contended_arg (f : _ @ contended -> _) x = f x
let uncontended_arg (x @ uncontended) = ()
let bad_use = use_contended_arg uncontended_arg ()
[%%expect{|
val use_contended_arg : ('a @ contended -> 'b) -> 'a -> 'b = <fun>
val uncontended_arg : 'a -> unit = <fun>
Line 3, characters 32-47:
3 | let bad_use = use_contended_arg uncontended_arg ()
                                    ^^^^^^^^^^^^^^^
Error: This expression has type 'a -> unit
       but an expression was expected of type 'b @ contended -> 'c
|}]
