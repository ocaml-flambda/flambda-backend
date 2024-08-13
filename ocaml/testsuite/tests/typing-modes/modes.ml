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
Error: Unrecognized mode name foo.
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
Error: Found a shared value where a unique value was expected
|}]

let x =
  let bar = "world" in
  foo ~(bar @ unique)
[%%expect{|
Line 3, characters 8-11:
3 |   foo ~(bar @ unique)
            ^^^
Error: Found a shared value where a unique value was expected
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
Error: Found a shared value where a unique value was expected
|}]


let foo () =
  let bar = "hello" in
  let biz = "world" in
  ~(bar @ unique), ~(biz @ once)
[%%expect{|
Line 4, characters 4-7:
4 |   ~(bar @ unique), ~(biz @ once)
        ^^^
Error: Found a shared value where a unique value was expected
|}]
*)

(* arrow types *)
type r = local_ string @ unique once -> unique_ string @ local once
[%%expect{|
type r = local_ once_ unique_ string -> local_ once_ unique_ string
|}]

type r = local_ string * y:string @ unique once -> local_ string * w:string @ once
[%%expect{|
type r = local_ once_ unique_ string * string -> local_ once_ string * string
|}]

type r = x:local_ string * y:string @ unique once -> local_ string * w:string @ once
[%%expect{|
type r =
    x:local_ once_ unique_ string * string -> local_ once_ string * string
|}]


type r = local_ string @ foo -> string
[%%expect{|
Line 1, characters 25-28:
1 | type r = local_ string @ foo -> string
                             ^^^
Error: Unrecognized mode name foo.
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
  global_ x : string @@ shared
}
[%%expect{|
type r = { global_ x : string @@ shared; }
|}]

type r = {
  x : string @@ shared global many
}
[%%expect{|
type r = { global_ x : string @@ many shared; }
|}]

type r = {
  x : string @@ shared global many shared
}
(* CR reduced-modality: this should warn. *)
[%%expect{|
type r = { global_ x : string @@ many shared; }
|}]

type r = Foo of string @@ global shared many
[%%expect{|
type r = Foo of global_ string @@ many shared
|}]

(* mutable implies global shared many. No warnings are given since we imagine
   that the coupling will be removed soon. *)
type r = {
  mutable x : string @@ global shared many
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
Line 1, characters 17-48:
1 | let foo ?(local_ x : 'a. 'a -> 'a @@ unique once) = ()
                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
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
Line 1, characters 19-36:
1 | let foo ?x:(local_ (x,y) : 'a.'a->'a @@ unique once) () = ()
                       ^^^^^^^^^^^^^^^^^
Error: Optional parameters cannot be polymorphic
|}]

(* note: Legacy mode syntax is not parsed for patterns *)

(* CR zqian: currently all patterns, except those directly as function
   arguments, DO NOT pick up modes during type checking. This should be fixed in
   another PR. Here, we test that they at least parse. *)
let foo ((x @ unique once), (y@local unique)) = x + y
[%%expect{|
Line 1, characters 14-25:
1 | let foo ((x @ unique once), (y@local unique)) = x + y
                  ^^^^^^^^^^^
Error: Mode annotations on patterns are not supported yet.
|}]

let foo ((x : _ @@ unique once), (y : _ @@ local unique)) = x + y
[%%expect{|
Line 1, characters 19-30:
1 | let foo ((x : _ @@ unique once), (y : _ @@ local unique)) = x + y
                       ^^^^^^^^^^^
Error: Mode annotations on patterns are not supported yet.
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
Error: Unrecognized mode name foo.
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
