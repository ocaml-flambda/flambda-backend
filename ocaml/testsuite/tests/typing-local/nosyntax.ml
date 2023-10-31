(* TEST
   flags += "-disable-all-extensions"
   * expect *)

type fn = string -> int
type lfn = (string[@ocaml.local]) -> int
type lfn' = local_ string -> int
[%%expect{|
type fn = string -> int
type lfn = (string [@local]) -> int
Line 3, characters 19-25:
3 | type lfn' = local_ string -> int
                       ^^^^^^
Error: The local extension is disabled
       To enable it, pass the '-extension local' flag
|}]

let cast (x : fn) = (x : lfn)
[%%expect{|

Line 1, characters 21-22:
1 | let cast (x : fn) = (x : lfn)
                         ^
Error: This expression has type fn = string -> int
       but an expression was expected of type (string [@local]) -> int
|}]

let local_ref (f : lfn -> unit) =
  f (fun s -> let _ = [|s;s;s|] in 1)

[%%expect{|

Line 2, characters 24-25:
2 |   f (fun s -> let _ = [|s;s;s|] in 1)
                            ^
Error: This value escapes its region
|}]

type foo = {
  x : string
}
[%%expect{|

type foo = { x : string; }
|}]

type gfoo = {
  x : string [@ocaml.global]
}
[%%expect{|

type gfoo = { x : (string [@global]); }
|}]
type gfoo' = {
  global_ x :  string
}
[%%expect{|

Line 2, characters 2-21:
2 |   global_ x :  string
      ^^^^^^^^^^^^^^^^^^^
Error: The local extension is disabled
       To enable it, pass the '-extension local' flag
|}]

let cast  ((r : foo)[@ocaml.local]) : gfoo =
  match r with
  | {x} -> {x}
[%%expect{|

Line 3, characters 12-13:
3 |   | {x} -> {x}
                ^
Error: This value escapes its region
|}]

type foo = Foo of string
[%%expect{|

type foo = Foo of string
|}]
type gfoo = GFoo of (string [@ocaml.global])
[%%expect{|

type gfoo = GFoo of (string [@global])
|}]
type gfoo' = Gfoo of global_ string
[%%expect{|

Line 1, characters 29-35:
1 | type gfoo' = Gfoo of global_ string
                                 ^^^^^^
Error: The local extension is disabled
       To enable it, pass the '-extension local' flag
|}]


let cast ((r : foo)[@ocaml.local]) : gfoo =
  match r with
  | Foo x -> GFoo x

[%%expect{|

Line 3, characters 18-19:
3 |   | Foo x -> GFoo x
                      ^
Error: This value escapes its region
|}]

