(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

type ('a : value => value) t = unit 'a
[%%expect {|
type ('a : value => value) t = unit 'a
|}]

type 'a t = unit 'a
[%%expect {|
Line 1, characters 12-19:
1 | type 'a t = unit 'a
                ^^^^^^^
Error: Types of jkind (immediate) cannot be applied to '_representable_layout_1.
|}]

type ('a, 'm) t = 'a 'm
[%%expect {|
Line 1, characters 18-23:
1 | type ('a, 'm) t = 'a 'm
                      ^^^^^
Error: Types of jkind ('_representable_layout_2) cannot be applied to '_representable_layout_3.
|}]

type ('a, 'm : value => value) t = 'a 'm
[%%expect {|
type ('a, 'm : value => value) t = 'a 'm
|}]

type ('a, 'm : immediate => immediate) t = 'a 'm
[%%expect {|
type ('a : immediate, 'm : immediate => immediate) t = 'a 'm
|}]

type ('a, 'b, 'c : (value, immediate) => value) t = ('a, 'b) 'c
[%%expect {|
type ('a, 'b : immediate, 'c : (value, immediate) => value) t = ('a, 'b) 'c
|}]

module type M = sig
  val f : ('a : value => value). unit -> unit 'a
end
[%%expect {|
unit <= value
'a <= value
'a <= immediate
'a <= value
'b <= immediate

unit <= value
module type M = sig val f : ('a : value => value). unit -> unit 'a end
|}]

module type M = sig
  val f : unit -> unit 'a
end
[%%expect {|
Line 2, characters 18-25:
2 |   val f : unit -> unit 'a
                      ^^^^^^^
Error: Types of jkind (immediate) cannot be applied to '_representable_layout_4.
|}]
