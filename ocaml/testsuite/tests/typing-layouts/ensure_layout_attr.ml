(* TEST
   flags = "-extension layouts_alpha -w +A"
   * expect
*)


let f x = (ignore[@ensure_layout]) x
[%%expect {|
Line 1, characters 17-33:
1 | let f x = (ignore[@ensure_layout]) x
                     ^^^^^^^^^^^^^^^^
Warning 47 [attribute-payload]: illegal payload for attribute 'ensure_layout'.
Expecting a layout plus optional message
val f : 'a -> unit = <fun>
|}]

let f x = (ignore[@ensure_layout abc]) x
[%%expect {|
Line 1, characters 17-37:
1 | let f x = (ignore[@ensure_layout abc]) x
                     ^^^^^^^^^^^^^^^^^^^^
Warning 47 [attribute-payload]: illegal payload for attribute 'ensure_layout'.
Unknown layout encountered
val f : 'a -> unit = <fun>
|}]

let f x = (ignore[@ensure_layout "abc"]) x
[%%expect {|
Line 1, characters 17-39:
1 | let f x = (ignore[@ensure_layout "abc"]) x
                     ^^^^^^^^^^^^^^^^^^^^^^
Warning 47 [attribute-payload]: illegal payload for attribute 'ensure_layout'.
Expecting a layout plus optional message
val f : 'a -> unit = <fun>
|}]

let f x = (ignore[@ensure_layout abc ""]) x
[%%expect {|
Line 1, characters 17-40:
1 | let f x = (ignore[@ensure_layout abc ""]) x
                     ^^^^^^^^^^^^^^^^^^^^^^^
Warning 47 [attribute-payload]: illegal payload for attribute 'ensure_layout'.
Unknown layout encountered
val f : 'a -> unit = <fun>
|}]

let f x = ()
[%%expect {|
Line 1, characters 6-7:
1 | let f x = ()
          ^
Warning 27 [unused-var-strict]: unused variable x.
val f : 'a -> unit = <fun>
|}]

let f x = (ignore[@ensure_layout value "custom message"]) x
[%%expect {|
Line 1, characters 6-7:
1 | let f x = (ignore[@ensure_layout value "custom message"]) x
          ^
Warning 27 [unused-var-strict]: unused variable x.
val f : 'a -> unit = <fun>
|}]

let f x = (ignore[@ensure_layout void "custom message"]) x
[%%expect {|
Line 1, characters 57-58:
1 | let f x = (ignore[@ensure_layout void "custom message"]) x
                                                             ^
Error: Layout requirement not satisfied.
       'a has layout value, which does not overlap with void.
|}]

type t_any : any
type t_void : void
let f () = (ignore[@ensure_layout value "custom message"]) (assert false : t_void)
[%%expect {|
type t_any : any
type t_void : void
Line 3, characters 59-82:
3 | let f () = (ignore[@ensure_layout value "custom message"]) (assert false : t_void)
                                                               ^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type t_void but an expression was expected of type
         ('a : value)
       t_void has layout void, which is not a sublayout of value.
|}]

let f () = (ignore[@ensure_layout value "custom message"]) (assert false : t_any)
[%%expect {|
Line 1, characters 59-81:
1 | let f () = (ignore[@ensure_layout value "custom message"]) (assert false : t_any)
                                                               ^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type t_any but an expression was expected of type
         ('a : value)
       t_any has layout any, which is not a sublayout of value.
|}]

type t =
  { x : int
  ; y : int
  }

(* the statement does not mark variables as used *)
let sum ({ x; y } as t) = (ignore[@ensure_layout value "custom message"]) t; x + y

[%%expect{|
type t = { x : int; y : int; }
Line 7, characters 21-22:
7 | let sum ({ x; y } as t) = (ignore[@ensure_layout value "custom message"]) t; x + y
                         ^
Warning 26 [unused-var]: unused variable t.
val sum : t -> int = <fun>
|}]


let f x = (ignore[@ensure_layout value "custom message"]) (let y = 1 in x)

[%%expect{|
Line 1, characters 6-7:
1 | let f x = (ignore[@ensure_layout value "custom message"]) (let y = 1 in x)
          ^
Warning 27 [unused-var-strict]: unused variable x.
Line 1, characters 63-64:
1 | let f x = (ignore[@ensure_layout value "custom message"]) (let y = 1 in x)
                                                                   ^
Warning 26 [unused-var]: unused variable y.
val f : 'a -> unit = <fun>
|}]

(* body of the ensure_layout is only type checked and ommitted from the typedtree *)
let f x =
  let y = ref [] in
  (ignore) (y := 1 :: !y; x);
  (ignore[@ensure_layout value "custom message"]) (y := 2 :: !y; x);
  !y;;

f ()

[%%expect{|
val f : 'a -> int list = <fun>
- : int list = [1]
|}]
