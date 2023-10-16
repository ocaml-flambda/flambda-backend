(* TEST
   flags = "-extension layouts_alpha"
   * expect
*)

let f (x : bool) = (x : int)[@error_message]
[%%expect{|
Line 1, characters 28-44:
1 | let f (x : bool) = (x : int)[@error_message]
                                ^^^^^^^^^^^^^^^^
Warning 47 [attribute-payload]: illegal payload for attribute 'error_message'.
Error_message attribute expects a string argument
Line 1, characters 20-21:
1 | let f (x : bool) = (x : int)[@error_message]
                        ^
Error: This expression has type bool but an expression was expected of type
         int
|}]

let f (x : bool) = (x : int)[@error_message "A"][@error_message "B"]
[%%expect{|
Line 1, characters 48-68:
1 | let f (x : bool) = (x : int)[@error_message "A"][@error_message "B"]
                                                    ^^^^^^^^^^^^^^^^^^^^
Warning 47 [attribute-payload]: illegal payload for attribute 'error_message'.
More than one error_message attribute present. All of them will be ignored.
Line 1, characters 20-21:
1 | let f (x : bool) = (x : int)[@error_message "A"][@error_message "B"]
                        ^
Error: This expression has type bool but an expression was expected of type
         int
|}]

let f (x : bool) = (x : int)[@error_message "custom message"]

[%%expect{|
Line 1, characters 20-21:
1 | let f (x : bool) = (x : int)[@error_message "custom message"]
                        ^
Error: This expression has type bool but an expression was expected of type
         int

Hint: custom message
|}]

let f x: bool = (x : int)[@error_message "custom message"]

[%%expect{|
Line 1, characters 16-25:
1 | let f x: bool = (x : int)[@error_message "custom message"]
                    ^^^^^^^^^
Error: This expression has type int but an expression was expected of type
         bool
|}]


let g (x : int) = x
let f (x : bool) = (let y = false in g y : int)[@error_message "custom message"]

[%%expect{|
val g : int -> int = <fun>
Line 2, characters 39-40:
2 | let f (x : bool) = (let y = false in g y : int)[@error_message "custom message"]
                                           ^
Error: This expression has type bool but an expression was expected of type
         int
|}]

let f (x : string) = (x : (_ : immediate))[@error_message "custom message"]

[%%expect{|
Line 1, characters 22-23:
1 | let f (x : string) = (x : (_ : immediate))[@error_message "custom message"]
                          ^
Error: This expression has type string but an expression was expected of type
         ('a : immediate)
       string has layout value, which is not a sublayout of immediate.

Hint: custom message
|}]


let f () = (fun (x: int) -> x : string -> string)[@error_message "custom message"]

[%%expect{|
Line 1, characters 16-24:
1 | let f () = (fun (x: int) -> x : string -> string)[@error_message "custom message"]
                    ^^^^^^^^
Error: This pattern matches values of type int
       but a pattern was expected which matches values of type string
|}]
(* Not sure about this case *)

let g () = ""
let f () = (g () : (_ : immediate))[@error_message "custom message"]

[%%expect{|
val g : unit -> string = <fun>
Line 2, characters 12-16:
2 | let f () = (g () : (_ : immediate))[@error_message "custom message"]
                ^^^^
Error: This expression has type string but an expression was expected of type
         ('a : immediate)
       string has layout value, which is not a sublayout of immediate.

Hint: custom message
|}]

let g () = ""
let f () = (
       g ()    : (_ : immediate))[@error_message "custom message"]

[%%expect{|
val g : unit -> string = <fun>
Line 3, characters 7-11:
3 |        g ()    : (_ : immediate))[@error_message "custom message"]
           ^^^^
Error: This expression has type string but an expression was expected of type
         ('a : immediate)
       string has layout value, which is not a sublayout of immediate.

Hint: custom message
|}]

let g (x: int) = x
let f () = (g : (string -> string))[@error_message "custom message"]

[%%expect{|
val g : int -> int = <fun>
Line 2, characters 12-13:
2 | let f () = (g : (string -> string))[@error_message "custom message"]
                ^
Error: This expression has type int -> int
       but an expression was expected of type string -> string
       Type int is not compatible with type string

Hint: custom message
|}]

let f x = (x#f : (_: immediate))[@error_message "custom message"]

[%%expect{|
val f : ('a : immediate). < f : 'a; .. > -> 'a = <fun>
|}]
