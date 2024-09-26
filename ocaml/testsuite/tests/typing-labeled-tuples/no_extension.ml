(* TEST
   flags = "-extension-universe no_extensions";
   expect;
*)

type t = x:int * y:string

[%%expect{|
Line 1, characters 9-25:
1 | type t = x:int * y:string
             ^^^^^^^^^^^^^^^^
Error: The extension "labeled_tuples" is disabled and cannot be used
|}]

let f x y = ~x, ~y

[%%expect{|
Line 1, characters 12-18:
1 | let f x y = ~x, ~y
                ^^^^^^
Error: The extension "labeled_tuples" is disabled and cannot be used
|}]

let x = ~x:5, ~y:"hi"

[%%expect{|
Line 1, characters 8-21:
1 | let x = ~x:5, ~y:"hi"
            ^^^^^^^^^^^^^
Error: The extension "labeled_tuples" is disabled and cannot be used
|}]

let f x y = ~(x : int), ~(y : string)

[%%expect{|
Line 1, characters 12-37:
1 | let f x y = ~(x : int), ~(y : string)
                ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The extension "labeled_tuples" is disabled and cannot be used
|}]

let f (~x, ~y) = x, y

[%%expect{|
Line 1, characters 6-14:
1 | let f (~x, ~y) = x, y
          ^^^^^^^^
Error: The extension "labeled_tuples" is disabled and cannot be used
|}]

let f (~x:_, ~y:_) = ()

[%%expect{|
Line 1, characters 6-18:
1 | let f (~x:_, ~y:_) = ()
          ^^^^^^^^^^^^
Error: The extension "labeled_tuples" is disabled and cannot be used
|}]

let f (~(x:int), ~(y:string)) = x, y

[%%expect{|
Line 1, characters 6-29:
1 | let f (~(x:int), ~(y:string)) = x, y
          ^^^^^^^^^^^^^^^^^^^^^^^
Error: The extension "labeled_tuples" is disabled and cannot be used
|}]

let f ((x, ..) : (int * string * float)) = x

[%%expect{|
Line 1, characters 7-14:
1 | let f ((x, ..) : (int * string * float)) = x
           ^^^^^^^
Error: The extension "labeled_tuples" is disabled and cannot be used
|}]

let f ((x, y, ..) : (int * string * float)) = x, y

[%%expect{|
Line 1, characters 7-17:
1 | let f ((x, y, ..) : (int * string * float)) = x, y
           ^^^^^^^^^^
Error: The extension "labeled_tuples" is disabled and cannot be used
|}]
