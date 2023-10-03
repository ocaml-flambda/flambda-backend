(* TEST
 flags = "-dlambda -extension underscore_argument"
 * expect
 *)

let f (w:string) ~x ~y (z:int) = "hello"
[%%expect{|
(let (f/274 = (function {nlocal = 0} w/276 x/277 y/278 z/279[int] "hello"))
  (apply (field 1 (global Toploop!)) "f" f/274))
val f : string -> x:'a -> y:'b -> int -> string = <fun>
|}]

(* note that x and y are applied together, without side effects in between;
 but all after the side effect of the first argument *)
let g = f (print_endline "hello"; "world") ~y:_ ~x:_
[%%expect{|
(let
  (f/274 = (apply (field 0 (global Toploop!)) "f")
   g/280 =
     (let
       (arg/281 = (seq (apply (field 45 (global Stdlib!)) "hello") "world"))
       (function {nlocal = 0} underscore/282 underscore/283 stub
         ignore assert all zero_alloc
         (apply f/274 arg/281 underscore/282 underscore/283))))
  (apply (field 1 (global Toploop!)) "g" g/280))
val g : x:'_weak1 -> y:'_weak2 -> int -> string = <fun>
|}]

(* underscore-generated functions are outer than omitted-generated ones *)
let g = f ~y:_
[%%expect{|
(let
  (f/274 = (apply (field 0 (global Toploop!)) "f")
   g/284 =
     (function {nlocal = 0} underscore/285 param/286 stub
       ignore assert all zero_alloc
       (let (func/287 = (apply f/274 param/286))
         (function {nlocal = 0} param/288 stub ignore assert all zero_alloc
           (apply func/287 param/288 underscore/285)))))
  (apply (field 1 (global Toploop!)) "g" g/284))
val g : y:'a -> string -> x:'b -> int -> string = <fun>
|}]