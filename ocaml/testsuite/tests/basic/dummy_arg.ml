(* TEST
 flags="-dlambda -extension dummy_arguments"
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
let g r = f (r := "hello"; "world") ~y:_ ~x:_
[%%expect{|
(let
  (f/274 = (apply (field 0 (global Toploop!)) "f")
   g/280 =
     (function {nlocal = 0} r/282
       (let (arg/283 = (seq (setfield_ptr 0 r/282 "hello") "world"))
         (function {nlocal = 0} dummy/284 dummy/285 stub
           ignore assert all zero_alloc
           (apply f/274 arg/283 dummy/284 dummy/285)))))
  (apply (field 1 (global Toploop!)) "g" g/280))
val g : string ref -> x:'a -> y:'b -> int -> string = <fun>
|}]

(* underscore-generated functions are outer than omitted-generated ones *)
let g = f ~y:_
[%%expect{|
(let
  (f/274 = (apply (field 0 (global Toploop!)) "f")
   g/286 =
     (function {nlocal = 0} dummy/287 param/288 stub
       ignore assert all zero_alloc
       (let (func/289 = (apply f/274 param/288))
         (function {nlocal = 0} param/290 stub ignore assert all zero_alloc
           (apply func/289 param/290 dummy/287)))))
  (apply (field 1 (global Toploop!)) "g" g/286))
val g : y:'a -> string -> x:'b -> int -> string = <fun>
|}]