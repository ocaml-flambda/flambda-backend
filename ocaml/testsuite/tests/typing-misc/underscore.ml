(* TEST
   flags="-extension dummy_arguments"
   * expect

*)

(* Dummy partial application *)
let f ~(x : string) ~(y : int) (z:char) = ()
[%%expect{|
val f : x:string -> y:int -> char -> unit = <fun>
|}]

let g = f _
[%%expect{|
val g : char -> x:string -> y:int -> unit = <fun>
|}]

let g = f _ ~x:"hello"
[%%expect{|
val g : char -> y:int -> unit = <fun>
|}]

let g = f _ _
[%%expect{|
Line 1, characters 12-13:
1 | let g = f _ _
                ^
Error: The function applied to this argument has type
         x:string -> y:int -> unit
This argument cannot be applied without label
|}]

let g = f ~x:_
[%%expect{|
val g : x:string -> y:int -> char -> unit = <fun>
|}]

let g = f ~y:_
[%%expect{|
val g : y:int -> x:string -> char -> unit = <fun>
|}]

let g = f ~y:_ ~x:_
[%%expect{|
val g : x:string -> y:int -> char -> unit = <fun>
|}]

let g = f ~x:_ ~y:_
[%%expect{|
val g : x:string -> y:int -> char -> unit = <fun>
|}]

let g = f _ _ _
[%%expect{|
Line 1, characters 8-9:
1 | let g = f _ _ _
            ^
Warning 6 [labels-omitted]: labels x, y were omitted in the application of this function.
val g : x:string -> y:int -> char -> unit = <fun>
|}]

let f ?foo () = "hello"
[%%expect{|
val f : ?foo:'a -> unit -> string = <fun>
|}]

(* Due to our implementation, it's more natural to simply forbid dummy arguments
   wrapped in Some *)
let g = f ~foo:_
[%%expect{|
Line 1, characters 15-16:
1 | let g = f ~foo:_
                   ^
Error: Dummy argument cannot be supplied with tilda for optional parameter.
|}]

(* Dummy can still be supplied as optional argument raw *)
let g = f ?foo:_
[%%expect{|
val g : ?foo:'a -> unit -> string = <fun>
|}]

(* Poly bug reported by nick roberts *)
let f g = g _ _ _
[%%expect{|
val f : ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd = <fun>
|}]

let go () = let h = f (fun x y z -> x + y + z) in h 1
[%%expect{|
val go : unit -> int -> int -> int = <fun>
|}]

let f g =
   let h = g _ in
   let _x = h "hello" in
   let _y = h 42 in
   h
[%%expect{|
Line 4, characters 14-16:
4 |    let _y = h 42 in
                  ^^
Error: This expression has type int but an expression was expected of type
         string
|}]

(* Testing polymorphic_parameters vs. dummy_arguments *)
let f (g : 'a. 'a -> 'a) =
   let h = g _ in
   let _x = h "hello" in
   let _y = h 42 in
   h
[%%expect{|
val f : ('a. 'a -> 'a) -> 'b -> 'b = <fun>
|}]

let h = f (fun x -> x)
[%%expect{|
val h : '_weak1 -> '_weak1 = <fun>
|}]

let h' = f h
[%%expect{|
Line 1, characters 11-12:
1 | let h' = f h
               ^
Error: This argument has type 'b -> 'b which is less general than
         'a. 'a -> 'a
|}]


(* Reference that doesn't use dummy arguments *)
let f_ref (g : 'a. 'a -> 'a) =
   let h = fun x -> g x in
   let _x = h "hello" in
   let _y = h "42" in
   h
[%%expect{|
val f_ref : ('a. 'a -> 'a) -> 'b -> 'b = <fun>
|}]

let h_ref = f_ref (fun x -> x)
[%%expect{|
val h_ref : '_weak2 -> '_weak2 = <fun>
|}]

let h'_ref = f_ref h_ref
[%%expect{|
Line 1, characters 19-24:
1 | let h'_ref = f_ref h_ref
                       ^^^^^
Error: This argument has type 'b -> 'b which is less general than
         'a. 'a -> 'a
|}]

(* classes don't support dummy arguments *)
class c() = object
end

[%%expect{|
class c : unit -> object  end
|}]

class d = c _
[%%expect{|
Line 1, characters 12-13:
1 | class d = c _
                ^
Error: Dummy arguments are not supported for classes.
|}]
