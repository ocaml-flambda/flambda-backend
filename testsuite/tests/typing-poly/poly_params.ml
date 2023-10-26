(* TEST
   * expect
*)

let poly1 (id : 'a. 'a -> 'a) = id 3, id "three"
[%%expect {|
val poly1 : ('a. 'a -> 'a) -> int * string = <fun>
|}];;

let _ = poly1 (fun x -> x)
[%%expect {|
- : int * string = (3, "three")
|}];;

let _ = poly1 (fun x -> x + 1)
[%%expect {|
Line 1, characters 14-30:
1 | let _ = poly1 (fun x -> x + 1)
                  ^^^^^^^^^^^^^^^^
Error: This argument has type int -> int which is less general than
         'a. 'a -> 'a
|}];;

let id x = x
let _ = poly1 id
[%%expect {|
val id : 'a -> 'a = <fun>
- : int * string = (3, "three")
|}];;

let _ = poly1 (id (fun x -> x))
[%%expect {|
Line 1, characters 14-31:
1 | let _ = poly1 (id (fun x -> x))
                  ^^^^^^^^^^^^^^^^^
Error: This argument has type 'b -> 'b which is less general than
         'a. 'a -> 'a
|}];;

let _ = poly1 (let r = ref None in fun x -> r := Some x; x)
[%%expect {|
Line 1, characters 14-59:
1 | let _ = poly1 (let r = ref None in fun x -> r := Some x; x)
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This argument has type 'b -> 'b which is less general than
         'a. 'a -> 'a
|}];;

let escape f = poly1 (fun x -> f x; x)
[%%expect {|
Line 1, characters 21-38:
1 | let escape f = poly1 (fun x -> f x; x)
                         ^^^^^^^^^^^^^^^^^
Error: This argument has type 'b -> 'b which is less general than
         'a. 'a -> 'a
|}];;

let poly2 : ('a. 'a -> 'a) -> int * string =
  fun id -> id 3, id "three"
[%%expect {|
val poly2 : ('a. 'a -> 'a) -> int * string = <fun>
|}];;

let _ = poly2 (fun x -> x)
[%%expect {|
- : int * string = (3, "three")
|}];;

let _ = poly2 (fun x -> x + 1)
[%%expect {|
Line 1, characters 14-30:
1 | let _ = poly2 (fun x -> x + 1)
                  ^^^^^^^^^^^^^^^^
Error: This argument has type int -> int which is less general than
         'a. 'a -> 'a
|}];;

let poly3 : 'b. ('a. 'a -> 'a) -> 'b -> 'b * 'b option =
  fun id x -> id x, id (Some x)
[%%expect {|
val poly3 : ('a. 'a -> 'a) -> 'b -> 'b * 'b option = <fun>
|}];;

let _ = poly3 (fun x -> x) 8
[%%expect {|
- : int * int option = (8, Some 8)
|}];;

let _ = poly3 (fun x -> x + 1) 8
[%%expect {|
Line 1, characters 14-30:
1 | let _ = poly3 (fun x -> x + 1) 8
                  ^^^^^^^^^^^^^^^^
Error: This argument has type int -> int which is less general than
         'a. 'a -> 'a
|}];;

let rec poly4 p (id : 'a. 'a -> 'a) =
  if p then poly4 false id else id 4, id "four"
[%%expect {|
val poly4 : bool -> ('a. 'a -> 'a) -> int * string = <fun>
|}];;

let _ = poly4 true (fun x -> x)
[%%expect {|
- : int * string = (4, "four")
|}];;

let _ = poly4 true (fun x -> x + 1)
[%%expect {|
Line 1, characters 19-35:
1 | let _ = poly4 true (fun x -> x + 1)
                       ^^^^^^^^^^^^^^^^
Error: This argument has type int -> int which is less general than
         'a. 'a -> 'a
|}];;

let rec poly5 : bool -> ('a. 'a -> 'a) -> int * string =
  fun p id ->
    if p then poly5 false id else id 5, id "five"
[%%expect {|
val poly5 : bool -> ('a. 'a -> 'a) -> int * string = <fun>
|}];;

let _ = poly5 true (fun x -> x)
[%%expect {|
- : int * string = (5, "five")
|}];;

let _ = poly5 true (fun x -> x + 1)
[%%expect {|
Line 1, characters 19-35:
1 | let _ = poly5 true (fun x -> x + 1)
                       ^^^^^^^^^^^^^^^^
Error: This argument has type int -> int which is less general than
         'a. 'a -> 'a
|}];;


let rec poly6 : 'b. bool -> ('a. 'a -> 'a) -> 'b -> 'b * 'b option =
  fun p id x ->
    if p then poly6 false id x else id x, id (Some x)
[%%expect {|
val poly6 : bool -> ('a. 'a -> 'a) -> 'b -> 'b * 'b option = <fun>
|}];;

let _ = poly6 true (fun x -> x) 8
[%%expect {|
- : int * int option = (8, Some 8)
|}];;

let _ = poly6 true (fun x -> x + 1) 8
[%%expect {|
Line 1, characters 19-35:
1 | let _ = poly6 true (fun x -> x + 1) 8
                       ^^^^^^^^^^^^^^^^
Error: This argument has type int -> int which is less general than
         'a. 'a -> 'a
|}];;

let needs_magic (magic : 'a 'b. 'a -> 'b) = (magic 5 : string)
let _ = needs_magic (fun x -> x)
[%%expect {|
val needs_magic : ('a 'b. 'a -> 'b) -> string = <fun>
Line 2, characters 20-32:
2 | let _ = needs_magic (fun x -> x)
                        ^^^^^^^^^^^^
Error: This argument has type 'c. 'c -> 'c which is less general than
         'a 'b. 'a -> 'b
|}];;

let with_id (f : ('a. 'a -> 'a) -> 'b) = f (fun x -> x)
[%%expect {|
val with_id : (('a. 'a -> 'a) -> 'b) -> 'b = <fun>
|}];;

let _ = with_id (fun id -> id 4, id "four")
[%%expect {|
- : int * string = (4, "four")
|}];;

let non_principal1 p f =
  if p then with_id f
  else f (fun x -> x)
[%%expect {|
val non_principal1 : bool -> (('a. 'a -> 'a) -> 'b) -> 'b = <fun>
|}, Principal{|
Line 3, characters 7-21:
3 |   else f (fun x -> x)
           ^^^^^^^^^^^^^^
Warning 18 [not-principal]: applying a higher-rank function here is not principal.

val non_principal1 : bool -> (('a. 'a -> 'a) -> 'b) -> 'b = <fun>
|}];;

let non_principal2 p f =
  if p then f (fun x -> x)
  else with_id f
[%%expect {|
Line 3, characters 15-16:
3 |   else with_id f
                   ^
Error: This expression has type ('b -> 'b) -> 'c
       but an expression was expected of type ('a. 'a -> 'a) -> 'd
       The universal variable 'a would escape its scope
|}];;

let principal1 p (f : ('a. 'a -> 'a) -> 'b) =
  if p then f (fun x -> x)
  else with_id f
[%%expect {|
val principal1 : bool -> (('a. 'a -> 'a) -> 'b) -> 'b = <fun>
|}];;

let principal2 : bool -> (('a. 'a -> 'a) -> 'b) -> 'b =
  fun p f ->
    if p then f (fun x -> x)
    else with_id f
[%%expect {|
val principal2 : bool -> (('a. 'a -> 'a) -> 'b) -> 'b = <fun>
|}];;

type poly = ('a. 'a -> 'a) -> int * string

let principal3 : poly option list = [ None; Some (fun x -> x 5, x "hello") ]
[%%expect {|
type poly = ('a. 'a -> 'a) -> int * string
val principal3 : poly option list = [None; Some <fun>]
|}];;

let non_principal3 =
  [ (Some (fun x -> x 5, x "hello") : poly option);
    Some (fun y -> y 6, y "goodbye") ]
[%%expect {|
val non_principal3 : poly option list = [Some <fun>; Some <fun>]
|}, Principal{|
Line 3, characters 9-36:
3 |     Some (fun y -> y 6, y "goodbye") ]
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 18 [not-principal]: this higher-rank function is not principal.

val non_principal3 : poly option list = [Some <fun>; Some <fun>]
|}];;

let non_principal4 =
  [ Some (fun y -> y 6, y "goodbye");
    (Some (fun x -> x 5, x "hello") : poly option) ]
[%%expect {|
Line 2, characters 26-35:
2 |   [ Some (fun y -> y 6, y "goodbye");
                              ^^^^^^^^^
Error: This expression has type string but an expression was expected of type
         int
|}];;

(* Functions with polymorphic parameters are separate from other functions *)
type 'a arg = 'b
  constraint 'a = 'b -> 'c
type really_poly = (('a. 'a -> 'a) -> string) arg
[%%expect {|
type 'a arg = 'b constraint 'a = 'b -> 'c
Line 3, characters 20-44:
3 | type really_poly = (('a. 'a -> 'a) -> string) arg
                        ^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type ('a. 'a -> 'a) -> string should be an instance of type
         'b -> 'c
       The universal variable 'a would escape its scope
|}];;

(* Polymorphic parameters are (mostly) treated as invariant *)
type p1 = ('a. 'a -> 'a) -> int
type p2 = ('a 'b. 'a -> 'b) -> int
[%%expect {|
type p1 = ('a. 'a -> 'a) -> int
type p2 = ('a 'b. 'a -> 'b) -> int
|}];;

let foo (f : p1) : p2 = f
[%%expect {|
Line 1, characters 24-25:
1 | let foo (f : p1) : p2 = f
                            ^
Error: This expression has type p1 = ('a. 'a -> 'a) -> int
       but an expression was expected of type p2 = ('a 'b. 'a -> 'b) -> int
       Type 'a is not compatible with type 'b
|}];;

let foo f = (f : p1 :> p2)
[%%expect {|
Line 1, characters 12-26:
1 | let foo f = (f : p1 :> p2)
                ^^^^^^^^^^^^^^
Error: Type p1 = ('a. 'a -> 'a) -> int is not a subtype of
         p2 = ('a 'b. 'a -> 'b) -> int
       Type 'b is not a subtype of 'a
|}];;

module Foo (X : sig val f : p1 end) : sig val f : p2 end = X
[%%expect {|
Line 1, characters 59-60:
1 | module Foo (X : sig val f : p1 end) : sig val f : p2 end = X
                                                               ^
Error: Signature mismatch:
       Modules do not match:
         sig val f : p1 end
       is not included in
         sig val f : p2 end
       Values do not match: val f : p1 is not included in val f : p2
       The type p1 = ('a. 'a -> 'a) -> int is not compatible with the type
         p2 = ('a 'b. 'a -> 'b) -> int
       Type 'a is not compatible with type 'b
|}];;

let foo (f : p1) : p2 = (fun id -> f id)
[%%expect {|
val foo : p1 -> p2 = <fun>
|}];;

(* Following the existing behaviour for polymorphic methods, you can
   subtype from a polymorphic parameter to a monomorphic
   parameter. Elsewhere it still behaves as invariant. *)
type p1 = (bool -> bool) -> int
type p2 = ('a. 'a -> 'a) -> int

let foo (x : p1) : p2 = x
[%%expect {|
type p1 = (bool -> bool) -> int
type p2 = ('a. 'a -> 'a) -> int
Line 4, characters 24-25:
4 | let foo (x : p1) : p2 = x
                            ^
Error: This expression has type p1 = (bool -> bool) -> int
       but an expression was expected of type p2 = ('a. 'a -> 'a) -> int
       Type bool is not compatible with type 'a
|}];;

let foo x = (x : p1 :> p2)
[%%expect {|
val foo : p1 -> p2 = <fun>
|}];;

module Foo (X : sig val f : p1 end) : sig val f : p2 end = X
[%%expect {|
Line 1, characters 59-60:
1 | module Foo (X : sig val f : p1 end) : sig val f : p2 end = X
                                                               ^
Error: Signature mismatch:
       Modules do not match:
         sig val f : p1 end
       is not included in
         sig val f : p2 end
       Values do not match: val f : p1 is not included in val f : p2
       The type p1 = (bool -> bool) -> int is not compatible with the type
         p2 = ('a. 'a -> 'a) -> int
       Type bool is not compatible with type 'a
|}];;

let foo (f : p1) : p2 = (fun id -> f id)
[%%expect {|
val foo : p1 -> p2 = <fun>
|}];;
