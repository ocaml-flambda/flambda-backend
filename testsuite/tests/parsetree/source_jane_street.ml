(* TEST
   flags = "-extension-universe alpha";
   include stdlib_upstream_compatible;
   expect;
*)

(* There should be examples of all Jane Street syntax here. This file can
   thus additionally serve as a cheap-and-cheerful documentation for all of
   our features. *)

(* This file is three tests in one!
    - It's an expect-test, run in the normal way.
    - It is processed by [test.ml] (in this same directory) to make sure that Pprintast round-trips.
    - It is processed by [test_ppx.ml] (in this same directory) to make sure that ppxes run properly
      on examples of Jane Street syntax.
*)

(***********)
(* Layouts *)

let f (type a : immediate) (x : a) = x;;
let f (type (a : immediate)) (x : a) = x;;
let f (type (a : immediate) (b : immediate)) (x : a) = x;;
let f (type (a : immediate) (b : immediate) c) (x : a) = x;;

[%%expect{|
val f : ('a : immediate). 'a -> 'a = <fun>
val f : ('a : immediate). 'a -> 'a = <fun>
val f : ('a : immediate). 'a -> 'a = <fun>
val f : ('a : immediate). 'a -> 'a = <fun>
|}]

let f y (type a : immediate) (x : a) = x;;
let f y (type (a : immediate)) (x : a) = x;;
let f y (type (a : immediate) (b : immediate)) (x : a) = x;;

[%%expect{|
val f : 'b ('a : immediate). 'b -> 'a -> 'a = <fun>
val f : 'b ('a : immediate). 'b -> 'a -> 'a = <fun>
val f : 'b ('a : immediate). 'b -> 'a -> 'a = <fun>
|}]

let f y (type a : immediate) = y;;
let f y (type (a : immediate)) = y;;
let f y (type (a : immediate) (b : immediate)) = y;;
let f y (type (a : immediate) (b : immediate) c) = y;;

[%%expect{|
val f : 'a -> 'a = <fun>
val f : 'a -> 'a = <fun>
val f : 'a -> 'a = <fun>
val f : 'a -> 'a = <fun>
|}]

(* Just newtypes, no value parameters *)
let f (type a : immediate) (type b : immediate)
      (type (c : immediate) (d : immediate))
  = ();;

[%%expect{|
val f : unit = ()
|}]

module type S_for_layouts = sig
  type t : float64

  type variant = A : ('a : immediate). 'a -> variant

  val f1: ('a : float64) ('b : immediate) 'c . 'a -> 'b -> 'c
  val f2: ('a : float64) 'b ('c : bits64) . 'a -> 'b -> 'c
  val f3: 'a 'b ('c : word) . 'a -> 'b -> 'c
end;;

[%%expect{|
module type S_for_layouts =
  sig
    type t : float64
    type variant = A : ('a : immediate). 'a -> variant
    val f1 : ('a : float64) ('b : immediate) 'c. 'a -> 'b -> 'c
    val f2 : ('a : float64) 'b ('c : bits64). 'a -> 'b -> 'c
    val f3 : 'a 'b ('c : word). 'a -> 'b -> 'c
  end
|}]

type ('a : immediate) for_layouts = 'a;;

let f : ('a : float64) -> (_ : bits64) -> 'a = fun x _ -> x
let f : ('a : float64). 'a -> 'a = fun x -> x
let f : (('a : float64). 'a -> 'a) -> int = fun _ -> 5
let f : (unit as (_ : immediate)) -> unit = fun () -> ()

[%%expect{|
type ('a : immediate) for_layouts = 'a
val f : ('a : float64) ('b : bits64). 'a -> 'b -> 'a = <fun>
val f : ('a : float64). 'a -> 'a = <fun>
val f : (('a : float64). 'a -> 'a) -> int = <fun>
val f : unit -> unit = <fun>
|}]

type (_ : any) ignore = K1
type t = ..
type t += K : ('a : float64). 'a ignore -> t

[%%expect{|
type (_ : any) ignore = K1
type t = ..
type t += K : ('a : float64). 'a ignore -> t
|}]

(* CR layouts v2.8: re-enable this *)
(*
module M : sig
  kind_abbrev_ k = immediate
end = struct
  kind_abbrev_ k = immediate
end

[%%expect{|
>> Fatal error: kind_abbrev not supported!
Uncaught exception: Misc.Fatal_error

|}]
*)

type t1 : any
type t2 : any_non_null
type t3 : value_or_null
type t4 : value
type t5 : void
type t6 : immediate64
type t7 : immediate
type t8 : float64
type t9 : float32
type t10 : word
type t11 : bits32
type t12 : bits64

[%%expect{|
type t1 : any
type t2 : any_non_null
type t3 : value_or_null
type t4
type t5 : void
type t6 : immediate64
type t7 : immediate
type t8 : float64
type t9 : float32
type t10 : word
type t11 : bits32
type t12 : bits64
|}]

type t = #(int * float#)

let f xs = match xs with
  | #(x1, (y1 : float#)) -> #(x1, y1)

[%%expect{|
type t = #(int * float#)
val f : #('a * float#) -> #('a * float#) = <fun>
|}]

module M = struct
  type 'a t = 'a or_null [@@or_null_reexport]
end
let x () = #( M.Null, M.This "hi" )

[%%expect{|
module M :
  sig type 'a t = 'a or_null = Null | This of 'a [@@or_null_reexport] end
val x : unit -> #('a M.t * string M.t) = <fun>
|}]

external id : ('a : any). 'a -> 'a = "%identity" [@@layout_poly]

[%%expect{|
external id : ('a : any). 'a -> 'a = "%identity" [@@layout_poly]
|}]

(******************)
(* Comprehensions *)

(* simple range *)
let nums = [: x for x = 0 to 100 :];;
let nums = [| x for x = 0 to 100 |];;
let nums = [  x for x = 0 to 100   ];;

[%%expect{|
val nums : int iarray =
  [:0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20;
    21; 22; 23; 24; 25; 26; 27; 28; 29; 30; 31; 32; 33; 34; 35; 36; 37; 38;
    39; 40; 41; 42; 43; 44; 45; 46; 47; 48; 49; 50; 51; 52; 53; 54; 55; 56;
    57; 58; 59; 60; 61; 62; 63; 64; 65; 66; 67; 68; 69; 70; 71; 72; 73; 74;
    75; 76; 77; 78; 79; 80; 81; 82; 83; 84; 85; 86; 87; 88; 89; 90; 91; 92;
    93; 94; 95; 96; 97; 98; 99; 100:]
val nums : int array =
  [|0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20;
    21; 22; 23; 24; 25; 26; 27; 28; 29; 30; 31; 32; 33; 34; 35; 36; 37; 38;
    39; 40; 41; 42; 43; 44; 45; 46; 47; 48; 49; 50; 51; 52; 53; 54; 55; 56;
    57; 58; 59; 60; 61; 62; 63; 64; 65; 66; 67; 68; 69; 70; 71; 72; 73; 74;
    75; 76; 77; 78; 79; 80; 81; 82; 83; 84; 85; 86; 87; 88; 89; 90; 91; 92;
    93; 94; 95; 96; 97; 98; 99; 100|]
val nums : int list =
  [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20;
   21; 22; 23; 24; 25; 26; 27; 28; 29; 30; 31; 32; 33; 34; 35; 36; 37; 38;
   39; 40; 41; 42; 43; 44; 45; 46; 47; 48; 49; 50; 51; 52; 53; 54; 55; 56;
   57; 58; 59; 60; 61; 62; 63; 64; 65; 66; 67; 68; 69; 70; 71; 72; 73; 74;
   75; 76; 77; 78; 79; 80; 81; 82; 83; 84; 85; 86; 87; 88; 89; 90; 91; 92;
   93; 94; 95; 96; 97; 98; 99; 100]
|}]

(* simple in-comprehension *)
let nums = [: x for x in [: 1; 2; 3 :] :];;
let nums = [| x for x in [| 1; 2; 3 |] |];;
let nums = [  x for x in [  1; 2; 3  ]  ];;

[%%expect{|
val nums : int iarray = [:1; 2; 3:]
val nums : int array = [|1; 2; 3|]
val nums : int list = [1; 2; 3]
|}]

(* complex comprehension *)
let nums = [: x + y
    for x in [: 1; 2; 3 :]
    for y in [: 4; 5; 6 :]
    when x + y > 6
:];;

let nums = [| x + y
    for x in [| 1; 2; 3 |]
    for y in [| 4; 5; 6 |]
    when x + y > 6
|];;

let nums = [  x + y
    for x in [ 1; 2; 3 ]
    for y in [ 4; 5; 6 ]
    when x + y > 6
];;

let nums = [| n, m, x
    for n = 1 to 10
    and m = 5 downto 0
    when (n + m) mod 3 = 0
    for x in [| 1 + m; 2; 3 |]
    when (n + m + x) mod 2 = 0
  |];;

[%%expect{|
val nums : int iarray = [:7; 7; 8; 7; 8; 9:]
val nums : int array = [|7; 7; 8; 7; 8; 9|]
val nums : int list = [7; 7; 8; 7; 8; 9]
val nums : (int * int * int) array =
  [|(1, 5, 6); (1, 5, 2); (1, 2, 3); (1, 2, 3); (2, 4, 2); (2, 1, 3);
    (3, 3, 4); (3, 3, 2); (3, 0, 1); (3, 0, 3); (4, 5, 3); (4, 2, 2);
    (5, 4, 5); (5, 4, 3); (5, 1, 2); (5, 1, 2); (6, 3, 3); (6, 0, 2);
    (7, 5, 6); (7, 5, 2); (7, 2, 3); (7, 2, 3); (8, 4, 2); (8, 1, 3);
    (9, 3, 4); (9, 3, 2); (9, 0, 1); (9, 0, 3); (10, 5, 3); (10, 2, 2)|]
|}]

(* local_ is allowed in the parser in this one place, but the type-checker
   rejects *)
let broken_local =
  [ 5 for local_ n in [ 1; 2 ] ];;

[%%expect{|
Line 2, characters 10-30:
2 |   [ 5 for local_ n in [ 1; 2 ] ];;
              ^^^^^^^^^^^^^^^^^^^^
Error: This value escapes its region.
|}]

(* User-written attributes *)
let nums =
  ([(x[@test.attr1]) for (x[@test.attr2]) in ([][@test.attr3])] [@test.attr4]);;

[%%expect{|
val nums : 'a list = []
|}]

(*********)
(* Modes *)

(* parameters *)
let f (local_ unique_ x) ~(local_ once_ y) ~z:(unique_ once_ z)
      ?foo:(local_ unique_ once_ w = 1)
      ?bar:(local_ w : int = 1) () = ();;

[%%expect{|
val f :
  local_ 'a @ unique ->
  y:local_ 'b @ once ->
  z:'c @ once unique ->
  ?foo:local_ int @ once unique -> ?bar:local_ int -> unit -> unit = <fun>
|}]

(* bindings *)
let g () =
  let local_ unique_ f = () in
  let unique_ once_ f : 'a . 'a -> 'a = fun x -> x in
  let once_ local_ f x y = x + y in
  let local_ unique_ once_ f : int -> int = fun z -> z + z in
  let local_ f x: int -> int = x in
  let local_ f x y : int -> int = fun z -> x + y + z in

  let foo a b @ local = exclave_ "hello" in
  let foo = fun a b @ local -> exclave_ "hello" in
  let foo a b : int @@ local = 42 in
  let foo = fun a b : int @@ local -> 42 in
  ();;

[%%expect{|
Line 2, characters 21-22:
2 |   let local_ unique_ f = () in
                         ^
Warning 26 [unused-var]: unused variable f.

Line 3, characters 20-21:
3 |   let unique_ once_ f : 'a . 'a -> 'a = fun x -> x in
                        ^
Warning 26 [unused-var]: unused variable f.

Line 4, characters 19-20:
4 |   let once_ local_ f x y = x + y in
                       ^
Warning 26 [unused-var]: unused variable f.

Line 5, characters 27-28:
5 |   let local_ unique_ once_ f : int -> int = fun z -> z + z in
                               ^
Warning 26 [unused-var]: unused variable f.

Line 6, characters 13-14:
6 |   let local_ f x: int -> int = x in
                 ^
Warning 26 [unused-var]: unused variable f.

Line 7, characters 13-14:
7 |   let local_ f x y : int -> int = fun z -> x + y + z in
                 ^
Warning 26 [unused-var]: unused variable f.

Line 9, characters 6-9:
9 |   let foo a b @ local = exclave_ "hello" in
          ^^^
Warning 26 [unused-var]: unused variable foo.

Line 10, characters 6-9:
10 |   let foo = fun a b @ local -> exclave_ "hello" in
           ^^^
Warning 26 [unused-var]: unused variable foo.

Line 11, characters 6-9:
11 |   let foo a b : int @@ local = 42 in
           ^^^
Warning 26 [unused-var]: unused variable foo.

Line 12, characters 6-9:
12 |   let foo = fun a b : int @@ local -> 42 in
           ^^^
Warning 26 [unused-var]: unused variable foo.

val g : unit -> unit = <fun>
|}]

(* expressions *)
let g () = exclave_ unique_
  let f = unique_ once_ () in
  let f x y = exclave_ once_ (x + y) in
  local_ unique_ once_ ();;

[%%expect{|
Line 2, characters 6-7:
2 |   let f = unique_ once_ () in
          ^
Warning 26 [unused-var]: unused variable f.

Line 3, characters 6-7:
3 |   let f x y = exclave_ once_ (x + y) in
          ^
Warning 26 [unused-var]: unused variable f.

val g : unit -> local_ unit @ once = <fun>
|}]

(* types *)
type record =
  { global_ field : int
  ; normal_field : int
  };;

[%%expect{|
type record = { global_ field : int; normal_field : int; }
|}]

type 'a parameterized_record = {
  mutable a: 'a ;
  global_ c: 'a };;

[%%expect{|
type 'a parameterized_record = { mutable a : 'a; global_ c : 'a; }
|}]

type fn = local_ unique_ int -> local_ once_ int;;
type nested_fn = (local_ unique_ int -> local_ once_ int) -> local_ unique_ once_ int;;
type ('a, 'b) labeled_fn =
  a:local_ unique_ 'a -> ?b:local_ once_ 'b -> unique_ once_ 'a -> (int -> once_ unique_ 'b);;

[%%expect{|
type fn = local_ int @ unique -> local_ int @ once
type nested_fn =
    (local_ int @ unique -> local_ int @ once) -> local_ int @ once unique
type ('a, 'b) labeled_fn =
    a:local_ 'a @ unique ->
    ?b:local_ 'b @ once -> 'a @ once unique -> (int -> 'b @ once unique)
|}]

(* kitchen sink, with new @ syntax *)
let f ~(x1 @ many)
      ~(x2 : string @@ local)
      ~(x3 : string -> string @@ local)
      ~(x4 : 'a. 'a -> 'a @@ local)
      ~(local_ x5)
      ~x6:(local_ true | false @ many)
      ~x7:(local_ true | false : bool @@ many)
      ~x8:(local_ ())
      (_ : string @@ global)
      (_ @ local) =
  exclave_
  local_
  let x9 @ local = "hi" in
  let x10 : string @@ global = "hi" in
  let local_ x11 : int array @@ contended = [| |] in
  (* This next line actually doesn't pass the round-trip test: the parser
     does not accept [local_] and [type] on the same let. This seems annoying
     to fix, and we're getting rid of [local_] someday anyway. *)
  (* let x12 : type a b c. a -> (b -> a) @ local @@ local many = fun x _ -> x in *)
  let (x13 @ global) = "hi" in
  let (x14 @ local) y z = y + z in
  let (x15 @ local) y z : int = y + z in
  (* NO: let (x15 @ local) y z : int @@ local = y + z in *)
  let _ = ("hi" : string @@ global) in
  stack_ (x1, x2, x3, x4, x5, x9, x10, x11, (* x12, *) x13, x14, x15)

[%%expect{|
val f :
  x1:'b ->
  x2:local_ string ->
  x3:local_ (string -> string) ->
  x4:local_ ('a. 'a -> 'a) ->
  x5:local_ 'c ->
  x6:local_ bool ->
  x7:local_ bool ->
  x8:local_ unit ->
  string ->
  local_ 'd -> local_
  'b * string * (string -> string) * ('e -> 'e) * 'c * string * string *
  int array * string * (int -> local_ (int -> int)) *
  (int -> local_ (int -> int)) @ contended = <fun>
|}]

let f1 (_ @ local) = ()
let f2 () = let x @ local = [1; 2; 3] in f1 x [@nontail]

[%%expect{|
val f1 : local_ 'a -> unit = <fun>
val f2 : unit -> unit = <fun>
|}]

module type S = sig @@ portable contended
  module type S0 = sig @@ portable contended
    val x1 : string -> string @@ local
    val x2 : string -> string @@ portable
    val x3 : string -> string @@ nonportable
  end
  include S0 @@ portable many local
end

[%%expect{|
module type S =
  sig
    module type S0 =
      sig
        val x1 : string -> string
        val x2 : string -> string @@ portable
        val x3 : string -> string
      end
    val x1 : string -> string @@ many portable
    val x2 : string -> string @@ many portable
    val x3 : string -> string @@ many portable
  end
|}]

external x4 : string -> string @@ portable many = "%identity"

[%%expect{|
external x4 : string -> string @@ many portable = "%identity"
|}]

type t =
  | K1 of string @@ global * (float -> float) @@ many * string
  | K2 : string @@ global * (float -> float) @@ many * string -> t

type t = { x : string @@ global
         ; mutable y : float -> float @@ many
         ; global_ z : string @@ many }

type t1 = { mutable x : float
          ; mutable f : float -> float }

type t2 = { mutable x : float [@no_mutable_implied_modalities]
          ; mutable f : float -> float [@no_mutable_implied_modalities] }

[%%expect{|
type t =
    K1 of global_ string * (float -> float) @@ many * string
  | K2 : global_ string * (float -> float) @@ many * string -> t
type t = {
  global_ x : string;
  mutable y : float -> float;
  global_ z : string @@ many;
}
type t1 = { mutable x : float; mutable f : float -> float; }
type t2 = { mutable x : float; mutable f : float -> float; }
|}]

let f1 (x @ local) (f @ once) : t1 = exclave_ { x; f }

[%%expect{|
Line 1, characters 48-49:
1 | let f1 (x @ local) (f @ once) : t1 = exclave_ { x; f }
                                                    ^
Error: This value escapes its region.
|}]

let f2 (x @ local) (f @ once) : t2 = exclave_ { x; f }

[%%expect{|
val f2 : local_ float -> (float -> float) @ once -> local_ t2 @ once = <fun>
|}]


module type S = sig
  val bar : 'a -> 'a
  module M : sig
    val foo : 'a -> 'a
  end
end

module type S' = sig
  include [@no_recursive_modalities] S @@ portable
end

[%%expect{|
module type S =
  sig val bar : 'a -> 'a module M : sig val foo : 'a -> 'a end end
module type S' =
  sig
    val bar : 'a -> 'a @@ portable
    module M : sig val foo : 'a -> 'a end
  end
|}]

(* Modules *)

module type S = sig end
module M = struct end
[%%expect{|
module type S = sig end
module M : sig end
|}]

module F (X : S @@ portable) = struct
end
[%%expect{|
Line 1, characters 19-27:
1 | module F (X : S @@ portable) = struct
                       ^^^^^^^^
Error: Mode annotations on modules are not supported yet.
|}]

module F (_ : S @@ portable) = struct
end
[%%expect{|
Line 1, characters 19-27:
1 | module F (_ : S @@ portable) = struct
                       ^^^^^^^^
Error: Mode annotations on modules are not supported yet.
|}]

module M' = (M : S @@ portable)
[%%expect{|
Line 1, characters 22-30:
1 | module M' = (M : S @@ portable)
                          ^^^^^^^^
Error: Mode annotations on modules are not supported yet.
|}]

module F (M : S @@ portable) : S @@ portable = struct
end
[%%expect{|
Line 1, characters 19-27:
1 | module F (M : S @@ portable) : S @@ portable = struct
                       ^^^^^^^^
Error: Mode annotations on modules are not supported yet.
|}]

module F (M : S @@ portable) @ portable = struct
end
[%%expect{|
Line 1, characters 19-27:
1 | module F (M : S @@ portable) @ portable = struct
                       ^^^^^^^^
Error: Mode annotations on modules are not supported yet.
|}]



(* CR zqian: the similar syntax for expressions are not allowed because @ might
  be an binary operator *)
module M' = (M @ portable)
[%%expect{|
Line 1, characters 17-25:
1 | module M' = (M @ portable)
                     ^^^^^^^^
Error: Mode annotations on modules are not supported yet.
|}]

module M' = (M : S @@ portable)
[%%expect{|
Line 1, characters 22-30:
1 | module M' = (M : S @@ portable)
                          ^^^^^^^^
Error: Mode annotations on modules are not supported yet.
|}]

module M @ portable = struct end
[%%expect{|
Line 1, characters 11-19:
1 | module M @ portable = struct end
               ^^^^^^^^
Error: Mode annotations on modules are not supported yet.
|}]

module M : S @@ portable = struct end
[%%expect{|
Line 1, characters 16-24:
1 | module M : S @@ portable = struct end
                    ^^^^^^^^
Error: Mode annotations on modules are not supported yet.
|}]

module type S' = functor () (M : S @@ portable) (_ : S @@ portable) -> S @ portable
[%%expect{|
Line 1, characters 38-46:
1 | module type S' = functor () (M : S @@ portable) (_ : S @@ portable) -> S @ portable
                                          ^^^^^^^^
Error: Mode annotations on modules are not supported yet.
|}]


module type S' = () -> S @ portable -> S @ portable -> S @ portable
[%%expect{|
Line 1, characters 27-35:
1 | module type S' = () -> S @ portable -> S @ portable -> S @ portable
                               ^^^^^^^^
Error: Mode annotations on modules are not supported yet.
|}]

module (F @ portable) () = struct end
[%%expect{|
Line 1, characters 12-20:
1 | module (F @ portable) () = struct end
                ^^^^^^^^
Error: Mode annotations on modules are not supported yet.
|}]

module rec (F @ portable) () = struct end
and (G @ portable) () = struct end
[%%expect{|
Line 1, characters 16-24:
1 | module rec (F @ portable) () = struct end
                    ^^^^^^^^
Error: Mode annotations on modules are not supported yet.
|}]

module type T = sig
  module M : S @@ portable
  module M = N @ portable
  module (M @ portable) = N
  module M0 (_ : S @@ portable) (X : S @@ portable) : S @@ portable
  (* The above [@@ portable] is the return mode of the functor, not the modality on the
  module declaration; To do that, one must write in the following ways. *)
  module (M0 @ portable) (_ : S @@ portable) (X : S @@ portable) : S @@ portable
  module M0 : functor (_ : S @@ portable) (X : S @@ portable) -> S @ portable @@ portable

  module M0 : S @ portable -> S @ portable -> S @ portable @@ portable

  module rec F : sig end @@ portable
  and G : sig end @@ portable
end
[%%expect{|
Line 3, characters 13-14:
3 |   module M = N @ portable
                 ^
Error: Unbound module "N"
|}]

let foo () =
  let module (F @ portable) () = struct end in
  ()
[%%expect{|
Line 2, characters 18-26:
2 |   let module (F @ portable) () = struct end in
                      ^^^^^^^^
Error: Mode annotations on modules are not supported yet.
|}]

(**********)
(* stack *)

let f x = stack_ (ref x)

[%%expect{|
Line 1, characters 10-24:
1 | let f x = stack_ (ref x)
              ^^^^^^^^^^^^^^
Error: This value escapes its region.
  Hint: Cannot return a local value without an "exclave_" annotation.
|}]

type t = { a : int }
let f a =
  let y = stack_ { a } in
  y.a

[%%expect{|
type t = { a : int; }
val f : int -> int = <fun>
|}]

let apply ~(f @ local) x = f x [@nontail]
let double1 y = apply ~f:(stack_ fun x -> x + y) y [@nontail]
let double2 y = apply ~f:(stack_ function x -> x + y) y [@nontail]

[%%expect{|
val apply : f:local_ ('a -> 'b) -> 'a -> 'b = <fun>
val double1 : int -> int = <fun>
val double2 : int -> int = <fun>
|}]

let make_tuple x y z = stack_ (x, y), z
[%%expect{|
Line 1, characters 23-36:
1 | let make_tuple x y z = stack_ (x, y), z
                           ^^^^^^^^^^^^^
Error: This value escapes its region.
|}]

type u = A of unit | C of int | B of int * int | D

let create_us () =
  let a = stack_ A () in
  let b = stack_ B (1, 2) in
  let c = stack_ C 3 in
  let d = stack_ D in
  ()

[%%expect{|
type u = A of unit | C of int | B of int * int | D
Line 7, characters 17-18:
7 |   let d = stack_ D in
                     ^
Error: This expression is not an allocation site.
|}]

(**********)
(* unique *)

(* No syntax; the unique extension just enables the uniqueness checker. *)


(*******************)
(* Include functor *)

module F_struct (_ : sig end) = struct
end

module type F_sig = functor (_ : sig end) -> sig end

module T = struct
  include functor F_struct
end;;

module type S = sig
  include functor F_sig
end;;

[%%expect{|
module F_struct : sig end -> sig end
module type F_sig = sig end -> sig end
module T : sig end
module type S = sig end
|}]

(********************)
(* Immutable arrays *)

let f x =
  match x with
  | [::] -> [::]
  | ([:x:] [@test.attr1]) -> (([:x:])[@test.attr1])
  | ([:x;y:] [@test.attr2][@test.attr3]) ->
      ([:x;y:] [@test.attr2][@test.attr3])
  | _ -> assert false;;

[%%expect{|
val f : 'a iarray -> 'a iarray = <fun>
|}]

(******************)
(* Labeled tuples *)
let z, punned = 4, 5
let x_must_be_even _ = assert false
exception Odd

let x = (~x:1, ~y:2)
let x = ((~x:1, ~y:2) [@test.attr])
let _ = ( ~x: 5, 2, ~z, ~(punned:int))
let (x : (x:int * y:int)) = (~x:1, ~y:2)
let (x : ((x:int * y:int) [@test.attr])) = (~x:1, ~y:2)

[%%expect{|
val z : int = 4
val punned : int = 5
val x_must_be_even : 'a -> 'b = <fun>
exception Odd
val x : x:int * y:int = (~x:1, ~y:2)
val x : x:int * y:int = (~x:1, ~y:2)
- : x:int * int * z:int * punned:int = (~x:5, 2, ~z:4, ~punned:5)
val x : x:int * y:int = (~x:1, ~y:2)
val x : x:int * y:int = (~x:1, ~y:2)
|}]

let (~x:x0, ~s, ~(y:int), ..) : (x:int * s:string * y:int * string) =
   (~x: 1, ~s: "a", ~y: 2, "ignore me")

[%%expect{|
val x0 : int = 1
val s : string = "a"
val y : int = 2
|}]

module M : sig
  val f : (x:int * string) -> (x:int * string)
  val mk : unit -> (x:bool * y:string)
end = struct
  let f x = x
  let mk () = (~x:false, ~y:"hi")
end

module X_int_int = struct
   type t = (x:int * int)
end

[%%expect{|
module M :
  sig
    val f : (x:int * string) -> x:int * string
    val mk : unit -> x:bool * y:string
  end
module X_int_int : sig type t = x:int * int end
|}]

let foo xy k_good k_bad =
   match x_must_be_even xy with
   | (~x, y) -> k_good ()
   | exception Odd -> k_bad ()

let (~(x:int), ~y, _) = (~x: 1, ~y: 2, "ignore me")
let ((~(x:int), ~y, _) [@test.attr]) = (~x: 1, ~y: 2, "ignore me")
let f = fun (~foo, ~bar:bar) -> foo * 10 + bar
let f ((~(x:int),y) : (x:int * int)) : int = x + y

[%%expect{|
val foo : 'a -> (unit -> 'b) -> (unit -> 'b) -> 'b = <fun>
val x : int = 1
val y : int = 2
val x : int = 1
val y : int = 2
val f : (foo:int * bar:int) -> int = <fun>
val f : (x:int * int) -> int = <fun>
|}]

type xy = (x:int * y:int)

(* Reordering and partial matches *)
let lt = (~x:1, ~y:2, ~x:3, 4)

let matches =
  let (~x, .. ) = lt in
  x

let matches =
  let (~y, ~x, .. ) = lt in
  (x, y)

[%%expect{|
type xy = x:int * y:int
val lt : x:int * y:int * x:int * int = (~x:1, ~y:2, ~x:3, 4)
val matches : int = 1
val matches : int * int = (1, 2)
|}]

(********************)
(* Unboxed literals *)

module Float_u = Stdlib_upstream_compatible.Float_u
module Int32_u = Stdlib_upstream_compatible.Int32_u
module Int64_u = Stdlib_upstream_compatible.Int64_u
module Nativeint_u = Stdlib_upstream_compatible.Nativeint_u

[%%expect{|
module Float_u = Stdlib_upstream_compatible.Float_u
module Int32_u = Stdlib_upstream_compatible.Int32_u
module Int64_u = Stdlib_upstream_compatible.Int64_u
module Nativeint_u = Stdlib_upstream_compatible.Nativeint_u
|}]

let test_float s f =
  Format.printf "%s: %f\n" s (Float_u.to_float f); Format.print_flush ()
let test_int32 s f =
  Format.printf "%s: %ld\n" s (Int32_u.to_int32 f); Format.print_flush ()
let test_int64 s f =
  Format.printf "%s: %Ld\n" s (Int64_u.to_int64 f); Format.print_flush ()
let test_nativeint s f =
  Format.printf "%s: %s\n" s (Nativeint_u.to_string f); Format.print_flush ()

[%%expect{|
val test_float : string -> Float_u.t -> unit = <fun>
val test_int32 : string -> Int32_u.t -> unit = <fun>
val test_int64 : string -> Int64_u.t -> unit = <fun>
val test_nativeint : string -> Nativeint_u.t -> unit = <fun>
|}]

(* Expressions *)

let x = test_float "e" #2.718281828459045
let x = test_float "negative_one_half" (-#0.5)
let x = test_float "negative_one_half" (- #0.5)
let x = test_float "negative_one_half" (-.#0.5)
let x = test_float "negative_one_half" (-. #0.5)
let x = test_float "positive_one_dot" (+#1.)
let x = test_float "positive_one_dot" (+ #1.)
let x = test_float "positive_one_dot" (+.#1.)
let x = test_float "positive_one_dot" (+. #1.)
let x = test_float "one_billion" (#1e9)
let x = test_float "one_twenty_seven_point_two_five_in_floating_hex" (#0x7f.4)
let x = test_float "five_point_three_seven_five_in_floating_hexponent" (#0xa.cp-1)

let x = test_nativeint "zero" (#0n)
let x = test_int32 "positive_one" (+#1l)
let x = test_int32 "positive_one" (+ #1l)
let x = test_int64 "negative_one" (-#1L)
let x = test_int64 "negative_one" (- #1L)
let x = test_nativeint "two_fifty_five_in_hex" (#0xFFn)
let x = test_int32 "twenty_five_in_octal" (#0o31l)
let x = test_int64 "forty_two_in_binary" (#0b101010L)

[%%expect{|
e: 2.718282
val x : unit = ()
negative_one_half: -0.500000
val x : unit = ()
negative_one_half: -0.500000
val x : unit = ()
negative_one_half: -0.500000
val x : unit = ()
negative_one_half: -0.500000
val x : unit = ()
positive_one_dot: 1.000000
val x : unit = ()
positive_one_dot: 1.000000
val x : unit = ()
positive_one_dot: 1.000000
val x : unit = ()
positive_one_dot: 1.000000
val x : unit = ()
one_billion: 1000000000.000000
val x : unit = ()
one_twenty_seven_point_two_five_in_floating_hex: 127.250000
val x : unit = ()
five_point_three_seven_five_in_floating_hexponent: 5.375000
val x : unit = ()
zero: 0
val x : unit = ()
positive_one: 1
val x : unit = ()
positive_one: 1
val x : unit = ()
negative_one: -1
val x : unit = ()
negative_one: -1
val x : unit = ()
two_fifty_five_in_hex: 255
val x : unit = ()
twenty_five_in_octal: 25
val x : unit = ()
forty_two_in_binary: 42
val x : unit = ()
|}]

(* Patterns *)

let f x =
  match x with
  | #4. -> `Four
  | #5. -> `Five
  | _ -> `Other
;;

let x =
  match f #5. with
  | `Five -> ()
  | _ -> assert false;;

let f x =
  match x with
  | #4. -> #0.
  | #5. -> #1.
  | x ->  x
;;

[%%expect{|
val f : float# -> [> `Five | `Four | `Other ] = <fun>
val x : unit = ()
val f : float# -> float# = <fun>
|}]
;;
test_float "result" (f #7.);;

let f x =
  match x with
  | #4. -> #0.
  | #5. -> #1.
  | #6. -> #2.
  | #7. -> #3.
  | #8. -> #4.
  | #9. -> #5.
  | #10. -> #6.
  | #11. -> #7.
  | x ->  x
;;

test_float "larger match result" (f #7.);;


let f x =
  match x with
  | #4L -> `Four
  | #5L -> `Five
  | _ -> `Other
;;

[%%expect{|
result: 7.000000
- : unit = ()
val f : float# -> float# = <fun>
larger match result: 3.000000
- : unit = ()
val f : int64# -> [> `Five | `Four | `Other ] = <fun>
|}]

let x =
  match f #4L with
  | `Four -> ()
  | _ -> assert false;;

let f x =
  match x with
  | #4L -> #0L
  | #5L -> #1L
  | x ->  x
;;

test_int64 "result" (f #7L);;

[%%expect{|
val x : unit = ()
val f : int64# -> int64# = <fun>
result: 7
- : unit = ()
|}]

(*******************)
(* Unboxed records *)

type 'a with_idx : value & immediate = #{ data : 'a ; i : int }
let idx #{ data = _ ; i } = i
let #{ data = payload; _ } = #{ data = "payload" ; i = 0 }
let inc r = #{ r with i = r.#i + 1 }
[%%expect{|
type 'a with_idx = #{ data : 'a; i : int; }
val idx : 'a with_idx -> int = <fun>
val payload : string = "payload"
val inc : 'a with_idx -> 'a with_idx = <fun>
|}]

(***************)
(* Modal kinds *)

(* supported *)
type 'a list : immutable_data with 'a
type ('a, 'b) either : immutable_data with 'a * 'b
type 'a contended : immutable_data with 'a @@ contended
type 'a contended_with_int : immutable_data with 'a @@ contended with int

[%%expect{|
type 'a list : immutable_data with 'a @@ global aliased
type ('a, 'b) either : immutable_data with 'a * 'b @@ global aliased
type 'a contended : immutable_data with 'a @@ global aliased contended
type 'a contended_with_int
  : immutable_data
  with 'a @@ global aliased contended
|}]

(* not yet supported *)
module _ : sig
  type 'a gel : kind_of_ 'a mod global
  type 'a t : _
  kind_abbrev_ immediate = value mod global unique many sync uncontended
  kind_abbrev_ immutable_data = value mod sync uncontended many
  kind_abbrev_ immutable = value mod uncontended
  kind_abbrev_ data = value mod sync many
end = struct
  type 'a gel : kind_of_ 'a mod global
  type 'a t : _
  kind_abbrev_ immediate = value mod global unique many sync uncontended
  kind_abbrev_ immutable_data = value mod sync uncontended many
  kind_abbrev_ immutable = value mod uncontended
  kind_abbrev_ data = value mod sync many
end

(* CR layouts v2.8: Expect this output to change once modal kinds are
   supported. *)

[%%expect{|
Line 9, characters 16-27:
9 |   type 'a gel : kind_of_ 'a mod global
                    ^^^^^^^^^^^
Error: Unimplemented kind syntax
|}]

(**************************)
(* Polymorphic parameters *)

type t = ('a. 'a -> 'a) -> int

let f (_ : 'a. 'a -> 'a) = ()

[%%expect{|
type t = ('a. 'a -> 'a) -> int
val f : ('a. 'a -> 'a) -> unit = <fun>
|}]

(************************)
(* Module strengthening *)

module type S = sig
  type t1
  type t2
  type t3
end

module M = struct
  type t1 = int
  type t2 = K of string
  type t3
end

module type S2 = S with M

[%%expect{|
module type S = sig type t1 type t2 type t3 end
module M : sig type t1 = int type t2 = K of string type t3 end
module type S2 = sig type t1 = M.t1 type t2 = M.t2 type t3 = M.t3 end
|}]

(*****************)
(* small numbers *)

type t1 = float32
type t2 = float32#

let x = 3.14s
let x () = #3.14s

[%%expect{|
type t1 = float32
type t2 = float32#
val x : float32 = 3.1400001s
val x : unit -> float32# = <fun>
|}]

(********)
(* simd *)
(* CR mslater: Move documentation to GitHub *)

type t1 = int8x16
type t2 = int16x8
type t3 = int32x4
type t4 = int64x2
type t5 = float32x4
type t6 = float64x2

[%%expect{|
type t1 = int8x16
type t2 = int16x8
type t3 = int32x4
type t4 = int64x2
type t5 = float32x4
type t6 = float64x2
|}]

(**************)
(* zero_alloc *)

(* CR gyorsh or ccasinghino: Add examples *)
(* CR gyorsh or ccasinghino: Add documentation to GitHub *)

(*****************)
(* error_message *)

let f (x : bool) = (x : int)[@error_message "custom message"]

[%%expect{|
Line 1, characters 20-21:
1 | let f (x : bool) = (x : int)[@error_message "custom message"]
                        ^
Error: This expression has type "bool" but an expression was expected of type
         "int"
       custom message
|}]

let f (v : float#) : ((_ : value)[@error_message "need a value"]) = v

[%%expect{|
Line 1, characters 68-69:
1 | let f (v : float#) : ((_ : value)[@error_message "need a value"]) = v
                                                                        ^
Error: This expression has type "float#" but an expression was expected of type
         "('a : value)"
       The layout of float# is float64
         because it is the primitive type float#.
       But the layout of float# must be a sublayout of value
         because of the annotation on the wildcard _ at line 1, characters 22-33.
         need a value
|}]

(*********************************)
(* Instance names as identifiers *)

module Base (_ : sig end) (_ : sig end) (_ : sig end) (_ : sig end) = struct end
module Name1 = struct end
module Name2 = struct end
module Value1 = struct end
module Value2 (_ : sig end) (_ : sig end) = struct end
module Name2_1 = struct end
module Name2_1 = struct end

module _ = Base(Name1)(Value1)(Name2)(Value2(Name2_1)(Value2_1)) [@jane.non_erasable.instances]


[%%expect{|
module Base : sig end -> sig end -> sig end -> sig end -> sig end
module Name1 : sig end
module Name2 : sig end
module Value1 : sig end
module Value2 : sig end -> sig end -> sig end
module Name2_1 : sig end
module Name2_1 : sig end
Line 9, characters 11-95:
9 | module _ = Base(Name1)(Value1)(Name2)(Value2(Name2_1)(Value2_1)) [@jane.non_erasable.instances]
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Unbound module "Base[Name1:Value1][Name2:Value2[Name2_1:Value2_1]]"
|}]

(***************)
(* Overwriting *)


let overwrite_tuple = function
    (a, b) as t -> overwrite_ t with (b, _)

type record = { a : int; b : int }

let overwrite_record = function
    { a; b } as t -> overwrite_ t with { b = a; a = _ }

let overwrite_record = function
    { a; b } as t -> overwrite_ t with { b = a }

let ret_record () = { a = 1; b = 2 }

let overwrite_record () =
  overwrite_ (ret_record ()) with { b = a }

type constructor = C of { a : int; b : int }

let overwrite_constructor = function
    C { a; b } as t -> overwrite_ t with C { b = a; a = _ }

let overwrite_constructor = function
    C { a; b } as t -> overwrite_ t with C { b = a }
[%%expect{|
Line 2, characters 19-43:
2 |     (a, b) as t -> overwrite_ t with (b, _)
                       ^^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
Uncaught exception: File "parsing/location.ml", line 1107, characters 2-8: Assertion failed

|}]

(************)
(* call_pos *)

let f ~(x : [%call_pos]) () = x;;

[%%expect{|
val f : x:[%call_pos] -> unit -> lexing_position = <fun>
|}]

type t = x:[%call_pos] -> int

[%%expect{|
type t = x:[%call_pos] -> int
|}]

let f g here = g ~(here : [%call_pos])

[%%expect{|
val f : (here:[%call_pos] -> 'a) -> lexing_position -> 'a = <fun>
|}]
