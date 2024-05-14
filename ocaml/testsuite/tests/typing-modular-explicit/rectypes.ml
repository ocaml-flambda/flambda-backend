(* TEST
  flags = " -rectypes ";
  expect;
*)

module type T = sig
  type t
end

[%%expect{|
module type T = sig type t end
|}]

let f (x : (module M : T) -> (module M : T) -> 'a as 'a) =
  (x : ((module N : T) -> 'b) as 'b)

[%%expect{|
val f : ((module N : T) -> 'a as 'a) -> 'a = <fun>
|}, Principal{|
val f :
  ((module M/1 : T) -> (module M/2 : T) -> 'a as 'a) ->
  ((module N : T) -> 'b as 'b) = <fun>
|}]


let f (x : (module M : T) -> ((module M : T) -> 'a as 'a)) =
  (x : ((module N : T) -> 'b) as 'b)

[%%expect{|
val f : ((module M : T) -> ((module N : T) -> 'a as 'a)) -> 'a = <fun>
|}, Principal{|
val f :
  ((module M/1 : T) -> ((module M/2 : T) -> 'a as 'a)) ->
  ((module N : T) -> 'b as 'b) = <fun>
|}]

let f (module M : T) (x : (module M : T) -> 'a as 'a) =
  x (module M) (module M) (module M) (module M) (module M)

[%%expect{|
val f : (module T) -> ((module M : T) -> 'a as 'a) -> 'a = <fun>
|}, Principal{|
Line 2, characters 15-25:
2 |   x (module M) (module M) (module M) (module M) (module M)
                   ^^^^^^^^^^
Warning 18 [not-principal]: this module packing is not principal.

Line 2, characters 26-36:
2 |   x (module M) (module M) (module M) (module M) (module M)
                              ^^^^^^^^^^
Warning 18 [not-principal]: this module packing is not principal.

Line 2, characters 37-47:
2 |   x (module M) (module M) (module M) (module M) (module M)
                                         ^^^^^^^^^^
Warning 18 [not-principal]: this module packing is not principal.

Line 2, characters 48-58:
2 |   x (module M) (module M) (module M) (module M) (module M)
                                                    ^^^^^^^^^^
Warning 18 [not-principal]: this module packing is not principal.

val f :
  (module T) ->
  ((module M/1 : T) -> 'a as 'a) -> ((module M/2 : T) -> 'b as 'b) = <fun>
|}]

let f (x : (module M : T) -> (M.t * ((module N : T) -> 'a) as 'a)) =
  (x : ((module O : T) -> O.t * 'b) as 'b)

[%%expect{|
Line 2, characters 3-4:
2 |   (x : ((module O : T) -> O.t * 'b) as 'b)
       ^
Error: This expression has type
         "(module O : T) -> (O.t * ((module N : T) -> 'a) as 'a)"
       but an expression was expected of type
         "(module O : T) -> O.t * 'b as 'b"
       The module O would escape its scope
|}]

let f (x : (module M : T with type t = int) ->
  (M.t * ((module N : T with type t = int) -> 'a) as 'a)) =
(x : ((module O : T with type t = int) -> O.t * 'b) as 'b)

(* Error : this should be corrected *)
[%%expect{|
Line 2, characters 3-6:
2 |   (M.t * ((module N : T with type t = int) -> 'a) as 'a)) =
       ^^^
Error: Tuple element types must have layout value.
       The layout of M.t is any, because
         it's assigned a dummy layout that should have been overwritten.
         Please notify the Jane Street compilers group if you see this output.
       But the layout of M.t must be a sublayout of value, because
         it's the type of a tuple element.
|}]

let f (x : (module M : T) -> (M.t * ((module N : T) -> (N.t * 'a) as 'a))) =
  (x : ((module O : T) -> O.t * 'b) as 'b)

[%%expect{|
val f : ((module M : T) -> M.t * ((module O : T) -> O.t * 'a as 'a)) -> 'a =
  <fun>
|}, Principal{|
val f :
  ((module M : T) -> M.t * ((module N : T) -> N.t * 'a as 'a)) ->
  ((module O : T) -> O.t * 'b as 'b) = <fun>
|}]
