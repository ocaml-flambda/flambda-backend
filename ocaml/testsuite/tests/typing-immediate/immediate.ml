(* TEST
   * expect
*)

module type S = sig type t [@@immediate] end;;
module F (M : S) : S = M;;
[%%expect{|
module type S = sig type t : immediate end
module F : functor (M : S) -> S
|}];;

(* VALID DECLARATIONS *)

module A = struct
  (* Abstract types can be immediate *)
  type t [@@immediate]

  (* [@@immediate] tag here is unnecessary but valid since t has it *)
  type s = t [@@immediate]

  (* Again, valid alias even without tag *)
  type r = s

  (* Mutually recursive declarations work as well *)
  type p = q [@@immediate]
  and q = int

  (* Variants with only constant constructors are immediate *)
  type o = Foo | Bar | Baz [@@immediate]

  (* Can declare with a weaker immediacy than necessary *)
  type m = int [@@immediate64]

  (* ... and yet use the stronger one by expansion later *)
  type n = m [@@immediate]
end;;
[%%expect{|
module A :
  sig
    type t : immediate
    type s = t
    type r = s
    type p = q
    and q = int
    type o = Foo | Bar | Baz
    type m = int
    type n = m
  end
|}];;

(* Valid using with constraints *)
module type X = sig type t end;;
module Y = struct type t = int end;;
module Z = ((Y : X with type t = int) : sig type t [@@immediate] end);;
[%%expect{|
module type X = sig type t end
module Y : sig type t = int end
module Z : sig type t : immediate end
|}];;

(* Valid using an explicit signature *)
module M_valid : S = struct type t = int end;;
module FM_valid = F (struct type t = int end);;
[%%expect{|
module M_valid : S
module FM_valid : S
|}];;

(* Valid for empty types *)
module Empty_valid : S = struct type t = | end;;
[%%expect{|
module Empty_valid : S
|}];;

(* Valid when unboxed *)
module Unboxed_valid = struct
  type t = { x : int } [@@unboxed] [@@immediate]

  type u = { x : s } [@@unboxed] [@@immediate]
  and s = int
end;;
[%%expect{|
module Unboxed_valid :
  sig
    type t : immediate = { x : int; } [@@unboxed]
    type u : immediate = { x : s; } [@@unboxed]
    and s = int
  end
|}];;

(* Practical usage over modules *)
module Foo : sig type t val x : t ref end = struct
  type t = int
  let x = ref 0
end;;
[%%expect{|
module Foo : sig type t val x : t ref end
|}];;

module Bar : sig type t [@@immediate] val x : t ref end = struct
  type t = int
  let x = ref 0
end;;
[%%expect{|
module Bar : sig type t : immediate val x : t ref end
|}];;

let test f =
  let start = Sys.time() in f ();
  (Sys.time() -. start);;
[%%expect{|
val test : (unit -> 'a) -> float = <fun>
|}];;

let test_foo () =
  for i = 0 to 100_000_000 do
    Foo.x := !Foo.x
  done;;
[%%expect{|
val test_foo : unit -> unit = <fun>
|}];;

let test_bar () =
  for i = 0 to 100_000_000 do
    Bar.x := !Bar.x
  done;;
[%%expect{|
val test_bar : unit -> unit = <fun>
|}];;

(* Uncomment these to test. Should see substantial speedup!
let () = Printf.printf "No @@immediate: %fs\n" (test test_foo)
let () = Printf.printf "With @@immediate: %fs\n" (test test_bar) *)


(* INVALID DECLARATIONS *)

(* Cannot directly declare a non-immediate type as immediate *)
module B = struct
  type t = string [@@immediate]
end;;
[%%expect{|
Line 2, characters 2-31:
2 |   type t = string [@@immediate]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type string has layout value, which is not a sublayout of immediate.
|}];;

(* Cannot directly declare a non-immediate type as immediate (variant) *)
module B = struct
  type t = Foo of int | Bar [@@immediate]
end;;
[%%expect{|
Line 2, characters 2-41:
2 |   type t = Foo of int | Bar [@@immediate]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type t has layout value, which is not a sublayout of immediate.
|}];;

(* Cannot directly declare a non-immediate type as immediate (record) *)
module B = struct
  type t = { foo : int } [@@immediate]
end;;
[%%expect{|
Line 2, characters 2-38:
2 |   type t = { foo : int } [@@immediate]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type t has layout value, which is not a sublayout of immediate.
|}];;

(* Not guaranteed that t is immediate, so this is an invalid declaration *)
module C = struct
  type t
  type s = t [@@immediate]
end;;
[%%expect{|
Line 3, characters 2-26:
3 |   type s = t [@@immediate]
      ^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type t has layout value, which is not a sublayout of immediate.
|}];;

(* Can't ascribe to an immediate type signature with a non-immediate type *)
module D : sig type t [@@immediate] end = struct
  type t = string
end;;
[%%expect{|
Lines 1-3, characters 42-3:
1 | ..........................................struct
2 |   type t = string
3 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = string end
       is not included in
         sig type t : immediate end
       Type declarations do not match:
         type t = string
       is not included in
         type t : immediate
       the first has layout value, which is not a sublayout of immediate.
|}];;

(* Same as above but with explicit signature *)
module M_invalid : S = struct type t = string end;;
module FM_invalid = F (struct type t = string end);;
[%%expect{|
Line 1, characters 23-49:
1 | module M_invalid : S = struct type t = string end;;
                           ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match: sig type t = string end is not included in S
       Type declarations do not match:
         type t = string
       is not included in
         type t : immediate
       the first has layout value, which is not a sublayout of immediate.
|}];;

(* Can't use a non-immediate type even if mutually recursive *)
module E = struct
  type t = s [@@immediate]
  and s = string
end;;
[%%expect{|
Line 2, characters 2-26:
2 |   type t = s [@@immediate]
      ^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type s has layout value, which is not a sublayout of immediate.
|}];;


(* Aliases should be expanded to check immediacy *)
type 'a id = 'a
type s = int id [@@immediate]
[%%expect{|
type 'a id = 'a
type s = int id
|}];;
module F (X : sig type t end) = X
module I = struct type t = int end
type t = F(I).t [@@immediate]
[%%expect{|
module F : functor (X : sig type t end) -> sig type t = X.t end
module I : sig type t = int end
type t = F(I).t
|}];;
module F (X : sig type t end) = X
module I : sig type t = private int end = struct type t = int end
type t = F(I).t [@@immediate]
[%%expect{|
module F : functor (X : sig type t end) -> sig type t = X.t end
module I : sig type t = private int end
type t = F(I).t
|}];;
module type T = sig type t type s = t end
module F (X : T with type t = int) = struct
  type t = X.s [@@immediate]
end
[%%expect{|
module type T = sig type t type s = t end
module F :
  functor (X : sig type t = int type s = t end) -> sig type t = X.s end
|}];;
module type T = sig type t type s = t end
module F (X : T with type t = private int) = struct
  type t = X.s [@@immediate]
end
[%%expect{|
module type T = sig type t type s = t end
module F :
  functor (X : sig type t = private int type s = t end) ->
    sig type t = X.s end
|}];;
type t = int s [@@immediate] and 'a s = 'a
[%%expect{|
type t = int s
and 'a s = 'a
|}];;
