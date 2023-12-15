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
Error: The layout of type string is value, because
         it is the primitive value type string.
       But the layout of type string must be a sublayout of immediate, because
         of the definition of t at line 2, characters 2-31.
|}];;
(* CR layouts v2.9: The "of the definition of t ..." part is not great and it
   should only refer to definitions that type check. Fixing it will involve
   building a second [final_env] in [transl_type_decl] which is costly.  *)

(* Cannot directly declare a non-immediate type as immediate (variant) *)
module B = struct
  type t = Foo of int | Bar [@@immediate]
end;;
[%%expect{|
Line 2, characters 2-41:
2 |   type t = Foo of int | Bar [@@immediate]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t is value, because
         it's a boxed variant type.
       But the layout of type t must be a sublayout of immediate, because
         of the annotation on the declaration of the type t.
|}];;

(* Cannot directly declare a non-immediate type as immediate (record) *)
module B = struct
  type t = { foo : int } [@@immediate]
end;;
[%%expect{|
Line 2, characters 2-38:
2 |   type t = { foo : int } [@@immediate]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t is value, because
         it's a boxed record type.
       But the layout of type t must be a sublayout of immediate, because
         of the annotation on the declaration of the type t/2.
|}];;
(* CR layouts v2.9: Investigate why the "/2" is here and check if it's only limited
   to expect tests. *)

(* Not guaranteed that t is immediate, so this is an invalid declaration *)
module C = struct
  type t
  type s = t [@@immediate]
end;;
[%%expect{|
Line 3, characters 2-26:
3 |   type s = t [@@immediate]
      ^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type t is value, because
         of the definition of t at line 2, characters 2-8.
       But the layout of type t must be a sublayout of immediate, because
         of the definition of s at line 3, characters 2-26.
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
       The layout of the first is value, because
         it is the primitive value type string.
       But the layout of the first must be a sublayout of immediate, because
         of the definition of t at line 1, characters 15-35.
|}];;

(* Same as above but with explicit signature *)
module M_invalid : S = struct type t = string end;;
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
       The layout of the first is value, because
         it is the primitive value type string.
       But the layout of the first must be a sublayout of immediate, because
         of the definition of t at line 1, characters 20-40.
|}];;

module FM_invalid = F (struct type t = string end);;
[%%expect{|
Line 1, characters 20-50:
1 | module FM_invalid = F (struct type t = string end);;
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Modules do not match: sig type t = string end is not included in
       S
     Type declarations do not match:
       type t = string
     is not included in
       type t : immediate
     The layout of the first is value, because
       it is the primitive value type string.
     But the layout of the first must be a sublayout of immediate, because
       of the definition of t at line 1, characters 20-40.
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
Error: The layout of type s is value, because
         it is the primitive value type string.
       But the layout of type s must be a sublayout of immediate, because
         of the definition of t at line 2, characters 2-26.
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
