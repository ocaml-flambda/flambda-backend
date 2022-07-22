(* TEST
   * expect
*)

(* These tests check that the include functor feature doesn't work without the
   extension flag *)
module type S = sig
  type t
  val x : t
end

module F1 (X : S) = struct
  let y = X.x
end

module M1 = struct
  type t = int
  let x = 5

  include functor F1
end

let () = assert Int.(equal M1.y 5);;
[%%expect{|
module type S = sig type t val x : t end
module F1 : functor (X : S) -> sig val y : X.t end
Line 14, characters 2-20:
14 |   include functor F1
       ^^^^^^^^^^^^^^^^^^
Error: The include_functor extension is disabled
       To enable it, pass the '-extension include_functor' flag
|}];;

(* Test 2: Include functor in signature *)
module type T = sig
  type s
  val f : s -> bool
end

module type F2 = functor (X : S) -> T with type s = X.t

module type M2_sig = sig
  type t
  val x : t

  include functor F2
end

module M2_impl : M2_sig = struct
  type t = int
  type s = t

  let x = 5
  let f s = x = s
end
let () = assert (M2_impl.f M2_impl.x);;
[%%expect{|
module type T = sig type s val f : s -> bool end
module type F2 = functor (X : S) -> sig type s = X.t val f : s -> bool end
Line 12, characters 2-20:
12 |   include functor F2
       ^^^^^^^^^^^^^^^^^^
Error: The include_functor extension is disabled
       To enable it, pass the '-extension include_functor' flag
|}];;

(* Test 3: Include functor at top level. *)
type t = int
let x : t = 3
let x : t = 5
include functor F1

let () = assert (Int.(equal y 5));;
[%%expect{|
type t = int
val x : t = 3
val x : t = 5
Line 4, characters 0-18:
4 | include functor F1
    ^^^^^^^^^^^^^^^^^^
Error: The include_functor extension is disabled
       To enable it, pass the '-extension include_functor' flag
|}];;
