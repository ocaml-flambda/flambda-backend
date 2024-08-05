(* TEST
  flags = "-extension layouts_alpha";
  expect;
*)

(* Basic tests *)

let x = (42, 1337)
[%%expect {|
val x : int * int = (42, 1337)
|}]

type 'a l = new int * 'a
let x1 = (x :> int l)
[%%expect {|
type 'a l = new int * 'a
val x1 : int l = (42, 1337)
|}]

type 'a r = new 'a * int
let x2 = (x :> int r)
[%%expect {|
type 'a r = new 'a * int
val x2 : int r = (42, 1337)
|}]

type 'a l' = new 'a l
let x1' = (x :> int l')
[%%expect {|
type 'a l' = new 'a l
val x1' : int l' = (42, 1337)
|}]

let x2_ = (x1' :> int r) (* expand on both sides *)
[%%expect {|
val x2_ : int r = (42, 1337)
|}]


(* New types don't unify *)

let id_l (x : int l) = x
let id_r (x : int r) = x
let id_l' (x : int l') = x
[%%expect {|
val id_l : int l -> int l = <fun>
val id_r : int r -> int r = <fun>
val id_l' : int l' -> int l' = <fun>
|}]

let y11 = id_l x1
[%%expect {|
val y11 : int l = (42, 1337)
|}]
let y11' = id_l x1'
[%%expect {|
Line 1, characters 16-19:
1 | let y11' = id_l x1'
                    ^^^
Error: This expression has type int l' but an expression was expected of type
         int l
|}]
let y12 = id_l x2
[%%expect {|
Line 1, characters 15-17:
1 | let y12 = id_l x2
                   ^^
Error: This expression has type int r but an expression was expected of type
         int l
|}]

(* Nested coercions *)

let xs = [(0, 1); (1, 2); (2, 3)]
let xs1 = (xs :> int l list)
[%%expect {|
val xs : (int * int) list = [(0, 1); (1, 2); (2, 3)]
val xs1 : int l list = [(0, 1); (1, 2); (2, 3)]
|}]

let xs1' = (xs :> int l' list)
[%%expect {|
val xs1' : int l' list = [(0, 1); (1, 2); (2, 3)]
|}]


(* Non-abstract types cannot be new *)

type 'a t = new { foo : 'a }
[%%expect {|
Line 1, characters 0-28:
1 | type 'a t = new { foo : 'a }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Cannot define a non-abstract new type
|}]

type 'a t = new Foo | Bar
[%%expect {|
Line 1, characters 0-25:
1 | type 'a t = new Foo | Bar
    ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Cannot define a non-abstract new type
|}]


(* Inclusion checks *)

module M : sig
  type t = new int
end = struct
  type t = new int
end
[%%expect {|
Line 1:
Error: In module M:
       Modules do not match:
         sig type t = M.t end
       is not included in
         sig type t = new int end
       In module M:
       Type declarations do not match:
         type t = M.t
       is not included in
         type t = new int
       The type M.t is not equal to the type int
|}]

module M : sig
  type t = new int
end = struct
  type t = int
end
[%%expect {|
Line 1:
Error: In module M:
       Modules do not match:
         sig type t = M.t end
       is not included in
         sig type t = new int end
       In module M:
       Type declarations do not match:
         type t = M.t
       is not included in
         type t = new int
       The type M.t is not equal to the type int
|}]

module M : sig
  type t = new int
end = struct
  type t = new int
end
[%%expect {|
Line 1:
Error: In module M:
       Modules do not match:
         sig type t = M.t end
       is not included in
         sig type t = new int end
       In module M:
       Type declarations do not match:
         type t = M.t
       is not included in
         type t = new int
       The type M.t is not equal to the type int
|}]

module M : sig
  type t = new int
end = struct
  type t = private int
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private int
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = private int end
       is not included in
         sig type t = new int end
       Type declarations do not match:
         type t = private int
       is not included in
         type t = new int
       A private newtype would be revealed.
|}]

module M : sig
  type t = new int
end = struct
  type t
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t end
       is not included in
         sig type t = new int end
       Type declarations do not match:
         type t
       is not included in
         type t = new int
       The type t is not equal to the type int
|}]

module M : sig
  type t = int
end = struct
  type t = new int
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = new int
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = new int end
       is not included in
         sig type t = int end
       Type declarations do not match:
         type t = new int
       is not included in
         type t = int
       A private newtype would be revealed.
|}]

module M : sig
  type t = new int
end = struct
  type t = new int
end
[%%expect {|
Line 1:
Error: In module M:
       Modules do not match:
         sig type t = M.t end
       is not included in
         sig type t = new int end
       In module M:
       Type declarations do not match:
         type t = M.t
       is not included in
         type t = new int
       The type M.t is not equal to the type int
|}]

module M : sig
  type t = private int
end = struct
  type t = new int
end
[%%expect {|
module M : sig type t = private int end
|}]

module M : sig
  type t
end = struct
  type t = new int
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = new int
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = new int end
       is not included in
         sig type t end
       Type declarations do not match:
         type t = new int
       is not included in
         type t
       A private newtype would be revealed.
|}]
