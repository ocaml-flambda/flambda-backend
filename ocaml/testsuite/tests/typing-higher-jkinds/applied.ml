(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

type t : value => value

[%%expect {|
type t : value => value
|}]


type p = int t

[%%expect {|
type p = int t
|}]


module M : sig
  type p = int t
end = struct
  type p = int t
end

[%%expect {|
module M : sig type p = int t end
|}]


module M : sig
  type 'a p = 'a t
end = struct
  type 'a p = 'a t
end

[%%expect {|
module M : sig type 'a p = 'a t end
|}]


module M : sig
  type p
end = struct
  type p = int t
end

[%%expect {|
module M : sig type p end
|}]


type p = t t

[%%expect {|
Line 1, characters 9-10:
1 | type p = t t
             ^
Error: This type t should be an instance of type ('a : value)
       The kind of t is ((value) => value)
         because of the definition of t at line 1, characters 0-23.
       But the kind of t must be a subkind of value
         because of the definition of t at line 1, characters 0-23.
|}]


type r : (value => value) => value

[%%expect {|
type r : (value => value) => value
|}]


type ('a : value => value) r

[%%expect {|
type ('a : value => value) r
|}]


module type M = sig
  val g : ('a : value => value). 'a r -> 'a r
end

[%%expect{|
module type M = sig val g : ('a : value => value). 'a r -> 'a r end
|}]


module type M = sig
  type r : (value => value) => value
  type s : value => value
  val g : s r -> s r
end

[%%expect{|
module type M =
  sig
    type r : (value => value) => value
    type s : value => value
    val g : s r -> s r
  end
|}]


module type M = sig
  type r : (value => value) => value
  type s : value => value
  val g : int s r -> int s r
end

[%%expect{|
Line 4, characters 10-15:
4 |   val g : int s r -> int s r
              ^^^^^
Error: This type int s should be an instance of type ('a : value => value)
       The kind of int s is value
         because of the definition of s at line 3, characters 2-25.
       But the kind of int s must be a subkind of ((value) => value)
         because of the definition of r at line 2, characters 2-36.
|}]

type t : value => value
let f (x : 'a t) = x
[%%expect {|
type t : value => value
val f : 'a t -> 'a t = <fun>
|}]

(* Not a datatype *)
type 'a t
let f (x : 'a t) = x
[%%expect {|
type 'a t
val f : 'a t -> 'a t = <fun>
|}]

type l : value => value = list
let x : int l = [0]
[%%expect {|
type l = list
val x : int l = <abstr>
|}]

type l : value => value = list
let x : int l = [0]
[%%expect {|
type l = list
val x : int l = <abstr>
|}]

type ('a : value => value) t = int 'a

module M : sig
  type a = int list
end = struct
  type a = list t
end

[%%expect{|
type ('a : value => value) t = int 'a
module M : sig type a = int list end
|}]

type ('a : value => value) t = int 'a

module M : sig
  type a = int list
end = struct
  type a = list t
end

[%%expect{|
type ('a : value => value) t = int 'a
module M : sig type a = int list end
|}]

type ('a : value => value) t = int 'a

module M : sig
  type a = int list
end = struct
  type a = list t
end

[%%expect{|
type ('a : value => value) t = int 'a
module M : sig type a = int list end
|}]

type 'a t = 'a * 'a
let id : 'a ('b : value => value). 'a 'b -> 'a 'b = fun x -> x
[%%expect{|
type 'a t = 'a * 'a
val id : ('b : value => value) 'a. 'a 'b -> 'a 'b = <fun>
|}]

let y = id ((0, 1) : int t)
[%%expect{|
Line 1, characters 11-27:
1 | let y = id ((0, 1) : int t)
               ^^^^^^^^^^^^^^^^
Error: This expression has type int t = int * int
       but an expression was expected of type 'a 'b
|}]

let y = id (0, 1)
[%%expect{|
Line 1, characters 11-17:
1 | let y = id (0, 1)
               ^^^^^^
Error: This expression has type 'a * 'b
       but an expression was expected of type 'c 'd
|}]

module M : sig
  val id : 'a ('b : value => value). 'a 'b -> 'a 'b
end = struct
  let id x = x
end
[%%expect{|
module M : sig val id : ('b : value => value) 'a. 'a 'b -> 'a 'b end
|}]

module T : sig
  type 'a t
  val return : 'a -> 'a t
end = struct
  type 'a t = 'a * 'a
  let return x = (x, x)
end
[%%expect{|
module T : sig type 'a t val return : 'a -> 'a t end
|}]

let y = M.id (T.return 0)
[%%expect{|
Line 1, characters 13-25:
1 | let y = M.id (T.return 0)
                 ^^^^^^^^^^^^
Error: This expression has type int T.t
       but an expression was expected of type 'a 'b
|}]
