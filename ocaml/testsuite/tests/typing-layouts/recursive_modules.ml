(* TEST
   expect;
*)

(* Regression test *)

module type S_any = sig
  type t : any
end

module type S_value = sig
  type t : value
end

[%%expect{|
module type S_any = sig type t : any end
module type S_value = sig type t end
|}]

module rec M1 : sig
  type t
  include S_any with type t := t
  include S_value with type t := t
end = struct
  type t = int
end

and M2 : sig
  val x : M1.t list
end = struct
  let x = []
end

[%%expect{|
module rec M1 : sig type t end
and M2 : sig val x : M1.t list end
|}]

module rec M1 : sig
  type t
  include S_value with type t := t
  include S_any with type t := t
end = struct
  type t = int
end

and M2 : sig
  val x : M1.t list
end = struct
  let x = []
end

[%%expect{|
Line 10, characters 10-14:
10 |   val x : M1.t list
               ^^^^
Error: This type "M1.t" should be an instance of type "('a : value)"
       The layout of M1.t is any
         because of the definition of t at line 2, characters 2-14.
       But the layout of M1.t must be a sublayout of value
         because the type argument of list has layout value.
|}]
