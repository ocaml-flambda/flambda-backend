(* TEST
 flags = "-extension-universe alpha";
 expect;
*)
type t_non_null_value : non_null_value

[%%expect{|
type t_non_null_value : non_null_value
|}]

(* [non_null_value] can be used in regular functions and modules: *)

let non_null_id (x : t_non_null_value) = x

module type S1 = sig
  val x : t_non_null_value
  val f : t_non_null_value -> t_non_null_value option
end;;

[%%expect{|
val non_null_id : t_non_null_value -> t_non_null_value = <fun>
module type S1 =
  sig
    val x : t_non_null_value
    val f : t_non_null_value -> t_non_null_value option
  end
|}]


(* [non_null_value] is a sublayout of [value]: *)

let id_value : ('a : value). 'a -> 'a = fun x -> x

let id_non_null_value : ('a : non_null_value). 'a -> 'a = fun x -> id_value x

module type S2 = sig
  type t : value
  val f : t -> t
end

module F (X : sig type t : non_null_value val f : t -> t end) : S2 = X;;

[%%expect{|
val id_value : 'a -> 'a = <fun>
val id_non_null_value : ('a : non_null_value). 'a -> 'a = <fun>
module type S2 = sig type t : value val f : t -> t end
module F : functor (X : sig type t : non_null_value val f : t -> t end) -> S2
|}]

(* [value] is not a sublayout of [non_null_value]: *)

let id_value' : ('a : value). 'a -> 'a = fun x -> id_non_null_value x;;

[%%expect{|
Line 1, characters 41-69:
1 | let id_value' : ('a : value). 'a -> 'a = fun x -> id_non_null_value x;;
                                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This definition has type 'b -> 'b which is less general than
         'a. 'a -> 'a
       The layout of 'a is value, because
         of the annotation on the universal variable 'a.
       But the layout of 'a must be a sublayout of non_null_value, because
         of the definition of id_non_null_value at line 3, characters 4-21.
|}]

module type S3 = sig
  type t : non_null_value
  val f : t -> t
end

module F (X : sig type t : value val f : t -> t end) : S3 = X;;

[%%expect{|
module type S3 = sig type t : non_null_value val f : t -> t end
Line 6, characters 60-61:
6 | module F (X : sig type t : value val f : t -> t end) : S3 = X;;
                                                                ^
Error: Signature mismatch:
       Modules do not match:
         sig type t = X.t val f : t -> t end
       is not included in
         S3
       Type declarations do not match:
         type t = X.t
       is not included in
         type t : non_null_value
       The layout of the first is value, because
         of the definition of t at line 6, characters 18-32.
       But the layout of the first must be a sublayout of non_null_value, because
         of the definition of t at line 2, characters 2-25.
|}]

(* Something else like [float64] is also not a sublayout of [non_null_value]: *)

let id_float_64 : ('a : float64). 'a -> 'a = fun x -> id_non_null_value x;;

[%%expect{|
Line 1, characters 72-73:
1 | let id_float_64 : ('a : float64). 'a -> 'a = fun x -> id_non_null_value x;;
                                                                            ^
Error: This expression has type ('a : float64)
       but an expression was expected of type ('b : non_null_value)
       The layout of 'a is non_null_value, because
         of the definition of id_non_null_value at line 3, characters 4-21.
       But the layout of 'a must overlap with float64, because
         of the annotation on the universal variable 'a.
|}]

module type S4 = sig
  type t : non_null_value
  val f : t -> t
end

module F (X : sig type t : float64 val f : t -> t end) : S4 = X;;

[%%expect{|
module type S4 = sig type t : non_null_value val f : t -> t end
Line 6, characters 62-63:
6 | module F (X : sig type t : float64 val f : t -> t end) : S4 = X;;
                                                                  ^
Error: Signature mismatch:
       Modules do not match:
         sig type t = X.t val f : t -> t end
       is not included in
         S4
       Type declarations do not match:
         type t = X.t
       is not included in
         type t : non_null_value
       The layout of the first is float64, because
         of the definition of t at line 6, characters 18-34.
       But the layout of the first must be a sublayout of non_null_value, because
         of the definition of t at line 2, characters 2-25.
|}]
