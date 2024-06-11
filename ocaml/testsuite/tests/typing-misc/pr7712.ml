(* TEST
 expect;
*)

type 'a or_error = string

type ('a, 'b) t_ =
  | Bar : ('a, 'a or_error) t_

type 'a t = ('a, 'a) t_

let f : type a. a t -> a t = function
  | Bar -> Bar
;;
[%%expect{|
type ('a : any) or_error = string
type ('a : any, 'b : any) t_ = Bar : ('a, 'a or_error) t_
type ('a : any) t = ('a, 'a) t_
val f : 'a t -> 'a t = <fun>
|}];;
