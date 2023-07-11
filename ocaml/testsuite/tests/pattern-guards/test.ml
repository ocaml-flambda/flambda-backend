(* TEST
   * expect *)

(* CR-soon rgodse: We expect this output to change soon, but for now it shows that parsing
   works (as the compiler fails at typechecking). *)

let basic_usage ~f ~default x =
  match x with 
  | Some x when f x match Some y -> y
  | _ -> default
[%%expect{|
Uncaught exception: Failure("pattern guard typechecking unimplemented")

|}]
;;

let seq_predicate x ~f ~g ~default =
  match x with
  | Some x when f x; g x -> x
  | _ -> default
[%%expect{|
val seq_predicate :
  'a option -> f:('a -> 'b) -> g:('a -> bool) -> default:'a -> 'a = <fun>
|}]

let seq_pattern x ~f ~g ~default =
  match x with
  | Some x when (f x; g x) match Some y -> y
  | _ -> default
[%%expect{|
Uncaught exception: Failure("pattern guard typechecking unimplemented")

|}]
