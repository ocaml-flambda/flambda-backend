(* TEST
   * expect *)

let basic_usage ~f ~default x =
  match x with 
  | Some x when f x match Some y -> y
  | _ -> default
[%%expect{|
Uncaught exception: Failure("Typechecking for pattern guard unimplemented!\nAst:\n match x with | Some x when f x match Some y -> y | _ -> default")

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
Uncaught exception: Failure("Typechecking for pattern guard unimplemented!\nAst:\n match x with | Some x when f x; g x match Some y -> y | _ -> default")

|}]
