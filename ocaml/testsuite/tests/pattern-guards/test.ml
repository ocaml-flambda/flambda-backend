(* TEST
   * expect *)

(* CR-soon rgodse: We expect this output to change soon, but for now it shows that parsing
   works (as the compiler fails at typechecking). *)

let basic_usage ~f ~default x =
  match x with 
    | Some x when f x match Some y -> y
    | _ -> default
[%%expect{|
Uncaught exception: Failure("guard pattern translation unimplemented")

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
Uncaught exception: Failure("guard pattern translation unimplemented")

|}]

let complex_types (x : int list option) : bool =
  let strs_opt = match x with
    | Some [ y ] when y + 1 > 5 -> Some ("foo", "bar")
    | Some ys when List.length ys match 0 -> Some ("baz", "qux")
    | Some _ | None -> None
  in
  match strs_opt with
    | Some strs when strs match ("foo", s2) -> String.equal s2 "bar"
    | Some _ | None -> false
[%%expect {|
Uncaught exception: Failure("guard pattern translation unimplemented")

|}]

let ill_typed_pattern (x : int list option) : bool =
  match x with
    | Some [ y ] when y match None -> true
    | _ -> false
[%%expect{|
Line 3, characters 30-34:
3 |     | Some [ y ] when y match None -> true
                                  ^^^^
Error: This pattern matches values of type 'a option
       but a pattern was expected which matches values of type int
|}]

let ill_typed_pattern_var (x : int list option) : bool =
  match x with
    | Some xs when xs match [ y ] -> String.equal y "foo"
    | _ -> false
[%%expect{|
Line 3, characters 50-51:
3 |     | Some xs when xs match [ y ] -> String.equal y "foo"
                                                      ^
Error: This expression has type int but an expression was expected of type
         String.t = string
|}]

type void = |

let exhaustive_pattern_guards (x : (unit, void option) Either.t) : int =
  match x with
    | Left u when u match () -> 0
    | Right v when v match None -> 1
    | _ -> 2
[%%expect{|
type void = |
Line 77, characters 13-28:
77 |     | Left u when u match () -> 0
                  ^^^^^^^^^^^^^^^
Warning 73 [exhaustive-match]: This pattern guard matches all cases. Consider rewriting the guard as a nested match.
Line 78, characters 14-31:
78 |     | Right v when v match None -> 1
                   ^^^^^^^^^^^^^^^^^
Warning 73 [exhaustive-match]: This pattern guard matches all cases. Consider rewriting the guard as a nested match.
Uncaught exception: Failure("guard pattern translation unimplemented")

|}]
