(* TEST
   * expect *)

(* CR-soon rgodse: We expect this output to change soon, but for now it shows
   that parsing and typechecking work (as the compiler fails at translation). *)

let basic_usage ~f ~default x =
  match x with 
    | Some x when f x match Some y -> y
    | _ -> default
;;

let f x = if x > 0 then Some (x - 1) else None;;

basic_usage ~f ~default:~-1 (Some 5);;
basic_usage ~f ~default:~-1 (Some 0);;
basic_usage ~f ~default:~-1 (Some ~-5);;
basic_usage ~f ~default:~-1 None;;
[%%expect{|
val basic_usage : f:('a -> 'b option) -> default:'b -> 'a option -> 'b =
  <fun>
val f : int -> int option = <fun>
- : int = 4
- : int = -1
- : int = -1
- : int = -1
|}]
;;

let seq_predicate x ~f ~g ~default =
  match x with
    | Some x when f x; g x -> x
    | _ -> default
;;
[%%expect{|
val seq_predicate :
  'a option -> f:('a -> 'b) -> g:('a -> bool) -> default:'a -> 'a = <fun>
|}]

let seq_pattern x ~f ~g ~default =
  match x with
    | Some x when (f x; g x) match Some y -> y
    | _ -> default

let counter = ref 0;;
let f () = incr counter;;
let g () = if !counter > 1 then Some (!counter - 1) else None;;

seq_pattern (Some ()) ~f ~g ~default:0;;
seq_pattern (Some ()) ~f ~g ~default:0;;
seq_pattern None ~f ~g ~default:0;;
[%%expect{|
val seq_pattern :
  'a option -> f:('a -> 'b) -> g:('a -> 'c option) -> default:'c -> 'c =
  <fun>
val counter : int ref = {contents = 0}
val f : unit -> unit = <fun>
val g : unit -> int option = <fun>
- : int = 0
- : int = 1
- : int = 0
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
val complex_types : int list option -> bool = <fun>
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

let shadow_outer_variables (n : int option) (strs : string list) =
  match n with
  | Some n when List.nth_opt strs n match Some s -> s
  | _ -> "Not found"
;;

shadow_outer_variables (Some 0) [ "foo"; "bar" ];;
shadow_outer_variables (Some 1) [ "foo"; "bar" ];;
shadow_outer_variables (Some 2) [ "foo"; "bar" ];;
[%%expect{|
val shadow_outer_variables : int option -> string list -> string = <fun>
- : string = "foo"
- : string = "bar"
- : string = "Not found"
|}]

type ('a, 'b) eq = Eq : ('a, 'a) eq

let in_pattern_guard (type a b) (eq : (a, b) eq) (compare : a -> a -> int)
                     (x : a) (y : b) =
  match eq with
    | Eq when compare x y match 0 -> true
    | _ -> false
[%%expect{|
type ('a, 'b) eq = Eq : ('a, 'a) eq
val in_pattern_guard : ('a, 'b) eq -> ('a -> 'a -> int) -> 'a -> 'b -> bool =
  <fun>
|}]

let from_pattern_guard (type a b) (eqs : (a, b) eq option list)
                       (compare : a -> a -> int) (x : a) (y : b) =
  match eqs with
    | eq_opt :: _ when eq_opt match Some Eq -> compare x y
    | _ -> 0
[%%expect{|
val from_pattern_guard :
  ('a, 'b) eq option list -> ('a -> 'a -> int) -> 'a -> 'b -> int = <fun>
|}]

type void = |

let exhaustive_pattern_guards (x : (unit, void option) Either.t) : int =
  match x with
    | Left u when u match () -> 0
    | Right v when v match None -> 1
    | _ -> 2
[%%expect{|
type void = |
Line 144, characters 13-28:
144 |     | Left u when u match () -> 0
                   ^^^^^^^^^^^^^^^
Warning 73 [total-match-in-pattern-guard]: This pattern guard matches exhaustively. Consider rewriting the guard as a nested match.
Line 145, characters 14-31:
145 |     | Right v when v match None -> 1
                    ^^^^^^^^^^^^^^^^^
Warning 73 [total-match-in-pattern-guard]: This pattern guard matches exhaustively. Consider rewriting the guard as a nested match.
val exhaustive_pattern_guards : (unit, void option) Either.t -> int = <fun>
|}]
