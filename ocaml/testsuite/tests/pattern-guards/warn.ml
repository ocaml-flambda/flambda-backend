(* TEST
   flags = "-extension pattern_guards"
   * expect *)

(* Tests that warnings related to pattern guards are appropriately issued. *)

(* Ensure that warning 8 is issued appropriately in the presence of pattern
   guards. *)

let warn_partial = function
  | [] -> 0
  | xs when List.hd xs match Some y -> y
;;
[%%expect{|
Lines 1-3, characters 19-40:
1 | ...................function
2 |   | [] -> 0
3 |   | xs when List.hd xs match Some y -> y
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
_::_
(However, some guarded clause may match this value.)
val warn_partial : int option list -> int = <fun>
|}];;

warn_partial [];;
[%%expect{|
- : int = 0
|}];;
warn_partial [ Some 1; Some 2; Some 3 ];;
[%%expect{|
- : int = 1
|}];;
warn_partial [ None; Some 1 ];;
[%%expect{|
Exception: Match_failure ("", 1, 19).
|}];;

(* Ensure that cases guarded by total pattern guards count towards exhaustivity
   for warning 8. *)
let rec f = function
  | [] -> []
  | [ x ] when x match (
    | None -> []
    | Some y -> [ y ])
  | x :: xs when x match (
    | Some 0 -> [ 0 ]
    | _ -> f xs
    )
;;
[%%expect{|
Lines 3-5, characters 10-22:
3 | ..........when x match (
4 |     | None -> []
5 |     | Some y -> [ y ])
Warning 73 [total-match-in-pattern-guard]: This pattern guard matches exhaustively. Consider rewriting the guard as a nested match.
Lines 6-9, characters 12-5:
6 | ............when x match (
7 |     | Some 0 -> [ 0 ]
8 |     | _ -> f xs
9 |     )
Warning 73 [total-match-in-pattern-guard]: This pattern guard matches exhaustively. Consider rewriting the guard as a nested match.
val f : int option list -> int list = <fun>
|}];;

(* Ensure that warning 57 is issued when a pattern guard uses an ambiguously
   bound variable. *)

let warn_ambiguous = function
  | ([ x ], _) | (_, [ x ]) when (let one = 1 in Int.abs x + one) match 2 -> 1
  | _ -> 0
;;
[%%expect{|
Line 2, characters 4-27:
2 |   | ([ x ], _) | (_, [ x ]) when (let one = 1 in Int.abs x + one) match 2 -> 1
        ^^^^^^^^^^^^^^^^^^^^^^^
Warning 57 [ambiguous-var-in-pattern-guard]: Ambiguous or-pattern variables under guard;
variable x appears in different places in different or-pattern alternatives.
Only the first match will be used to evaluate the guard expression.
(See manual section 11.5)
val warn_ambiguous : int list * int list -> int = <fun>
|}];;

warn_ambiguous ([ 1 ], [ 0 ]);;
[%%expect{|
- : int = 1
|}];;
warn_ambiguous ([ 0 ], [ 1 ]);;
[%%expect{|
- : int = 0
|}];;
warn_ambiguous ([], [ 1 ]);;
[%%expect{|
- : int = 1
|}];;
warn_ambiguous ([], []);;
[%%expect{|
- : int = 0
|}];;

(* Ensure that warning 57 is not spuriously issued for pattern guards. *)

let dont_warn_ambiguous = function
  | ([ x ], _) | (_, [ x ]) when (let one = 1 in one + one) match 2 -> x
  | _ -> 0
;;
[%%expect{|
val dont_warn_ambiguous : int list * int list -> int = <fun>
|}];;

dont_warn_ambiguous ([ 10 ], [ 20 ]);;
[%%expect{|
- : int = 10
|}];;
dont_warn_ambiguous ([], [ 20 ]);;
[%%expect{|
- : int = 20
|}];;
dont_warn_ambiguous ([], []);;
[%%expect{|
- : int = 0
|}];;

(* Ensure that warning 57 is issued for nested guards. *)

let warn_ambiguous_nested = function
  | ([ x ], _, y) | (_, [ x ], y)
      when y match Some y
      when x + y match 0 ->
      1
  | _ -> 0
;;
[%%expect{|
Line 2, characters 4-33:
2 |   | ([ x ], _, y) | (_, [ x ], y)
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 57 [ambiguous-var-in-pattern-guard]: Ambiguous or-pattern variables under guard;
variable x appears in different places in different or-pattern alternatives.
Only the first match will be used to evaluate the guard expression.
(See manual section 11.5)
val warn_ambiguous_nested : int list * int list * int option -> int = <fun>
|}];;

warn_ambiguous_nested ([ 1 ], [ 2 ], Some ~-1);;
[%%expect{|
- : int = 1
|}];;
warn_ambiguous_nested ([ 1 ], [ 2 ], Some ~-2);;
[%%expect{|
- : int = 0
|}];;
warn_ambiguous_nested ([], [ 2 ], Some ~-2);;
[%%expect{|
- : int = 1
|}];;
warn_ambiguous_nested ([ 1 ], [ 2 ], None);;
[%%expect{|
- : int = 0
|}];;
warn_ambiguous_nested ([], [], Some ~-1);;
[%%expect{|
- : int = 0
|}];;

(* Ensure that warning 73 is issued whenever a pattern guard matches totally. *)

let warn_total_guards (x : (unit, int option) Either.t) : int =
  match x with
    | Left u when u match () -> 0
    | Right v when v match (
        | None -> 1
        | Some n -> ~-n
        )
;;
[%%expect{|
Line 3, characters 13-33:
3 |     | Left u when u match () -> 0
                 ^^^^^^^^^^^^^^^^^^^^
Warning 73 [total-match-in-pattern-guard]: This pattern guard matches exhaustively. Consider rewriting the guard as a nested match.
Lines 4-7, characters 14-9:
4 | ..............when v match (
5 |         | None -> 1
6 |         | Some n -> ~-n
7 |         )
Warning 73 [total-match-in-pattern-guard]: This pattern guard matches exhaustively. Consider rewriting the guard as a nested match.
val warn_total_guards : (unit, int option) Either.t -> int = <fun>
|}];;

warn_total_guards (Left ());;
[%%expect{|
- : int = 0
|}];;
warn_total_guards (Right None);;
[%%expect{|
- : int = 1
|}];;
warn_total_guards (Right (Some 1));;
[%%expect{|
- : int = -1
|}];;
