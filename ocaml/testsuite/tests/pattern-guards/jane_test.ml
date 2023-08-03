(* TEST
   * expect *)

(* Tests interaction of pattern guards with Jane Street internal features.
   Remove when upstreaming. *)

let pattern_guard_returns_local f x =
  let local_ one = 1 in
  match x with
  | Some x when f x match (
    | [] -> 0
    | [ _ ] -> one
    | [ _; _ ] -> 2
  )
  | _ -> 3
;;
[%%expect{|
Line 6, characters 15-18:
6 |     | [ _ ] -> one
                   ^^^
Error: This local value escapes its region
  Hint: Cannot return local value without an explicit "local_" annotation
|}]

let pattern_guard_doesnt_return_local f x =
  let local_ one = 1 in
  match x with
  | Some x when one match (
    | 0 -> 0
    | 1 -> 1
    | 2 -> 2
  )
  | _ -> 3
;;
[%%expect{|
val pattern_guard_doesnt_return_local : 'a -> 'b option -> int = <fun>
|}]
