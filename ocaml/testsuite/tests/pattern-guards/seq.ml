(* TEST
   flags = "-extension pattern_guards"
   * expect *)

(* Demonstrate interaction of sequences [(e1; e2; ..; en)] with pattern
   guards. *)

(* Demonstrate parsing of sequences in boolean guards. *)

let seq_boolean x ~f ~g ~default =
  match x with
    | Some x when f x; g x -> x
    | _ -> default
;;
[%%expect{|
val seq_boolean :
  'a option -> f:('a -> 'b) -> g:('a -> bool) -> default:'a -> 'a = <fun>
|}];;

(* Demonstrate semantics of sequences in pattern guard scrutinees. *)

let seq_pattern x ~f ~g ~default =
  match x with
    | Some x when (f x; g x) match Some y -> y
    | _ -> default
;;
[%%expect{|
val seq_pattern :
  'a option -> f:('a -> 'b) -> g:('a -> 'c option) -> default:'c -> 'c =
  <fun>
|}];;

let counter = ref 0;;
[%%expect{|
val counter : int ref = {contents = 0}
|}];;
let f () = incr counter;;
[%%expect{|
val f : unit -> unit = <fun>
|}];;
let g () = if !counter > 1 then Some (!counter - 1) else None;;
[%%expect{|
val g : unit -> int option = <fun>
|}];;

seq_pattern (Some ()) ~f ~g ~default:0;;
[%%expect{|
- : int = 0
|}];;
seq_pattern (Some ()) ~f ~g ~default:0;;
[%%expect{|
- : int = 1
|}];;
seq_pattern None ~f ~g ~default:0;;
[%%expect{|
- : int = 0
|}];;
