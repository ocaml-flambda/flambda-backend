(* TEST
 expect;
*)

(* A bug in typecore leading to extra expansion led this to be rejected. *)

type (_, _) refl = Refl : ('a, 'a) refl

[%%expect{|
type (_, _) refl = Refl : ('a, 'a) refl
|}]

let apply (_ : unit -> 'a) : 'a = assert false
let go (type a) (Refl : (unit, a) refl) = apply (fun () : a -> ())

[%%expect{|
val apply : (unit -> 'a) -> 'a = <fun>
val go : (unit, 'a) refl -> 'a = <fun>
|}]

let apply (_ : x:unit -> unit -> 'a) : 'a = assert false
let go (type a) (Refl : (unit, a) refl) = apply (fun ~x:_ () : a -> ())

[%%expect{|
val apply : (x:unit -> unit -> 'a) -> 'a = <fun>
val go : (unit, 'a) refl -> 'a = <fun>
|}]

let apply (_ : ?x:unit -> unit -> 'a) : 'a = assert false
let go (type a) (Refl : (unit, a) refl) = apply (fun ?x:_ () : a -> ())

[%%expect{|
val apply : (?x:unit -> unit -> 'a) -> 'a = <fun>
Line 2, characters 42-71:
2 | let go (type a) (Refl : (unit, a) refl) = apply (fun ?x:_ () : a -> ())
                                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "a" = "unit"
       but an expression was expected of type "'a"
       This instance of "unit" is ambiguous:
       it would escape the scope of its equation
|}]

let apply (_ : unit -> x:unit -> 'a) : 'a = assert false
let go (type a) (Refl : (unit, a) refl) = apply (fun () ~x:_ : a -> ())

[%%expect{|
val apply : (unit -> x:unit -> 'a) -> 'a = <fun>
val go : (unit, 'a) refl -> 'a = <fun>
|}]

let apply (_ : unit -> ?x:unit -> 'a) : 'a = assert false
let go (type a) (Refl : (unit, a) refl) = apply (fun () ?x:_ : a -> ())

[%%expect{|
val apply : (unit -> ?x:unit -> 'a) -> 'a = <fun>
Line 2, characters 59-60:
2 | let go (type a) (Refl : (unit, a) refl) = apply (fun () ?x:_ : a -> ())
                                                               ^
Warning 16 [unerasable-optional-argument]: this optional argument cannot be erased.

Line 2, characters 42-71:
2 | let go (type a) (Refl : (unit, a) refl) = apply (fun () ?x:_ : a -> ())
                                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "a" = "unit"
       but an expression was expected of type "'a"
       This instance of "unit" is ambiguous:
       it would escape the scope of its equation
|}]
