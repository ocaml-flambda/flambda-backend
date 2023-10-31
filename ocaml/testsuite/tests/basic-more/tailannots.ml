(* TEST
   * expect *)

let nop () = ()

let good_annot () =
  nop () [@nontail];
  nop () [@tail]
[%%expect{|
val nop : unit -> unit = <fun>
val good_annot : unit -> unit = <fun>
|}]

let bad_annot_1 () =
  nop () [@tail] [@nontail]
[%%expect{|
Line 2, characters 2-8:
2 |   nop () [@tail] [@nontail]
      ^^^^^^
Error: The tail-call annotation on this application is contradictory.
|}]

let bad_annot_2 () =
  nop () [@tail];
  nop ()
[%%expect{|

Line 2, characters 2-8:
2 |   nop () [@tail];
      ^^^^^^
Error: The tail-call annotation on this application is not on a tail call.
|}]

let bad_annot_3 () =
  nop () [@tail hint] [@nontail];
  nop ()
[%%expect{|

Line 2, characters 2-8:
2 |   nop () [@tail hint] [@nontail];
      ^^^^^^
Error: The tail-call annotation on this application is contradictory.
|}]

let bad_annot_4 () =
  nop () [@tail ajsdiof];
  nop ()
[%%expect{|

Line 2, characters 9-24:
2 |   nop () [@tail ajsdiof];
             ^^^^^^^^^^^^^^^
Warning 47 [attribute-payload]: illegal payload for attribute 'tail'.
Only 'hint' is supported

val bad_annot_4 : unit -> unit = <fun>
|}]

let good_annot_2 () =
  nop () [@tail hint];
  nop () [@tail hint]
[%%expect{|

val good_annot_2 : unit -> unit = <fun>
|}]
