(* TEST
   flags = "-extension layouts_alpha"
   * expect
*)

(*****************************************)
(* Expressions *)

let e = #2.718281828459045
[%%expect{|
Line 1, characters 8-26:
1 | let e = #2.718281828459045
            ^^^^^^^^^^^^^^^^^^
Error: Unboxed float literals aren't supported yet.
|}]

let negative_one_half = -#0.5
[%%expect{|
Line 1, characters 24-29:
1 | let negative_one_half = -#0.5
                            ^^^^^
Error: Unboxed float literals aren't supported yet.
|}]

let negative_one_half = - #0.5
[%%expect{|
Line 1, characters 24-30:
1 | let negative_one_half = - #0.5
                            ^^^^^^
Error: Unboxed float literals aren't supported yet.
|}]

let negative_one_half = -.#0.5
[%%expect{|
Line 1, characters 24-30:
1 | let negative_one_half = -.#0.5
                            ^^^^^^
Error: Unboxed float literals aren't supported yet.
|}]

let negative_one_half = -. #0.5
[%%expect{|
Line 1, characters 24-31:
1 | let negative_one_half = -. #0.5
                            ^^^^^^^
Error: Unboxed float literals aren't supported yet.
|}]

let positive_one_dot = +#1.
[%%expect{|
Line 1, characters 23-27:
1 | let positive_one_dot = +#1.
                           ^^^^
Error: Unboxed float literals aren't supported yet.
|}]

let positive_one_dot = + #1.
[%%expect{|
Line 1, characters 23-28:
1 | let positive_one_dot = + #1.
                           ^^^^^
Error: Unboxed float literals aren't supported yet.
|}]

let positive_one_dot = +.#1.
[%%expect{|
Line 1, characters 23-28:
1 | let positive_one_dot = +.#1.
                           ^^^^^
Error: Unboxed float literals aren't supported yet.
|}]

let positive_one_dot = +. #1.
[%%expect{|
Line 1, characters 23-29:
1 | let positive_one_dot = +. #1.
                           ^^^^^^
Error: Unboxed float literals aren't supported yet.
|}]

let one_billion = #1e9
[%%expect{|
Line 1, characters 18-22:
1 | let one_billion = #1e9
                      ^^^^
Error: Unboxed float literals aren't supported yet.
|}]

let zero = #0n
[%%expect{|
Line 1, characters 11-14:
1 | let zero = #0n
               ^^^
Error: Unboxed int literals aren't supported yet.
|}]

let positive_one = +#1l
[%%expect{|
Line 1, characters 19-23:
1 | let positive_one = +#1l
                       ^^^^
Error: Unboxed int literals aren't supported yet.
|}]

let positive_one = + #1l
[%%expect{|
Line 1, characters 19-24:
1 | let positive_one = + #1l
                       ^^^^^
Error: Unboxed int literals aren't supported yet.
|}]

let negative_one = -#1L
[%%expect{|
Line 1, characters 19-23:
1 | let negative_one = -#1L
                       ^^^^
Error: Unboxed int literals aren't supported yet.
|}]

let negative_one = - #1L
[%%expect{|
Line 1, characters 19-24:
1 | let negative_one = - #1L
                       ^^^^^
Error: Unboxed int literals aren't supported yet.
|}]

let two_fifty_five_in_hex = #0xFFn
[%%expect{|
Line 1, characters 28-34:
1 | let two_fifty_five_in_hex = #0xFFn
                                ^^^^^^
Error: Unboxed int literals aren't supported yet.
|}]

let twenty_five_in_octal = #0o31l
[%%expect{|
Line 1, characters 27-33:
1 | let twenty_five_in_octal = #0o31l
                               ^^^^^^
Error: Unboxed int literals aren't supported yet.
|}]

let forty_two_in_binary = #0b101010L
[%%expect{|
Line 1, characters 26-36:
1 | let forty_two_in_binary = #0b101010L
                              ^^^^^^^^^^
Error: Unboxed int literals aren't supported yet.
|}]

let one_twenty_seven_point_two_five_in_floating_hex = #0x7f.4
[%%expect{|
Line 1, characters 54-61:
1 | let one_twenty_seven_point_two_five_in_floating_hex = #0x7f.4
                                                          ^^^^^^^
Error: Unboxed float literals aren't supported yet.
|}]

let five_point_three_seven_five_in_floating_hexponent = #0xa.cp-1
[%%expect{|
Line 1, characters 56-65:
1 | let five_point_three_seven_five_in_floating_hexponent = #0xa.cp-1
                                                            ^^^^^^^^^
Error: Unboxed float literals aren't supported yet.
|}]

let unknown_floating_point_suffix = #0.P
[%%expect{|
Line 1, characters 36-40:
1 | let unknown_floating_point_suffix = #0.P
                                        ^^^^
Error: Unboxed float literals aren't supported yet.
|}]

(*****************************************)
(* Patterns *)

let f x =
  match x with
  | #4L -> `Four
  | #5L -> `Five
  | _ -> `Other
;;

f #4L;;
[%%expect {|
Line 3, characters 4-7:
3 |   | #4L -> `Four
        ^^^
Error: Unboxed int literals aren't supported yet.
|}];;

let f x =
  match x with
  | #4. -> `Four
  | #5. -> `Five
  | _ -> `Other
;;

f #5.;;
[%%expect {|
Line 3, characters 4-7:
3 |   | #4. -> `Four
        ^^^
Error: Unboxed float literals aren't supported yet.
|}];;

(*****************************************)
(* Lexing edge cases *)

(* Unboxed literals at the beginning of the line aren't directives. *)
let f _ _ = ();;
let () = f
#2.
#2L
;;

let () = f
#2. #2.
;;

[%%expect{|
val f : 'a -> 'b -> unit = <fun>
Line 3, characters 0-3:
3 | #2.
    ^^^
Error: Unboxed float literals aren't supported yet.
|}];;

let () = f
(* This lexes as a directive. #2 is not a valid unboxed int literal
   anyway, as it lacks a suffix.
*)
#2 "literals.ml"
()
()
;;

[%%expect{|
|}];;
