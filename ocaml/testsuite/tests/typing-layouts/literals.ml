(* TEST
   flags = "-extension layouts_alpha"
   * expect
*)

let e = #2.718281828459045
[%%expect{|
val e : float# = <abstr>
|}]

let negative_one_half = -#0.5
[%%expect{|
val negative_one_half : float = -0.5
|}]

let positive_one_dot = +#1.
[%%expect{|
val positive_one_dot : float = 1.
|}]

let one_billion = #1e9
[%%expect{|
val one_billion : float# = <abstr>
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
val positive_one : int32 = 1l
|}]

let negative_one = -#1L
[%%expect{|
val negative_one : int64 = -1L
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
val one_twenty_seven_point_two_five_in_floating_hex : float# = <abstr>
|}]

let five_point_three_seven_five_in_floating_hexponent = #0xa.cp-1
[%%expect{|
val five_point_three_seven_five_in_floating_hexponent : float# = <abstr>
|}]

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
Line 4, characters 0-3:
4 | #2L
    ^^^
Error: Unboxed int literals aren't supported yet.
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
