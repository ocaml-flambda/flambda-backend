(* TEST
   flags = "-extension layouts_alpha"
   * expect
*)

let e = #2.718281828459045
[%%expect{|
val e : float = 2.71828182845904509
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
val one_billion : float = 1000000000.
|}]

let zero = #0n
[%%expect{|
val zero : nativeint = 0n
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
val two_fifty_five_in_hex : nativeint = 255n
|}]

let twenty_five_in_octal = #0o31l
[%%expect{|
val twenty_five_in_octal : int32 = 25l
|}]

let forty_two_in_binary = #0b101010L
[%%expect{|
val forty_two_in_binary : int64 = 42L
|}]

let one_twenty_seven_point_two_five_in_floating_hex = #0x7f.4
[%%expect{|
val one_twenty_seven_point_two_five_in_floating_hex : float = 127.25
|}]

let five_point_three_seven_five_in_floating_hexponent = #0xa.cp-1
[%%expect{|
val five_point_three_seven_five_in_floating_hexponent : float = 5.375
|}]
