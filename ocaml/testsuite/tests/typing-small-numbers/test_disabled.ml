(* TEST
 expect;
*)

type t = float32;;
[%%expect{|
Line 1, characters 9-16:
1 | type t = float32;;
             ^^^^^^^
Error: Unbound type constructor float32
Hint: Did you mean float, float# or float32x4?
|}];;

let _ = 1.0s;;
[%%expect{|
Line 1, characters 8-12:
1 | let _ = 1.0s;;
            ^^^^
Error: Found 32-bit float literal 1.0s, but float32 is not enabled. You must enable -extension small_numbers to use this feature.
|}];;

let _ = 1.s;;
[%%expect{|
Line 1, characters 8-11:
1 | let _ = 1.s;;
            ^^^
Error: Found 32-bit float literal 1.s, but float32 is not enabled. You must enable -extension small_numbers to use this feature.
|}];;

let _ = 1e10s;;
[%%expect{|
Line 1, characters 8-13:
1 | let _ = 1e10s;;
            ^^^^^
Error: Found 32-bit float literal 1e10s, but float32 is not enabled. You must enable -extension small_numbers to use this feature.
|}];;

let _ = 1e+1s;;
[%%expect{|
Line 1, characters 8-13:
1 | let _ = 1e+1s;;
            ^^^^^
Error: Found 32-bit float literal 1e+1s, but float32 is not enabled. You must enable -extension small_numbers to use this feature.
|}];;

let _ = 1e-1s;;
[%%expect{|
Line 1, characters 8-13:
1 | let _ = 1e-1s;;
            ^^^^^
Error: Found 32-bit float literal 1e-1s, but float32 is not enabled. You must enable -extension small_numbers to use this feature.
|}];;

let _ = 0x111.000s;;
[%%expect{|
Line 1, characters 8-18:
1 | let _ = 0x111.000s;;
            ^^^^^^^^^^
Error: Found 32-bit float literal 0x111.000s, but float32 is not enabled. You must enable -extension small_numbers to use this feature.
|}];;

let _ = 0x1.4p+0s;;
[%%expect{|
Line 1, characters 8-17:
1 | let _ = 0x1.4p+0s;;
            ^^^^^^^^^
Error: Found 32-bit float literal 0x1.4p+0s, but float32 is not enabled. You must enable -extension small_numbers to use this feature.
|}];;

let _ = 0xf.ffffffffffff8p+1020s;;
[%%expect{|
Line 1, characters 8-32:
1 | let _ = 0xf.ffffffffffff8p+1020s;;
            ^^^^^^^^^^^^^^^^^^^^^^^^
Error: Found 32-bit float literal 0xf.ffffffffffff8p+1020s, but float32 is not enabled. You must enable -extension small_numbers to use this feature.
|}];;

let _ = 0x8p-972s;;
[%%expect{|
Line 1, characters 8-17:
1 | let _ = 0x8p-972s;;
            ^^^^^^^^^
Error: Found 32-bit float literal 0x8p-972s, but float32 is not enabled. You must enable -extension small_numbers to use this feature.
|}];;

let _ = 0xc.d5e6fp+1_24s;;
[%%expect{|
Line 1, characters 8-24:
1 | let _ = 0xc.d5e6fp+1_24s;;
            ^^^^^^^^^^^^^^^^
Error: Found 32-bit float literal 0xc.d5e6fp+1_24s, but float32 is not enabled. You must enable -extension small_numbers to use this feature.
|}];;

let () =
  match 0.0s with
  | _ -> ()
;;
[%%expect{|
Line 2, characters 8-12:
2 |   match 0.0s with
            ^^^^
Error: Found 32-bit float literal 0.0s, but float32 is not enabled. You must enable -extension small_numbers to use this feature.
|}];;

let _ = #1.0s;;
[%%expect{|
Line 1, characters 8-13:
1 | let _ = #1.0s;;
            ^^^^^
Error: Found 32-bit float literal #1.0s, but float32 is not enabled. You must enable -extension small_numbers to use this feature.
|}];;

let _ = -#1.0s;;
[%%expect{|
Line 1, characters 8-14:
1 | let _ = -#1.0s;;
            ^^^^^^
Error: Found 32-bit float literal -#1.0s, but float32 is not enabled. You must enable -extension small_numbers to use this feature.
|}];;
