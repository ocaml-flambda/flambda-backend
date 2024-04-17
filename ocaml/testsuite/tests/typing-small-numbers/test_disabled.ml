(* TEST
   * expect
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
Error: Unknown modifier 's' for literal 1.0s
|}];;

let _ = 1.s;;
[%%expect{|
Line 1, characters 8-11:
1 | let _ = 1.s;;
            ^^^
Error: Unknown modifier 's' for literal 1.s
|}];;

let _ = 1e10s;;
[%%expect{|
Line 1, characters 8-13:
1 | let _ = 1e10s;;
            ^^^^^
Error: Unknown modifier 's' for literal 1e10s
|}];;

let _ = 1e+1s;;
[%%expect{|
Line 1, characters 8-13:
1 | let _ = 1e+1s;;
            ^^^^^
Error: Unknown modifier 's' for literal 1e+1s
|}];;

let _ = 1e-1s;;
[%%expect{|
Line 1, characters 8-13:
1 | let _ = 1e-1s;;
            ^^^^^
Error: Unknown modifier 's' for literal 1e-1s
|}];;

let _ = 0x111.000s;;
[%%expect{|
Line 1, characters 8-18:
1 | let _ = 0x111.000s;;
            ^^^^^^^^^^
Error: Unknown modifier 's' for literal 0x111.000s
|}];;

let _ = 0x1.4p+0s;;
[%%expect{|
Line 1, characters 8-17:
1 | let _ = 0x1.4p+0s;;
            ^^^^^^^^^
Error: Unknown modifier 's' for literal 0x1.4p+0s
|}];;

let _ = 0xf.ffffffffffff8p+1020s;;
[%%expect{|
Line 1, characters 8-32:
1 | let _ = 0xf.ffffffffffff8p+1020s;;
            ^^^^^^^^^^^^^^^^^^^^^^^^
Error: Unknown modifier 's' for literal 0xf.ffffffffffff8p+1020s
|}];;

let _ = 0x8p-972s;;
[%%expect{|
Line 1, characters 8-17:
1 | let _ = 0x8p-972s;;
            ^^^^^^^^^
Error: Unknown modifier 's' for literal 0x8p-972s
|}];;

let _ = 0xc.d5e6fp+1_24s;;
[%%expect{|
Line 1, characters 8-24:
1 | let _ = 0xc.d5e6fp+1_24s;;
            ^^^^^^^^^^^^^^^^
Error: Unknown modifier 's' for literal 0xc.d5e6fp+1_24s
|}];;

let () =
  match 0.0s with
  | _ -> ()
;;
[%%expect{|
Line 2, characters 8-12:
2 |   match 0.0s with
            ^^^^
Error: Unknown modifier 's' for literal 0.0s
|}];;
