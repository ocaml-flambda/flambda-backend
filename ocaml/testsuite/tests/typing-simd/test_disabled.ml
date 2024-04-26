(* TEST
 flags = "-no-extension simd";
 expect;
*)

type t = int8x16;;
[%%expect{|
Line 1, characters 9-16:
1 | type t = int8x16;;
             ^^^^^^^
Error: Unbound type constructor int8x16
|}];;

type t = int16x8;;
[%%expect{|
Line 1, characters 9-16:
1 | type t = int16x8;;
             ^^^^^^^
Error: Unbound type constructor int16x8
Hint: Did you mean int64 or int64#?
|}];;

type t = int32x4;;
[%%expect{|
Line 1, characters 9-16:
1 | type t = int32x4;;
             ^^^^^^^
Error: Unbound type constructor int32x4
Hint: Did you mean int32 or int32#?
|}];;

type t = int64x2;;
[%%expect{|
Line 1, characters 9-16:
1 | type t = int64x2;;
             ^^^^^^^
Error: Unbound type constructor int64x2
Hint: Did you mean int64 or int64#?
|}];;

type t = float32x4;;
[%%expect{|
Line 1, characters 9-18:
1 | type t = float32x4;;
             ^^^^^^^^^
Error: Unbound type constructor float32x4
|}];;

type t = float64x2;;
[%%expect{|
Line 1, characters 9-18:
1 | type t = float64x2;;
             ^^^^^^^^^
Error: Unbound type constructor float64x2
|}];;
