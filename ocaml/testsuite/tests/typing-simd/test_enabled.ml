(* TEST
   flags = "-extension simd"
   * expect
*)

type t = int8x16;;
[%%expect{|
type t = int8x16
|}];;

type t = int16x8;;
[%%expect{|
type t = int16x8
|}];;

type t = int32x4;;
[%%expect{|
type t = int32x4
|}];;

type t = int64x2;;
[%%expect{|
type t = int64x2
|}];;

type t = float32x4;;
[%%expect{|
type t = float32x4
|}];;

type t = float64x2;;
[%%expect{|
type t = float64x2
|}];;

