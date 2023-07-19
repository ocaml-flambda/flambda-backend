(* TEST
   flags = "-extension simd"
   * expect
*)

type t = vec128;;
[%%expect{|
type t = vec128
|}];;
