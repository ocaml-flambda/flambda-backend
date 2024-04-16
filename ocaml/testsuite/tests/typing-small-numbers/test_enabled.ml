(* TEST
   flags = "-extension small_numbers"
   * expect
*)

(* Operations are tested in tests/small_numbers/float32.ml *)

type t = float32;;
[%%expect{|
type t = float32
|}];;

let _ : float32 = 1.0s;;
[%%expect{|
- : float32 = <abstr>
|}];;

let _ : float32 = 1.s;;
[%%expect{|
- : float32 = <abstr>
|}];;

let _ : float32 = 1e10s;;
[%%expect{|
- : float32 = <abstr>
|}];;

let _ : float32 = 1e+1s;;
[%%expect{|
- : float32 = <abstr>
|}];;

let _ : float32 = 1e-1s;;
[%%expect{|
- : float32 = <abstr>
|}];;

let _ : float32 = 0x111.000s;;
[%%expect{|
- : float32 = <abstr>
|}];;

let _ : float32 = 0x1.4p+0s;;
[%%expect{|
- : float32 = <abstr>
|}];;

let _ : float32 = 0xf.ffffffffffff8p+1020s;;
[%%expect{|
- : float32 = <abstr>
|}];;

let _ : float32 = 0x8p-972s;;
[%%expect{|
- : float32 = <abstr>
|}];;

let _ : float32 = 0xc.d5e6fp+1_24s;;
[%%expect{|
- : float32 = <abstr>
|}];;

let () =
  match 0.0s with
  | 0.0s -> ()
  | 1.0s
  | 0.5s
  | 1234.1234s
  | 1e10s
  | 1e+1s
  | 1.12345e+12s
  | 0x2_2p+0s
  | 0x2p+0s
  | 0x3p+0s
  | 0x5p+0s
  | 0xcp-4s
  | 0x1p-4s
  | 0xf.ffffffffffff8p+1020s
  | 0x2.fffp+12s
  | 0x1.000002p+0s
  | 0x1.ffffp-24s
  | 0x2._fff006p+12s
  | 0x1.fffp+0s
  | 0xc.d5e6fp+1_24s
  | 0x2.6af378p-128s
  | 0x5p-128s
  | 0x1____p-128s
  | 0x8p-152s
  | 0x8p+124s
  | _ -> assert false
;;
[%%expect{|
Exception: Assert_failure ("", 28, 9).
|}];;

let () =
  match 0.0s with
  | 0.25s -> ()
;;
[%%expect{|
Lines 2-3, characters 2-15:
2 | ..match 0.0s with
3 |   | 0.25s -> ()
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
0.

Exception: Match_failure ("", 2, 2).
|}];;

let () =
  match 0.0s with
  | 0.0s -> ()
  | 1.0s -> ()
  | 2.0s -> ()
;;
[%%expect{|
Lines 2-5, characters 2-14:
2 | ..match 0.0s with
3 |   | 0.0s -> ()
4 |   | 1.0s -> ()
5 |   | 2.0s -> ()
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
3.

Exception: Match_failure ("", 2, 2).
|}];;
