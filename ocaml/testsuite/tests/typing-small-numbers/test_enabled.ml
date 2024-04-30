(* TEST
 flags = "-extension small_numbers";
 expect;
*)

(* Operations are tested in tests/small_numbers/float32.ml *)

type t = float32;;
[%%expect{|
type t = float32
|}];;

let _ : float32 = 1.0s;;
[%%expect{|
- : float32 = 1.
|}];;

let _ : float32 = 1.s;;
[%%expect{|
- : float32 = 1.
|}];;

let _ : float32 = 1e10s;;
[%%expect{|
- : float32 = 1e+10
|}];;

let _ : float32 = 1e+1s;;
[%%expect{|
- : float32 = 10.
|}];;

let _ : float32 = 1e-1s;;
[%%expect{|
- : float32 = 0.100000001
|}];;

let _ : float32 = 0x111.000s;;
[%%expect{|
- : float32 = 273.
|}];;

let _ : float32 = 0x1.4p+0s;;
[%%expect{|
- : float32 = 1.25
|}];;

let _ : float32 = 0xf.ffffffffffff8p+1020s;;
[%%expect{|
- : float32 = inf
|}];;

let _ : float32 = 0x8p-972s;;
[%%expect{|
- : float32 = 0.
|}];;

let _ : float32 = 0xc.d5e6fp+1_24s;;
[%%expect{|
- : float32 = 2.72982066e+38
|}];;

(* A (trivial) match with no float32 cases is allowed. *)
let () =
  match 0.0s with
  | _ -> ()
;;
[%%expect{|
|}];;

let () =
  match 0.0s with
  | 0.0s -> ()
  | _ -> ()
;;
[%%expect{|
Line 1:
Error: float32 literal patterns are not supported.
|}];;

let () =
  match 0.0s with
  | 0.0s -> ()
  | 1.0s -> ()
  | _ -> ()
;;
[%%expect{|
Line 1:
Error: float32 literal patterns are not supported.
|}];;

let () =
  match 0.0s with
  | 0.0s -> ()
;;
[%%expect{|
Line 1:
Error: float32 literal patterns are not supported.
|}];;
