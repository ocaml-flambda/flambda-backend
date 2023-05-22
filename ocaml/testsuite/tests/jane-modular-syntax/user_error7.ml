(* TEST
   * expect
*)

(* If you don't use these as applications, they don't pass through the modular
   extensions machinery and fail with a normal OCaml error *)

let _ = [%jane];;
[%%expect{|
Line 1, characters 10-14:
1 | let _ = [%jane];;
              ^^^^
Error: Uninterpreted extension 'jane'.
|}];;

let _ = [%jane "payload"];;
[%%expect{|
Line 1, characters 10-14:
1 | let _ = [%jane "payload"];;
              ^^^^
Error: Uninterpreted extension 'jane'.
|}];;
