(* TEST
   * expect
*)

(* If you don't use these as applications, they don't pass through the modular
   extensions machinery and fail with a normal OCaml error *)

let _ = [%extension];;
[%%expect{|
Line 1, characters 10-19:
1 | let _ = [%extension];;
              ^^^^^^^^^
Error: Uninterpreted extension 'extension'.
|}];;

let _ = [%extension "payload"];;
[%%expect{|
Line 1, characters 10-19:
1 | let _ = [%extension "payload"];;
              ^^^^^^^^^
Error: Uninterpreted extension 'extension'.
|}];;




