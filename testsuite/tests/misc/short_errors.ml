(* TEST
   flags = "-error-style short";
   expect;
*)

let _ = 1 + true

[%%expect{|
Line 1, characters 12-16:
Error: This expression has type bool but an expression was expected of type
         int
|}]
