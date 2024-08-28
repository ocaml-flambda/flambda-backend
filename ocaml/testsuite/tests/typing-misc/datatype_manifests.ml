(* TEST
 expect;
*)

type t = int * int = { foo : string }
[%%expect{|
Line 1, characters 0-37:
1 | type t = int * int = { foo : string }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type
         int * int
|}];;

let f (x : int * int) = (x : t)
[%%expect{|
Line 1, characters 29-30:
1 | let f (x : int * int) = (x : t)
                                 ^
Error: Unbound type constructor t
|}];;
