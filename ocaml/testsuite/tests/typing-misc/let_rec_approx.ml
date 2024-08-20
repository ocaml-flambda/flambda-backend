(* TEST
 expect;
*)

module M = struct type t = A | B end
let rec f () = g A
and g (x : M.t) = f ()
[%%expect{|
module M : sig type t = A | B end
Line 2, characters 17-18:
2 | let rec f () = g A
                     ^
Error: Unbound constructor A
|}]

let rec f () = g 42
and g (x : string) = f ()
[%%expect{|
Line 2, characters 6-18:
2 | and g (x : string) = f ()
          ^^^^^^^^^^^^
Error: This pattern matches values of type string
       but a pattern was expected which matches values of type int
|}]

let rec opt_error ?(opt : string) () = f ?opt ()
[%%expect{|
Line 1, characters 20-32:
1 | let rec opt_error ?(opt : string) () = f ?opt ()
                        ^^^^^^^^^^^^
Error: This pattern matches values of type string
       but a pattern was expected which matches values of type 'a option
|}]

let rec opt_ok_f () = opt_ok_g ~foo:A ~bar:A ()
and opt_ok_g ?(foo : M.t option) ?(bar : M.t = M.A) () = opt_ok_f ()
[%%expect{|
Line 1, characters 36-37:
1 | let rec opt_ok_f () = opt_ok_g ~foo:A ~bar:A ()
                                        ^
Error: Unbound constructor A
|}]
