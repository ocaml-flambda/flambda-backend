(* TEST

   expect;
*)

(* This test just shows you need layouts beta to use the various unboxed product
   syntactic form. It can be removed when we move products out of beta. *)

type t : value & value
[%%expect{|
Line 1, characters 9-22:
1 | type t : value & value
             ^^^^^^^^^^^^^
Error: Layout value & value is more experimental than allowed by the enabled layouts extension.
       You must enable -extension layouts_beta to use this feature.
|}]

type t = #(int * bool)
[%%expect{|
Line 1, characters 9-22:
1 | type t = #(int * bool)
             ^^^^^^^^^^^^^
Error: This construct requires the beta version of the extension "layouts", which is disabled and cannot be used
|}]

let _ =
  let _ = #(1,2) in
  ()
[%%expect{|
Line 2, characters 10-16:
2 |   let _ = #(1,2) in
              ^^^^^^
Error: This construct requires the beta version of the extension "layouts", which is disabled and cannot be used
|}]

let f x =
  let #(_,_) = x in
  ()
[%%expect{|
Line 2, characters 6-12:
2 |   let #(_,_) = x in
          ^^^^^^
Error: This construct requires the beta version of the extension "layouts", which is disabled and cannot be used
|}]

