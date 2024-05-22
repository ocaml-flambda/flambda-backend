(* TEST
 expect;
*)

let one = Sys.opaque_identity 1;;
let two = Sys.opaque_identity 2;;
let three = Sys.opaque_identity 3;;

let phi = Sys.opaque_identity 1.618;;
let e = Sys.opaque_identity 2.718281828459045;;
let pi = Sys.opaque_identity 3.14159265358;;

let alpha = Sys.opaque_identity "alpha";;
let beta = Sys.opaque_identity "beta";;
let gamma = Sys.opaque_identity "gamma";;

[%%expect{|
val one : int = 1
val two : int = 2
val three : int = 3
val phi : float = 1.618
val e : float = 2.71828182845904509
val pi : float = 3.14159265358
val alpha : string = "alpha"
val beta : string = "beta"
val gamma : string = "gamma"
|}];;

[:one; two; three:];;
[%%expect{|
- : int iarray = [:1; 2; 3:]
|}];;

[:0; one; two; three; 4:];;
[%%expect{|
- : int iarray = [:0; 1; 2; 3; 4:]
|}];;

[:phi; e; pi:];;
[%%expect{|
- : float iarray = [:1.618; 2.71828182845904509; 3.14159265358:]
|}];;

[:1.414; phi; e; pi; 6.28:];;
[%%expect{|
- : float iarray = [:1.414; 1.618; 2.71828182845904509; 3.14159265358; 6.28:]
|}];;

[:alpha; beta; gamma:];;
[%%expect{|
- : string iarray = [:"alpha"; "beta"; "gamma":]
|}];;

[:"Greek:"; alpha; beta; gamma; "delta":];;
[%%expect{|
- : string iarray = [:"Greek:"; "alpha"; "beta"; "gamma"; "delta":]
|}];;
