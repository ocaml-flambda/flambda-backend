(* TEST
 readonly_files = "contexts_1.ml contexts_2.ml contexts_3.ml";
 expect;
*)

#use "contexts_1.ml";;
[%%expect {|
Unknown directive "use".
|}]

let _ = example_1 ();;
(* <unknown constructor> means that we got an 'unsound boolean',
   which is neither 'true' nor 'false'. There was a bug here! *)
[%%expect {|
Line 1, characters 8-17:
1 | let _ = example_1 ();;
            ^^^^^^^^^
Error: Unbound value "example_1"
|}]

#use "contexts_2.ml";;
[%%expect {|
Unknown directive "use".
|}];;

let _ = example_2 ();;
(* Also a bug! *)
[%%expect {|
Line 1, characters 8-17:
1 | let _ = example_2 ();;
            ^^^^^^^^^
Error: Unbound value "example_2"
|}]

#use "contexts_3.ml";;
[%%expect {|
Unknown directive "use".
|}];;

let _ = example_3 ();;
(* This one works correctly. *)
[%%expect {|
Line 1, characters 8-17:
1 | let _ = example_3 ();;
            ^^^^^^^^^
Error: Unbound value "example_3"
|}]
