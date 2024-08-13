(* TEST
 expect;
*)

(* Test a success case *)
#use_output {|echo let x = 42|}
[%%expect {|
Unknown directive "use_output".
|}];;

(* When the command fails *)
#use_output {|false|}
[%%expect {|
Unknown directive "use_output".
|}];;

(* When the code is invalid *)
#use_output {|echo 1 :: x|}
[%%expect {|
Unknown directive "use_output".
|}];;
