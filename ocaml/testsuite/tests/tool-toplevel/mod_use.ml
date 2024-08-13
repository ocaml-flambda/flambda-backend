(* TEST
 readonly_files = "mod.ml";
 expect;
*)

#mod_use "mod.ml"
[%%expect {|
Unknown directive "mod_use".
|}];;
