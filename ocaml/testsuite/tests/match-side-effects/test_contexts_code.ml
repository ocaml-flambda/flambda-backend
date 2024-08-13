(* TEST
 readonly_files = "contexts_1.ml contexts_2.ml contexts_3.ml";
 flags = "-dsource -dlambda";
 expect;
*)

#use "contexts_1.ml";;
(* Notice that (field_mut 1 input) occurs twice, it
   is evaluated once in the 'false' branch and once in the 'true'
   branch. The compiler assumes that its static knowledge about the
   first read (it cannot be a [Right] as we already matched against it
   and failed) also applies to the second read, which is unsound.
*)
[%%expect {|

#use  "contexts_1.ml";;
Unknown directive "use".
|}]

#use "contexts_2.ml";;
[%%expect {|

#use  "contexts_2.ml";;
Unknown directive "use".
|}]

#use "contexts_3.ml";;
[%%expect {|

#use  "contexts_3.ml";;
Unknown directive "use".
|}]
