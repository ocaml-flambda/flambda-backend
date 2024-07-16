(* TEST
<<<<<<< HEAD
 plugins = "odoc_test.ml";
 flags = "-I ${ocamlsrcdir}/ocamldoc -I ${ocamlsrcdir}/typing";
 ocamldoc;
||||||| 121bedcfd2
   plugins="odoc_test.ml"
   * ocamldoc
    flags="-I ${ocamlsrcdir}/ocamldoc"
=======
 plugins = "odoc_test.ml";
 flags = "-I ${ocamlsrcdir}/ocamldoc";
 ocamldoc;
>>>>>>> 5.2.0
*)

module Foo = struct type t = int let x = 1 end;;
module type MT = module type of Foo;;
module Bar = struct type t = int let x = 2 end;;

module type MT2 = sig type t val x : t end;;
module type Gee = MT2 with type t = float ;;
module T = (val
    (if true
     then (module Foo:MT2 with type t = int)
     else (module Bar: MT2 with type t = int))
    : MT2 with type t = int)
;;
