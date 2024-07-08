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

module Foo = struct type u type t = int let x = 1 end;;
module type TFoo = module type of Foo;;

module type TBar = TFoo with type u := float;;

module type Gee =
  sig
    module M : module type of Foo
    include module type of Foo
  end
