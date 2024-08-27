(* TEST
<<<<<<< HEAD
 flags = "-g";
 ocamlrunparam += ",b=1";
 ocamlopt_flags = "-inline 0";
 exit_status = "2";
||||||| a198127529
=======
 runtime5;
 flags = "-g";
 ocamlrunparam += ",b=1";
 ocamlopt_flags = "-inline 0";
 exit_status = "2";
 native;
>>>>>>> 57461473bf
*)

let f () = raise
  Exit [@@inline never]

let () =
  f ()
