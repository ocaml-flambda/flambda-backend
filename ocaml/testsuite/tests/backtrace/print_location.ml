(* TEST
 runtime5;
 flags = "-g";
 ocamlrunparam += ",b=1";
 ocamlopt_flags = "-inline 0";
 exit_status = "2";
 {
   bytecode;
 }{
   native;
 }
*)

let f () = raise
  Exit [@@inline never]

let () =
  f ()
