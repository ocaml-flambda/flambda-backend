(* TEST
   runtime5;
   exit_status = "2";
   { bytecode; }
   { native; }
*)

open Effect
type _ t += E : unit t
let _ = perform E
