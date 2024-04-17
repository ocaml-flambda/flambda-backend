(* TEST
 {
   exit_status = "2";
   runtime5;
 }{
   runtime5;
 }
*)

open Effect
type _ t += E : unit t
let _ = perform E
