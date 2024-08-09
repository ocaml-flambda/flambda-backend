(* TEST
 {
   exit_status = "2";
   skip;
 }{
   runtime5;
   reason = "CR ocaml 5 effects: re-enable this test";
   skip;
 }
*)

open Effect
type _ t += E : unit t
let _ = perform E
