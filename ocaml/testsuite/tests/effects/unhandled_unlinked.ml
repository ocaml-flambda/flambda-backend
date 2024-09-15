(* TEST
   runtime5;
   { bytecode; }
   { native; }
*)

open Effect
type _ t += E : unit t
let _ = perform E
