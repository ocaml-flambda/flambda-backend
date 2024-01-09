(* TEST
   * runtime5

     exit_status= "2"
     * runtime5
     reason = "CR ocaml 5 effects: re-enable this test"
*)

open Effect
type _ t += E : unit t
let _ = perform E
