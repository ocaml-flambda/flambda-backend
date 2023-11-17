(* TEST
   * skip

     exit_status= "2"
     * skip
     reason = "OCaml 5 only"
*)

open Effect
type _ t += E : unit t
let _ = perform E
