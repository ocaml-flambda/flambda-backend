(* TEST
   flags = "-extension layouts_alpha"
   * toplevel
*)

type t_void : void;;
(assert false : t_void);;
(* CR layouts v2.9: This error message is bad, but it should be gone when merged back into
   main. Remember to remove the unused reason constructor at that point. *)
