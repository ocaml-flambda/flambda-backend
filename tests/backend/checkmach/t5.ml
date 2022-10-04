let[@noalloc assume][@inline never] test x = [x;x+1]
(* The test below to make sure "noalloc" on external is still handled correctly. *)
external external_test : unit -> unit = "test" [@@noalloc]
