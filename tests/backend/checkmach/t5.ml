let[@noalloc_strict assume][@inline never] test x = [x;x+1]
(* The test below to make sure "noalloc" on external is still handled correctly. *)
external external_test : unit -> unit = "test" [@@noalloc]

let[@noalloc assume][@inline never] test4 x =
  if x > (-100)
  then x
  else raise (Failure ("don't be so negative, "^(string_of_int x)))
