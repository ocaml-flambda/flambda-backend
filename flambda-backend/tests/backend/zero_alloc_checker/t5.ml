let[@zero_alloc strict assume][@inline never][@specialise never] test x = [x;x+1]
(* The test below to make sure "noalloc" on external is still handled correctly. *)
external external_test_noalloc : unit -> unit = "test" [@@noalloc]
external external_test : unit -> unit = "test"

let[@zero_alloc assume][@inline never][@specialise never] test4 x =
  if x > (-100)
  then x
  else raise (Failure ("don't be so negative, "^(string_of_int x)))

let[@zero_alloc strict] test5 x =
  external_test_noalloc ()

exception Exn

let[@zero_alloc] test6 x =
  external_test (); raise Exn
