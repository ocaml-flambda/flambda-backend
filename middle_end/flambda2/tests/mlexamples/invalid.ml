(* This example showed an isntance where [Simplify] produced an invalid, which
   when translated by `to_cmm`, caused delayed let bindings to be lost because
   there was a missing flush, see PR#1126 *)

let[@inline never] check t1 t2 =
  if Array.length t1 <> Array.length t2 then failwith "lengths"

let[@inline] bar t1 t2 =
  check t1 t2;
  Array.unsafe_get t2 0 = 0.

let foo () = bar [| 1. |] [||]
