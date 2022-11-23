(* Paired with code_id_join_b.ml.

   This creates a join between three children of the same code ID, and a bug in
   the code age relation join made it return the parent of their parent instead
   of their common parent as expected. *)

let[@inline always] twice f =
  let () = () in
  let[@inline never] f' x = f (f x) in
  f'

let f0 x = x + 1

let f1 x = x * 2

let f2 x = x * x

let f = function 0 -> twice f0 | 1 -> twice f1 | _ -> twice f2
