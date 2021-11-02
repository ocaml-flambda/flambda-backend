(* Paired with code_id_join_a.ml. Optimisation level -O2 was needed to trigger
   the original bug.

   This creates a join between three children of the same code ID, and a bug in
   the code age relation join made it return the parent of their parent instead
   of their common parent as expected. *)

let () = [0; 1; 2] |> List.iter (fun n -> ignore (Code_id_join_a.f n 42 : int))
