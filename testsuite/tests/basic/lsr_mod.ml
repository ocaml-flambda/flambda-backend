(* TEST *)

let [@inline never] lsr_63 (x : int) =
  x lsr 63

let [@inline never] mod_2 (x : int) =
  x mod 2

let [@inline never] lsr_63_pipe_mod_2 (x : int) =
  (lsr_63 x) |> mod_2

let [@inline never] lsr_63_mod_2 (x : int) =
  (x lsr 63) mod 2

let () =
  assert (Int.equal (lsr_63_pipe_mod_2 0) (lsr_63_mod_2 0))
