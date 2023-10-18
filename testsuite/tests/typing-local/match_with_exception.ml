(* TEST
   * native *)

let[@inline never] f x =
  local_ (x, (0, 0))

let[@inline never] g x =
  local_ (x, 0)

let[@inline never] h x =
  match f x with
  | exception Not_found -> 0
  | p ->
    (* The try-region must not have been closed, otherwise [p2] will
       clobber [p] *)
    let p2 = g x in
    (fst (snd p)) + fst p2

let () =
  Printf.printf "%d\n" (h 0)
