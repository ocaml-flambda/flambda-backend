(* type 'a t1 = A of 'a | B of 'a * (int * int) *)

(* let glop () = *)
(*   let [@inline never][@local never] f x y = *)
(*     let chose = *)
(*       if x then *)
(*         A y *)
(*       else *)
(*         let bloup = 12, Sys.opaque_identity 42 in *)
(*         B (y, bloup) *)
(*     in *)
(*     let [@inline never][@local never] fermeture () = *)
(*       match chose with *)
(*       | A v -> v *)
(*       | B (v, _) -> v *)
(*     in *)
(*     let () = () in *)
(*     fermeture *)
(*   in *)
(*   let cond = Sys.opaque_identity true in *)
(*   f cond 12 *)

type 'a t2 =
  | A
  | B of 'a * (int * int)

let glop_int () =
  let[@inline never] [@local never] f x y =
    let chose =
      if x
      then A
      else
        let bloup = 12, Sys.opaque_identity 42 in
        B (y, bloup)
    in
    let[@inline never] [@local never] fermeture () =
      match chose with A -> y | B (v, _) -> v
    in
    let () = () in
    fermeture
  in
  let cond = Sys.opaque_identity true in
  f cond 12
