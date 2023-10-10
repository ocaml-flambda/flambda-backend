(* TEST
   * bytecode
   * native
*)

(* This was producing an error in ocamlopt because the call to [failwith] had
   the close-at-apply flag on, which wasn't cleared when tmc rewrote the call to
   one not in tail position. *)

let[@tail_mod_cons] rec map2_exn l0 l1 ~f =
  match l0, l1 with
  | [], [] -> []
  | h0 :: t0, h1 :: t1 -> f h0 h1 :: map2_exn t0 t1 ~f
  | _ -> failwith (let message () = "urk" in message ())
;;
