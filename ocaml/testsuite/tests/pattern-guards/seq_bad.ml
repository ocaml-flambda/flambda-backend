(* TEST
  * setup-ocamlopt.opt-build-env
  ** ocamlopt.opt
    ocamlopt_opt_exit_status = "2"
  *** check-ocamlopt.opt-output*)

let seq_bad x ~f ~g ~default =
  match x with
  | Some x when f x; g x match Some y -> y 
  | _ -> y