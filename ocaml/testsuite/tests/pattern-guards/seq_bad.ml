(* TEST
   flags = "-extension pattern_guards"
  * setup-ocamlopt.opt-build-env
  ** ocamlopt.opt
    ocamlopt_opt_exit_status = "2"
  *** check-ocamlopt.opt-output*)

(* One might innocuously write the below code hoping that it parses as
   [(f x; g x) match ...]. This test demonstrates that this intentionally results in
   a type error. *)

let seq_bad x ~f ~g ~default =
  match x with
  | Some x when f x; g x match Some y -> y 
  | _ -> y

