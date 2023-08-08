(* TEST
  * setup-ocamlopt.opt-build-env
  ** ocamlopt.opt
    ocamlopt_opt_exit_status = "2"
  *** check-ocamlopt.opt-output*)

(* Demonstrate that [when e1; e2 match P -> e3] is a parse error.
   One might believe that it parses as [when (e1; e2) match P -> e3], but this
   is the wrong precedence. *)

let seq_bad x ~f ~g ~default =
  match x with
  | Some x when f x; g x match Some y -> y
  | _ -> y
;;

