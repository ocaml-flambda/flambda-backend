(* TEST
 stack-allocation;
 setup-ocamlopt.opt-build-env;
 ocamlopt_opt_exit_status = "2";
 ocamlopt.opt;
 check-ocamlopt.opt-output;
*)

(* Cannot use TMC on local-returning functions *)
let[@tail_mod_cons] rec copy_list (local_ li) = exclave_
  match li with
  | [] -> []
  | x :: xs -> x :: copy_list xs
