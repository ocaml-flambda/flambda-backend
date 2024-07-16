<<<<<<< HEAD
(* TEST_BELOW
(* Blank lines added here to preserve locations. *)








||||||| 121bedcfd2
(* TEST
  * setup-ocamlopt.byte-build-env
  ** ocamlopt.byte
ocamlopt_byte_exit_status = "2"
  *** check-ocamlopt.byte-output
=======
(* TEST_BELOW
(* Blank lines added here to preserve locations. *)







>>>>>>> 5.2.0

*)

let[@poll error] rec c x l =
  match l with
  | [] -> 0
  | _ :: tl -> (c[@tailcall]) (x+1) tl
<<<<<<< HEAD

(* TEST
 poll-insertion;
 {
   setup-ocamlopt.byte-build-env;
   ocamlopt_byte_exit_status = "2";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
 }{
   setup-ocamlopt.opt-build-env;
   ocamlopt_opt_exit_status = "2";
   ocamlopt.opt;
   check-ocamlopt.opt-output;
 }
*)
||||||| 121bedcfd2
=======

(* TEST
 {
   setup-ocamlopt.byte-build-env;
   ocamlopt_byte_exit_status = "2";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
 }{
   setup-ocamlopt.opt-build-env;
   ocamlopt_opt_exit_status = "2";
   ocamlopt.opt;
   check-ocamlopt.opt-output;
 }
*)
>>>>>>> 5.2.0
