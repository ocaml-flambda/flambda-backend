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

let[@inline never][@local never] v x = x + 1

let[@poll error] c x =
  let y = Sys.opaque_identity(ref 42) in
    let x2 = v x in
      for c = 0 to x2 do
        ignore(Sys.opaque_identity(42))
      done;
      x2 + !y
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
