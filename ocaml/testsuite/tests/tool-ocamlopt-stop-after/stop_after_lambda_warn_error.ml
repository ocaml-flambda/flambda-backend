(* TEST
 setup-ocamlopt.byte-build-env;
 flags = "-stop-after lambda -warn-error +53";
 ocamlopt_byte_exit_status = "0";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* Bug: This should have exit code 2, because we've set -warn-error for a
   warning that is issued.  But it exits with code 0. *)

let[@inlined] f x = x
