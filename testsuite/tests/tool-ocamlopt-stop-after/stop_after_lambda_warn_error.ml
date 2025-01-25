(* TEST
 setup-ocamlopt.byte-build-env;
 flags = "-stop-after lambda -warn-error +53";
 ocamlopt_byte_exit_status = "2";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* This should have exit code 2, because we've set -warn-error for a warning
   that is issued. *)

let[@inlined] f x = x
