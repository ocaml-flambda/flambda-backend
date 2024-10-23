(* TEST
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* See the comment in user_error3.ml *)
let _misnamed_extension = () [@jane.non_erasable];;
