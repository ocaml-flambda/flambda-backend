(* TEST
   flags = "-extension-universe beta";
   setup-ocamlc.byte-build-env;
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

(* This does not parse, but might one day. *)

type t = #()
