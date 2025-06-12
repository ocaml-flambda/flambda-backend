(* TEST
 flags = "-extension-universe beta";
 modules = "stubs.c";
 ocamlopt_opt_exit_status = "2";
 setup-ocamlopt.opt-build-env;
 ocamlopt.opt;
 check-ocamlopt.opt-output;
*)

(* [@untagged] attributes are disallowed on [maybe_null] arguments. *)

type int_or_null : value_or_null mod external_

external untagged_add
    :  (int_or_null [@untagged])
    -> (int_or_null [@untagged])
    -> (int_or_null [@untagged])
    =  "caml_untagged_add"
