(* TEST
   flags = "-extension comprehensions"
   ocamlc_byte_exit_status = "2"
   * setup-ocamlc.byte-build-env
   ** ocamlc.byte
   *** check-ocamlc.byte-output
*)

[:x for x = 1 to 10:];;
