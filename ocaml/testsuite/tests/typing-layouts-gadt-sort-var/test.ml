(* TEST

   readonly_files = "gadt.ml insert.ml extract.ml"
   flags = "-extension layouts_alpha"
   * setup-ocamlc.byte-build-env
   ** ocamlc.byte
   module = "gadt.ml"
   *** ocamlc.byte
   module = "extract.ml"
   **** ocamlc.byte
   module = "insert.ml"
   ocamlc_byte_exit_status = "2"
   ***** check-ocamlc.byte-output
*)
