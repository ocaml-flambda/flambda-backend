(* TEST
   setup-ocamlc.byte-build-env;
   ocamlc_byte_exit_status = "0";
   ocamlc.byte;
   check-ocamlc.byte-output;
  *)

  module type S = _

  include List

  module type T = _

  module M = List

  module type U = _
