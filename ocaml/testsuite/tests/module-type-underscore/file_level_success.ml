(* TEST
   setup-ocamlc.byte-build-env;
   ocamlc_byte_exit_status = "0";
   ocamlc.byte;
  *)

  module type S = _

  (* include List

  module type T = _ *)  (* CR selee: For now, we don't support includes *)

  module M = List

  module type U = _

  module F(X:U) = struct
   let foo : X.t -> int List.t = fun x -> x
  end
