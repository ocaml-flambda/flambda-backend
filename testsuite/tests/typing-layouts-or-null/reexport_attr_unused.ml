(* TEST
 flags = "-extension-universe alpha";
 ocamlc_byte_exit_status = "0";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

let[@or_null_reexport] x = 5

module A = struct
  [@@@or_null_reexport]
  type t = int
end
