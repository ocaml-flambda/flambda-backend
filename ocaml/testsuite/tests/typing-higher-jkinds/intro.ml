(* TEST
 flags = "-g -extension layouts_alpha";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)


module M : sig
  type a : (value, value) => value
end = struct
  type a : (value, value) => value
end
