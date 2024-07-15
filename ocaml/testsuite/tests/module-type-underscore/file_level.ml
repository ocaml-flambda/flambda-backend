(* TEST
   setup-ocamlc.byte-build-env;
   compiler_reference = "${test_source_directory}/file_level.reference";
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
   check-ocamlc.byte-output;
  *)

module type S = _

module M1 : S = struct
  type t
  let foo x = x
  let bar x = ignore x
end

module M2 : S = struct
  type t = int
  let foo x = x * x
  let bar x = x + x
end
;;
