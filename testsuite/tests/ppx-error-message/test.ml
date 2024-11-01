(* TEST
 readonly_files = "ppx_error_message.ml";
 include ocamlcommon;
 setup-ocamlc.byte-build-env;
 program = "${test_build_directory}/ppx_error_message.exe";
 all_modules = "ppx_error_message.ml";
 ocamlc.byte;
 module = "test.ml";
 flags = "-I ${test_build_directory} -ppx ${program}";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

module Good = struct
  let () = [%sorted "abcd"]
end

module Bad = struct
  let () = [%sorted "not_in_alphabetical_order"]
end


