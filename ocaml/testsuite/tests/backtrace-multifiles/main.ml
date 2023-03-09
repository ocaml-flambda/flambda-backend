(* TEST_BELOW *)

let test s =
  let s' = (String.cat[@inlined never]) "! " s in
  (Foo.h[@inlined]) s'; (* CR gbury: remove [@inlined] *)
  (Foo.print_stack[@inlined]) (); (* CR gbury: remove [@inlined] *)
  (print_endline[@inlined never]) "end of test"

let () =
  Printexc.record_backtrace true;
  test "foobar"

(* TEST

   readonly_files ="foo.ml"

   * setup-ocamlopt.byte-build-env
     compiler_directory_suffix = ".O3"
   ** ocamlopt.byte
      module = "foo.ml"
      flags = "-g -O3"
   *** ocamlopt.byte
       module = "main.ml"
       flags = "-g -O3"
   **** ocamlopt.byte
        module = ""
        all_modules = "foo.cmx main.cmx"
   ***** run
   ****** check-program-output

   * setup-ocamlopt.byte-build-env
     compiler_directory_suffix = ".Oclassic"
   ** ocamlopt.byte
      module = "foo.ml"
      flags = "-g -Oclassic"
   *** ocamlopt.byte
       module = "main.ml"
       flags = "-g -Oclassic"
   **** ocamlopt.byte
        module = ""
        all_modules = "foo.cmx main.cmx"
   ***** run
   ****** check-program-output

   * setup-ocamlopt.byte-build-env
     compiler_directory_suffix = ".O3-Oclassic"
   ** ocamlopt.byte
      module = "foo.ml"
      flags = "-g -O3"
   *** ocamlopt.byte
       module = "main.ml"
       flags = "-g -Oclassic"
   **** ocamlopt.byte
        module = ""
        all_modules = "foo.cmx main.cmx"
   ***** run
   ****** check-program-output

   * setup-ocamlopt.byte-build-env
     compiler_directory_suffix = ".Oclassic-O3"
   ** ocamlopt.byte
      module = "foo.ml"
      flags = "-g -Oclassic"
   *** ocamlopt.byte
       module = "main.ml"
       flags = "-g -O3"
   **** ocamlopt.byte
        module = ""
        all_modules = "foo.cmx main.cmx"
   ***** run
   ****** check-program-output

*)
