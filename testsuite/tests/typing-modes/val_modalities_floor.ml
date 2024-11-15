(* TEST
 readonly_files = "\
   def_portable.ml \
   use_portable.ml use_portable.reference \
  ";
{
   setup-ocamlopt.byte-build-env;
   flags = "-extension mode_alpha";

   {
    src = "def_portable.ml";
    dst = "maybe_portable.ml";
    copy;

    module = "maybe_portable.ml";
    ocamlopt_byte_exit_status = "0";
    ocamlopt.byte;

    module = "use_portable.ml";
    compiler_output = "use_portable.output";
    ocamlopt_byte_exit_status = "2";
    ocamlopt.byte;

    compiler_reference = "use_portable.reference";
    check-ocamlopt.byte-output;
    }
}
*)

(* We test that cmi generated without mli has the modalities pushed to the
legacy instead of strongest *)
