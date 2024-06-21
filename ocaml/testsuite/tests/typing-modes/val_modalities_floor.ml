(* We test that cmi generaeted without mli has the modalities pushed to the
strongest instead of legacy *)

(* TEST
 readonly_files = "\
   def_portable.ml \
   def_nonportable.ml \
   use_portable.ml use_portable.bad.reference \
 ";
{
   setup-ocamlopt.byte-build-env;

   {
    src = "def_portable.ml";
    dst = "maybe_portable.ml";
    copy;

    module = "maybe_portable.ml";
    ocamlopt_byte_exit_status = "0";
    ocamlopt.byte;

    module = "use_portable.ml";

    ocamlopt_byte_exit_status = "0";
    ocamlopt.byte;
   }

   {
    src = "def_nonportable.ml";
    dst = "maybe_portable.ml";
    copy;

    module = "maybe_portable.ml";
    ocamlopt_byte_exit_status = "0";
    ocamlopt.byte;

    module = "use_portable.ml";
    compiler_output = "use_portable.bad.output";
    ocamlopt_byte_exit_status = "2";
    ocamlopt.byte;

    compiler_reference = "use_portable.bad.reference";
    check-ocamlopt.byte-output;
    }
}
*)
