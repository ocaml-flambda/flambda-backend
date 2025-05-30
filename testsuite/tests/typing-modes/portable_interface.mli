(* TEST
    readonly_files = "portable_interface.mli use_portable_interface.ml";
    flags += "-extension mode_alpha";
    setup-ocamlc.byte-build-env;
    module = "portable_interface.mli";
    ocamlc.byte;
    module = "use_portable_interface.ml";
    ocamlc.byte;
    check-ocamlc.byte-output;
*)

@@portable

val foo : 'a -> 'a
val bar : 'a -> 'a @@ nonportable
