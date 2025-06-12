(* TEST
  readonly_files = "gen-native.sh test_byte.ml test_native.ml";

  setup-ocamlc.byte-build-env;

  (* Test that we've got [test_byte.ml] and [test_native.ml] in sync. If this fails, run
     [gen-native.sh] in this directory. *)

  script="./gen-native.sh --check";
  script;
*)
