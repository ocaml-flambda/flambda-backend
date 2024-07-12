(* TEST
  expect;
  flags = "-g";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
  check-ocamlc.byte-output;
*)

type p : value => value
type q : (value, value) => value
type r : (value => value, value) => value
type s : (value, value mod local) => value
type t : (value) => ((value) => value)

[%%expect{|
Uncaught exception: Jkind.Unexpected_higher_jkind("Arrow annotation parsing is not implemented yet")

|}]
