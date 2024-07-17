(* TEST
  expect;
  ocamlc_byte_exit_status = "2";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
  check-ocamlc.byte-output;
*)

type p : value => value
type q : (value, value) => value
type r : (value => value, value) => value
type s : (value, value mod local) => value
type t : (value) => (value) => value

[%%expect{|
Line 1, characters 9-23:
1 | type p : value => value
             ^^^^^^^^^^^^^^
Error: Arrow jkind (=>) syntax parsed, but annotations are not implemented
|}]
