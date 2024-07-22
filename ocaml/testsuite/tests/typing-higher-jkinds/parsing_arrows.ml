(* TEST
  expect;
*)

type p : value => value
type q : (value) => value
type r : (value => value) => value
type s : (value, value) => value
type t : (value => value, value) => value
type u : (value, value mod local) => value
type v : (value) => (value) => value
type w : value => value => value

[%%expect{|
Line 1, characters 9-23:
1 | type p : value => value
             ^^^^^^^^^^^^^^
Error: Arrow jkind (=>) syntax parsed, but annotations are not implemented
|}]
