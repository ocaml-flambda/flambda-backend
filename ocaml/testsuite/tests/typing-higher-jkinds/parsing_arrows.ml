(* TEST
  flags = "-extension layouts_alpha";
  expect;
*)

type p : value => value
type q : (value, value) => value
type r : (value => value, value) => value
type s : (value, value mod local) => value
type t : (value) => (value) => value

[%%expect{|
type p : (value) => value
type q : (value, value) => value
type r : ((value) => value, value) => value
type s : (value, value mod local) => value
type t : (value) => (value) => value
|}]
