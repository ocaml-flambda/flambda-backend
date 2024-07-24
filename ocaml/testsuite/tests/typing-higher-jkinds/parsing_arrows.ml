(* TEST
  flags = "-extension layouts_alpha";
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
type p : value => value
type q : value => value
type r : (value => value) => value
type s : (value, value) => value
type t : (value => value, value) => value
type u : (value, value mod local) => value
type v : value => value => value
type w : value => value => value
|}]
