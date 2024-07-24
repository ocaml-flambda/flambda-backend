(* TEST
  flags = "-extension layouts_alpha";
  expect;
*)

type ('m : value => value) monad = {
  return : 'a. 'a -> 'a 'm;
  bind : 'a 'b. 'a 'm -> ('a -> 'b 'm) -> 'b 'm
}

[%%expect{|
type ('m : value => value) monad = {
  return : 'a. 'a -> <Tapp>;
  bind : 'a 'b. <Tapp> -> ('a -> <Tapp>) -> <Tapp>;
}
|}]
