(* TEST
  flags = "-extension layouts_alpha";
  expect;
*)

type ('m : value => value) monad = {
  return : 'a. 'a -> 'a 'm;
  bind : 'a 'b. 'a 'm -> ('a -> 'b 'm) -> 'b 'm
}

[%%expect{|
Uncaught exception: Failure("General type application is not implemented")

|}]

let list : list monad = {
  return = fun x -> [x];
  bind = fun xs f -> List.concat_map f xs
}

[%%expect{|
Line 1, characters 16-21:
1 | let list : list monad = {
                    ^^^^^
Error: Unbound type constructor monad
|}]
