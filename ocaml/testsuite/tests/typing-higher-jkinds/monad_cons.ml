(* TEST
  flags = "-extension layouts_alpha";
  expect;
*)

type ('m : value => value) monad = {
  return : 'a. 'a -> 'a 'm;
  bind : 'a 'b. 'a 'm -> ('a -> 'b 'm) -> 'b 'm
}

[%%expect{|
type ('m : (value) => value) monad = {
  return : 'a. 'a -> <Tapp>;
  bind : 'a 'b. <Tapp> -> ('a -> <Tapp>) -> <Tapp>;
}
|}]

let list : list monad = {
  return = (fun x -> [x]);
  bind = (fun xs f -> List.concat_map f xs)
}

[%%expect{|
val singleton : 'a -> 'a list = <fun>
val concat_map : 'a list -> ('a -> 'b list) -> 'b list = <fun>
val list : list monad = {return = <fun>; bind = <fun>}
|}]


let prod m x y = m.bind x (fun a -> m.bind y (fun b -> m.return (a, b)))

let result = prod list [-1; 1] [0; 1; 2]


[%%expect{|
val prod :
  ('a : (value) => value) 'b 'c. 'a monad -> <Tapp> -> <Tapp> -> <Tapp> =
  <fun>
val xs : int list = [-1; 1]
val ys : int list = [0; 1; 2]
val result : <Tapp> = [(-1, 0); (-1, 1); (-1, 2); (1, 0); (1, 1); (1, 2)]
|}]
