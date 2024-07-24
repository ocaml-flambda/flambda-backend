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

let list : list monad = {
  return = (fun x -> [x]);
  bind = (fun xs f -> List.concat_map f xs)
}

[%%expect{|
val list : list monad = {return = <fun>; bind = <fun>}
|}]


let prod m x y = m.bind x (fun a -> m.bind y (fun b -> m.return (a, b)))

let result = prod list [-1; 1] [0; 1; 2]


[%%expect{|
val prod :
  ('a : value => value) 'b 'c. 'a monad -> <Tapp> -> <Tapp> -> <Tapp> = <fun>
val result : <Tapp> = <Tapp-val>
|}]
