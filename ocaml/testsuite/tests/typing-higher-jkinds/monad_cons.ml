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
  return : 'a. 'a -> 'a 'm;
  bind : 'a 'b. 'a 'm -> ('a -> 'b 'm) -> 'b 'm;
}
|}]

let list = {
  return = (fun x -> [x]);
  bind = (fun xs f -> List.concat_map f xs)
}
[%%expect{|
val list : list monad = {return = <fun>; bind = <fun>}
|}]


let prod m x y = m.bind x (fun a -> m.bind y (fun b -> m.return (a, b)))
[%%expect{|
val prod :
  ('a : value => value) 'b 'c. 'a monad -> 'b 'a -> 'c 'a -> ('b * 'c) 'a =
  <fun>
|}]

let result = prod list [-1; 1] [0; 1; 2]
[%%expect{|
val result : (int * int) list =
  [(-1, 0); (-1, 1); (-1, 2); (1, 0); (1, 1); (1, 2)]
|}]

let option = {
  return = (fun x -> Some x);
  bind = Option.bind
}
[%%expect{|
val option : option monad = {return = <fun>; bind = <fun>}
|}]

let t1 = prod option (Some 5) (Some 7)
let t1 = prod option (Some 5) None
let t1 = prod option None (Some 7)
let t1 = prod option None None
[%%expect{|
val t1 : (int * int) option = Some (5, 7)
val t1 : (int * 'a) option = None
val t1 : ('a * int) option = None
val t1 : ('a * 'b) option = None
|}]

module type M = sig
  type t : top => value
  val a : int t
  val b : list t
  val c : monad t
end
[%%expect{|
module type M =
  sig type t : top => value val a : int t val b : list t val c : monad t end
|}]
