(* TEST
 reason = "Need partial application";
 skip;
 flags = "-extension layouts_alpha";
 expect; *)

type (_, _ : value => value) freer =
  | Pure : 'a -> ('a, 'm) freer
  | Impure : 'a 'm * ('a -> ('b, 'm) freer) -> ('b, 'm) freer
;;
[%%expect{|
type (_, _ : value => value) freer =
    Pure : 'a ('m : value => value). 'a -> ('a, 'm) freer
  | Impure : 'b ('m : value => value) 'a. 'a 'm *
      ('a -> ('b, 'm) freer) -> ('b, 'm) freer
|}]

type ('f : value => value) func = {
  map : 'a 'b. ('a -> 'b) -> ('a 'f -> 'b 'f)
}
type ('m : value => value) monad = {
  return : 'a. 'a -> 'a 'm;
  bind : 'a 'b. 'a 'm -> ('a -> 'b 'm) -> 'b 'm
}
[%%expect{|
type ('f : value => value) func = {
  map : 'a 'b. ('a -> 'b) -> 'a 'f -> 'b 'f;
}
type ('m : value => value) monad = {
  return : 'a. 'a -> 'a 'm;
  bind : 'a 'b. 'a 'm -> ('a -> 'b 'm) -> 'b 'm;
}
|}]

let comp (monad : 'm monad) f g a = monad.bind (f a) (fun b -> g b)
[%%expect{|
val comp :
  ('m : value => value) 'a 'b 'c.
    'm monad -> ('a -> 'b 'm) -> ('b -> 'c 'm) -> 'a -> 'c 'm =
  <fun>
|}]


let free_monad_return a = Pure a
[%%expect{|
val free_monad_return : 'a ('b : value => value). 'a -> ('a, 'b) freer =
  <fun>
|}]

let free_monad_bind func m k =
  let rec bind : type a b (f : value => value). (a, f) freer -> (a -> (b, f) freer) -> (b, f) freer =
    let comp f g a = bind (f a) (fun b -> g b) in
    fun m k -> match m with
    | Pure a -> k a
    | Impure (m, k') -> Impure (m, comp k' k)
  in bind m (fun a -> k a)
[%%expect{|
val free_monad_bind :
  'a 'b ('c : value => value) 'd.
    'a -> ('b, 'c) freer -> ('b -> ('d, 'c) freer) -> ('d, 'c) freer =
  <fun>
|}]

let free_monad func = {
  return = free_monad_return;
  bind = free_monad_bind
}
[%%expect{|
Line 2, characters 11-28:
2 |   return = free_monad_return;
               ^^^^^^^^^^^^^^^^^
Error: This expression has type 'a -> ('a, 'b) freer
       but an expression was expected of type 'a -> 'a 'c
       The layout of freer is ((value, ((value) => value)) => value), because
         of the definition of freer at lines 1-3, characters 0-61.
       But the layout of freer must be a sublayout of ((value) => value), because
         of the definition of monad at lines 4-7, characters 0-1.
|}]
