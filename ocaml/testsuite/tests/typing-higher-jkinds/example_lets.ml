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
  return : 'a. 'a -> 'a  'm;
  bind : 'a 'b. 'a  'm -> ('a -> 'b  'm) -> 'b  'm;
}
|}]

let monad_let (m : 'm monad) =
  let return = m.return in
  let (let*) = m.bind in
  let (let+) x f =
    let* a = x in
    return (f a)
  in
  let (and*) x y =
    let* a = x in
    let* b = y in
    return (a, b)
  in
  let (and+) = (and*) in
  (return, (let*), (let+), (and*), (and+))
[%%expect{|
val monad_let :
  ('m : value => value) 'a 'b 'c 'd 'e 'f 'g 'h 'i.
    'm monad ->
    ('a -> 'a  'm) * ('b  'm -> ('b -> 'c  'm) -> 'c  'm) *
    ('d  'm -> ('d -> 'e) -> 'e  'm) * ('f  'm -> 'g  'm -> ('f * 'g)  'm) *
    ('h  'm -> 'i  'm -> ('h * 'i)  'm) =
  <fun>
|}]

let both m x y =
  let return, (let*), _, (and*), _ = monad_let m in
  let* a = x
  and* b = y in
  return (a, b)
[%%expect{|
val both :
  ('a : value => value) 'b 'c. 'a monad -> 'b  'a -> 'c  'a -> ('b * 'c)  'a =
  <fun>
|}]
