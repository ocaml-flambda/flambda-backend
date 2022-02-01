let f a b c = 1
let f (local_ a) ~foo:(local_ b) ?foo:(local_ c= 1) ~(local_ d) = ()
let f () =
  let a = [local_ 1] in
  let local_ r = 1 in
  let local_ f : 'a . 'a -> 'a = fun x -> x in
  local_ "asdfasdfasdfasdfasdfasdfasdf"
type 'a r = {
  mutable a: 'a ;
  nonlocal_ b: 'a ;
  global_ c: 'a }
type ('a, 'b) cfn =
  a:local_ 'a -> ?b:local_ b -> local_ 'a -> (int -> local_ 'b)
