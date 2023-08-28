let f a b c = 1
let f (local_ a) ~foo:(local_ b) ?foo:(local_ c= 1) ~(local_ d) = ()
let f () =
  let a = [local_ 1] in
  let local_ r = 1 in
  let local_ f : 'a . 'a -> 'a = fun x -> x in
  local_ "asdfasdfasdfasdfasdfasdfasdf"
type 'a r = {
  i: int ;
  mutable a: 'a ;
  global_ c: 'a }
type t =
  | C of int * global_ string * float 
  | R of {
  i: int ;
  global_ s: string ;
  mutable x: float } 
type ('a, 'b) cfn =
  a:local_ 'a -> ?b:local_ b -> local_ 'a -> (int -> local_ 'b)
let () =
  let x = ((local_ ((42)[@inner ]))[@outer ]) in let local_ y = 47 in ()
let _ = (local_ 1) + (exclave_ 2)
