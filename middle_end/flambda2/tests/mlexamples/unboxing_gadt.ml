type foo =
  | A of float
  | B of int

type bar = Other

type 'a t =
  | Foo : foo -> foo t
  | Bar : bar t

let f (type a) : a t -> a = function Foo x -> x | Bar -> Other

let baz p x z =
  let a = if p then f x else B z in
  Sys.opaque_identity a
