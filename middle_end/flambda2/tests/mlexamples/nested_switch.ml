(* Check that the nested switch (after inlining) is simplified away *)
type foo =
  | Foo
  | Bar

let[@inline] test foo x y = match foo with Foo -> x > y | Bar -> x < y

let f foo x y = match foo with Foo -> test foo x y | Bar -> false
