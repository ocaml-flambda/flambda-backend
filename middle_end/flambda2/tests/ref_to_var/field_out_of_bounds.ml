external id : 'a -> 'a = "%opaque"

type foo = { x : int }

type bar =
  { x : int;
    mutable y : int
  }

type 'a kind =
  | Foo : foo kind
  | Bar : bar kind

let rightmost_field (type a) (k : a kind) (t : a) : int =
  match k, t with Foo, { x } -> x | Bar, { x; y } -> y

let foobar x =
  let k = id Foo in
  let t = { x } in
  (rightmost_field [@inlined]) k t
