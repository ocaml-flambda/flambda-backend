
type t =
  | Foo1 of { foo : int; bar : int; }
  | Foo2 of { foo : int; bar : float; }
  | Foo3 of { foo : int; foobar : string; }

let foo b b' x1 x2 y =
  let f =
    if b then
      if b' then
        Foo1 { foo = x1; bar = x2; }
      else
        Foo1 { foo = x2; bar = x1; }
    else
      Foo2 { foo = x2; bar = y; }
  in
  match f with
  | Foo1 { foo; bar; } -> foo + bar
  | Foo2 { foo; bar; } -> foo + (int_of_float bar)
  | Foo3 _ -> assert false

