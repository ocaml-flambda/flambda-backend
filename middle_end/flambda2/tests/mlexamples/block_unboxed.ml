
type t = { foo : int; bar : float; }

let foo b x y =
  let f =
    if b
    then { foo = x; bar = y +. 1.; }
    else { bar = y +. 1.; foo = x; }
  in
  match f with
  | { foo; bar; } -> foo + (int_of_float bar)


