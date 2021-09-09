type float_rec =
  { a : float;
    b : float
  }

type (_, _) c =
  | F : (float_rec, unit) c
  | B : (int * int, int * int) c

let f (type x y) (b : (x, y) c) w x (y : y) : x * unit =
  let x : x =
    match b with
    | F -> { a = w; b = x }
    | B ->
      let u, v = y in
      (* This creates a row_like of type Other ( value, value ) *)
      let _ = Sys.opaque_identity (u + v) in
      y
  in
  (* Here happens the join of Tag_254 ( float, float ) with Other ( value, value
     ) If nothing was done to prevent it, there would be a join between products
     with different kinds, which is prohibited *)
  x, ()
