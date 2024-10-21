(* Disable warning 8 as it's unrelated to this test: It's the inexhaustive
   match warning when you don't have a wildcard case for extensible variants.
*)
let[@ocaml.warning "-8"] f (Gadt_extensible.Mk (produce, consume)) =
  let (surely_is_a_value, _) = (produce (), 5) in
  consume surely_is_a_value
