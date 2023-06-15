let f (Gadt.Mk (produce, consume)) =
  let (surely_is_a_value, _) = (produce (), 5) in
  consume surely_is_a_value
