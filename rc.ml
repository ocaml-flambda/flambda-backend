let nonopt x a b =
  if x >= 0 && x <= a then a else b

let opt x b =
  let a = 10000 in
  if x >= 0 && x <= a then a else b

let opt_rev x b =
  let a = 10000 in
  if x <= a && x >= 0 then a else b

let opt_l x b =
  let a = 10000 in
  if 0 <= x && a >= x then a else b

let opt_l_rev x b =
  let a = 10000 in
  if a >= x && 0 <= x then a else b

let opt_strict x b =
  let a = 10000 in
  if x >= 0 && x < a then a else b

let opt_strict_rev x b =
  let a = 10000 in
  if x < a && x >= 0 then a else b

let opt_strict_l x b =
  let a = 10000 in
  if 0 <= x && a > x then a else b

let opt_strict_l_rev x b =
  let a = 10000 in
  if a > x && 0 <= x then a else b
