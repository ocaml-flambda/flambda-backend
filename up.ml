external make_unboxed_pair_v_v : 'a -> 'b -> ('a, 'b) unboxed_pair = "%make_unboxed_pair_v_v"
external unboxed_pair_field_0_v_v : ('a, 'b) unboxed_pair -> 'a = "%unboxed_pair_field_0_v_v"
external unboxed_pair_field_1_v_v : ('a, 'b) unboxed_pair -> 'b = "%unboxed_pair_field_1_v_v"

let f i x y =
  let p = make_unboxed_pair_v_v x y in
  if i < 0 then unboxed_pair_field_0_v_v p
  else unboxed_pair_field_1_v_v p

external make_unboxed_pair_vup_vup :
  ('a, 'b) unboxed_pair -> ('c, 'd) unboxed_pair
  -> (('a, 'b) unboxed_pair, ('c, 'd) unboxed_pair) unboxed_pair
  = "%make_unboxed_pair_vup_vup"

external unboxed_pair_field_0_vup_vup :
  (('a, 'b) unboxed_pair, _) unboxed_pair
  -> ('a, 'b) unboxed_pair = "%unboxed_pair_field_0_vup_vup"

let g i x y =
  let p = make_unboxed_pair_v_v x y in
  let q = make_unboxed_pair_vup_vup p p in
  let p_again = unboxed_pair_field_0_vup_vup q in
  if i < 0 then unboxed_pair_field_0_v_v p_again
  else unboxed_pair_field_1_v_v p_again

external unboxed_pair_field_1_vup_vup :
  (_, ('a, 'b) unboxed_pair) unboxed_pair
  -> ('a, 'b) unboxed_pair = "%unboxed_pair_field_1_vup_vup"

let h i x y =
  let p = make_unboxed_pair_v_v x y in
  let p' = make_unboxed_pair_v_v y x in
  let q = make_unboxed_pair_vup_vup p p' in
  let r =
    if i < 0 then unboxed_pair_field_0_vup_vup q
    else unboxed_pair_field_1_vup_vup q
  in
  if i < 0 then unboxed_pair_field_0_v_v r
  else unboxed_pair_field_1_v_v r
