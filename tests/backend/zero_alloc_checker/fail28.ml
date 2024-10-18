[@@@zero_alloc all_opt]

let foo x y = (x,y)

let bar x y = [x;y]

let outer x =
  (** [inner] function should also show in the error message. *)
  let[@inline never][@local never][@specialize never] inner x =
    if x > 0 then (x,x)
    else raise (Failure "boo")
  in
  inner (x + 1)

let[@zero_alloc assume] do_not_check_me x =
  (x,x+1)

let[@zero_alloc strict opt] only_check_me_in_opt x y =
  (x,y,x+y)

let[@zero_alloc strict] check_me_strict x y =
  if x > 0 then 0
  else raise (Failure (Printf.sprintf "not positive %d\n" x))
