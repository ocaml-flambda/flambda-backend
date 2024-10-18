let[@local never][@inline never] bar y = Some y

let[@local never][@inline never][@zero_alloc assume_unless_opt] foo x =
  match bar x with
  | Some y -> y
  | None -> x

module Pass_check = struct

  let[@inline] bar y = Some y

  let[@zero_alloc assume_unless_opt] foo x =
    match (bar[@inlined]) x with
    | Some y -> y
    | None -> x

  let[@zero_alloc] test x = foo x

end

