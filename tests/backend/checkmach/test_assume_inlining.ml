exception Exn of int

(* don't inline bar : passes *)
module A1 = struct
  let[@inline never] f x = if x = 0 then raise (Exn x) else x * x

  let[@inline never] g x n =
    let y = x+n in if  y > 10 then raise (Exn y) else y

  let[@zero_alloc assume][@inline never] bar x =
    try
      f x
    with
    | Exn n -> g x n

  let[@zero_alloc] foo x =
    bar x
end

(* inline bar : passes *)

module A2 = struct
  let[@inline never] f x = if x = 0 then raise (Exn x) else x * x

  let[@inline always] g x n =
    let y = x+n in if  y > 10 then raise (Exn y) else y

  let[@zero_alloc assume][@inline always] bar x =
    try
      f x
    with
    | Exn n -> g x n

  let[@zero_alloc] foo x =
    bar x
end


(* inline bar and assume f is strict: passes *)

module A3 = struct
  let[@inline never][@zero_alloc assume strict] f x = if x = 0 then raise (Exn x) else x * x

  let[@inline never] g x n =
    let y = x+n in if  y > 10 then raise (Exn y) else y

  let[@zero_alloc assume][@inline always] bar x =
    try
      f x
    with
    | Exn n -> g x n

  let[@zero_alloc] foo x =
    bar x
end

(* inline bar and assume f is strict and f is inlined: passes *)

module A4 = struct
  let[@inline always][@zero_alloc assume strict] f x = if x = 0 then raise (Exn x) else x * x

  let[@inline never] g x n =
    let y = x+n in if  y > 10 then raise (Exn y) else y

  let[@zero_alloc assume][@inline always] bar x =
    try
      f x
    with
    | Exn n -> g x n

  let[@zero_alloc] foo x =
    bar x
end


(* inline bar and assume g never returns normally: passes *)

module A5 = struct
  let[@inline never] f x = if x = 0 then raise (Exn x) else x * x

  let[@inline never][@zero_alloc assume never_returns_normally] g x n =
    let y = x+n in if  y > 10 then raise (Exn y) else y

  let[@zero_alloc assume][@inline always] bar x =
    try
      f x
    with
    | Exn n -> g x n

  let[@zero_alloc] foo x =
    bar x
end


(* inline bar and assume g never returns normally and inline g: passes *)

module A6 = struct
  let[@inline never] f x = if x = 0 then raise (Exn x) else x * x

  let[@inline always][@zero_alloc assume never_returns_normally] g x n =
    let y = x+n in if  y > 10 then raise (Exn y) else y

  let[@zero_alloc assume][@inline always] bar x =
    try
      f x
    with
    | Exn n -> g x n

  let[@zero_alloc] foo x =
    bar x
end
