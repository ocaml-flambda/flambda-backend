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

(* same as A2 but the assume is on the call and not the definition of bar:
   fails. Needs change in the middle-end to propagate the assume to
   try-with body as strict after inlining. *)
module A7 = struct
  let[@inline never] f x = if x = 0 then raise (Exn x) else x * x

  let[@inline always] g x n =
    let y = x+n in if  y > 10 then raise (Exn y) else y

  let[@inline always] bar x =
    try
      f x
    with
    | Exn n -> g x n

  let[@zero_alloc] foo x =
    (bar[@zero_alloc assume]) x
end

(* Show that propagating assume to inlined trywith body is not sound
   in some cases. When bar is not inlined - the check fails. *)
module A8 = struct
  let[@inline never][@zero_alloc] f x = if x = 0 then
      raise (Exn x) else x * x

  let[@inline never][@zero_alloc] g x n =
    let y = x+n in if  y > 10 then raise (Exn y) else y

  let[@inline never][@zero_alloc assume] bar x =
    try
      f x
    with
    | Exn n -> n

  let[@zero_alloc] foo x =
    try bar x with
    | _ ->
      (Sys.opaque_identity 0)
end

(* Same as A8 except bar is inlined and the check passes.
   Inlining does not eliminate any annotations in this case,
   the check passes because of the heuristic we use to
   propagate "assume" when try-with body is inlined.
*)
module A9 = struct
  let[@inline never][@zero_alloc] f x = if x = 0 then
      raise (Exn x) else x * x

  let[@inline never][@zero_alloc] g x n =
    let y = x+n in if  y > 10 then raise (Exn y) else y

  let[@inline always][@zero_alloc assume] bar x =
    try
      f x
    with
    | Exn n -> n

  let[@zero_alloc] foo x =
    try bar x with
    | _ ->
      (Sys.opaque_identity 0)
end


(* Same as A2 but using "match-exception" instead of "try-with". *)
module A10 = struct
  let[@inline never] f x = if x = 0 then raise (Exn x) else x * x

  let[@inline always] g x n =
    let y = x+n in if  y > 10 then raise (Exn y) else y

  let[@zero_alloc assume][@inline always] bar x =
    match
      f x
    with
    | exception (Exn n) -> g x n
    | r -> r

  let[@zero_alloc] foo x =
    bar x
end
