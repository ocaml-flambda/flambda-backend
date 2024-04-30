(* A1: zero_alloc check fails on [foo] because there is an allocation on the
   path to exceptional return from the call to [f x].
   This exception is handled in foo and then [foo] returns normally,
   making this allocation on a normal return of [foo].
*)
module A1 = struct
  let[@inline never][@local never] h s = [Random.int s ; Random.int s]

  let[@inline never][@local never][@zero_alloc] g x =
    if x < 0 then Sys.opaque_identity (x+1) else raise (Failure ((string_of_int) x))

  let[@inline never][@local never] handle_exn exn =
    (match exn with
     | Failure s -> print_string s; print_newline ()
     | _ -> failwith "Boo");
    0
  ;;

  let[@zero_alloc] f x =
    match g x with
    | exception exn ->
      (handle_exn[@zero_alloc assume never_returns_normally]) exn
      |> h |> List.hd  (* ignore this allocation *)
    | answer -> (answer * 2)

  let[@zero_alloc] foo x =
    try f x with
    | _ -> 0
end

(* A2 is a more elaborate version of A1 with more allocations in [foo]
   that are all correctly reported. *)
module A2 = struct
  let[@inline never][@local never] h s = [Random.int s ; Random.int s]

  let[@inline never][@local never][@zero_alloc] g x =
    if x < 0 then Sys.opaque_identity (x+1) else raise (Failure ((string_of_int) x))

  let[@inline never][@local never] handle_exn exn =
    (match exn with
     | Failure s -> print_string s; print_newline ()
     | _ -> failwith "Boo");
    0
  ;;

  let[@zero_alloc] f x =
    match g x with
    | exception exn ->
      (handle_exn[@zero_alloc assume never_returns_normally]) exn
      |> h |> List.hd  (* ignore this allocation *)
    | answer -> (answer * 2)

  let[@zero_alloc] foo x =
    try let y = f x in (x,y) with
    | _ ->
      Printf.printf "BOO %x" x;  (* ignore this indirect call *)
      (x,0)
end

(* A3: shows that "assume error" causes the check to ignore allocations
   in [foo] that come after the exceptional return from [f x]
   and correctly report allocations that happen after normal return
   from [f x]. *)
module A3 = struct
  let[@inline never][@local never] h s = [Random.int s ; Random.int s]

  let[@inline never][@local never][@zero_alloc] g x =
    if x < 0 then Sys.opaque_identity (x+1) else raise (Failure ((string_of_int) x))

  let[@inline never][@local never] handle_exn exn =
    (match exn with
     | Failure s -> print_string s; print_newline ()
     | _ -> failwith "Boo");
    0
  ;;

  let[@zero_alloc] f x =
    match g x with
    | exception exn ->
      (handle_exn[@zero_alloc assume error]) exn
      |> h |> List.hd  (* ignore this allocation *)
    | answer -> (answer * 2)

  let[@zero_alloc] foo x =
    try
      let y = f x in
      (x,y)  (* report this allocation *)
    with
    | _ ->
      Printf.printf "BOO %x" x;  (* ignore this indirect call *)
      (x,0)
end

(* A4: is a succesful version of A3, without allocation after normal return
   from [f x]. *)
module A4 = struct
  let[@inline never][@local never] h s = [Random.int s ; Random.int s]

  let[@inline never][@local never][@zero_alloc] g x =
    if x < 0 then Sys.opaque_identity (x+1) else raise (Failure ((string_of_int) x))

  let[@inline never][@local never] handle_exn exn =
    (match exn with
     | Failure s -> print_string s; print_newline ()
     | _ -> failwith "Boo");
    0
  ;;

  let[@zero_alloc] f x =
    match g x with
    | exception exn ->
      (handle_exn[@zero_alloc assume error]) exn
      |> h |> List.hd  (* ignore this allocation *)
    | answer -> (answer * 2)

  let[@zero_alloc] foo x =
    try f x
    with
    | _ ->
      Printf.printf "BOO %x" x;  (* ignore this indirect call *)
      0
end

(* A5: same as A4 but with "assume error" on the function definition instead
   of the call site. *)
module A5 = struct
  let[@inline never][@local never] h s = [Random.int s ; Random.int s]

  let[@inline never][@local never][@zero_alloc] g x =
    if x < 0 then Sys.opaque_identity (x+1) else raise (Failure ((string_of_int) x))

  let[@inline never][@local never][@zero_alloc assume error] handle_exn exn =
    (match exn with
     | Failure s -> print_string s; print_newline ()
     | _ -> failwith "Boo");
    0
  ;;

  let[@zero_alloc] f x =
    match g x with
    | exception exn ->
      handle_exn exn
      |> h |> List.hd  (* ignore this allocation *)
    | answer -> (answer * 2)

  let[@zero_alloc] foo x =
    try f x
    with
    | _ ->
      Printf.printf "BOO %x" x;  (* ignore this indirect call *)
      0
end

(* A6: shows that "assume error" is not enough to prove "strict",
   because the call to [string_of_int] is not known to not diverge.
*)
module A6 = struct
  let[@inline never][@local never] h s = [Random.int s ; Random.int s]

  let[@inline never][@local never][@zero_alloc] g x =
    if x < 0 then Sys.opaque_identity (x+1) else raise (Failure ((string_of_int) x))

  let[@inline never][@local never][@zero_alloc assume error] handle_exn exn =
    (match exn with
     | Failure s -> print_string s; print_newline ()
     | _ -> failwith "Boo");
    0
  ;;

  let[@zero_alloc strict] f x =
    match g x with
    | exception exn ->
      handle_exn exn
      |> h |> List.hd  (* ignore this allocation *)
    | answer -> (answer * 2)

end

(* A7 is the same as A6 but without [string_of_int] and we can prove strict annotation on
   [f]*)
module A7 = struct
  let[@inline never][@local never] h s = [Random.int s ; Random.int s]

  let[@inline never][@local never][@zero_alloc] g x =
    if x < 0 then Sys.opaque_identity (x+1) else raise (Failure "boo")

  let[@inline never][@local never][@zero_alloc assume error] handle_exn exn =
    (match exn with
     | Failure s -> print_string s; print_newline ()
     | _ -> failwith "Boo");
    0
  ;;

  let[@zero_alloc strict] f x =
    match g x with
    | exception exn ->
      handle_exn exn
      |> h |> List.hd  (* ignore this allocation *)
    | answer -> (answer * 2)

end

(* A8 is the same as A7 but with "assume never_returns_normally" only instead of
   "assume error" and it is not enough to prove "strict" on [f]. *)
module A8 = struct
  let[@inline never][@local never] h s = [Random.int s ; Random.int s]

  let[@inline never][@local never][@zero_alloc] g x =
    if x < 0 then Sys.opaque_identity (x+1) else raise (Failure "boo")

  let[@inline never][@local never][@zero_alloc assume never_returns_normally] handle_exn exn =
    (match exn with
     | Failure s -> print_string s; print_newline ()
     | _ -> failwith "Boo");
    0
  ;;

  let[@zero_alloc strict] f x =
    match g x with
    | exception exn ->
      handle_exn exn
      |> h |> List.hd  (* ignore this allocation *)
    | answer -> (answer * 2)

end
