(* TEST *)
module Bug : sig
end = struct
  let app fn = fn 1 2 3
  let pair_opt ?opt:_ ~kw:_ a b c = a + b + c
  let[@inline never] go () =
    app (pair_opt ~kw:())
  let _ =
    Printf.printf "%d\n" (go ())
end

