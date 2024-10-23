(* TEST flags = "-w -71"; *)

module Test1 = struct
  let[@tail_mod_cons] rec f () = Some (g ())
  and[@tail_mod_cons] g () = false && true

  let () = assert (f () = Some false)
end

module Test2 = struct
  let[@tail_mod_cons] rec f () = Some (g ())
  and[@tail_mod_cons] g () = true || false

  let () = assert (f () = Some true)
end
