let run () =
  let fancy_int =
    Fancy_int.create 4
      (Fancy_int.Flourish.create 8)
      (Fancy_int.Ornament.create "fifteen")
  in
  let fancy_string =
    Fancy_string.create "sixteen"
      (Fancy_string.Flourish.create "twenty-three")
      (Fancy_string.Ornament.create "42")
  in
  print_endline (fancy_int |> Fancy_int.to_string);
  print_endline (fancy_string |> Fancy_string.to_string);
  fancy_int, fancy_string

let _ : Fancy_int.PI.t =
  (* See [Fancy__] for what we're testing here *)
  Fancy_int.PI.create ()
