(* Parameters: (none) *)

let run () =
  let basic_int = Basic_int.create 4 in
  let basic_string = Basic_string.create "sixteen" in
  print_endline (basic_int |> Basic_int.to_string);
  print_endline (basic_string |> Basic_string.to_string);
  basic_int, basic_string
