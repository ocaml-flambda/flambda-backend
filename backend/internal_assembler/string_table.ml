type t =
  { mutable current_length : int;
    mutable strings : string list
  }

(* An index of 0 in the string table is reserved for the null string *)
let create () = { current_length = 1; strings = [""] }

let add_string t string =
  t.current_length <- t.current_length + String.length string + 1;
  t.strings <- string :: t.strings

let current_length t = t.current_length

let write t sh_offset buf =
  let cursor = Owee.Owee_buf.cursor buf ~at:(Int64.to_int sh_offset) in
  List.iter (Owee.Owee_buf.Write.zero_string cursor) (List.rev t.strings)
