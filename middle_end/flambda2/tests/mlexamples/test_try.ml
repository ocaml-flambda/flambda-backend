external op_id : 'a -> 'a = "%opaque"
external raise : exn -> 'a = "%raise"

let _ =
  try if op_id true then raise Not_found else 0
  with Not_found -> 1

let _ =
  match (if op_id true then raise Not_found else None) with
  | Some x -> x
  | None | exception Not_found -> 0
